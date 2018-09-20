namespace CorrelationDrawing.LogNodes

  module Svg =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open CorrelationDrawing
    open Helper
    open Recursive

    module Calc =
      let offset (nodeLevel : IMod<int>) (nodeOffset : float)
                 ((secondaryLevel : int), (secondaryLevelWidth : float)) = 
        adaptive {
          let! lvl = nodeLevel 
          return match (lvl = secondaryLevel), lvl = 0 with
                  | true, true  -> nodeOffset + secondaryLevelWidth
                  | false, true -> nodeOffset + secondaryLevelWidth
                  | true, false -> nodeOffset
                  | false, false -> nodeOffset
        }



    let rec createView  (offset        : float)
                        (secondaryLvl  : (int * float))
                        (model         : MLogNode) 
                        (viewFunction  : float 
                                        -> MLogNode 
                                        -> IMod<(float 
                                                  -> Option<float> 
                                                  -> list<DomNode<'msg>>
                                        )>
                        )
                        : alist<DomNode<'msg>> =
      let (secLvlNr, breadthSec) = secondaryLvl 
      let offset = Calc.offset model.level offset secondaryLvl
          
      let childrenView () = 
        alist {
          let! os = offset
          for c in model.children do               
            let v = (createView os secondaryLvl c viewFunction)
            yield! v
        }
    
      let rval =
        alist {
          let! offset = offset
          let! selfViewFunction = viewFunction offset model
          let! hasCs = hasChildren model
          let selfView = selfViewFunction offset None
          let! lvl = model.level 
          if lvl = secLvlNr then
            for v in (selfViewFunction 0.0 (Some breadthSec)) do yield v
          match hasCs with
            | false  -> 
                for v in selfView do
                  yield v                
            | true   ->  
              let! hasHierarchicalNodes = (hasHierarchicalNodes' model)
              if not hasHierarchicalNodes then 
                for v in selfView do
                  yield v 
              yield (Svg.Attributes.toGroup' (childrenView ()) AMap.empty)           
        }
      rval
    
    module private Default =
      //let voidDomNode<'msg> = 
      //  DomNode.Void<'msg> ("",(AttributeMap.ofList []))
      let debugOutput (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.Attributes.toGroup
          [Svg.Base.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
          (Svg.Events.onClickAttribute selectionCallback)

      let angularNode (yPos : float) (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.Attributes.toGroup
          [Svg.Base.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
          (Svg.Events.onClickAttribute selectionCallback)

      let metricNode (yPos : float) (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.Attributes.toGroup
          [Svg.Base.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
          (Svg.Events.onClickAttribute selectionCallback)

      let levelToWeight (level : int) = //TODO hardcoded
        ((8.0 - (float level)) * 0.3)



    let private createDomNode    // (viewType            : CorrelationPlotViewType)
                          (nodeType            : LogNodeType)
                          (size                : V2d)
                          (color               : C4b)
                          (uBorderColor        : C4b)
                          (lBorderColor        : C4b)
                          (dottedRBorder       : bool)
                          (weight              : float)
                          (selectionCallback   : list<string> -> 'msg)
                          (buttons             : Option<(DomNode<'msg> * DomNode<'msg>)>)
                          (isSelected          : bool) 
                          (position            : V2d)
                          (offset              : float) 
                          (secondaryLvlBreadth : Option<float>): list<DomNode<'msg>> =
          
      let (size, position, offset) =
        match secondaryLvlBreadth with
          | Some b -> (new V2d(b, size.Y), new V2d(0.0, position.Y), position.X)
          | None   -> (size, position, offset + position.X)

      let drawRectangle () =  
        let rfun = Svg.Base.drawBorderedRectangle
                    (new V2d(offset, position.Y))
                    size.X size.Y
                    color lBorderColor uBorderColor
                    weight
                    selectionCallback
                    isSelected
        match dottedRBorder with
                | true ->
                    rfun true
                | false ->
                    rfun false

      let btns = 
        match buttons with
          | Some (lb, ub) -> [lb;ub]
          | None -> []

      let domNode = 
        match nodeType with
            | LogNodeType.Angular -> []
            | LogNodeType.Metric  -> []
            | LogNodeType.Hierarchical -> [drawRectangle ()]@btns
            | LogNodeType.HierarchicalLeaf -> [drawRectangle ()]@btns
            | LogNodeType.PosInfinity
            | LogNodeType.NegInfinity ->
                [Default.debugOutput "LogNode neg or pos infinity" selectionCallback]
            | LogNodeType.Empty | LogNodeType.Infinity ->
                [Default.debugOutput "LogNode empty or infinity" selectionCallback]
      domNode


    let private getDomNodeFunction // (viewType            : IMod<CorrelationPlotViewType>) 
                            (flags               : IMod<SvgFlags>)
                            (options             : MSvgOptions) 
                            (styleFun            : float -> IMod<LogAxisSection>) //TODO rename
                            (selectAction        : LogNodeId -> 'a)
                            (mapper              : Border.Action -> 'a)
                            (offset              : float) 
                            (model               : MLogNode) =
      adaptive {
        let! size          =  (model.svgSize)
        let! s             = styleFun size.X
        let! pos           = model.svgPos
        let size           = new V2d(size.X, size.Y)
        let color          = s.color
        let! dottedRBorder = model.hasDefaultX
        let! (lvl : int)   = model.level
        let weight         = (Default.levelToWeight lvl)
        let! isSelected    = model.isSelected
        let position       = new V2d(offset, pos.Y)
        let! lnType        = model.nodeType
        //let! viewType      = viewType
        let! flags         = flags

        let! (uBorderColor, lBorderColor) =
          match (Flags.isSet SvgFlags.BorderColour flags) with
            | true ->
              (model.uBorder.color, model.lBorder.color) //change color to anno/color
            | false ->
              (Mod.constant C4b.Black, Mod.constant C4b.Black)
        let! sLvlWidth = options.secLevelWidth
        let! (btnL, btnU) = 
              (Border.Svg.getCorrelationButtons model (offset + sLvlWidth) weight)// buttonCallback) //TODO performance
        let btns = (btnL |> UI.map mapper , btnU |> UI.map mapper)
        let btns = 
          match (Flags.isSet SvgFlags.EditCorrelations flags) with
            | true  -> Some btns
            | false -> None
        let selCb = (fun lst -> selectAction model.id)


        
        let f = (
                  createDomNode //viewType
                                lnType
                                size                          
                                color             
                                uBorderColor     
                                lBorderColor     
                                dottedRBorder        
                                weight            
                                selCb //(selectionCallback model.id)
                                btns
                                isSelected       
                                position
                )      
        return f
      }





    let view  (model        : MLogNode) 
              (secondaryLvl : IMod<int>)
              (flags        : IMod<SvgFlags>)
              (options      : MSvgOptions)
              (styleFun     : float -> IMod<LogAxisSection>) =
      let f = getDomNodeFunction 
                flags options styleFun 
                (ToggleSelectNode) 
                (BorderMessage)
                  
      adaptive {
        let! sLvl = secondaryLvl
        let! sLevelWidth = options.secLevelWidth
        return createView 0.0 (sLvl, sLevelWidth) model f          
      } 


/////////////////////
    // calculate the size of nodes in svg. 
    let calcSizeX (model : LogNode) (xAxis : SemanticId) (xAxisScaleFactor : float) = 
      let metricNodes = childrenWith model (fun n -> n.lBorder.anno.semanticId = xAxis)
      let metricValues = metricNodes |> List.map (fun n -> calcMetricValue n)
      let sizeX = (metricValues |> List.filterNone |> List.maxOrZero) * xAxisScaleFactor
      {model with svgSize = (model.svgSize * V2d.OI) + (V2d.IO) * sizeX}


    let xPosAndSize (id : SemanticId) 
                    (xAxisScaleFactor : float) 
                    (nodes : plist<LogNode>) : (plist<LogNode> * float) =
      let updNodes = 
        nodes 
          |> LogNodes.Recursive.applyAll (fun n -> (calcSizeX n id xAxisScaleFactor))

      // nodes without grainsize/Annotations of x-Axis semantic type get avg
      let sizes = 
        updNodes
          |> PList.filter (fun n -> n.svgSize.X <> 0.0) 
          |> PList.map (fun (n : LogNode) -> n.svgSize.X)

      if sizes.Count = 0 then printfn "%s" "calc svg x position/size failed" //TODO if list empty //TODO bug/refactor
        
      let avg = 
        match (PList.averageOrZero sizes) with
          | n when n = 0.0 -> 10.0 //TODO hardcoded size if no grain size annotations in log
          | n -> n

      let updNodes =
        updNodes |> PList.map (fun n -> LogNodes.Recursive.defaultIfZero n avg)
      (updNodes, avg)



    let yPosAndSize (logHeight : float) (optMapper : option<float>) (plst : plist<LogNode>) =
      let calcSvgFactor availableHeight (logNodes : list<LogNode>) = 
        let accumulatedHeight =
          logNodes |> List.sumBy (fun x -> (abs (Rangef.calcRangeNoInf x.range)))
        availableHeight / accumulatedHeight

      let calcSvgX (svgFactor : float) (node : LogNode) =
        {node with svgSize = V2d.OO 
                               + V2d.OI 
                               * (Rangef.calcRangeNoInf node.range) 
                               * svgFactor
        }

      let calcCurrentY (prev : LogNode) (current : LogNode) =
        {current with svgPos = V2d(0.0, prev.svgPos.Y + prev.svgSize.Y)}                

      let rec calcRecYPosSize (height : float) (startAt : float) (nodes : plist<LogNode>) =
        let lst = PList.toList nodes
        match lst with 
          | [] -> PList.empty
          | lst ->
              let factor = calcSvgFactor height lst
              let result = 
                lst
                  |> List.map (calcSvgX factor)
                  |> List.scan calcCurrentY {LogNodes.Init.empty with svgPos = V2d(0.0, startAt)}
                  |> List.tail  
                  |> List.map (fun (x : LogNode) ->
                                {x with children = calcRecYPosSize 
                                                    x.svgSize.Y
                                                    x.svgPos.Y
                                                    x.children
                                }
                )
                  |> PList.ofList
              result




      let lst = PList.toList plst
      match lst with 
        | [] -> (PList.empty, 1.0)
        | lst ->

        let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRangeNoInf x.range)))
        let factor = 
          match optMapper with
            | None -> logHeight / accHeight
            | Some m -> m
        let result = 
          lst
            |> List.map (fun (n : LogNode) ->
                          {n with svgSize = V2d.OO 
                                              + V2d.OI
                                              * (Rangef.calcRangeNoInf n.range) 
                                              * factor
                                  nativeSize = V2d.OO 
                                                + V2d.OI 
                                                * (Rangef.calcRangeNoInf n.range) 
                                                  
                          }
                        )
            |> List.scan (fun (a : LogNode) (b : LogNode) -> 
                              {b with svgPos    = V2d(b.svgPos.X, a.svgPos.Y + a.svgSize.Y)
                                      nativePos = V2d(b.nativePos.X, a.nativePos.Y + a.nativeSize.Y)
                              }
                          ) (LogNodes.Init.empty) 
            |> List.tail  
            |> List.map (fun (x : LogNode) ->
                          {x with children = calcRecYPosSize 
                                              x.svgSize.Y
                                              x.svgPos.Y
                                              x.children
                          }
                        )
            |> PList.ofList
        (result, factor)

