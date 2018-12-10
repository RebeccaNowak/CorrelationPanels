namespace CorrelationDrawing.LogNodes

  module Svg =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open CorrelationDrawing
    open Helper
    open Recursive

    type SecondaryLevelDescription = {
      level    : IMod<NodeLevel>
      svgWidth : IMod<float>
    }


    module PreCalc =
      let offset (lvl : NodeLevel) (nodeOffset : float)
                 (secLvl : NodeLevel) (secLvlWidth : float) =
        match (lvl = secLvl), lvl.level = 0 with
            | true, true  -> nodeOffset + secLvlWidth
            | false, true -> nodeOffset + secLvlWidth
            | true, false -> nodeOffset
            | false, false -> nodeOffset
        

        

 ///////////////////////////////////////////////////////////////////////////////////////////////
    module Calc =
      open System.Runtime.InteropServices

      //let offset (nodeLevel : IMod<NodeLevel>) (nodeOffset : float)
      //           (secondaryLevel : SecondaryLevelDescription) =
      //           //((secondaryLevel : NodeLevel), (secondaryLevelWidth : float)) = 
      //  adaptive {
      //    let! secLvl = secondaryLevel.level
      //    let! secLvlWidth = secondaryLevel.svgWidth
      //    let! lvl = nodeLevel 
      //    return match (lvl = secLvl), lvl.level = 0 with
      //            | true, true  -> nodeOffset + secLvlWidth
      //            | false, true -> nodeOffset + secLvlWidth
      //            | true, false -> nodeOffset
      //            | false, false -> nodeOffset
      //  }

      let sizeY (factor : float) (n : LogNode) : LogNode =
        {n with svgSize = {width = n.svgSize.X; height = n.svgSizeY factor}
                nativeSize = {width = n.nativeSize.X; height = n.nativeSizeY}
        }   
        
      let svgFactor (availableHeight : float) (logNodes : list<LogNode>) = 
        let accumulatedHeight =
          logNodes |> List.sumBy (fun x -> (abs (Rangef.calcRangeNoInf x.range)))
        availableHeight / accumulatedHeight

      let yStart (prev : LogNode) (current : LogNode) =
        let svgPosX = 
          match current.nodeType with
           | LogNodeType.Angular | LogNodeType.Metric -> //TODO need posX for rose diagrams
             prev.svgPos.X + prev.svgSize.X
           | _ -> 0.0
        let svgPosY = (prev.svgPos.Y + prev.svgSize.Y)

        {current with svgPos    = V2d(svgPosX, svgPosY)
                      nativePos = V2d.withY current.nativePos 
                                            (prev.nativePos.Y + prev.nativeSize.Y)}  

      let rec yPosAndSize (height     : float) 
                          (startAt    : float) 
                          (svgMapper  : option<float>) 
                          (nodes      : plist<LogNode>) =
        match nodes.IsEmptyOrNull () with 
          | true -> (PList.empty, 0.0)
          | false ->
            let lst = PList.toList nodes
            let factor = 
              match svgMapper with
                | None -> (svgFactor height lst)
                | Some m -> m
            let updNodes = 
              lst
                |> List.map (sizeY factor)
                |> List.scan yStart {LogNodes.Init.empty with svgPos = V2d(0.0, startAt)}
                |> List.tail  
                |> List.map (fun (x : LogNode) ->
                              let (c, f) = 
                                yPosAndSize x.svgSize.Y x.svgPos.Y
                                                (Some factor) x.children
                              {x with children = c}
                            )
                |> PList.ofList
            (updNodes, factor)

      let sizeX (model : LogNode) (xAxis : SemanticId) 
                (xAxisScaleFactor : float) (annoApp : AnnotationApp)= 
        
        let metricNodes = 
          childrenWith model (fun n -> 
                                  let sId = LogNodes.Helper.semanticIdOrInvalid n annoApp
                                  sId = xAxis
                             )
        let metricValues = metricNodes |> List.map (fun n -> calcMetricValue n annoApp)
        let sizeX = (metricValues |> List.filterNone |> List.maxOrZero) * xAxisScaleFactor
        {model with svgSize = {width = sizeX; height = model.svgSize.height}}


      let xPosAndSize (id               : SemanticId) 
                      (xAxisScaleFactor : float) 
                      (nodes            : plist<LogNode>) 
                      (annoApp          : AnnotationApp)
                      (opts             : SvgOptions)
                      : (plist<LogNode> * float) =
        let updNodes = 
          nodes 
            |> LogNodes.Recursive.applyAll (fun n -> (sizeX n id xAxisScaleFactor annoApp))

        // nodes without grainsize/Annotations of x-Axis semantic type get avg
        let sizes = 
          updNodes
            |> PList.filter (fun n -> n.svgSize.X <> 0.0) 
            |> PList.map (fun (n : LogNode) -> n.svgSize.X)

        if sizes.Count = 0 then Debug.warn "calc svg x position/size failed" //TODO if list empty //TODO bug/refactor
        let avg = 
          match (PList.averageOrZero sizes) with
            | n when n = 0.0 -> 10.0 //TODO hardcoded size if no grain size annotations in log
            | n -> n
        let updNodes =
          updNodes |> PList.map (fun n -> LogNodes.Recursive.defaultSizeXIfZero n avg)

        let updFirstLevel n =
          let posX = opts.secLevelWidth
          {n with svgPos = V2d.withX n.svgPos posX}

        let updSvgPosX (parent : LogNode) (current : LogNode) =
          let svgPosX = parent.svgPos.X + parent.svgSize.X
          {current with svgPos = V2d.withX current.svgPos svgPosX}

        let updNodes =
          updNodes |> PList.map (fun n -> applyWithParent None n updFirstLevel updSvgPosX)
        
        (updNodes, avg)


///////////////////////////////////////////////////////////////////////////////////////////////

    let rec createView  
     // (offset                   : float)
     // (secondaryLvl             : SecondaryLevelDescription)
      (model                    : MLogNode) 
      (generalCreationFunction  : MLogNode -> IMod<list<DomNode<'msg>>>)
                              : alist<DomNode<'msg>> =
      //let offset = Calc.offset model.level offset secondaryLvl
          
      let childrenView () = 
        alist {
          for c in model.children do               
            let v = (createView c generalCreationFunction)
            yield! v
        }
    
      let rval =
        alist {
          //let! offset = offset
          let! selfView = generalCreationFunction model
          let! hasCs = hasChildren model
          //let selfView = selfViewFunction offset 0.0
          //let! lvl = model.level 
          //let! secLvlNr = secondaryLvl.level

          //if lvl = secLvlNr then
          //  for v in (generalCreationFunction 0.0 model) do yield v
          match hasCs with
            | false  -> 
                for v in selfView do
                  yield v                
            | true   ->  
              let! hasHierarchicalNodes = (hasHierarchicalNodes' model)
              if not hasHierarchicalNodes then 
                for v in selfView do
                  yield v 
              yield (Svgplus.Attributes.Incremental.toGroup (childrenView ()) AMap.empty)           
        }
      rval
    
    //module private Default =
    //  //let voidDomNode<'msg> = 
    //  //  DomNode.Void<'msg> ("",(AttributeMap.ofList []))
    //  let debugOutput (txt : string) (selectionCallback   : list<string> -> 'msg) = 
    //    Svg.Attributes.toGroup
    //      [Svgplus.Base.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
    //      (Svg.Events.onClickAttribute selectionCallback)

    //let getBorderColors (model : MLogNode) (flags : IMod<SvgFlags>) =
    //  adaptive {
    //    let! flags = flags;
    //    let! (uBorderColor, lBorderColor) =
    //      match (Flags.isSet SvgFlags.BorderColour flags) with
    //        | true ->
    //          let uc = Border.colorOrDefault model.uBorder
    //          let lc = Border.colorOrDefault model.lBorder
    //          (uc, lc) //change color to anno/color
    //        | false ->
    //          (Mod.constant C4b.Black, Mod.constant C4b.Black)
    //    return {upper = uBorderColor; lower = lBorderColor}
    //  }

    let view  (model        : MLogNode) 
              (secondaryLvl : IMod<NodeLevel>)
              (flags        : IMod<SvgFlags>)
              (options      : MSvgOptions)
              (styleFun     : float -> IMod<LogAxisSection>)
              (annoApp      : MAnnotationApp) =

      let getCreationFunction 
                              (flags               : IMod<SvgFlags>)
                              (options             : MSvgOptions) 
                              (styleFun            : float -> IMod<LogAxisSection>) //TODO rename
                              (selectAction        : LogNodeId -> 'a)
                              (mapper              : Border.Action -> 'a)
                              //(offset              : float) 
                              (model               : MLogNode) =

        //let offset = options.secLevelWidth |> Mod.map (fun w -> w + offset)
        let pos = model.svgPos
        
        let axisSectionColor = model.svgSize 
                                  |> Mod.bind (fun size -> styleFun size.width)
                                  |> Mod.map (fun section -> section.color)
        let selectionCallback = (fun lst -> selectAction model.id)

        let createHierarchicalNode () = 
              Svgplus.Incremental.drawBorderedRectangle
                                pos
                                model.svgSize
                                axisSectionColor
                                (Mod.constant  {upper = C4b.Black; lower = C4b.Black})
                                //(getBorderColors model flags)
                                (Mod.constant SimpleTypes.SvgWeight.init)
                                selectionCallback
                                model.isSelected
                                model.hasDefaultX


        let buttons () = 
          let optButtons = 
              (Border.Svg.getCorrelationButtons model)
          adaptive {
            let! optButtons    = optButtons
            let! flags         = flags
            return 
              match (Flags.isSet SvgFlags.EditCorrelations flags), optButtons with
                | true, Some btns  -> 
                    let (btnL, btnU) = btns
                    [btnL |> UI.map mapper]@[btnU |> UI.map mapper]
                | _,_ -> []
          }


        adaptive {
          let! nodeType      = model.nodeType
          match nodeType with
              | LogNodeType.Angular -> return []
              | LogNodeType.Metric  -> return []
              | LogNodeType.Hierarchical -> 
                  let! btns = buttons ()
                  let! dn = createHierarchicalNode ()
                  return [dn]@btns
              | LogNodeType.HierarchicalLeaf ->
                  let! btns = buttons ()
                  let! dn = createHierarchicalNode ()
                  return [dn]@btns
              | LogNodeType.PosInfinity
              | LogNodeType.NegInfinity -> return []
                 // return [Default.debugOutput "LogNode neg or pos infinity" selectionCallback]
              | LogNodeType.Empty | LogNodeType.Infinity -> return []
                 // return [Default.debugOutput "LogNode empty or infinity" selectionCallback]
        }



      let generalCreationFunction = getCreationFunction 
                                      flags options styleFun 
                                      (ToggleSelectNode) 
                                      (BorderMessage)      

      createView model generalCreationFunction          


    


   //let private createSvgNode    // (viewType            : CorrelationPlotViewType)
    //                      (nodeType            : LogNodeType)
    //                      (size                : V2d)
    //                      (color               : C4b)
    //                      (uBorderColor        : C4b)
    //                      (lBorderColor        : C4b)
    //                      (dottedRBorder       : bool)
    //                      (weight              : float)
    //                      (selectionCallback   : list<string> -> 'msg)
    //                      (buttons             : Option<(DomNode<'msg> * DomNode<'msg>)>)
    //                      (isSelected          : bool) 
    //                      (position            : V2d)
    //                      (offset              : float) 
    //                      (secondaryLvlBreadth : Option<float>): list<DomNode<'msg>> =
          
    //  let (size, position, offset) =
    //    match secondaryLvlBreadth with
    //      | Some b -> (new V2d(b, size.Y), new V2d(0.0, position.Y), position.X)
    //      | None   -> (size, position, offset + position.X)

    //  let drawRectangle () =  
    //    let rfun = Svgplus.Base.drawBorderedRectangle
    //                (new V2d(offset, position.Y))
    //                size.X size.Y
    //                color lBorderColor uBorderColor
    //                weight
    //                selectionCallback
    //                isSelected
    //    match dottedRBorder with
    //            | true ->
    //                rfun true
    //            | false ->
    //                rfun false

    //  let btns = 
    //    match buttons with
    //      | Some (lb, ub) -> [lb;ub]
    //      | None -> []

    //  let domNode = 
    //    match nodeType with
    //        | LogNodeType.Angular -> []
    //        | LogNodeType.Metric  -> []
    //        | LogNodeType.Hierarchical -> [drawRectangle ()]@btns
    //        | LogNodeType.HierarchicalLeaf -> [drawRectangle ()]@btns
    //        | LogNodeType.PosInfinity
    //        | LogNodeType.NegInfinity ->
    //            [Default.debugOutput "LogNode neg or pos infinity" selectionCallback]
    //        | LogNodeType.Empty | LogNodeType.Infinity ->
    //            [Default.debugOutput "LogNode empty or infinity" selectionCallback]
    //  domNode