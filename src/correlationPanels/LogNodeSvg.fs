﻿namespace CorrelationDrawing

  module LogNodeSvg =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI

    let hasChildren (model : MLogNode) =
      let isEmpty = AList.isEmpty model.children
      Mod.map (fun x -> not x) isEmpty


    let private containsHNodes (node : MLogNode) =
      let foo = (Mod.force node.children.Content) 
                  |> PList.filter (fun n -> (Mod.force n.nodeType = LogNodeType.Hierarchical))
      (not (foo.IsEmpty()))


        
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
      let (secLvlNr, breadthSec) = secondaryLvl //TODO hardcoded
      let offset =
        adaptive {
          let! lvl = model.level 
          let sLvl = secondaryLvl
          return match (lvl = secLvlNr), lvl = 0 with
                  | true, true  -> offset + breadthSec
                  | false, true -> offset + breadthSec
                  | true, false -> offset
                  | false, false -> offset
        }
          
      let childrenView = 
        alist {
          let! os = offset
          for c in model.children do               
            let v = (createView os secondaryLvl c viewFunction)
            for it in (v : alist<DomNode<'msg>>) do
              yield it
        }
    
      let rval =
        alist {
          let! os = offset
          let! selfViewFunction = viewFunction os model
          let! hasCs = hasChildren model
          let selfView = selfViewFunction os None
          let! lvl = model.level 
          if lvl = secLvlNr then
            for v in (selfViewFunction 0.0 (Some breadthSec)) do yield v
          match hasCs with
            | false  -> 
                for v in selfView do
                  yield v                
            | true   ->  
              let! lstChildren = childrenView.Content     
              match (containsHNodes model) with
                | true  ->
                  yield (Svg.toGroup (lstChildren |> PList.toList) [])                                              
                | false ->
                  for v in selfView do
                    yield v 
                  yield (Svg.toGroup (lstChildren |> PList.toList) [])
        }
      rval
    
    module Default =
      //let voidDomNode<'msg> = 
      //  DomNode.Void<'msg> ("",(AttributeMap.ofList []))
      let debugOutput (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.toGroup
          [Svg.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
          (Svg.Events.onClickAttribute selectionCallback)

      let angularNode (yPos : float) (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.toGroup
          [Svg.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
          (Svg.Events.onClickAttribute selectionCallback)

      let metricNode (yPos : float) (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.toGroup
          [Svg.drawText (new V2d(5.0, 5.0)) txt Orientation.Horizontal]
          (Svg.Events.onClickAttribute selectionCallback)

      let levelToWeight (level : int) = //TODO hardcoded
        ((8.0 - (float level)) * 0.3)



    let createDomNode    // (viewType            : CorrelationPlotViewType)
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

      let drawRectangle =  
        let rfun = Svg.drawBorderedRectangle
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
            | LogNodeType.Angular -> 
              [
                Svg.drawCircleButton 
                  (new V2d(offset + size.X, (position.Y + size.Y) * 0.5))
                  2.0 C4b.Black false 0.5 selectionCallback
              ]
            | LogNodeType.Metric  -> 
              [Svg.drawCircleButton 
                (new V2d(offset + size.X, (position.Y + size.Y) * 0.5))
                2.0 C4b.Black false 0.5 selectionCallback]
            | LogNodeType.Hierarchical -> [drawRectangle]@btns
            | LogNodeType.HierarchicalLeaf -> [drawRectangle]@btns
            | LogNodeType.PosInfinity
            | LogNodeType.NegInfinity ->
                [Default.debugOutput "LogNode neg or pos infinity" selectionCallback]
            | LogNodeType.Empty | LogNodeType.Infinity ->
                [Default.debugOutput "LogNode empty or infinity" selectionCallback]
      domNode


    let getDomNodeFunction // (viewType            : IMod<CorrelationPlotViewType>) 
                            (flags               : IMod<SvgFlags>)
                            (options             : SvgOptions) 
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

        let! (btnL, btnU) = 
              (Border.Svg.getCorrelationButtons model (offset + options.secLevelWidth) weight)// buttonCallback) //TODO performance
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