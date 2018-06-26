namespace CorrelationDrawing

  module LogNodeSvg =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.UI
    open Aardvark.UI.Primitives

    module Default =
      let debugOutput (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.toGroup
          [Svg.drawText (new V2d(5.0, 5.0)) txt]
          (Svg.Events.onClickAttribute selectionCallback)

      let angularNode (yPos : float) (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.toGroup
          [Svg.drawText (new V2d(5.0, 5.0)) txt]
          (Svg.Events.onClickAttribute selectionCallback)

      let metricNode (yPos : float) (txt : string) (selectionCallback   : list<string> -> 'msg) = 
        Svg.toGroup
          [Svg.drawText (new V2d(5.0, 5.0)) txt]
          (Svg.Events.onClickAttribute selectionCallback)

      let levelToWeight (level : int) = 
        ((8.0 - (float level)) * 0.3)



    let createDomNode     (viewType            : CorrelationPlotViewType)
                          (nodeType            : LogNodeType)
                          (size                : V2d)
                          (color               : C4b)
                          (uBorderColor        : C4b)
                          (lBorderColor        : C4b)
                          (dottedRBorder       : bool)
                          (weight              : float)
                          (selectionCallback   : list<string> -> 'msg)
                          (buttonCallback      : Option<list<string> -> 'msg>)
                          (isSelected          : bool) 
                          (position            : V2d)
                          (offset              : float) 
                          (secondaryLvlBreadth : Option<float>): list<DomNode<'msg>> =
          
      let (size, position, offset) =
        match secondaryLvlBreadth with
          | Some b -> (new V2d(b, size.Y), new V2d(0.0, position.Y), 0.0)
          | None   -> (size, position, offset)

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
                    rfun true buttonCallback
                | false ->
                    rfun false buttonCallback

      let domNode = 
        match nodeType with
            | LogNodeType.Angular -> Default.angularNode position.Y "A" selectionCallback
            | LogNodeType.Metric  -> Default.metricNode position.Y "M" selectionCallback
            | LogNodeType.Hierarchical -> 
              drawRectangle
            | LogNodeType.HierarchicalLeaf -> 
              drawRectangle
            | LogNodeType.PosInfinity
            | LogNodeType.NegInfinity ->
                Default.debugOutput "LogNode neg or pos infinity" selectionCallback
            | LogNodeType.Empty | LogNodeType.Infinity ->
                Default.debugOutput "LogNode empty or infinity" selectionCallback
      [domNode]


    let getDomNodeFunction (viewType            : CorrelationPlotViewType) 
                           (styleFun            : float -> IMod<LogNodeStyle>) 
                           (selectionCallback   : LogNodeId -> list<string> -> 'msg)
                           (buttonCallback      : Option<LogNodeId -> list<string> -> 'msg>)
                           (offset              : float) 
                           (model               : MLogNode) =
      adaptive {
        let! size              =  (model.size)
        let! s                 = styleFun size.X
        let! ypos              = model.logYPos
        let size               = new V2d(size.X, size.Y)
        let color              = s.color
        let! uBorderColor      = model.uBoundary.color
        let! lBorderColor      = model.lBoundary.color
        let! dottedRBorder     = model.hasDefaultX
        let! (lvl : int)         = model.level
        let weight             = (Default.levelToWeight lvl)
        let! isSelected        = model.isSelected
        let position           = new V2d(offset, ypos)
        let! lnType            = model.nodeType
            
            
        let f = (createDomNode viewType
                               lnType
                               size                          
                               color             
                               uBorderColor     
                               lBorderColor     
                               dottedRBorder        
                               weight            
                               (selectionCallback model.id)
                               (Option.map (fun x -> x model.id) buttonCallback)
                               isSelected       
                               position)      
        return f //createView 0.0 secondaryLvl model f      
      }