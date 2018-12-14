namespace CorrelationDrawing.LogNodes

  module Svg =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.UI
    open CorrelationDrawing

    module Calc =
      open Aardvark.Base

      let calcSvgHeight (node : LogNode) (factor : float) =
        (Helper.elevationRange node).range * factor  

      let calcYScaleFactor (availableHeight : float) 
                           (logNodes : plist<LogNode>) = 
        let accumulatedHeight =
          logNodes 
            |> PList.toList
            |> List.sumBy (fun x -> (abs (Helper.elevationRange x).range))
        availableHeight / accumulatedHeight

      let mapPrev (lst : plist<'a>) (f : option<'a> -> 'a -> 'b) =
        lst
          |> PList.mapi (fun i n ->
                          let prev = lst.TryGet (Index.before i)
                          f prev n
                        )

      let calcDim (xScaleFactor     : float)
                  (yScaleFactor     : float) 
                  (annoApp          : AnnotationApp)
                  (node             : LogNode) =

        let _height = calcSvgHeight node yScaleFactor
        let metricValue = 
          node 
            |> Recursive.metricChildren
            |> List.map (fun n -> Helper.calcMetricValue n annoApp)
            |> List.filterNone
            |> List.tryMax

        match metricValue with
          | None  ->
            let _n = Lens.svgHeight.Set (node, _height)
            let _n = Lens.svgWidth.Set  (_n, 10.0)
            _n
          | Some metricVal -> 
            let hierChildren = 
              node.children 
                |> PList.filter 
                  (fun (n : LogNode) -> 
                   n.nodeType == LogNodeType.Hierarchical)
            let _n = Lens.svgHeight.Set (node, _height)
            let _n = Lens.svgWidth.Set  (_n, metricVal * xScaleFactor)
            let _n = Lens.hasAverageWidth.Set(_n, false)
            let draw = ((hierChildren.IsEmptyOrNull ()))
            let _n = Lens.draw.Set (_n, draw)
            _n

      let rec calcPosition (startAtY         : float) 
                           (startAtX         : float)
                           (annoApp          : AnnotationApp)
                           (thisLevel        : plist<LogNode>) = 

        let thisLevelWithPos =
          let setY (prev : LogNode) (n : LogNode) = 
            let sY = startAtY + (LogNodes.Lens.svgHeight.Get prev)
            Lens.svgY.Set  (n, sY)       
          let _nodes = thisLevel |> PList.map (fun n -> Lens.svgY.Set (n, startAtY)) 
          (PList.mapPrev _nodes None setY)


        thisLevelWithPos 
          |> PList.map (fun n -> 
                          if n.children.IsEmptyOrNull () then 
                            {n with children = PList.empty}
                          else 
                            {n with children = calcPosition startAtY startAtX annoApp n.children}
                        )

      let posDimAll  (startAtY         : float) 
                     (startAtX         : float)
                     (availableHeight  : float) 
                     (xScaleFactor     : float)
                     (yScaleFactor     : Option<float>) 
                     (annoApp          : AnnotationApp)
                     (nodes            : plist<LogNode>) = 
          match nodes.IsEmptyOrNull () with
            | true  -> (PList.empty, 0.0)
            | false -> 
              let factor = 
                match yScaleFactor with 
                  | Some factor -> factor
                  | None        -> calcYScaleFactor availableHeight nodes
              let _nodes = 
                nodes
                 |> LogNodes.Recursive.applyAll 
                    (fun (n : LogNode) -> calcDim  xScaleFactor factor annoApp n)
                 |> calcPosition startAtY startAtX annoApp
              (_nodes, factor)
    

    let rec view  (model        : MLogNode) 
                  (flags        : IMod<SvgFlags>)
                  (options      : MSvgOptions)
                 // (styleFun     : float -> IMod<LogAxisSection>)
                  (annoApp      : MAnnotationApp) =
      let childrenView = 
        model.children
          |> AList.map (fun c -> view c flags options annoApp)  
      alist {
        let! t = model.nodeType
        if t = LogNodeType.Hierarchical then
          let v = (Svgplus.Rectangle.view model.mainBody 
                    |> AList.map (UI.map Action.RectangleMessage))
          yield! v
        for c in childrenView do
          yield! c
      }
        
