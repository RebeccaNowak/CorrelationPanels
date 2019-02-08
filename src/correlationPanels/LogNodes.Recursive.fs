namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Recursive = 
    open Aardvark.Base
    open Aardvark.UI
    open Helper
    open Aardvark.Base.Incremental
    open Aardvark.Base.Monads.Optics

    let rec collect (n : LogNode) =
      match PList.count n.children with
        | 0      -> [n]
        | other  -> 
          [n] @ 
            (n.children 
                  |> PList.toList
                  |> List.collect (fun (x : LogNode) -> collect x)
            )

    let rec collectAll (lst : plist<LogNode>) =
      seq {
        for n in lst do
          yield! (collect n)
      } |> List.ofSeq

    let rec collect' (n : MLogNode) =
      alist {
        let! nr = AList.count n.children 
        match nr with
          | 0      -> yield n
          | other  -> 
            yield n
            let children = n.children
            for c in children do
              yield! collect' c
      }       

    let rec collectAndFilterAll_M (n : MLogNode) (f : MLogNode -> IMod<bool>) =
      let filter' (lst: alist<MLogNode>) (f : MLogNode -> IMod<bool>) =
        alist {
          for el in lst do
            let! b = f el
            if b = true then yield el
        }

      alist {
        let! nr = AList.count n.children 
        match nr with
          | 0      -> 
            yield! (filter' (AList.single n) f)
          | other  -> 
            yield! (filter' (AList.single n) f)
            let children = n.children
            for c in children do
              yield! collectAndFilterAll_M c f
      }

    let rec mapAndCollect_M (n : MLogNode) (f : MLogNode -> 'a) =
      alist {
        let! nr = AList.count n.children 
        match nr with
          | 0      -> 
            yield! f n
          | other  -> 
            yield! f n
            let children = n.children
            for c in children do
              yield! mapAndCollect_M c f
      }

    let rec mapAndCollect (n : LogNode) (f : LogNode -> 'a) =
      match PList.count n.children with
      | 0      -> [f n]
      | other  -> 
        [f n] @ 
          (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> mapAndCollect x f))

    let collectAll' (lst : alist<MLogNode>) =
      alist {
        for el in lst do 
          yield! (collect' el)
      }

    let rec apply (n : LogNode) (f : LogNode -> LogNode) =
      match PList.count n.children with
              | 0     -> f n 
              | other -> 
                  let c = n.children |> PList.map (fun (n : LogNode) -> apply n f)
                  f {n with children = c}

    let rec applyWithParent (p : option<LogNode>) (n : LogNode) 
                            (g: LogNode -> LogNode) 
                            (f : LogNode -> LogNode -> LogNode) =
      match PList.count n.children, p with
        | 0, Some p     -> f p n 
        | 0, None  -> g n
        | other, Some p -> 
          let fOfn = f p n
          let children = 
            n.children 
              |> PList.map (fun (c : LogNode) -> applyWithParent (Some fOfn) c g f)
          {n with children = children}
        | other, None ->
          let children = 
            n.children 
              |> PList.map (fun (c : LogNode) -> applyWithParent (Some n) c g f)
          {n with children = children}


    let applyAll (f : LogNode -> LogNode) (lst : plist<LogNode>) = 
      lst |> PList.map (fun n -> apply n f)

    let rec apply' (parent : option<LogNode>) (current : LogNode) 
            (applyParentCurrent : LogNode -> LogNode -> LogNode) =
      let children = current.children
      let updCurrentAndChildren () = 
        let updChildren = 
          children 
            |> PList.map (fun n -> apply' (Some current) n applyParentCurrent)
        {current with children = updChildren}
      match parent with
        | None   -> updCurrentAndChildren ()
        | Some p ->
          match children.IsEmptyOrNull () with
            | true  -> 
              (applyParentCurrent p current)
            | false ->
              updCurrentAndChildren ()

    let rec filterAndCollect (f : LogNode -> bool) (n : LogNode) =
      match PList.count n.children, f n with
      | 0, true      -> [n]
      | 0, false     -> []
      | other, true  -> 
        [n] @ 
          (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> filterAndCollect f x))
      | other, false -> 
        [] @  (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> filterAndCollect f x))

    let childrenWith (n : LogNode) (f : LogNode -> bool) =
      filterAndCollect f n

    let rec hasChildrenWith (n : LogNode) (f : LogNode -> bool) =
      match PList.count n.children with
      | 0     -> f n
      | other ->  
          (f n) || (n.children 
                    |> PList.map (fun n -> (hasChildrenWith n f))
                    |> DS.PList.anyTrue (fun x -> x)
                   )


    let rec hasNodesWith' (lst : plist<MLogNode>) (f : MLogNode -> IMod<bool>) =
      let count = PList.count lst

      match count with
        | 0     -> Mod.constant false
        | 1     -> f (lst.Item 0)
        | other -> 
          Mod.map2 (fun bThis bChildren -> bThis || bChildren)
                    (f (lst.Item 0))
                    (hasNodesWith' (DS.PList.tail lst) f)

    let hasHierarchicalNodes' (model : MLogNode) =
      adaptive {
        let! cont = model.children.Content
        return! hasNodesWith' cont (fun n -> (Helper.hasNodeType n LogNodeType.Hierarchical))
      }

    let metricChildren (node : LogNode) =
       childrenWith node (fun n ->  n.nodeType = LogNodeType.Metric) 

    let hierarchicalChildren (node : LogNode) =
       childrenWith node (fun n ->  n.nodeType = LogNodeType.Hierarchical) 

/////////////////////////////////////////////////////

    let angularChildren' (node : MLogNode) =
       collectAndFilterAll_M node 
         (fun n -> 
           Mod.map (fun (t : LogNodeType) -> 
                       t = LogNodeType.Angular) 
                   n.nodeType
         )
      
    let metricChildren' (node : MLogNode) =
       collectAndFilterAll_M node 
         (fun n -> 
           Mod.map (fun (t : LogNodeType) -> 
                     t = LogNodeType.Metric) 
                   n.nodeType
         )  
         
    //let defaultSizeXIfZero (model : LogNode) (defaultSizeX : float) =
    //  let diz (node : LogNode) = 
    //    match node with
    //      | n when n.mainBody.dim.X = 0.0 -> 
    //        let _n = LogNodes.Lens.svgWidth.Set(n, defaultSizeX)
    //        LogNodes.Lens.hasAverageWidth.Set(_n, true)
    //      | _ -> node
    //  apply model diz

    //let xRange (lst : plist<LogNode>) =
    //  lst |> collectAll
    //      |> List.map (fun (n : LogNode) -> n.mainBody.dim.X) //TODO only hier
    //      |> List.max


          
    let calcMetricValue (model : LogNode) (annoApp : AnnotationApp) =
      let mc = metricChildren model
      let someVals = mc |> List.map (fun n -> LogNodes.Helper.calcMetricValue n annoApp)
      let vals = someVals |> DS.List.filterNone
      match vals.IsEmptyOrNull () with
        | true  -> None
        | false -> Some (vals |> List.max)


     

    //let rec elevationRange (node : LogNode) (range : Rangef) (annoApp : AnnotationApp) =
    //  match node.children.IsEmptyOrNull () with
    //    | true  -> 
    //      match Option.map (fun b -> Border.tryElevation b) node.lBorder,
    //            Option.map (fun b -> Border.tryElevation b) node.uBorder with
    //        | Some l, Some u ->
    //            {Rangef.init with min = l.point.Length
    //                              max = u.point.Length}
    //    | false -> 
    //  let children = 
    //    collectAll node.children 
    //  children 
    //    |> List.map (fun (n : LogNode) -> Svg.elevationRange n)
        
          

    //let calcDimX (nodes : plist<LogNode>)
    //             (xAxis : SemanticId) 
    //             (xAxisScaleFactor : float) 
    //             (annoApp : AnnotationApp) =
    //  let calcDimX (model : LogNode)          (xAxis : SemanticId) 
    //               (xAxisScaleFactor : float) (annoApp : AnnotationApp) = 
        
    //    let metricNodes = 
    //      childrenWith model (fun n -> 
    //                              let sId = LogNodes.Helper.semanticIdOrInvalid n annoApp
    //                              sId = xAxis
    //                          )
    //    let metricValues = metricNodes |> List.map (fun n -> calcMetricValue n annoApp)
    //    let sizeX = (metricValues |> DS.List.filterNone |> DS.List.maxOrZero) * xAxisScaleFactor
    //    {model with 
    //      mainBody = 
    //        {model.mainBody with dim = {width = sizeX; height = model.mainBody.dim.height}}
    //    }    

    //  let updNodes = 
    //    nodes 
    //      |> applyAll (fun n -> (calcDimX n xAxis xAxisScaleFactor annoApp))

    //  // nodes without grainsize/Annotations of x-Axis semantic type get avg
    //  let sizes = 
    //    updNodes
    //      |> PList.filter (fun n -> n.mainBody.dim.X <> 0.0) 
    //      |> PList.map (fun (n : LogNode) -> n.mainBody.dim.X)

    //  if sizes.Count = 0 then Debug.warn "calc svg x size failed" //TODO if list empty //TODO bug/refactor
    //  let avg = 
    //    match (DS.PList.averageOrZero sizes) with
    //      | n when n = 0.0 -> 10.0 //TODO hardcoded size if no grain size annotations in log
    //      | n -> n
    //  let updNodes =
    //    updNodes |> PList.map (fun n -> defaultSizeXIfZero n avg)
    //  (updNodes, avg)

