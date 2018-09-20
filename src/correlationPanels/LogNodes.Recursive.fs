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

    let rec collectAndFilterAll' (n : MLogNode) (f : MLogNode -> IMod<bool>) =
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
              yield! collectAndFilterAll' c f
      }

    let rec mapAndCollect (n : LogNode) (f : LogNode -> 'a) =
      match PList.count n.children with
      | 0      -> [f n]
      | other  -> 
        [f n] @ 
          (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> mapAndCollect x f))

    let collectAll (lst : alist<MLogNode>) =
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

    let applyAll (f : LogNode -> LogNode) (lst : plist<LogNode>) = 
      lst |> PList.map (fun n -> apply n f)

    let rec filterAndCollect (n : LogNode) (f : LogNode -> bool) =
      match PList.count n.children, f n with
      | 0, true      -> [n]
      | 0, false     -> []
      | other, true  -> 
        [n] @ 
          (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> filterAndCollect x f))
      | other, false -> 
        [] @  (n.children 
                |> PList.toList
                |> List.collect (fun (x : LogNode) -> filterAndCollect x f))

    let childrenWith (n : LogNode) (f : LogNode -> bool) =
      filterAndCollect n f

    let rec hasChildrenWith (n : LogNode) (f : LogNode -> bool) =
      match PList.count n.children with
      | 0     -> f n
      | other ->  
          (f n) || (n.children 
                    |> PList.map (fun n -> (hasChildrenWith n f))
                    |> PList.anyTrue (fun x -> x)
                   )


    let rec hasNodesWith' (lst : plist<MLogNode>) (f : MLogNode -> IMod<bool>) =
      let count = PList.count lst

      match count with
        | 0     -> Mod.constant false
        | 1     -> f (lst.Item 0)
        | other -> 
          Mod.map2 (fun bThis bChildren -> bThis || bChildren)
                    (f (lst.Item 0))
                    (hasNodesWith' (PList.tail lst) f)

    let hasHierarchicalNodes' (model : MLogNode) =
      adaptive {
        let! cont = model.children.Content
        return! hasNodesWith' cont (fun n -> (Helper.hasNodeType n LogNodeType.Hierarchical))
      }

/////////////////////////////////////////////////////

    let angularChildren (node : MLogNode) =
       collectAndFilterAll' node 
         (fun n -> 
           Mod.map (fun (t : LogNodeType) -> 
                       t = LogNodeType.Angular) 
                   n.nodeType
         )
      
    let metricChildren (node : MLogNode) =
       collectAndFilterAll' node 
         (fun n -> 
           Mod.map (fun (t : LogNodeType) -> 
                     t = LogNodeType.Metric) 
                   n.nodeType
         )  
         





    
    let defaultIfZero (model : LogNode) (defaultSizeX : float) =
      let diz (node : LogNode) = 
        match node with
          | n when n.svgSize.X = 0.0 -> 
            {n with svgSize        = (node.svgSize * V2d.OI) + (V2d.IO) * defaultSizeX
                    hasDefaultX = true}
          | _ -> node
      apply model diz