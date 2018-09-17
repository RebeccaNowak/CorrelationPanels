namespace CorrelationDrawing.LogNodes
open CorrelationDrawing

  module Recursive = 
    open Aardvark.Base
    open Aardvark.UI
    open UI
    open Aardvark.Base.Incremental

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

    let rec collectAll (lst : alist<MLogNode>) =
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