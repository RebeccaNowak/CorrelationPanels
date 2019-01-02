namespace CorrelationDrawing

open System
open Aardvark.Base.Incremental
open Aardvark.Base


module Seq =
  let properPairwiseOpt (f : 'a -> 'a -> 'b) (neutral : 'b) (s : seq<Option<'a>>) =
    s
      |> Seq.chunkBySize 2
      |> Seq.map 
        (fun arr -> match arr with
                      | [| a;b |] -> match a, b with
                                      | Some c, Some d -> Some (f c d)
                                      | _ -> None
                      | _ -> None)

  let properPairwise (f : 'a -> 'a -> 'b) (neutral : 'b) (s : seq<'a>) =
    s
      |> Seq.chunkBySize 2
      |> Seq.map 
        (fun arr -> match arr with
                      | [| a;b |] -> (f a b)
                      | _ -> neutral)

module List =
  let averageOrZero (lst : list<float>) = 
    match lst with
      | [] -> 0.0
      | li -> List.average li

  let maxOrZero (lst : list<float>) = 
    match lst with
      | [] -> 0.0
      | li -> List.max li

  let tryMax (lst : list<float>) = 
    match lst with
      | [] -> None
      | li -> Some (List.max li)


  let contains' (f : 'a -> bool) (lst : List<'a>)  =
    match lst with
      | []  -> false
      | l   ->
        lst |> List.map (fun x -> f x)
            |> List.reduce (fun x y -> x || y)

  let contains'' (f : 'a -> 'b) (a : 'a)  (lst : List<'a>) =
    match lst with
      | []  -> false
      | l   ->
        lst
           |> List.map f
           |> List.contains (f a)

//  let l = [1..5]
//  l |> (contains'' (fun (x : int) -> (sprintf "%i" x)) 7)

  let reduce' (f1 : 'a -> 'b) (f2 : 'b -> 'b -> 'b) (lst : List<'a>) =
    lst
      |> List.map f1
      |> List.reduce f2

  let filterNone (lst : list<option<'a>>) =
    lst
      |> List.filter (fun el -> 
                        match el with
                          | Some el -> true
                          | None    -> false)
      |> List.map (fun el -> el.Value)

module PList =
  let fromHMap (input : hmap<_,'a>) : plist<'a> = 
    input |> HMap.toSeq |> PList.ofSeq |> PList.map snd 

  let contains (f : 'a -> bool) (lst : plist<'a>) =
    let filtered = 
      lst 
        |> PList.filter f
    not (filtered.IsEmpty ())  

  let mapiInt (lst : plist<'a>) =
    let i = ref 0
    seq {
      for item in lst do
        yield (item, i.Value)
        i := !i + 1
    }
    |> PList.ofSeq

  let deleteFirst (lst : plist<'a>) (f : 'a -> bool) =
    match lst.FirstIndexOf f with
      | ind when ind = -1 -> (false, lst)
      | ind -> (true, lst.RemoveAt ind)

  

  


  let rec deleteAll (f : 'a -> bool) (lst : plist<'a>) =
    match deleteFirst lst f with
      | (true, li)  -> deleteAll f li
      | (false, li) -> li

  let filterNone (lst : plist<option<'a>>) =
    lst
      |> PList.filter (fun (el : option<'a>) -> el.IsSome)
      |> PList.map (fun el -> el.Value)

  let min (lst : plist<'a>) : 'a =
    lst
      |> PList.toList
      |> List.min

  let mapMin (f : 'a -> 'b) (lst : plist<'a>) : 'b =
    lst |> PList.toList
        |> List.map f
        |> List.min


  let minBy (f : 'a -> 'b) (lst : plist<'a>) : 'a =
    lst
      |> PList.toList
      |> List.reduce (fun x y -> if (f x) < (f y) then x else y)

  let maxBy (f : 'a -> 'b) (lst : plist<'a>) : 'a =
    lst
      |> PList.toList
      |> List.reduce (fun x y -> if (f x) > (f y) then x else y)

  let tryMinBy (f : 'a -> 'b) (lst : plist<'a>) : option<'a> =
    match lst.IsEmptyOrNull() with
      | true  -> None
      | false -> Some (minBy f lst)

  let tryMaxBy (f : 'a -> 'b) (lst : plist<'a>) : option<'a> =
    match lst.IsEmptyOrNull() with
      | true  -> None
      | false -> Some (maxBy f lst)

  let minMapBy (mapTo: 'a -> 'c) (minBy : 'a -> 'b) (lst : plist<'a>) : 'c =
    lst
      |> PList.toList
      |> List.reduce (fun x y -> if (minBy x) < (minBy y) then x else y)
      |> mapTo 

  let maxMapBy (mapTo: 'a -> 'c) (maxBy : 'a -> 'b) (lst : plist<'a>) : 'c =
    lst
      |> PList.toList
      |> List.reduce (fun x y -> if (maxBy x) < (maxBy y) then x else y)
      |> mapTo 

  let average (lst : plist<float>) : float =
    lst
      |> PList.toList
      |> List.average

  let tail (lst : plist<'a>) =
    match lst.IsEmptyOrNull () with
      | true  -> PList.empty
      | false -> PList.removeAt 0 lst

  let tryHead (lst : plist<'a>) =
    match lst.IsEmptyOrNull () with
      | true  -> None
      | false -> Some (lst.Item 0)

  let rec reduce (f : 'a -> 'a -> 'a) 
                 (acc : 'a) 
                 (lst : plist<'a>) =
    match (tryHead lst) with
      | None           -> acc
      | Some h         -> 
        reduce f (f acc h) (tail lst)

  let rec allTrueOrEmpty (f : 'a -> bool) (lst : plist<'a>) =
    match lst.IsEmptyOrNull () with
      | true  -> true
      | false -> 
        match f (lst.Item 0) with
          | true  -> allTrueOrEmpty f (tail lst)
          | false -> false

  let rec anyTrue (f : 'a -> bool) (lst : plist<'a>) =
    match lst.IsEmptyOrNull () with
      | true -> false
      | false -> 
        match f (lst.Item 0) with
          | true  -> true
          | false -> anyTrue f (tail lst)
        
  let averageOrZero (lst : plist<float>) =
    match lst.IsEmptyOrNull () with
      | true -> 0.0
      | false -> average lst

  let rec mapPrev (lst  : plist<'a>) 
                  (prev : option<'a>)
                  (f    : 'a -> 'a -> 'a) =
    let current = tryHead lst
    match prev, current with
      | None, None     -> PList.empty
      | None, Some c   -> 
        PList.append c (mapPrev (tail lst) current f)
      | Some p, None   -> PList.empty
      | Some p, Some c -> 
        let foo = 
          mapPrev (tail lst) current f
        let bar =
          PList.append (f p c) foo
        bar   

  //let rec mapPrev' (keys  : plist<'key>) 
  //                 (items : hmap<'key, 'b>)
  //                 (prev  : option<'key>)
  //                 (f     : 'key -> 'key -> 'b) : hmap<'key, 'b> =
  //  let current = tryHead keys
  //  match prev, current with
  //    | None, None     -> HMap.empty
  //    | None, Some c   -> 
  //      HMap.add c (items.Item c) (mapPrev' (tail keys) items current f)
  //    | Some p, None   -> HMap.empty
  //    | Some p, Some c -> 
  //      let _current = (f p c)
  //      let _items   = (HMap.remove p items)
  //      let _items   = HMap.update c (fun optv -> _current) _items
  //      let rest = 
  //        mapPrev' (tail keys) _items current f
  //      let bar =
  //        HMap.add c _current rest
  //      bar   

module HMap =
  let toSortedPlist (input : hmap<'k,'a>) (projection : ('a -> 'b)) : plist<'a> =
    input 
        |> HMap.toSeq 
        |> Seq.map snd
        |> Seq.sortBy projection
        |> PList.ofSeq

  let filterNone (input : hmap<'k,option<'a>>) =
    input |> HMap.filter (fun k v -> v.IsSome)
          |> HMap.map (fun k v -> v.Value)



  let toPList (input : hmap<_,'a>)  : plist<'a> =
    input 
        |> HMap.toSeq 
        |> Seq.map snd
        |> PList.ofSeq

  let values  (input : hmap<'k,'a>)  : list<'a> =
    input 
        |> HMap.toSeq 
        |> Seq.map snd
        |> List.ofSeq

  let keys  (input : hmap<'k,'a>)  : list<'k> =
    input 
        |> HMap.toSeq 
        |> Seq.map fst
        |> List.ofSeq

  let toPairList (input : hmap<'k,'a>)  : list<'k * 'a> =
    let keys = (keys input)
    let values = (values input)
    List.zip  keys values

  let toSwappedPairList (input : hmap<'k,'a>)  : list<'a * 'k> =
    let keys = (keys input)
    let values = (values input)
    List.zip values keys
  
  let inline negate2 (f : 'a -> 'b -> bool) (a : 'a) (b : 'b) =
    not (f a b)

  let split (input : hmap<'k,'a>) (f : 'k -> 'a -> bool) =
    let trueMap = HMap.filter f input
    let falseMap = HMap.filter (negate2 f) input
    (trueMap, falseMap)
  //let filter (map : hmap<'k,'a>) (f : 'a -> bool) =
  //  let newMap = HMap.empty
  //  let lst = HMap.fi
  //  hmap {
  //    for (k,a) in map do
  //      yield k 
  //      yield a
  //  }

module AMap =
  let tryFind (map : amap<'k, 'a>) (key : 'k) =
   adaptive {
     let! content = map.Content
     return content.TryFind key
   }

  let valuesToASet (map : amap<'k, 'a>) =
    map
      |> AMap.toASet 
      |> ASet.map (fun (k,a) -> a)

  let valuesToAList (map : amap<'k, 'a>) =
    map
      |> AMap.toASet 
      |> ASet.map (fun (k,a) -> a)
      |> AList.ofASet


module PairList =
  let filterNone (lst : List<'a * option<'b>>) =
    lst 
      |> List.filter (fun (a, opt) -> opt.IsSome)
      |> List.map (fun (a, opt) -> (a, opt.Value))
     
