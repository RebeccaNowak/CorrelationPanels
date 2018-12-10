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



module AList =
  open Aardvark.Base.Incremental

  let bindIMod (lst : alist<IMod<'a>>) =
    alist {
      for a in lst do
        let! a = a
        yield a
    }

  let tryHead (lst : alist<'a>) =
    adaptive {
      let! plst = lst.Content
      return 
        match plst.IsEmptyOrNull () with
          | true -> None
          | false -> Some (plst.Item 0)
    }
   
  let fromAMap (input : amap<_,'a>) : alist<'a> = 
    input |> AMap.toASet |> AList.ofASet |> AList.map snd 

  let isEmpty (alst: alist<'a>) =
    alst.Content 
      |> Mod.map (fun x -> (x.Count < 1))

  let exists (f : 'a -> IMod<bool>) (alst: alist<'a>) = //performance :(
    let res = 
      alist {
        for a in alst do 
          let! b = (f a)
          if b then yield true
      }
    Mod.map (fun c -> (c > 0)) (AList.count res)
    
  let findAll (alst : alist<'a>) (f : 'a -> bool) =
    alist {
      let count = ref 0
      for a in alst do
        if f a then 
          incr count
          yield (count.Value, a)
    }

  let findFirst (alst : alist<'a>) (f : 'a -> bool) =
    let all = findAll alst f //runtime :(
    all
      |> AList.map snd
      |> tryHead
    
      

  let reduce (f : 'a -> 'a -> 'a) (alst: alist<'a>) = //TODO tryReduce
    alst.Content
      |> Mod.map (fun (x : plist<'a>) -> 
                      let r =
                        PList.toList x
                           |> List.reduce f
                      r
                  )

  let tryReduce (f : 'a -> 'a -> 'a) (alst: alist<'a>) = //TODO tryReduce
    adaptive {
      let! count = AList.count alst
      match count = 0 with
        | true -> return None
        | false ->
          let! cont = alst.Content
          let res = 
            cont 
              |> PList.toList 
              |> List.reduce f
          return Some res
    }

  let minBy (f : 'a -> 'b) (alst : alist<'a>) =
    alst
      |> tryReduce (fun x y -> if (f x) < (f y) then x else y)
      
  let min (alst : alist<'a>) =
     alst |>
      tryReduce (fun x y -> if x < y then x else y)

  let maxBy (f : 'a -> 'b) (alst : alist<'a>)  =
    alst
      |> tryReduce (fun x y -> if (f x) > (f y) then x else y)
      
  let tryMax (alst : alist<'a>) =
    alst
      |> tryReduce (fun x y -> if x > y then x else y)

  let average (alst : alist<float>) =
    let sum =
      alst |> reduce (fun x y -> x + y)
    Mod.map2 (fun s c -> s / (float c)) sum (AList.count alst)

  let tryAverage (alst : alist<float>) =
    let sum =
      alst |> tryReduce (fun x y -> x + y)
    adaptive {
      let! sum = sum
      let! c = AList.count alst
      return Option.map (fun s -> s / (float c)) sum
    }

  let sortByDescending (f : 'a -> 'b)  (alst : alist<'a>)  =
    alst |> AList.sortWith (fun c d -> compare (f d) (f c))
    


    //|> AList.map mapper
    //     |> bindIMod

  let averageOf (f : 'a -> float) (alst : alist<'a>) = //TODO make dynamic
    alst
      |> AList.map f
      |> average

  let filter' (f : 'a -> IMod<bool>) (alst : alist<'a>) =
    alist {
      for el in alst do
        let! fil = f el
        if fil then yield el
    }

  let filterNone (lst : alist<option<'a>>) =
    lst
      |> AList.filter (fun el -> 
                        match el with
                          | Some el -> true
                          | None    -> false)
      |> AList.map (fun el -> el.Value)

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
     
