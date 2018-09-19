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
      |> PList.filter (fun el -> 
                        match el with
                          | Some el -> true
                          | None    -> false)
      |> PList.map (fun el -> el.Value)

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

  let tail (lst : plist<'a>) =
    match lst.IsEmptyOrNull () with
      | true  -> PList.empty
      | false -> PList.removeAt 0 lst

  let tryHead (lst : plist<'a>) =
    match lst.IsEmptyOrNull () with
      | true  -> None
      | false -> Some (lst.Item 0)
  
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
        


  

module AList =
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
    
  let reduce (f : 'a -> 'a -> 'a) (alst: alist<'a>) =
    alst.Content
      |> Mod.map (fun (x : plist<'a>) -> 
                      let r =
                        PList.toList x
                           |> List.reduce f
                      r
                  )

  let minBy (f : 'a -> 'b) (alst : alist<'a>) =
    alst
      |> reduce (fun x y -> if (f x) < (f y) then x else y)
      
  let min (alst : alist<'a>) =
     alst |>
      reduce (fun x y -> if x < y then x else y)

  let maxBy (f : 'a -> 'b) (alst : alist<'a>)  =
    alst
      |> reduce (fun x y -> if (f x) > (f y) then x else y)
      
  let max (alst : alist<'a>) =
    alst
      |> reduce (fun x y -> if x > y then x else y)

  let average (alst : alist<float>) =
    let sum =
      alst |> reduce (fun x y -> x + y)
    Mod.map2 (fun s c -> s / (float c)) sum (AList.count alst)
    

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
  let toSortedPlist (input : hmap<_,'a>) (projection : ('a -> 'b)) : plist<'a> =
    input 
        |> HMap.toSeq 
        |> Seq.map snd
        |> Seq.sortBy projection
        |> PList.ofSeq