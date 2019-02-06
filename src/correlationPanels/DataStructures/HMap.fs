﻿namespace DS

  module HMap =
    open System
    open Aardvark.Base.Incremental
    open Aardvark.Base

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