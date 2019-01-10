namespace DS

module PList =
  open Aardvark.Base.Incremental
  open Aardvark.Base


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

  let moveLeft (a : 'a) (lst : plist<'a>)  =
    let f (ind : Index) (current : 'a) =
      match lst.TryGet (ind.After ()), lst.TryGet (ind.Before ()) with
        | Some next, _ -> 
          if a == next then
            next
          else
            current
        | _, Some prev ->
          if a = prev then
            prev
          else
            current
        | None, None   -> current
    PList.mapi f lst

  let moveRight (a : 'a) (lst : plist<'a>)  =
    lst

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

  let rec mapPrev' (keys  : plist<'key>) 
                   (items : hmap<'key, 'b>)
                   (prev  : option<'key>)
                   (f     : 'b -> 'b -> 'b) : hmap<'key, 'b> =
    let current = tryHead keys
    let result = 
      match prev, current with
        | None, None     -> HMap.empty
        | None, Some c   -> 
          HMap.add c (items.Item c) (mapPrev' (tail keys) items current f)
        | Some p, None   -> HMap.empty
        | Some p, Some c -> 
          let prev = items.Item p
          let curr = items.Item c
          let _current = (f prev curr)
          let _items   = HMap.update c (fun optv -> _current) items
          let rest = 
            mapPrev' (tail keys) _items current f
          let bar =
            HMap.add c _current rest
          bar   
    result