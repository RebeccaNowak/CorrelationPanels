namespace DS

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

