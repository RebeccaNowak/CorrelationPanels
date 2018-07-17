namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationPoint =
  open System
  open Aardvark.Base

  let initial : AnnotationPoint = {
    point = V3d.OOO
    selected = false
  }

  let elevation (model : AnnotationPoint) = 
    model.point.Length

  let calcRange (annos : plist<Annotation>) : Rangef = //TODO refactor (not in the right place)
    let rangeMin = 
      annos 
        |> PList.map Annotation.lowestPoint
        |> PList.map elevation
        |> PList.minBy (fun x -> x) //TODO unsafe

    let rangeMax = 
      annos 
        |> PList.map Annotation.highestPoint
        |> PList.map elevation
        |> PList.maxBy (fun x -> x) //TODO unsafe
    {min = rangeMin; max = rangeMax}

  let tryCalcRange (annos : plist<Annotation>) : option<Rangef> = //TODO refactor (not in the right place)
    let rangeMin = 
      annos 
        |> PList.map Annotation.tryLowestPoint
        |> PList.map (fun x -> Option.map elevation x)
        |> PList.filterNone
        |> PList.tryMinBy (fun x -> x) 

    let rangeMax = 
      annos 
        |> PList.map Annotation.tryHighestPoint
        |> PList.map (fun x -> Option.map elevation x)
        |> PList.filterNone
        |> PList.tryMaxBy (fun x -> x) 
    Option.map2 (fun min max -> {min = min; max = max}) rangeMin rangeMax