namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationPoint =
  open System
  open Aardvark.Base

  let initial : AnnotationPoint = {
    point = V3d.OOO
    selected = false
  }




  let calcRange (annos : plist<Annotation>) : Rangef = //TODO refactor (not in the right place)
    let rangeMin = 
      annos 
        |> PList.map (fun a -> (a, Annotation.lowestPoint a))
        |> PList.map (fun (a,p) -> a.elevation p.point)
        |> DS.PList.minBy (fun x -> x) //TODO unsafe

    let rangeMax = 
      annos 
        |> PList.map (fun a -> (a, Annotation.highestPoint a))
        |> PList.map (fun (a,p) -> a.elevation p.point)
        |> DS.PList.maxBy (fun x -> x) //TODO unsafe
    {min = rangeMin; max = rangeMax}

  let tryCalcRange (annos : plist<Annotation>) = //TODO refactor (not in the right place)
    let foo ((a,p) : (Annotation * Option<AnnotationPoint>)) =
      Option.map (fun (x : AnnotationPoint) -> a.elevation x.point) p
      

    let rangeMin = 
      annos 
        |> PList.map (fun a -> (a, Annotation.tryLowestPoint a))
        |> PList.map (fun x -> foo x)
        |> DS.PList.filterNone
        |> DS.PList.tryMinBy (fun x -> x) 

    let rangeMax = 
      annos 
        |> PList.map (fun a -> (a, Annotation.tryHighestPoint a))
        |> PList.map (fun x -> foo x)
        |> DS.PList.filterNone
        |> DS.PList.tryMaxBy (fun x -> x) 
    Option.map2 (fun min max -> {min = min; max = max}) rangeMin rangeMax