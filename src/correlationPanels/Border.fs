namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Border =

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.Base.Incremental.Operators
  open Aardvark.Base.Rendering
  open Aardvark.UI
  open Aardvark.Rendering.Text

//  let initial id : Border = {
//    anno  = Annotation.initialDummy
//    point = V3d.OOO
//  }


  let posInf = V3d.PositiveInfinity
  let negInf = V3d(0.0)

  let initial anno point : Border = {
    anno        = anno //Annotation.initialDummy
    point       = point //V3d.OOO
    color       = C4b.Black
    weight      = 1.0
    styleType   = BorderStyle.Annotation
    borderType  =
      match point with
        | p when p = posInf -> BorderType.PositiveInfinity
        | p when p = negInf -> BorderType.NegativeInfinity
        | _      -> BorderType.Normal
  }

  let initNegInf = 
    initial (Annotation.initialDummyWithPoints (negInf)) negInf
  
  let initPosInf = 
    initial (Annotation.initialDummyWithPoints (posInf)) posInf

  //type Action

  //let update (model : Border) =


  let getAvgY (points : alist<V3d>) =
    (AList.toList points) |> List.averageBy  (fun x -> x.Y)

  let getMinY (points : alist<V3d>) =
    (AList.toList points) |> List.minBy  (fun x -> x.Y)

  let calcElevation (border : Border) = 
    border.point.Length
    
  module Sg =

    let createLabel (str : string) (pos : V3d) =
      Sg.text (Font.create "courier" FontStyle.Regular) C4b.White (Mod.constant str)
          |> Sg.billboard
          |> Sg.noEvents
          |> Sg.trafo(Mod.constant (Trafo3d.Translation pos))

    

//    let view' (annos : alist<MAnnotation>)  =
//        let sortedAnnos = annos |> AList.sortBy (fun x -> getAvgY x.points)
//        sortedAnnos |> AList.map (fun x -> createLabel x.id (getMinY x.points))
//          |> AList.toList
//          |> Sg.ofList
//          
//
//    let view (model : MBorder)  =
//        let sortedAnnos = model.annotations |> AList.sortBy (fun x -> getAvgY x.points)
//        sortedAnnos |> AList.map (fun x -> createLabel x.id (getMinY x.points))
//          |> AList.toList
//          |> Sg.ofList
//        for anno in sortedAnnos |> AList.toList do
//          createLabel anno.id


