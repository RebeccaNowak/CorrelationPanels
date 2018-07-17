namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Border =

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Aardvark.Rendering.Text

//  let initial id : Border = {
//    anno  = Annotation.initialDummy
//    point = V3d.OOO
//  }


  let posInf = V3d.PositiveInfinity
  let negInf = V3d(0.0)

  let initialEmpty : Border = 
    {
      id          = BorderId.invalid
      nodeId      = LogNodeId.invalid
      logId       = LogId.invalid
      isSelected  = false
      correlation = None
      anno        = Annotation.initialDummy
      point       = V3d.OOO
      color       = C4b.Gray
      weight      = 0.0
      borderType  = BorderType.Invalid
      svgPosition = V2d(0.0)
    }

  let initial anno point nodeId logId : Border = {
    id          = BorderId.newId()
    nodeId      = nodeId
    logId       = logId
    isSelected  = false
    correlation = None
    anno        = anno //Annotation.initialDummy
    point       = point //V3d.OOO
    color       = C4b.Gray
    weight      = 1.0
    borderType  =
      match point with
        | p when p = posInf -> BorderType.PositiveInfinity
        | p when p = negInf -> BorderType.NegativeInfinity
        | _      -> BorderType.Normal
    svgPosition = V2d(0.0)
  }

  let initNegInf = 
    initial (Annotation.initialDummyWithPoints (negInf)) negInf LogNodeId.invalid
  
  let initPosInf = 
    initial (Annotation.initialDummyWithPoints (posInf)) posInf LogNodeId.invalid

  type Action =
    | Correlate of BorderId 
    | ToggleSelect of (BorderId * V2d)

  let update (model : Border) (action : Action) =
    match action with
      | Correlate b   -> {model with correlation = Some b}
      | ToggleSelect (id, pos) -> 
        match id = model.id with
          | true  -> {model with isSelected = not model.isSelected}
          | false -> model

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

  module Svg = 
    let getCorrelationButtons (model : MLogNode) (offset : float) (weight : float) = //callback  =
      let btnSize = 4.0 //TODO might want to make this an argument
      adaptive {
        let! uBorderColor = model.uBorder.color
        let! lBorderColor = model.lBorder.color
        
        let! size = model.svgSize
        let! pos  = model.svgPos
        //let pos   = new V2d(offset, pos.Y)

        let posL  = (new V2d(offset + size.X, pos.Y + size.Y - (weight * 0.5)))
        let posU  = (new V2d(offset + size.X, pos.Y + (weight * 0.5)))

        let posLBtn = new V2d(posL.X, posL.Y - (btnSize * 0.5))
        let posUBtn = new V2d(posU.X, posU.Y + (btnSize * 0.5))

        let lcb = (fun lst -> ToggleSelect (model.lBorder.id, posL)) //callback model.lBorder.id
        let ucb = (fun lst -> ToggleSelect (model.uBorder.id, posU)) //callback model.uBorder.id

        let! lSel = model.lBorder.isSelected
        let btnLowerBorder =              
         Svg.drawCircleButton 
           posLBtn
           5.0 lBorderColor lSel 1.0 lcb

        let! uSel = model.uBorder.isSelected
        let btnUpperBorder = 
          Svg.drawCircleButton 
            posUBtn
            5.0 uBorderColor uSel 1.0 ucb

        return ((btnLowerBorder), (btnUpperBorder))
      }


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


