namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Border =

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Aardvark.Rendering.Text

  let posInf = V3d.PositiveInfinity
  let negInf = V3d(0.0)
  let defaultColor = C4b.Red

  let initialEmpty : Border = 
    {
      id          = BorderId.invalid
      nodeId      = LogNodeId.invalid
      logId       = LogId.invalid
      isSelected  = false
      correlation = None
      annotationId = AnnotationId.invalid
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
    annotationId        = anno //Annotation.initialDummy
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
    //initial (Annotation.initialDummyWithPoints (negInf)) negInf LogNodeId.invalid
    initial AnnotationId.invalid negInf LogNodeId.invalid
  
  let initPosInf = 
    initial AnnotationId.invalid posInf LogNodeId.invalid
   // initial (Annotation.initialDummyWithPoints (posInf)) posInf LogNodeId.invalid



////////////////////////////////////////////////
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

  ////////////////////////////////////////////////

  let annotationId (optModel : option<Border>)  =
    Option.map (fun (m : Border) -> m.annotationId) optModel

  let tryElevation (model : Border) (annoApp : AnnotationApp) =  
    (AnnotationApp.tryElevation annoApp model.annotationId)

  let elevation' (model : MBorder) (annoApp : MAnnotationApp) =
    (AnnotationApp.elevation' annoApp model.annotationId)

  let colorOrDefault (model : IMod<Option<MBorder>>)  =
    Option.extractOrDefault model
                            (fun b -> b.color)
                            C4b.Red


        

  module Sg =
    let createLabel (str : string) (pos : V3d) =
      Sg.text (Font.create "courier" FontStyle.Regular) C4b.White (Mod.constant str)
          |> Sg.billboard
          |> Sg.noEvents
          |> Sg.trafo(Mod.constant (Trafo3d.Translation pos))

  module Svg = 
    open SimpleTypes
    //open Svgplus

    type Corners = {
      upperRight : V2d
      lowerRight : V2d
    }

    let getCorners (svgSize : IMod<Size2D>) (svgPos : IMod<V2d>)
                   (weight : IMod<float>)
                   (buttonSize : IMod<float>) =
      adaptive {
        let! size = svgSize
        let! pos = svgPos
        let! buttonSize = buttonSize
        let! weight = weight
        let halfButtonSize = buttonSize * 0.5
        let halfWeight = weight * 0.5
        let posL  = (new V2d(pos.X + size.width, pos.Y 
                              + size.height - halfWeight - halfButtonSize))
        let posU  = (new V2d(pos.X + size.width, pos.Y               
                                            + halfWeight + halfButtonSize))
        return {upperRight = posU; lowerRight = posL}
      }

    //let getCorrelationButtons (model : MLogNode) =
    //  adaptive {
    //    let! lBorder = model.lBorder
    //    let! uBorder = model.uBorder

    //    match lBorder, uBorder with  
    //      | Some lowerBorder, Some upperBorder ->
    //        let buttonSize = Mod.constant 4.0 //TODO might want to make this an argument
    //        let! uCol = colorOrDefault model.uBorder
    //        let! lCol = colorOrDefault model.lBorder
    //        let weight = model.level |> Mod.map (fun level -> level.weight)
    //        let! corners = getCorners model.mainBody.dim model.mainBody.svgPos weight buttonSize
    //        let lcb = (fun lst -> ToggleSelect (lowerBorder.id, corners.lowerRight)) 
    //        let ucb = (fun lst -> ToggleSelect (upperBorder.id, corners.upperRight))

    //        let! lSel = lowerBorder.isSelected
    //        let btnLowerBorder =              
    //         Svgplus.Base.drawCircleButton 
    //           corners.lowerRight
    //           5.0 lCol lSel 1.0 lcb

    //        let! uSel = upperBorder.isSelected
    //        let btnUpperBorder = 
    //          Svgplus.Base.drawCircleButton 
    //            corners.upperRight
    //            5.0 uCol uSel 1.0 ucb

    //        return Some ((btnLowerBorder), (btnUpperBorder))
    //      | _,_ -> return None
    //  }


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


