namespace CorrelationDrawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.UI
open Aardvark.SceneGraph
open UtilitiesGUI


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
  let initial (id : string) = 
      {     
          id              = id
          semanticType    = SemanticType.Hierarchical
          geometry        = GeometryType.Line
          semanticId      = SemanticId.invalid
          points          = plist<AnnotationPoint>.Empty
          segments        = plist.Empty
          projection      = Projection.Viewpoint
          visible         = true
          text            = "text"
          overrideStyle   = None
          //overrideLevel   = None
          selected        = false
          hovered         = false
      }

  let initialDummy = 
    {     
        id              = "-1"
        semanticType    = SemanticType.Dummy
        geometry        = GeometryType.Line
        semanticId      = SemanticId.invalid
        points          = plist.Empty
        segments        = plist.Empty
        projection      = Projection.Viewpoint
        visible         = true
        text            = "dummy"
        overrideStyle   = None
        //overrideLevel   = None
        selected        = false
        hovered         = false
    }

  let initialDummyWithPoints (v : V3d)= 
    {     
        id              = "-2"
        semanticType    = SemanticType.Dummy
        geometry        = GeometryType.Point
        semanticId      = SemanticId.invalid
        points          = PList.ofList [{point    = v
                                         selected = false
                                       }]
        segments        = plist.Empty
        projection      = Projection.Viewpoint
        visible         = false
        text            = "dummy"
        overrideStyle   = 
          Some {Style.color     = { c = C4b.Black }
                Style.thickness = Numeric.init}
        //overrideLevel   = Some 0
        selected        = false
        hovered         = false
    }

  type Action =
      | SetSemantic     of option<SemanticId>
      | ToggleSelected  of V3d
      | Select          of V3d 
      | HoverIn         of string
      | HoverOut        of string
      | Deselect

  let getConstColor  (anno : MAnnotation) (semanticApp : MSemanticApp) (ignoreOverride : bool) = 
    let foo = Mod.force anno.overrideStyle
    let bar = match foo, ignoreOverride with //TODO refactor
                //| null      -> ((SemanticApp.getColor semanticApp anno.semanticId))
                | Some f, false    -> f.color.c
                | Some f, true     -> ((SemanticApp.getColor semanticApp anno.semanticId))
                | None, true       -> ((SemanticApp.getColor semanticApp anno.semanticId))
                | None, false      -> ((SemanticApp.getColor semanticApp anno.semanticId))
    bar

  let getColor' (anno : MAnnotation) (semanticApp : MSemanticApp) =          
    let rval = 
      adaptive {
        return! anno.overrideStyle 
                  |> Mod.bind (fun (st : Option<MStyle>) ->
                      match st with
                        | Some st -> st.color.c
                        | None -> ((SemanticApp.getColor semanticApp anno.semanticId)))
      }
    rval

  let update  (a : Action) (anno : Annotation) =
    match a with
        | SetSemantic str -> match str with
                              | Some s -> {anno with semanticId = s}
                              | None -> anno
        | ToggleSelected (point) -> 
          let ind =       
            anno.points.FirstIndexOf(fun (p : AnnotationPoint) -> V3d.AllEqual(p.point,point)) 
          match ind with
            | -1 -> anno
            | _  ->
              let setTo = not (anno.points.Item ind).selected
              let deselected =
                anno.points |> PList.map (fun p -> {p with selected = false})
              let upd = 
                deselected.Update  
                  (ind, (fun (p : AnnotationPoint) -> {p with selected = setTo}))
              {anno with points = upd}
        | Select (point) ->
          let ind =       
            anno.points.FirstIndexOf(fun (p : AnnotationPoint) -> V3d.AllEqual(p.point,point)) 
          let upd = anno.points.Update (ind, (fun (p : AnnotationPoint) -> {p with selected = true}))
          {anno with points         = upd}
        | HoverIn    id  -> 
          match (id = anno.id) with
            |  true -> 
              {anno with  hovered        = true
                          overrideStyle  = Some {Style.color      = {c = C4b.DarkYellow};
                                                Style.thickness  = {Numeric.init with value = 3.0}
                                                }
              }
            | false -> anno
        | HoverOut   id  ->
          match (id = anno.id) with
            | true -> {anno with hovered = false
                                 overrideStyle = None}
            | false -> anno
        | Deselect       -> {anno with points = anno.points |> PList.map (fun p -> {p with selected = false})
                                       overrideStyle  = None}


  let getSelected (anno : Annotation) =
    let sel = 
      anno.points.Filter (fun i p -> p.selected)
        |> PList.tryAt 0
    sel |> Option.map (fun s -> (s, anno))

  module View = 
    let getColourIcon (model : MAnnotation) (semanticApp : MSemanticApp) =
      let iconAttr =
        amap {
          yield clazz "circle icon"
          let! c = getConstColor model semanticApp false
          yield style (sprintf "color:%s" (colorToHexStr c))
        }      
      Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList [])

    let viewSelected (model : MAnnotation)  (semanticApp : MSemanticApp) = 
      let semanticsNode = 
        let iconAttr =
          amap {
            yield clazz "circle outline icon"
            let! c = SemanticApp.getColor semanticApp model.semanticId
            yield style (sprintf "color:%s" (colorToHexStr c))
          }      
        td [clazz "center aligned"; style lrPadding] 
           [
            Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList []);
            Incremental.text (SemanticApp.getLabel semanticApp model.semanticId)
           ]

        
      let geometryTypeNode =
        td [clazz "center aligned"; style lrPadding] 
           //[label  [clazz "ui label"] 
                   [text (model.geometry.ToString())]

      let projectionNode =
        td [clazz "center aligned"; style lrPadding] 
           //[label  [clazz "ui label"] 
                   [text (model.projection.ToString())]

      let annotationTextNode = 
          td [clazz "center aligned"; style lrPadding] 
             //[label  [clazz "ui label"] 
                     [Incremental.text model.text]
    
      [semanticsNode;geometryTypeNode;projectionNode;annotationTextNode]

    let viewDeselected (model : MAnnotation)  (semanticApp : MSemanticApp) = 
      let semanticsNode = 
        let iconAttr =
          amap {
            yield clazz "circle icon"
            let! c = SemanticApp.getColor semanticApp model.semanticId
            yield style (sprintf "color:%s" (colorToHexStr c))
  //          yield attribute "color" "blue"
          }      
        td [clazz "center aligned"; style lrPadding] 
           [
            Incremental.i (AttributeMap.ofAMap iconAttr) (AList.ofList []);
            //label  [clazz "ui label"] 
                   Incremental.text (SemanticApp.getLabel semanticApp model.semanticId)]

        
      let geometryTypeNode =
        td [clazz "center aligned"; style lrPadding] 
           //[label  [clazz "ui label"] 
                   [text (model.geometry.ToString())]

      let projectionNode =
        td [clazz "center aligned"; style lrPadding] 
           //[label  [clazz "ui label"] 
                   [text (model.projection.ToString())]

      let annotationTextNode = 
          td [clazz "center aligned"; style lrPadding] 
             //[label  [clazz "ui label"] 
                     [Incremental.text model.text]
    
      [semanticsNode;geometryTypeNode;projectionNode;annotationTextNode]


    let view  (model : MAnnotation)  (semanticApp : MSemanticApp) = 
      model.selected
        |> Mod.map (fun d -> 
            match d with
              | true  -> viewSelected model semanticApp
              | false -> viewDeselected model semanticApp)
 
  module Sg =
    let view (model : MAnnotation) (cam : IMod<CameraView>) (semApp : MSemanticApp) =
      let annoPointToSg (point : MAnnotationPoint) (color : IMod<C4b>) (weight : IMod<float>) =  
        let trafo = (Mod.constant (Trafo3d.Translation(point.point))) //TODO dynamic
        let pickSg = 
           Sg.sphereWithEvents (Mod.constant C4b.White) weight 
                [
                  Sg.onClick (fun p -> ToggleSelected (point.point))
                  Sg.onEnter (fun _ -> HoverIn model.id)
                  Sg.onLeave (fun _ -> HoverOut model.id)
                ]
              |> (Sg.trafo trafo)
              |> Sg.depthTest (Mod.constant DepthTestMode.Never)
          
        point.selected 
          |> Mod.map (fun sel -> 
                        let col =
                          match sel with
                            | true -> (Mod.constant C4b.Yellow)   
                            | false -> color
                        Sg.sphereDyn col weight  
                                |> Sg.trafo(trafo))
          |> Sg.dynamic
          |> Sg.andAlso pickSg


      let makeLinesSg (model : MAnnotation) (color : IMod<C4b>) (width : IMod<float>) = 
          Mod.constant (Sg.pathDyn false (model.points //TODO dynamic
                              |> AList.toList 
                              |> List.map (fun ap -> ap.point)))
              |> Sg.lines color
              |> Sg.effect [
                  toEffect DefaultSurfaces.trafo
                  toEffect DefaultSurfaces.vertexColor
                  toEffect DefaultSurfaces.thickLine                                
                  ] 
              |> Sg.noEvents
              |> Sg.uniform "LineWidth" width
              |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
              |> Sg.depthTest (Mod.constant DepthTestMode.None)  

      let color = getColor' model semApp
      let thickness = SemanticApp.getThickness semApp model.semanticId
      let dots =       
        aset {
          for p in model.points |> ASet.ofAList do
            yield annoPointToSg
                    p
                    color 
                    (thickness |> Mod.map  (fun x -> x * 0.1)) // (computeScale view (p.point) 5.0) 
        } 
        |> Sg.set

      [
       Sg.noEvents <| makeLinesSg model color thickness;
       Sg.noEvents <| dots
      ] |> ASet.ofList



      
    
              //let makeSphereSg' color size trafo (anno : MAnnotation) (point : MAnnotationPoint) (view : IMod<CameraView>) =      
      //  let elevationLabel =
      //    makeLblSg (sprintf "%.2f" (Mod.force point.point).Length) (Mod.force point.point)
  ///////// HELPER FUNCTIONS
  let getColor (imAnno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) =
    adaptive {
      let! optAnno = imAnno
      let (a : IMod<C4b>, b : IMod<bool>) = 
          match optAnno with
            | Some an -> 
              let col =
                an.overrideStyle 
                  |> Mod.bind (fun (st : Option<MStyle>) ->
                                match st with
                                  | Some st -> st.color.c
                                  | None -> ((SemanticApp.getColor semanticApp an.semanticId))
                              )
              (col, an.hovered)
//                let! style = a.overrideStyle
//                match style with
//                  | Some st -> st.color
//                  | None -> ((SemanticApp.getColor semanticApp a.semanticId), a.hovered)
//              foo
            //let foo = ((SemanticApp.getColor semanticApp a.semanticId), a.hovered)
            //foo
            | None -> ((Mod.constant C4b.Red), (Mod.constant false))

      let! hover = b
      let! col = a
//      return match hover with
//              | true -> C4b.Yellow
//              | false -> col
      return col
    }
//      Mod.map2 (fun c h -> 
//                  match h with 
//                    | true -> Mod.constant C4b.Yellow
//                    | false -> c
//               ) c h 

      //let! h = anno.hovered
//      return! match h with
//                | true -> Mod.constant C4b.Yellow
//                | false -> SemanticApp.getColor semanticApp anno.semanticId


  
    
 

  let getThickness (anno : IMod<Option<MAnnotation>>) (semanticApp : MSemanticApp) = 
    Mod.bind (fun (a : Option<MAnnotation>)
                  -> match a with
                      | Some a -> SemanticApp.getThickness semanticApp a.semanticId
                      | None -> Mod.constant Semantic.ThicknessDefault) anno   

  let getThickness' (anno : MAnnotation) (semanticApp : MSemanticApp) = 
    SemanticApp.getThickness semanticApp anno.semanticId

//  let calcElevation (v : V3d) =
//    v.Y

//  let getAvgElevation (anno : MAnnotation) =
//    anno.points
//      |> AList.averageOf (calcElevation 
//
  let elevation (anno : Annotation) =
    anno.points 
      |> PList.toList
      |> List.map (fun x -> x.point.Length)
      |> List.average

  let lowestPoint (anno : Annotation) =
    anno.points 
      |> PList.minBy (fun x -> x.point.Length)

  let highestPoint (anno : Annotation) =
    anno.points 
      |> PList.maxBy (fun x -> x.point.Length)

  let elevation' (anno : MAnnotation) =
    adaptive {
      let! lst = anno.points.Content
      return lst
              |> PList.map (fun x -> x.point.Length)
              |> PList.toList
              |> List.average
    }

  let isElevationBetween (a : float) (b : float) (model : Annotation)  =
    (a < (elevation model) && (b > (elevation model)))

  let sortByElevation (p1 : V3d, a1 : Annotation)  (p2 : V3d, a2 : Annotation) =
    let (lp, la) = if p1.Length < p2.Length then (p1, a1) else (p2, a2) //TODO refactor
    let (up, ua) = if p1.Length < p2.Length then (p2, a2) else (p1, a1)
    ((lp,la),(up,ua))

//
//  let getMinElevation (anno : MAnnotation) = 
//    anno.points
//      |> AList.minBy calcElevation
//
//  let getMaxElevation (anno : MAnnotation) = 
//    anno.points
//      |> AList.maxBy calcElevation
//
//  let getRangeMinMax (anno : MAnnotation) =
//    let min = anno.points
//                |> AList.minBy calcElevation
//    let max = anno.points
//                |> AList.maxBy calcElevation
//    Mod.map2 (fun (x : V3d) (y : V3d) -> V2d(x.Y,y.Y)) min max
//
//  let getRange (anno : MAnnotation) =
//    let min = anno.points
//                |> AList.minBy calcElevation
//    let max = anno.points
//                |> AList.maxBy calcElevation
//    Mod.map2 (fun (mi : V3d) (ma : V3d) -> (ma.Y - mi.Y)) min max

//
//  let getMinElevation' (annos : alist<MAnnotation>) =
//    AList.toArray (annos |> AList.map (fun x -> getAvgElevation x))
//      |> Array.min
//
//  let getMaxElevation' (annos : alist<MAnnotation>) =
//    AList.toArray (annos |> AList.map (fun x -> getAvgElevation x))
//      |> Array.max
//
//  let getMinMaxElevation (annos : alist<MAnnotation>) =
//    let avgs = (annos |> AList.map (fun x -> getAvgElevation x))
//    Mod.map2 (fun  (x : float) (y : float) -> V2d(x,y)) (avgs |> AList.min) (avgs |> AList.max)

  let getType (semanticApp : SemanticApp) (anno : Annotation) =
    let s = (SemanticApp.getSemantic semanticApp anno.semanticId)
    match s with
      | Some s -> s.semanticType
      | None   -> SemanticType.Undefined

  let getLevel (semanticApp : SemanticApp) (anno : Annotation) =
    //match anno.overrideLevel with
    //  | Some o -> o
    //  | None ->
        let s = (SemanticApp.getSemantic semanticApp anno.semanticId)
        match s with
          | Some s -> s.level
          | None   -> -1

  let onlyHierarchicalAnnotations (semanticApp : SemanticApp) (nodes : List<Annotation>) =
    nodes
      |> List.filter (fun (a : Annotation) -> 
      match (SemanticApp.getSemantic semanticApp a.semanticId) with
        | Some s  -> s.semanticType = SemanticType.Hierarchical
        | None    -> false)

  let splitByLevel (semanticApp : SemanticApp) (annos : List<Annotation>) =
    let sem (a : Annotation) =  
      match (SemanticApp.getSemantic semanticApp a.semanticId) with
        | Some s -> s
        | None -> Semantic.initInvalid //TODO something useful

    let levels = 
      annos 
        |> List.map (fun x -> (sem x).level) 
        |> List.distinct
      
    levels 
      |> List.map (fun (lvl : int) ->
                    annos 
                      |> List.filter (fun a -> (sem a).level = lvl))

  let onlyLvli (semanticApp : SemanticApp) (i : int) (annos : List<Annotation>) =
    annos
      |> List.filter (fun (a : Annotation) -> 
      match (SemanticApp.getSemantic semanticApp a.semanticId) with
        | Some s  -> s.level = i
        | None    -> false)

  
      
        
        
      












