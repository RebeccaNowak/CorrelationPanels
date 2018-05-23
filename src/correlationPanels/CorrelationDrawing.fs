namespace CorrelationDrawing


module CorrelationDrawing =
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI

    open System
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.SceneGraph
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.Rendering.Text

    open Aardvark.SceneGraph.SgPrimitives
    open Aardvark.SceneGraph.FShadeSceneGraph
    open Annotation
    open UtilitiesGUI
    open UtilitiesDatastructures

    let initial : CorrelationDrawingModel = {
        draw = false
        hoverPosition = None
        working = None
        projection = Projection.Viewpoint
        geometry = GeometryType.Line
        selectedAnnotation = None
        //annotations = plist.Empty
        exportPath = @"."
       // log = GeologicalLog.intial (Guid.NewGuid().ToString()) plist<AnnotationPoint * Annotation>.Empty
    }

    type Action =
        | DoNothing
        | AnnotationMessage       of Annotation.Action
        | SetGeometry             of GeometryType
        | SetProjection           of Projection
        | SetExportPath           of string
        | Move                    of V3d
        | Exit    
        | AddPoint                of V3d
        | ToggleSelectPoint       of (string * V3d)
        | DeselectAllPoints       
        | SelectPoints            of List<(V3d * string)>
        | KeyDown                 of key : Keys
        | KeyUp                   of key : Keys      
        | Export
           
    let isDone (model : CorrelationDrawingModel) =
      match model.working with
        | Some w -> 
          match (w.geometry, (w.points |> PList.count)) with
                                  | GeometryType.Point, 0 ->  true
                                  | GeometryType.Line,  1 ->  true
                                  | _                     ->  false
        | None -> false

    let addPoint  (model : CorrelationDrawingModel) 
                  (semanticApp : SemanticApp)
                  (point : V3d) =
      let working = 
        match model.working with
          | Some w  ->                                     
              let newAnno = 
                {w with points = w.points 
                                  |> PList.append {AnnotationPoint.initial with point = point
                                                                                selected = false} 
                }
              newAnno
          | None    -> 
              let id      = Guid.NewGuid().ToString()                                     
              let newAnno = {Annotation.initial id with
                              points        = PList.ofList [{AnnotationPoint.initial with point = point
                                                                                          selected = false}];  
                              semanticId    = semanticApp.selectedSemantic
                              geometry      = model.geometry
                              projection    = model.projection}//add annotation states
              newAnno

      {model with working  = Some working}

    let update (model : CorrelationDrawingModel) 
                   (semanticApp : SemanticApp) 
                   (act : Action)  =
        match (act, model.draw) with
            | DoNothing, _ -> model
            | KeyDown Keys.LeftCtrl, _ ->                     
                { model with draw = true }
            | KeyUp Keys.LeftCtrl, _ -> 
                {model with draw = false; hoverPosition = None }
            | Move p, true -> 
                { model with hoverPosition = Some (Trafo3d.Translation p) }
            | AddPoint m, true -> 
                match isDone model with
                  | true  -> 
                    let model = addPoint model semanticApp m
                    {model with working  = None
                                draw     = false}
                  | false -> addPoint model semanticApp m
            //| DeselectAllPoints, _  -> 
            //  {model with annotations = 
            //                model.annotations 
            //                  |> PList.map
            //                      (fun a -> Annotation.update (Annotation.Action.Deselect) a)
            //  }    
            //| SelectPoints lst, false ->
            //  let deselected = model.annotations 
            //                    |> PList.map
            //                        (fun a -> Annotation.update (Annotation.Action.Deselect) a)
            //  let annoInList (a : Annotation) = 
            //    lst 
            //      |> List.map snd
            //      |> List.contains a.id
              
            //  let updateFunction (anno : Annotation) =
            //      match annoInList anno with
            //          | true  -> 
            //            let ind = lst.FirstIndexOf (fun (p, a) -> a = anno.id)
            //            let (pt, a) = lst.Item ind
            //            Annotation.update (Annotation.Action.Select pt) anno
            //          | false -> anno
            //  let updated = deselected |> PList.map updateFunction
                  
            //  {model with annotations = updated}
              

            | KeyDown Keys.Enter, _ -> 
                  match model.working with
                    | Some w  ->
                        {model with working  = None
                                    draw     = false}
                    | None   -> model
            | Exit, _ -> 
                    { model with hoverPosition = None }
            | SetGeometry mode, _   ->
                    { model with geometry = mode }
            | SetProjection mode, _ ->
                    { model with projection = mode }        
            | SetExportPath s, _    ->
                    { model with exportPath = s }
            | Export, _             ->
                    //let path = Path.combine([model.exportPath; "drawing.json"])
                    //printf "Writing %i annotations to %s" (model.annotations |> PList.count) path
                    //let json = model.annotations |> PList.map JsonTypes.ofAnnotation |> JsonConvert.SerializeObject
                    //Serialization.writeToFile path json 
                    
                    model
            | _ -> model

    module UI =
        open Aardvark.Base.Incremental    
       
        //let viewAnnotationTools (model:MCorrelationDrawingModel) (semanticApp : MSemanticApp) =  
        //  [div [clazz "item"] 
        //      [div [clazz "ui small right labeled input"] [
        //              label [clazz "ui basic label"] [text "Geometry"]  // style "color:white"
        //              Html.SemUi.dropDown model.geometry SetGeometry]];
        //  div [clazz "item"] 
        //      [div [clazz "ui small right labeled input"] [
        //              label [clazz "ui basic label"] [text "Projections"]
        //              Html.SemUi.dropDown model.projection SetProjection]]]



        //let viewAnnotations (model : MCorrelationDrawingModel) (semanticApp : MSemanticApp) = 
        //  let domList = 
        //    alist {
        //      for a in model.annotations do
        //        let! annoView = (Annotation.View.view a semanticApp)
        //        yield (tr 
        //          ([style tinyPadding])
        //          (List.map (fun x -> x |> UI.map AnnotationMessage) annoView)
        //        )
        //    }  
        //  div [] [
        //    div[clazz "ui compact horizontal inverted menu"; 
        //        style "float:middle; vertical-align: middle"] 
        //        (viewAnnotationTools model semanticApp)
        //    Html.SemUi.accordion "Annotations" "File Outline" true [
        //      table 
        //        ([clazz "ui celled striped inverted table unstackable"; 
        //          style "padding: 1px 5px 1px 5px"]) 
        //        [thead [][tr[][th[][text "Semantic"];
        //                              th[][text "Geometry"];
        //                              th[][text "Projection"];
        //                              th[][text "Text"]
        //                  ]];
        //        Incremental.tbody (AttributeMap.ofList []) domList]    
        //    ]
        //  ]
          

    module Sg =        
        let computeScale (view : IMod<CameraView>) (p:IMod<V3d>) (size:float) =
            adaptive {
                let! p = p
                let! v = view
                let distV = p - v.Location
                let distF = V3d.Dot(v.Forward, distV)
                return distF * size / 800.0 //needs hfov at this point
            }
        


            
        let makeBrushSg (hovered : IMod<Trafo3d option>) (color : IMod<C4b>) = //(size : IMod<float>)= 
            let trafo =
                hovered |> Mod.map (function o -> match o with 
                                                    | Some t-> t
                                                    | None -> Trafo3d.Scale(V3d.Zero))
            Sg.sphereDyn (color) (Mod.constant 0.05) |> Sg.trafo(trafo) // TODO hardcoded stuff
       
        let view (model         : MCorrelationDrawingModel)
                 (semanticApp   : MSemanticApp) 
                 (cam           : IMod<CameraView>)  =
                 //(annotationSg  : ISg<Action>)
                 //(additionalSg  : ISg<Action>) =             

                
            let sgWorking = 
              model.working |> Mod.map (fun x ->
                match x with
                  | Some a -> (Annotation.Sg.view a cam semanticApp) 
                                |> ASet.map (fun sg -> sg |> Sg.map AnnotationMessage)
                                |> Sg.set
                  | None   -> Sg.ofList [])
                |> Sg.dynamic
            [
              //[Mars.Terrain.mkISg() 
              //  |> Sg.effect Mars.Terrain.defaultEffects
              //  |> Sg.noEvents
              //] 
              //|> Sg.ofList;
              //pick;
              Mars.Terrain.mars [
                    Sg.onMouseMove (fun p -> (Action.Move p))
                    Sg.onClick(fun p -> Action.AddPoint p)
                    Sg.onLeave (fun _ -> Action.Exit)
                  ]
              sgWorking
              makeBrushSg model.hoverPosition (SemanticApp.getColor semanticApp semanticApp.selectedSemantic);
              //annotationSg;
              //additionalSg;
            ] //@ createAnnotationSgs semanticApp model.working cam
            //|> Sg.ofList
            
            
            
