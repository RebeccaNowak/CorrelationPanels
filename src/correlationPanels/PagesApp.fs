namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pages =
  open System.Windows.Forms
  open Aardvark.UI
  open Aardvark.UI.Primitives
  open Aardvark.Application
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.Base.Rendering
  open UtilitiesGUI

  type Action =
      | CameraMessage                 of CameraController.Message
      | KeyDown                       of key : Keys
      | KeyUp                         of key : Keys     
      | CorrPlotMessage               of CorrelationPlotApp.Action
      | SemanticAppMessage            of SemanticApp.Action
      | AnnotationAppMessage          of AnnotationApp.Action
      | CorrelationDrawingMessage     of CorrelationDrawing.Action
      | CenterScene
      | UpdateConfig                  of DockConfig
      | Export
      | Save
      | Load
      | Clear
      | Undo
      | Redo
      | SetCullMode                   of CullMode
      | ToggleFill
      

  let initialCamera = {CameraController.initial with
                          view = CameraView.lookAt (10.0 * Mars.Terrain.up + V3d.OOI * -20.0) 
                                                   (10.0 * Mars.Terrain.up) 
                                                    Mars.Terrain.up}

  let initial   = 
    { 
        past        = None
        future      = None
        camera      = initialCamera
        cullMode    = CullMode.None
        fill        = true
        rendering = RenderingPars.initial
        dockConfig  =
            config {
                content (
                 // element {id "render"; title "Render View"; weight 5}
                  vertical 0.6 [
                    horizontal 0.5 [
                      element {id "render"; title "Render View"; weight 5}
                      element {id "semantics"; title "Semantics"; weight 3}
//                      stack 9.0 (Some "render") [dockelement {id "render"; title "Render View"; weight 5};
//                                                 dockelement { id "semantics"; title "Semantics"; weight 5}]
                    ]
                    horizontal 0.5 [
                      stack 1.0 (Some "render") [
                        dockelement { id "svg"; title "SVG"; weight 1}
                        dockelement { id "logs"; title "Logs"; weight 1}
                        dockelement { id "controls"; title "Controls"; weight 1 }
                      ]]
                    ]
                   
                  

                  
                )
                appName "CDPages"
                useCachedConfig false
            }
        annotationApp = AnnotationApp.initial
        semanticApp   = SemanticApp.getInitialWithSamples
        drawingApp    = CorrelationDrawing.initial 
        corrPlotApp   = CorrelationPlotApp.initial 
    }

  let update (model : Pages) (msg : Action) =
      match msg, model.corrPlotApp.creatingNew, model.drawingApp.draw with
          | KeyDown Keys.Enter, _, _ ->                          
            let aa = AnnotationApp.update 
                        model.annotationApp 
                        (AnnotationApp.Action.AddAnnotation model.drawingApp.working.Value) //TODO make this safe
            let da = CorrelationDrawing.update 
                                model.drawingApp
                                model.semanticApp
                                (CorrelationDrawing.Action.KeyDown Keys.Enter)
            {model with drawingApp    = da
                        annotationApp = aa}
          | KeyDown k, _,_       -> 
            printf "%s" "key down"
            let d = CorrelationDrawing.update 
                      model.drawingApp
                      model.semanticApp
                      (CorrelationDrawing.Action.KeyDown k)
            {model with drawingApp   = d
                        camera       = CameraController.update model.camera (CameraController.Message.KeyDown k)}
          | KeyUp k, _,_        -> 
            printf "%s" "key up"
            let d = CorrelationDrawing.update 
                      model.drawingApp 
                      model.semanticApp
                      (CorrelationDrawing.Action.KeyUp k)
            { model with drawingApp  = d
                         camera      = CameraController.update model.camera (CameraController.Message.KeyUp k)}
          | SemanticAppMessage m, false, false ->
                {model with semanticApp = SemanticApp.update model.semanticApp m}
          | AnnotationAppMessage m, creatingLog, drawing -> 
            {model with annotationApp = AnnotationApp.update model.annotationApp m}
          | CorrelationDrawingMessage m, creatingPlot, drawing ->
            let (corrApp, drawingApp, annoApp) =               
                match m, creatingPlot, drawing with
                  | CorrelationDrawing.AddPoint p, creatingLog, drawing-> 
                    printf "%s" "Add Point"
                    let (drawingApp, annoApp) =
                      match CorrelationDrawing.isDone model.drawingApp with
                        | true  -> 
                          let da = CorrelationDrawing.addPoint model.drawingApp model.semanticApp p
                          let aa = AnnotationApp.update model.annotationApp (AnnotationApp.Action.AddAnnotation da.working.Value) //TODO safe but  maybe do this differently
                          let da = {da with working = None
                                            draw    = false}
                          (da, aa)
                        | false -> (CorrelationDrawing.update model.drawingApp model.semanticApp m, model.annotationApp)
                    (
                      model.corrPlotApp, 
                      drawingApp,
                      annoApp
                    ) 
                  | _ ,_ ,_-> 
                    (
                      model.corrPlotApp, 
                      CorrelationDrawing.update model.drawingApp model.semanticApp m,
                      model.annotationApp
                    )
            {model with drawingApp    = drawingApp
                        corrPlotApp   = corrApp
                        annotationApp = annoApp}

          | CorrPlotMessage m, _, false -> // TODO refactor
            let corrPlotApp = 
              let sel = (AnnotationApp.getSelected model.annotationApp)
                          |> PList.toList
                          |> List.map (fun (p, a) -> (p.point, a))
              {model.corrPlotApp with selectedPoints = sel}
                |> CorrelationPlotApp.update 
                    model.annotationApp.annotations
                    model.semanticApp m
            match m with
              | CorrelationPlotApp.NewLog | CorrelationPlotApp.FinishLog -> 
                {model with 
                  drawingApp = 
                        (CorrelationDrawing.update model.drawingApp model.semanticApp CorrelationDrawing.DeselectAllPoints)
                    
                  corrPlotApp = corrPlotApp
                }

              | _ -> {model with corrPlotApp = corrPlotApp}
              //  match logId with
              //    | Some lid -> 
              //      let (drawingApp, cApp) =
              //        let updateBoth = 
              //          let ind = model.corrPlotApp.logs.FirstIndexOf (fun l -> l.id = lid)
              //          let selLog = model.corrPlotApp.logs.Item ind
              //          let cdUpdate = CorrelationDrawing.update 
              //                model.drawingApp.drawing 
              //                model.semanticApp 
              //                (CorrelationDrawing.SelectPoints (
              //                    selLog.annoPoints
              //                      |> List.map (fun (p,a) -> (p, a.id))
              //                  )
              //                )
                          
              //            (cdUpdate, {corrPlotApp with selectedLog = logId})

              //        match model.corrPlotApp.selectedLog with
              //          | Some se -> 
              //            match (se = lid) with 
              //              | true  ->                           
              //                ((CorrelationDrawing.update 
              //                          model.drawingApp
              //                          model.semanticApp 
              //                          (CorrelationDrawing.DeselectAllPoints))
              //                , {corrPlotApp with selectedLog = None})
              //              | false -> updateBoth

              //          | None -> updateBoth
                          
              //      {model with corrPlotApp = cApp
              //                  drawingApp  = drawingApp}
              //    | None -> model
              //| _ -> 
              //  { model with corrPlotApp = corrPlotApp}
          | CenterScene, _, _ -> 
              { model with camera = initialCamera }

          | UpdateConfig cfg, _,_->
              { model with dockConfig = cfg; past = Some model }

          | SetCullMode mode, _,_ ->
              { model with cullMode = mode; past = Some model }

          | ToggleFill, _,_ ->
              { model with fill = not model.fill; past = Some model }
          | Save, false, false -> 
              //let path = "./saved"
              //let cdApp = CorrelationDrawingApp.update model.drawingApp model.semanticApp CorrelationDrawingApp.Action.Save
              model
          | Load, false, false -> 
              //let path = "./saved"
              //let cdApp = CorrelationDrawingApp.update model.drawingApp model.semanticApp CorrelationDrawingApp.Action.Load
              model
//                  
          | Clear, _,_ -> model
//                  { model with drawing = { model.drawing with annotations = PList.empty }}            

//          | Undo ->
//              match model.past with
//                  | Some p -> { p with future = Some model; cameraState = model.cameraState }
//                  | None -> model
//
//          | Redo ->
//              match model.future with
//                  | Some f -> { f with past = Some model; cameraState = model.cameraState }
//                  | None -> model
          | CameraMessage m, _,_ -> 
                { model with camera = CameraController.update model.camera m }   
          | _   -> model

  let viewScene (model : MPages) =
      Sg.box (Mod.constant C4b.Green) (Mod.constant Box3d.Unit)
      |> Sg.shader {
          do! DefaultSurfaces.trafo
          do! DefaultSurfaces.vertexColor
          do! DefaultSurfaces.simpleLighting
      }
      |> Sg.cullMode model.cullMode
      |> Sg.fillMode (model.fill |> Mod.map (function true -> FillMode.Fill | false -> FillMode.Line))


  let view  (runtime : IRuntime) (model : MPages) =
    //let toggleBox (str : string) (state : IMod<bool>) (toggle : 'msg) =
    //      let attributes = 
    //          amap {
    //                  yield attribute "type" "checkbox"
    //                  yield onChange (fun _ -> toggle)
    //                  let! check = state
    //                  if check then
    //                      yield attribute "checked" "checked"
    //          }

    //      onBoot "$('#__ID__').checkbox()" (
    //          div [clazz "ui small toggle checkbox"] [
    //              Incremental.input (AttributeMap.ofAMap attributes)
    //              label [] [text str]
    //          ]
    //      )
  
    let menu = 
      let menuItems = [
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Save)] 
                          [i [clazz "small save icon"] [] ] |> wrapToolTip "save"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Load)] 
                          [i [clazz "small folder outline icon"] [] ] |> wrapToolTip "load"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Clear)]
                          [i [clazz "small file outline icon"] [] ] |> wrapToolTip "clear"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Export)]
                          [i [clazz "small external icon"] [] ] |> wrapToolTip "export"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Undo)] 
                          [i [clazz "small arrow left icon"] [] ] |> wrapToolTip "undo"];
              div [clazz "item"]
                  [button [clazz "ui icon button"; onMouseClick (fun _ -> Redo)] 
                          [i [clazz "small arrow right icon"] [] ] |> wrapToolTip "redo"]]

      body [style "width: 100%; height:100%; background: transparent; overflow: auto"] [
        div [style "vertical-align: middle"]
            [div [
                    clazz "ui horizontal inverted menu";
                    style "float:middle; vertical-align: middle"
                 ]
                 menuItems
            ]
      ]
    

//    let renderControl =
//        CameraController.controlledControl model.cameraState Camera (Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.constant) 
//                    (AttributeMap.ofList [ style "width: 100%; height:100%"; attribute "data-samples" "8" ]) 
//                    (viewScene model)

    page (fun request -> 
        match Map.tryFind "page" request.queryParams with
            //| Some "render" ->
            //    let msg = "Unknown page"
            //    require (myCss) (
            //      body [] [
            //          Incremental.div (AttributeMap.ofList [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"]) 
            //                          (alist {yield text msg})
            //      ]  
            //    )
            | Some "render" -> // renderControl
              let annoSg         = AnnotationApp.Sg.view model.annotationApp model.semanticApp model.camera.view 
                                     |> Sg.map AnnotationAppMessage                
              let drawingSgList  = CorrelationDrawing.Sg.view model.drawingApp model.semanticApp model.camera.view
                                     |> List.map (fun x -> x |> Sg.map CorrelationDrawingMessage) 
              let corrSg         = CorrelationPlotApp.getLogConnectionSgs model.corrPlotApp model.semanticApp model.camera
                                     |> Sg.map CorrPlotMessage
              let frustum = Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)
              require (myCss) (
                body [clazz "ui"; style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"] [
                  div [] [
                    CameraController.controlledControl 
                        model.camera
                        CameraMessage 
                        frustum
                        (AttributeMap.ofList [
                                    onKeyDown (KeyDown)
                                    onKeyUp (KeyUp)
                                    attribute "style" "width:70%; height: 100%; float: left;"]
                        )

                        (drawingSgList @ [annoSg; corrSg]
                          |> Sg.ofList 
                          |> Sg.fillMode (model.rendering.fillMode)     
                          |> Sg.cullMode (model.rendering.cullMode))                                                                                                             
                  ]
                ]
              )
                

                  
            | Some "controls" -> 
                require Html.semui (
                    menu
                )

            | Some "svg" -> 
                body [] [
                  div [] [
                    CorrelationPlotApp.viewSvg model.corrPlotApp model.semanticApp |> (UI.map CorrPlotMessage)
                  ]
                ]

            | Some "logs" ->
                CorrelationPlotApp.view model.corrPlotApp model.semanticApp
                  |> UI.map CorrPlotMessage

            | Some "semantics" ->
              //require Html.semui (
                SemanticApp.viewSemantics model.semanticApp 
                  |> UI.map SemanticAppMessage
              //)
              
            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                ]  

            | None -> 
                model.dockConfig |> Mod.force |> Mod.constant |> docking [
                    style "width:100%;height:100%;"
                    onLayoutChanged UpdateConfig
                ]
    )

//  let threads (model : Pages) = 
//      CameraController.threads model.cameraState |> ThreadPool.map Camera


  let start (runtime: IRuntime) =
      App.start {
          unpersist = Unpersist.instance
          threads = fun _ -> ThreadPool.empty//threads
          view = view runtime
          update = update
          initial = initial
      }

//  let app =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
//      {
//          unpersist = Unpersist.instance     
//          threads   = pool runtime signature 
//          initial   = initial
//          update    = update 
//          view      = view
//      }
