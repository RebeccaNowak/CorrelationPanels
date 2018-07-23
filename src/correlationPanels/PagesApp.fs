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
  open Aardvark.Base.MultimethodTest


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
      | Load                          of SaveIndex
      | Clear
      | Undo
      | Redo
      | SetCullMode                   of CullMode
      | ToggleFill
      | TopLevelEvent
      

  let initialCamera = {CameraController.initial with
                          view = CameraView.lookAt (10.0 * Mars.Terrain.upDummy + V3d.OOI * -20.0) //TODO real mars
                                                   (10.0 * Mars.Terrain.upDummy) 
                                                    Mars.Terrain.upDummy}

  let initial   = 
    let semApp  = SemanticApp.getInitialWithSamples
    {   
      past        = None
      future      = None
      camera      = initialCamera
      cullMode    = CullMode.None
      fill        = true
      rendering   = RenderingPars.initial
      dockConfig  =
          config {
              content (
                // element {id "render"; title "Render View"; weight 5}
                horizontal 0.1 [
                  element { id "controls"; title "Controls"; weight 0.08 }
                  vertical 0.5 [
                    horizontal 0.5 [
                      element {id "render"; title "Render View"; weight 0.4}
                      element {id "semantics"; title "Semantics"; weight 0.6}
  //                      stack 9.0 (Some "render") [dockelement {id "render"; title "Render View"; weight 5};
  //                                                 dockelement { id "semantics"; title "Semantics"; weight 5}]
                    ]
                    horizontal 0.5 [
                      element { id "svg"; title "SVG"; weight 0.5}
                      element { id "logs"; title "Logs"; weight 0.5}
                        //dockelement { id "annotations"; title "Annotations"; weight 1.0}
                    ]
                  ]
                ]
              )
              appName "CDPages"
              useCachedConfig false
          }
      annotationApp = AnnotationApp.initial
      semanticApp   = semApp
      drawingApp    = CorrelationDrawing.initial 
      corrPlotApp   = CorrelationPlotApp.initial
      saveIndex     = SaveIndex.init
    }

  let tmpDebug =
    printf "%s" "PagesAppAction"

  let update (model : Pages) (msg : Action) = //TODO model always last?
    let updateDrawingApp =
      CorrelationDrawing.update model.drawingApp model.semanticApp
    let updateAnnotationApp =
      AnnotationApp.update model.annotationApp 
    let updateCamera =
      CameraController.update model.camera 
    let updatePlot =
      CorrelationPlotApp.update model.corrPlotApp
    let updateSemantics m model = 
      let updSemApp = SemanticApp.update model.semanticApp m
      {model with semanticApp = updSemApp //TODO refactor
                  corrPlotApp = {model.corrPlotApp with semanticApp     = updSemApp
                                                        correlationPlot = {model.corrPlotApp.correlationPlot with semanticApp = updSemApp}
                                }
      }
    let loadSemantics (ind : SaveIndex) model = //TODO refactor
      let updSemApp = SemanticApp.load model.semanticApp (ind.filename SaveType.Semantics)
      {model with semanticApp = updSemApp 
                  corrPlotApp = {model.corrPlotApp with semanticApp     = updSemApp
                                                        correlationPlot = {model.corrPlotApp.correlationPlot with semanticApp = updSemApp}
                            }
      }
    let loadAnnotations (ind : SaveIndex) model = 
      let annoApp = AnnotationApp.load model.annotationApp (ind.filename SaveType.Annotations)
      let updCPA = (CorrelationPlotApp.update model.corrPlotApp CorrelationPlotApp.Clear)
      {model with   
              annotationApp = annoApp
              corrPlotApp   = updCPA
      }
    match msg, model.corrPlotApp.correlationPlot.creatingNew, model.drawingApp.isDrawing with
      | KeyDown Keys.Enter, _, true ->                          
        match model.drawingApp.working with
          | None   -> model
          | Some w ->
            {
              model with 
                drawingApp    = updateDrawingApp 
                                  (CorrelationDrawing.KeyDown Keys.Enter)
                annotationApp = updateAnnotationApp 
                                  (AnnotationApp.AddAnnotation w)
            }

      | KeyDown k, _, _       -> 
        {
          model with 
            drawingApp = updateDrawingApp (CorrelationDrawing.KeyDown k)
            camera     = updateCamera (CameraController.Message.KeyDown k)
        }

      | KeyUp k, _, _         -> 
        {  
          model with 
            drawingApp = updateDrawingApp (CorrelationDrawing.KeyUp k)
            camera     = updateCamera (CameraController.Message.KeyUp k)
        }

      | SemanticAppMessage m, false, false ->
        updateSemantics m model

      | AnnotationAppMessage m, _, _ -> 
        {model with annotationApp = updateAnnotationApp m}

      | CorrelationDrawingMessage m, _, _ ->
        let (corrApp, drawingApp, annoApp) =               
            match m with
              | CorrelationDrawing.AddPoint p -> 
                let (drawingApp, annoApp) =
                  match CorrelationDrawing.isDone model.drawingApp with
                    | true  -> 
                      let da = CorrelationDrawing.addPoint model.drawingApp model.semanticApp p
                      let aa = updateAnnotationApp (AnnotationApp.AddAnnotation da.working.Value) //TODO safe but  maybe do this differently
                      let da = {da with working   = None
                                        isDrawing = false}
                      (da, aa)
                    | false -> (updateDrawingApp m, model.annotationApp)
                (
                  model.corrPlotApp, 
                  drawingApp,
                  annoApp
                ) 
              | _  -> 
                (
                  model.corrPlotApp, 
                  updateDrawingApp m,
                  model.annotationApp
                )
        {model with drawingApp    = drawingApp
                    corrPlotApp   = corrApp
                    annotationApp = annoApp}

      | CorrPlotMessage m, _, false -> // TODO refactor
        let corrPlotApp = 
          let sel      = AnnotationApp.getSelectedPoints' model.annotationApp
          let updModel = //TODO refactor
            {model.corrPlotApp with 
              correlationPlot = {model.corrPlotApp.correlationPlot with selectedPoints = sel
                                                                        annotations    = model.annotationApp.annotations}}
          CorrelationPlotApp.update updModel m
                //model.annotationApp.annotations
                //model.semanticApp m
        {model with corrPlotApp = corrPlotApp}

      | CenterScene, _, _ -> 
          { model with camera = initialCamera }

      | UpdateConfig cfg, _,_->
          { model with dockConfig = cfg; past = Some model }

      | SetCullMode mode, _,_ ->
          { model with cullMode = mode; past = Some model }

      | ToggleFill, _,_ ->
          { model with fill = not model.fill; past = Some model }

      | Save, false, false -> 
          let newSaveInd = model.saveIndex.next
          ignore (SemanticApp.save model.semanticApp (newSaveInd.filename SaveType.Semantics))
          ignore (AnnotationApp.save model.annotationApp (newSaveInd.filename SaveType.Annotations))
          {model with saveIndex = newSaveInd}

      | Load ind, false, false -> 
        model
          |> loadSemantics ind
          |> loadAnnotations ind
//                  
      | Clear, _,_ -> 
        { model with  annotationApp = updateAnnotationApp (AnnotationApp.Clear)
                      drawingApp    = updateDrawingApp (CorrelationDrawing.Clear)
                      corrPlotApp   = updatePlot (CorrelationPlotApp.Clear) 
        }            

        | Undo,_,_ ->
            match model.past with
                | Some p -> { p with future = Some model; camera = model.camera }
                | None -> model

        | Redo,_,_ ->
            match model.future with
                | Some f -> { f with past = Some model; camera = model.camera }
                | None -> model

        | CameraMessage m, _,_ -> 
              { model with camera = updateCamera m }   

        | TopLevelEvent, _ , _ -> 
              model
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
    let menu = 
      let menuLoad =
        let inds = 
          SaveIndex.findSavedIndices
            |> List.map (fun i -> 
                          div [clazz "item"; onClick (fun _ -> Load i)]
                              [text (sprintf "Load %s" (string i.ind))])
                  //iconButton "small folder icon" (string i) (fun _ -> Load i))

        div [clazz "left floated item"] [
            div [clazz "ui simple pointing dropdown top left"]
              [ 
                span [clazz "text"][text "Load"]
                i [clazz "dropdown icon"][]
                div [clazz "menu";style "margin-top: 0rem"]
                    inds //[div [clazz "header"][text "load"]]
              ]
        ]
            

      let menuItems = [
        menuLoad
        iconButton "small save icon"          "save"    (fun _ -> Save)
        iconButton "small file outline icon"  "clear"   (fun _ -> Clear)
        iconButton "small external icon"      "export"  (fun _ -> Export)
        iconButton "small arrow left icon"    "undo"    (fun _ -> Undo)
        iconButton "small arrow right icon"   "redo"    (fun _ -> Redo)
      ]

      body [style "width: 100%; height:100%; background: transparent; overflow: auto"] [
        div [
              clazz "ui vertical inverted menu"; 
              //style "float:middle; vertical-align: middle; display: inline-block"
            ]
            menuItems     
      ]
    
    let renderView = 
      let annoSg         = AnnotationApp.Sg.view model.annotationApp model.semanticApp model.camera.view 
                                       |> Sg.map AnnotationAppMessage                
      let drawingSgList  = CorrelationDrawing.Sg.view model.drawingApp model.semanticApp model.camera.view
                              |> List.map (fun x -> x |> Sg.map CorrelationDrawingMessage) 
      let corrSg         = CorrelationPlot.getLogConnectionSgs model.corrPlotApp.correlationPlot model.semanticApp model.camera
                              |> Sg.map CorrPlotMessage
      let frustum = Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)
      
      require (myCss) (
        (
          body [clazz "ui"; style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"] [
            div [] [
              CameraController.controlledControl 
                  model.camera
                  CameraMessage 
                  frustum
                  (AttributeMap.ofList [
                              onKeyDown (KeyDown)
                              onKeyUp (KeyUp)
                              attribute "style" "width:100%; height: 100%; float: left;"]
                  )

                  (drawingSgList @ [annoSg; corrSg]
                    |> Sg.ofList 
                    |> Sg.fillMode (model.rendering.fillMode)     
                    |> Sg.cullMode (model.rendering.cullMode))                                                                                                             
            ]
          ]
        )
      ) 

    page (fun request -> 
      let qu = request.queryParams
      //for q in qu do //DEBUG
      //  printf "%s=%s" q.Key q.Value
        //require [
        //  { kind = Stylesheet; name = "docking"; url = "docking.css" }
          
      // override stylesheet for docking layout
      onBoot "$('head').append('<link rel=\"stylesheet\" type=\"text/css\" href=\"docking.css\">');" (
        // override some semantic ui stuff
        onBoot "$('head').append('<link rel=\"stylesheet\" type=\"text/css\" href=\"semui-overrides.css\">');" (
          // get semantic ui toggle buttons to work
          onBoot "$('head').append('<script> $(document).ready( function() {$(\".ui.button.toggle\").state();} );');" 
        
            (
              match Map.tryFind "page" request.queryParams with
                  | Some "render" -> // renderControl
                    renderView
                  | Some "controls" -> 
                      require Html.semui (
                          menu
                      )
                  | Some "svg" -> 
                    require (myCss) (
                      body [attribute "overflow-x" "hidden";attribute "overflow-y" "hidden"] [
                          CorrelationPlotApp.viewSvg model.corrPlotApp |> (UI.map CorrPlotMessage)
                      ]
                    )

                  | Some "logs" ->
                      CorrelationPlotApp.view model.corrPlotApp
                        |> UI.map CorrPlotMessage

                  | Some "semantics" ->
                      SemanticApp.viewSemantics model.semanticApp 
                        |> UI.map SemanticAppMessage
             
                  | Some "annotations" ->
                    require (myCss) (
                      body [] [
                        AnnotationApp.view model.annotationApp model.semanticApp |> UI.map AnnotationAppMessage
                      ]
                    )
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

          ))))
    

  let threads (model : Pages) = 
      CameraController.threads model.camera |> ThreadPool.map CameraMessage
        |> ThreadPool.union (CorrelationPlotApp.threads model.corrPlotApp)



  let start (runtime: IRuntime) =
      App.start {
          unpersist = Unpersist.instance
          threads = threads //fun _ -> ThreadPool.empty//threads
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
