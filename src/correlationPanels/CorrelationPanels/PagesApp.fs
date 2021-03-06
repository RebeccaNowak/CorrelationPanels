﻿namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pages =
  open Aardvark.UI
  open Aardvark.UI.Primitives
  open Aardvark.Application
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.Base.Rendering
  open UIPlus
  
  open Svgplus
  open CorrelationDrawing.CorrelationPlotTypes
  open CorrelationDrawing.Types
  open CorrelationDrawing.CorrelationPlotTypes
  open CorrelationDrawing.SemanticTypes
  open CorrelationDrawing.CorrelationPlotTypes



  type Action =
      | CameraMessage                 of CameraController.Message
      | MouseDown                     of (MouseButtons * V2i)
      | MouseUp                       of (MouseButtons * V2i)
      | MouseMove                     of V2d
      | KeyboardMessage               of Keyboard.Action
      //| KeyDown                       of key : Keys
      //| KeyUp                         of key : Keys     
      | CorrPlotMessage               of CorrelationPlotTypes.Action
      | SemanticAppMessage            of SemanticApp.Action
      | AnnotationAppMessage          of AnnotationApp.Action
      | CorrelationDrawingMessage     of CorrelationDrawing.Action
      | ColourMapMessage              of ColourMap.Action
      | CentreScene
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
      | ToggleAppFlag                 of AppFlags
      | ToggleSgFlag                  of SgFlags    


  //let initialCameraMars = 
  //  let r = Trafo3d.RotateInto(V3d.OOI, Mars.Terrain.upReal)
  //  let camPos  = r.Forward.TransformPos( V3d(8.0, 8.0, 6.0) )
  //  let camView = CameraView.lookAt camPos V3d.OOO Mars.Terrain.upReal
  //  {CameraController.initial with view = camView}

  let defaultLayout = 
    let stackitems = 
      [
        dockelement {id "semanticsMini"; title "Annotation Type";               weight 1.0}
        dockelement {id "semantics";     title "Annotation Type: Expert View";  weight 1.0}
        dockelement {id "mappings";      title "Mappings";                      weight 1.0}
        dockelement {id "logs";          title "Logs";                          weight 1.0}
        dockelement {id "lognode";       title "Selected Layer";                weight 1.0}
      ]

    config 
      {
        content (
          // element {id "render"; title "Render View"; weight 5}
          horizontal 0.1 [
            element { id "controls"; title "Controls"; weight 0.08 }
            vertical 0.6 [
              horizontal 1.0 [
                element {id "render"; title "Render View"; weight 1.0}
                stack 0.1 (Some "semanticsMini") stackitems
              ]

              element { id "svg"; title "Correlation Panel"; weight 0.5}
                //stack 1.0 (Some "render") [dockelement {id "logs"; title "Logs"; weight 5};
                //                           dockelement {id "debug"; title "Debug"; weight 1}]
                          
                  //dockelement { id "annotations"; title "Annotations"; weight 1.0}
                        
            ]
          ]
        )
        appName "CDPages"
        useCachedConfig false
      }



  let updateSemantics action (model : Pages) = 
    let updSemApp = SemanticApp.update model.semanticApp action
    { model with 
        semanticApp = updSemApp //TODO refactor
    }
  
  let loadSemantics (ind : SaveIndex) model = //TODO refactor
    let updSemApp = SemanticApp.load model.semanticApp (ind.filename SaveType.Semantics)
    { model with semanticApp = updSemApp}

  let loadAnnotations (ind : SaveIndex) model = 
      let annoApp = AnnotationApp.load model.annotationApp (ind.filename SaveType.Annotations)
      let updCPA = (CorrelationPlot.update model.annotationApp model.semanticApp model.corrPlot CorrelationPlotTypes.Clear)
      {model with   
         annotationApp = annoApp
         corrPlot   = updCPA
      }

  let clear model = 
      { model with  
         annotationApp = AnnotationApp.update model.annotationApp (AnnotationApp.Clear)
         drawingApp    = CorrelationDrawing.update model.drawingApp model.semanticApp (CorrelationDrawing.Clear)
         corrPlot   = CorrelationPlot.update model.annotationApp model.semanticApp model.corrPlot (CorrelationPlotTypes.Clear)                     
      } 

  let centerScene model =
      match Flags.isSet SgFlags.TestTerrain model.sgFlags with
        | true ->
          { model with camera = Mars.Terrain.Test.initialCameraDummy }
        | false ->
          { model with camera = Mars.Terrain.CapeDesire.initialCamera }

 // let 
  let updateCorrelationDrawing model =
    CorrelationDrawing.update model.drawingApp model.semanticApp

  let updateAnnoApp model =
    AnnotationApp.update model.annotationApp 

  let updateCamera model =
    CameraController.update model.camera 

  let update (model : Pages) (msg : Action) = //TODO model always last?
    let printCameraDebugInformation () =
      Log.line "Camera Position: %s" 
               (String.fromV3d model.camera.view.Location)
      Log.line "Up: %s" 
               (String.fromV3d model.camera.view.Up)
      Log.line "Forward: %s" 
               (String.fromV3d model.camera.view.Forward)
        
    match msg, model.drawingApp.isDrawing with
      | MouseUp bp,_ ->
        let message =
            (CorrelationPlotTypes.SvgCameraMessage 
              (SvgCamera.Action.MouseUp bp)
            )
          
        {model with 
          corrPlot = 
           CorrelationPlot.update model.annotationApp 
                                     model.semanticApp 
                                     model.corrPlot 
                                     message
        }
      | MouseDown bp,_-> 
        let message =
            (CorrelationPlotTypes.SvgCameraMessage 
              (SvgCamera.Action.MouseDown bp)
            )
          
        {model with 
          corrPlot = 
           CorrelationPlot.update model.annotationApp 
                                  model.semanticApp
                                  model.corrPlot 
                                  message
        }
      | MouseMove p,_ -> 
        let message =
            (CorrelationPlotTypes.SvgCameraMessage 
              (SvgCamera.Action.MouseMove p)
            )
          
                                           
        {model with 
          corrPlot = 
           CorrelationPlot.update model.annotationApp model.semanticApp model.corrPlot message
        }
      | KeyboardMessage m, _ ->
        let _annoApp = 
          match m with 
            | Keyboard.KeyDown Keys.Enter ->
              match model.drawingApp.working with
                | None   -> 
                  model.annotationApp
                | Some w ->
                  updateAnnoApp model (AnnotationApp.AddAnnotation w)
            | _ -> model.annotationApp
        let _corrPlot = 
          let m = (CorrelationPlotTypes.KeyboardMessage m)
          CorrelationPlot.update model.annotationApp model.semanticApp model.corrPlot m

        let _drawingApp = CorrelationDrawing.update 
                                              model.drawingApp 
                                              model.semanticApp 
                                              (CorrelationDrawing.KeyboardMessage m)
        let _annoApp = AnnotationApp.update 
                                      _annoApp
                                      (AnnotationApp.KeyboardMessage m)
        let _model = 
          {model with corrPlot      = _corrPlot
                      drawingApp    = _drawingApp
                      annotationApp = _annoApp
          }
        _model
    
      | SemanticAppMessage a, false ->
        updateSemantics a model

      | AnnotationAppMessage m, _ -> 
        let _selectedPoints = 
          Pages.Lens.corrPlot |. CorrelationPlot.Lens.selectedPoints
        
        let sel = AnnotationApp.getSelectedPoints' model.annotationApp
       
        { model with annotationApp = updateAnnoApp model m } |> Lenses.set _selectedPoints sel 
      | CorrelationDrawingMessage m, _ -> // used for drawing annotations
        let (drawingApp, annoApp) =               
            match m with
              | CorrelationDrawing.AddPoint p -> 
                let (drawingApp, annoApp) =
                  match CorrelationDrawing.isDone model.drawingApp model.semanticApp with
                    | true  -> 
                      let correlationDrawing = CorrelationDrawing.addPoint model.drawingApp model.semanticApp p
                      let annotationModel = 
                        updateAnnoApp model (AnnotationApp.AddAnnotation correlationDrawing.working.Value) //TODO safe but  maybe do this differently

                      //clear corr drawing state
                      let correlationDrawing = { correlationDrawing with working = None}

                      (correlationDrawing, annotationModel)
                    | false -> 
                      (updateCorrelationDrawing model m, model.annotationApp)
                (                 
                  drawingApp,
                  annoApp
                ) 
              | _  -> 
                (                
                  updateCorrelationDrawing model m,
                  model.annotationApp
                )
        {model with 
          drawingApp    = drawingApp
          corrPlot      = model.corrPlot
          annotationApp = annoApp}

      | CorrPlotMessage m, false -> // TODO refactor
        let updCorrPlotApp m = 
          let sel      = AnnotationApp.getSelectedPoints' model.annotationApp
          let updModel = //TODO refactor
            {model.corrPlot with selectedPoints = sel}
          let _corrPlot = 
            CorrelationPlot.update model.annotationApp model.semanticApp updModel m
          _corrPlot

        let (annoApp, corrPlotApp) =
          match m with 
          | CorrelationPlotTypes.SelectLog id ->
            let selPoints = CorrelationPlot.getPointsOfLog model.corrPlot id
            let upd =
              AnnotationApp.update model.annotationApp AnnotationApp.DeselectAllPoints
            let annoApp = 
              AnnotationApp.update upd (AnnotationApp.SelectPoints selPoints)
            let cPlot =
              CorrelationPlot.update model.annotationApp 
                                      model.semanticApp
                                      model.corrPlot (CorrelationPlotTypes.SelectLog id)
            (annoApp, cPlot)
          | _ -> (model.annotationApp, updCorrPlotApp m)
        
                //model.annotationApp.annotations
                //model.semanticApp m
        {model with corrPlot = corrPlotApp; annotationApp = annoApp }
      | ColourMapMessage m, _ -> 
        let _cp = CorrelationPlot.update 
                    model.annotationApp
                    model.semanticApp
                    model.corrPlot 
                    (CorrelationPlotTypes.GrainSizeTypeMessage m)
        {model with corrPlot = _cp}
            
      | CentreScene,  _ -> 
        centerScene model

      | UpdateConfig cfg, _->
          { model with dockConfig = cfg; past = Some model }

      | SetCullMode mode, _ ->
          { model with cullMode = mode; past = Some model }

      | ToggleFill, _ ->
          { model with fill = not model.fill; past = Some model }

      | ToggleAppFlag f, _ ->
          {model with appFlags = Flags.toggle f model.appFlags}

      | ToggleSgFlag f, _ ->
          let model = 
            {model with sgFlags = Flags.toggle f model.sgFlags}
          match (f.Equals SgFlags.TestTerrain) with
            | true  -> model 
                        |> clear
                        |> centerScene
            | false -> model
      | Save,  false -> 
          let newSaveInd = SaveIndex.nextAvaible ()
          ignore (SemanticApp.save model.semanticApp (newSaveInd.filename SaveType.Semantics))
          ignore (AnnotationApp.save model.annotationApp (newSaveInd.filename SaveType.Annotations))
          {model with saveIndices = SaveIndex.findSavedIndices ()}

      | Load ind, false -> 
        (clear model)
          |> loadSemantics ind
          |> loadAnnotations ind
//                  
      | Clear, _ -> 
        clear model

      | Undo,_ ->
          match model.past with
              | Some p -> { p with future = Some model; camera = model.camera }
              | None -> model

      | Redo,_ ->
          match model.future with
              | Some f -> { f with past = Some model; camera = model.camera }
              | None -> model

      | CameraMessage m,_ -> 
            { model with camera = updateCamera model m }   

      | TopLevelEvent, _ -> 
            model
      | _   -> model
        

  let initial   = 
    let semApp  = SemanticApp.getInitialWithSamples
    let onEnter (model : Pages) =
      match model.drawingApp.working with
        | None   -> model
        | Some w ->
          {
            model with 
              drawingApp    = updateCorrelationDrawing model (CorrelationDrawing.KeyDown Keys.Enter)
              annotationApp = updateAnnoApp model (AnnotationApp.AddAnnotation w)
          }

    let resetLayout (model : Pages) = // R
      {model with dockConfig = defaultLayout}

    let keyboard = Keyboard.init ()
    let _keyboard = 
      keyboard
        |> (Keyboard.register
              {
                update = resetLayout
                key    = Keys.R
                ctrl   = false
                alt    = false
              }
           )
        |> (Keyboard.register
              {
                update = onEnter
                key    = Keys.Enter
                ctrl   = true
                alt    = false
              }
            )
    //let keyDown (model : Pages) =

    //  //let annoApp = 
      //  match k with
      //    //| Keys.C -> 
      //    //    printCameraDebugInformation ()
      //    //    model.annotationApp
      //    | Keys.Enter -> 
      //      match model.drawingApp.working with
      //        | Some anno -> AnnotationApp.update model.annotationApp (AnnotationApp.AddAnnotation anno)
      //        | None  -> model.annotationApp
      //    | _ -> model.annotationApp
      //{
      //  _model with 
      //    drawingApp    = updateCorrelationDrawing (CorrelationDrawing.KeyDown k)
      //    annotationApp = AnnotationApp.update annoApp (AnnotationApp.KeyDown k)
      //    camera        = updateCamera (CameraController.Message.KeyDown k)
      //    corrPlot      = _corrPlot
      //}

    ////| KeyDown Keys.Enter, true ->                          

    ////    match model.drawingApp.working with
    ////      | None   -> model
    ////      | Some w ->
    ////        {
    ////          model with 
    ////            drawingApp    = updateCorrelationDrawing (CorrelationDrawing.KeyDown Keys.Enter)
    ////            annotationApp = updateAnnotationApp (AnnotationApp.AddAnnotation w)
    ////        }
    //  | KeyDown k, _       -> 
    //    let _model =
    //      match k with
    //        | Keys.R  -> 
    //          {model with dockConfig = defaultLayout}
    //        | _ -> 
    //          model
    //    let _corrPlot =
    //      let m = CorrelationPlot.CorrelationPlotMessage
    //                (CorrelationPlot.KeyboardMessage (Keyboard.KeyDown k))
    //      CorrelationPlot.update model.annotationApp model.corrPlot m
          
    //    let annoApp = 
    //      match k with
    //        | Keys.C -> 
    //           printCameraDebugInformation ()
    //           model.annotationApp
    //        | Keys.Enter -> 
    //          match model.drawingApp.working with
    //            | Some anno -> AnnotationApp.update model.annotationApp (AnnotationApp.AddAnnotation anno)
    //            | None  -> model.annotationApp
    //        | _ -> model.annotationApp
    //    {
    //      _model with 
    //        drawingApp    = updateCorrelationDrawing (CorrelationDrawing.KeyDown k)
    //        annotationApp = AnnotationApp.update annoApp (AnnotationApp.KeyDown k)
    //        camera        = updateCamera (CameraController.Message.KeyDown k)
    //        corrPlot      = _corrPlot
    //    }
    //  | KeyUp k, _         -> 
    //    let _corrPlot =
    //      let m = CorrelationPlot.CorrelationPlotMessage
    //                (CorrelationPlot.KeyboardMessage (Keyboard.KeyDown k))
          
    //      CorrelationPlot.update model.annotationApp model.corrPlot m
          
    //    {  
    //      model with 
    //        drawingApp = updateCorrelationDrawing (CorrelationDrawing.KeyUp k)
    //        camera     = updateCamera (CameraController.Message.KeyUp k)
    //        corrPlot   = _corrPlot
    //    }

    {   
      past        = None
      future      = None
      camera      = Mars.Terrain.CapeDesire.initialCamera
      keyboard    = _keyboard
      cullMode    = CullMode.None
      rendering   = {fillMode = FillMode.Fill;cullMode = CullMode.None}
      fill        = true
      appFlags    = AppFlags.None
      sgFlags     = SgFlags.None
      dockConfig  = defaultLayout
      annotationApp = AnnotationApp.initial
      semanticApp   = semApp
      drawingApp    = CorrelationDrawing.initial 
      corrPlot   = CorrelationPlot.initial
      saveIndices   = SaveIndex.findSavedIndices ()
    }



  let view  (runtime : IRuntime) (model : MPages) =
    let menu = 
      let loadMenu = // TODO overkill
        let indexToMenuItem (ind : SaveIndex) =
          let label = 
            sprintf "Load %s" (string ind.ind)
          UIPlus.Menus.toMenuItem label (Load ind) 

        let items = 
          model.saveIndices
            |> Mod.map (fun lst ->
                          lst 
                            |> List.map indexToMenuItem
                       )
                              
        let lst =
          alist { 
            let! lst = items
            for item in lst do
              yield item
          }
        lst |> UIPlus.Menus.Incremental.toMouseOverMenu
        

      let newLogButton = 
          (
            button [clazz "ui button"; 
                  (onMouseClick (fun _ -> CorrelationPlotTypes.FinishLog)) 
                 ] 
                 [text "New Log"]
          )
            |> UI.map Action.CorrPlotMessage

      let menuItems = 
        [
          loadMenu
          newLogButton
          Buttons.iconButton "small save icon"          "save"    (fun _ -> Save)
          Buttons.iconButton "small file outline icon"  "clear"   (fun _ -> Clear)
          //Buttons.iconButton "small external icon"      "export"  (fun _ -> Export)
          Buttons.iconButton "small arrow left icon"    "undo"    (fun _ -> Undo)
          Buttons.iconButton "small arrow right icon"   "redo"    (fun _ -> Redo)
          Buttons.iconButton "small bullseye icon"      "centre"  (fun _ -> CentreScene)
        ]

      body [style "width: 100%; height:100%; background: transparent; overflow: auto"] [
        div [
              clazz "ui vertical inverted menu"
              onKeyDown (fun k -> KeyboardMessage (Keyboard.Action.KeyDown k))
              onKeyUp (fun k -> KeyboardMessage (Keyboard.Action.KeyUp k))
              //style "float:middle; vertical-align: middle; display: inline-block"
            ]
            menuItems     
      ]
    
    let renderView = 
      let annoSg = 
        (AnnotationApp.Sg.view 
          model.annotationApp 
          model.semanticApp 
          model.camera.view) |> Sg.map AnnotationAppMessage   
          
      let drawingSgList  = 
        (CorrelationDrawing.Sg.view 
          model.drawingApp 
          model.semanticApp 
          model.camera.view 
          model.sgFlags) |> List.map (fun x -> x |> Sg.map CorrelationDrawingMessage) 

      //let corrSg = 
      //  (CorrelationPlot.getLogConnectionSgs 
      //    model.corrPlotApp.correlationPlot 
      //    model.semanticApp 
      //    model.camera) |> Sg.map CorrPlotMessage
      let frustum = Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)
      
      require (GUI.CSS.myCss) (
        (
          body [clazz "ui"
                style "background: #1B1C1E; width: 100%; height:100%; overflow: auto;"
                onLayoutChanged UpdateConfig
               ] [
                  div [] [
                    CameraController.controlledControl 
                        model.camera
                        CameraMessage 
                        frustum
                        (AttributeMap.ofList [
                                    onKeyDown (fun k -> KeyboardMessage (Keyboard.Action.KeyDown k))
                                    onKeyUp (fun k -> KeyboardMessage (Keyboard.Action.KeyUp k))
                                    attribute "style" "width:100%; height: 100%; float: left;"]
                        )

                        (drawingSgList @ [annoSg]//; corrSg]
                          |> Sg.ofList 
                          |> Sg.fillMode (model.rendering.fillMode)     
                          |> Sg.cullMode (model.rendering.cullMode))                                                                                                             
                  ]
               ]
        )
      ) 

    page (fun request -> 
      let qu = request.queryParams
          
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
                    require (GUI.CSS.myCss) (
                      body [attribute "overflow-x" "auto";
                            attribute "overflow-y" "auto"; 
                            (onMouseDown (fun b p -> MouseDown (b,p)))
                            (onMouseUp (fun b p -> MouseUp (b,p)))
                            (onMouseMove (fun p -> MouseMove (V2d p)))
                            onKeyDown (fun k -> KeyboardMessage (Keyboard.Action.KeyDown k))
                            onKeyUp (fun k -> KeyboardMessage (Keyboard.Action.KeyUp k))
                            onLayoutChanged UpdateConfig
                           ] [
                            CorrelationPlot.viewSvg model.annotationApp.annotations model.corrPlot
                              |> (UI.map CorrPlotMessage)
                      ]
                    )

                  | Some "logs" -> //DEBUG
                      CorrelationPlot.listView model.corrPlot model.annotationApp.annotations model.semanticApp
                        |> UI.map CorrPlotMessage
                  //| Some "Debug" ->
                  //    CorrelationPlot.View.view model.corrPlotApp model.annotationApp model.semanticApp
                  //      |> UI.map CorrPlotMessage
                  | Some "semantics" ->
                      SemanticApp.View.expertGUI model.semanticApp 
                        |> UI.map SemanticAppMessage

                  | Some "semanticsMini" ->
                      SemanticApp.View.simpleView model.semanticApp 
                        |> UI.map SemanticAppMessage
                  | Some "mappings" ->
                     let domNode = 
                       (ColourMap.view model.corrPlot.colourMapApp)
                         |> UI.map Action.ColourMapMessage

                     domNode

                  | Some "lognode" -> body [] []
                    //adaptive {
                    //  let! optSel = model.selectedNode
                    //  match optSel with
                    //    | Some sel ->
                    //      let node = LogNodes.Helper. m odel.corrPlotApp
                    //      LogNodes.View.listViewSingle 
                    //    | None -> body [] []

                    //}

                  
                      
                        
                  | Some "annotations" ->
                    require (GUI.CSS.myCss) (
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
        |> ThreadPool.union (CorrelationPlot.threads model.corrPlot)
        |> ThreadPool.union (SemanticApp.threads model.semanticApp)

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
