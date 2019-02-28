namespace CorrelationDrawing

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


  type Action =
      | CameraMessage                 of CameraController.Message
      | MouseDown                     of (MouseButtons * V2i)
      | MouseUp                       of (MouseButtons * V2i)
      | MouseMove                     of V2d
      | KeyDown                       of key : Keys
      | KeyUp                         of key : Keys     
      | CorrPlotMessage               of CorrelationPlotApp.Action
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

  let initial   = 
    let semApp  = SemanticApp.getInitialWithSamples
    {   
      past        = None
      future      = None
      camera      = Mars.Terrain.CapeDesire.initialCamera
      cullMode    = CullMode.None
      fill        = true
      appFlags    = AppFlags.None
      sgFlags     = SgFlags.None
      rendering   = RenderingPars.initial
      dockConfig  = defaultLayout
      annotationApp = AnnotationApp.initial
      semanticApp   = semApp
      drawingApp    = CorrelationDrawing.initial 
      corrPlot   = CorrelationPlotApp.initial
      saveIndices   = SaveIndex.findSavedIndices ()
    }

  let updateSemantics action model = 
    let updSemApp = SemanticApp.update model.semanticApp action

    { model with 
        semanticApp = updSemApp //TODO refactor
        corrPlot = 
          {
            model.corrPlot with 
              semanticApp = updSemApp
              correlationPlot = {model.corrPlot.correlationPlot with semanticApp = updSemApp }
          }
    }
  
  let loadSemantics (ind : SaveIndex) model = //TODO refactor
    let updSemApp = SemanticApp.load model.semanticApp (ind.filename SaveType.Semantics)

    { model with 
        semanticApp = updSemApp 
        corrPlot = 
          { 
            model.corrPlot with 
              semanticApp = updSemApp
              correlationPlot = {model.corrPlot.correlationPlot with semanticApp = updSemApp}
          }
    }

  let loadAnnotations (ind : SaveIndex) model = 
      let annoApp = AnnotationApp.load model.annotationApp (ind.filename SaveType.Annotations)
      let updCPA = (CorrelationPlotApp.update model.annotationApp model.corrPlot CorrelationPlotApp.Clear)
      {model with   
         annotationApp = annoApp
         corrPlot   = updCPA
      }

  let clear model = 
      { model with  
         annotationApp = AnnotationApp.update model.annotationApp (AnnotationApp.Clear)
         drawingApp    = CorrelationDrawing.update model.drawingApp model.semanticApp (CorrelationDrawing.Clear)
         corrPlot   = CorrelationPlotApp.update model.annotationApp model.corrPlot (CorrelationPlotApp.Clear)                     
      } 

  let centerScene model =
      match Flags.isSet SgFlags.TestTerrain model.sgFlags with
        | true ->
          { model with camera = Mars.Terrain.Test.initialCameraDummy }
        | false ->
          { model with camera = Mars.Terrain.CapeDesire.initialCamera }

 // let 

  let update (model : Pages) (msg : Action) = //TODO model always last?
    let printCameraDebugInformation () =
      Log.line "Camera Position: %s" 
               (String.fromV3d model.camera.view.Location)
      Log.line "Up: %s" 
               (String.fromV3d model.camera.view.Up)
      Log.line "Forward: %s" 
               (String.fromV3d model.camera.view.Forward)

    let updateCorrelationDrawing =
      CorrelationDrawing.update model.drawingApp model.semanticApp

    let updateAnnotationApp =
      AnnotationApp.update model.annotationApp 

    let updateCamera =
      CameraController.update model.camera 
        
    match msg, model.drawingApp.isDrawing with
      | MouseUp bp,_ ->
        let message =
          (CorrelationPlotApp.Action.CorrelationPlotMessage 
            (CorrelationPlot.Action.SvgCameraMessage 
              (SvgCamera.Action.MouseUp bp)
            )
          )
        {model with 
          corrPlot = 
           CorrelationPlotApp.update model.annotationApp 
                                     model.corrPlot 
                                     message
        }
      | MouseDown bp,_-> 
        let message =
          (CorrelationPlotApp.Action.CorrelationPlotMessage 
            (CorrelationPlot.Action.SvgCameraMessage 
              (SvgCamera.Action.MouseDown bp)
            )
          )
        {model with 
          corrPlot = 
           CorrelationPlotApp.update model.annotationApp 
                                     model.corrPlot 
                                     message
        }
      | MouseMove p,_ -> 
        let message =
          (CorrelationPlotApp.Action.CorrelationPlotMessage 
            (CorrelationPlot.Action.SvgCameraMessage 
              (SvgCamera.Action.MouseMove p)
            )
          )
                                           
        {model with 
          corrPlot = 
           CorrelationPlotApp.update model.annotationApp model.corrPlot message
        }
      | KeyDown Keys.Enter, true ->                          

        match model.drawingApp.working with
          | None   -> model
          | Some w ->
            {
              model with 
                drawingApp    = updateCorrelationDrawing (CorrelationDrawing.KeyDown Keys.Enter)
                annotationApp = updateAnnotationApp (AnnotationApp.AddAnnotation w)
            }
      | KeyDown k, _       -> 
        let _model =
          match k with
            | Keys.R  -> 
              {model with dockConfig = defaultLayout}
            | _ -> 
              model
        let _corrPlot =
          let m = CorrelationPlotApp.CorrelationPlotMessage
                    (CorrelationPlot.KeyboardMessage (Keyboard.KeyDown k))
          CorrelationPlotApp.update model.annotationApp model.corrPlot m
          
        let annoApp = 
          match k with
            | Keys.C -> 
               printCameraDebugInformation ()
               model.annotationApp
            | Keys.Enter -> 
              match model.drawingApp.working with
                | Some anno -> AnnotationApp.update model.annotationApp (AnnotationApp.AddAnnotation anno)
                | None  -> model.annotationApp
            | _ -> model.annotationApp
        {
          _model with 
            drawingApp    = updateCorrelationDrawing (CorrelationDrawing.KeyDown k)
            annotationApp = AnnotationApp.update annoApp (AnnotationApp.KeyDown k)
            camera        = updateCamera (CameraController.Message.KeyDown k)
            corrPlot      = _corrPlot
        }
      | KeyUp k, _         -> 
        let _corrPlot =
          let m = CorrelationPlotApp.CorrelationPlotMessage
                    (CorrelationPlot.KeyboardMessage (Keyboard.KeyDown k))
          
          CorrelationPlotApp.update model.annotationApp model.corrPlot m
          
        {  
          model with 
            drawingApp = updateCorrelationDrawing (CorrelationDrawing.KeyUp k)
            camera     = updateCamera (CameraController.Message.KeyUp k)
            corrPlot   = _corrPlot
        }
      | SemanticAppMessage a, false ->
        updateSemantics a model

      | AnnotationAppMessage m, _ -> 
        let _selectedPoints = 
          Pages.Lens.corrPlot |. 
          CorrelationPlotModel.Lens.correlationPlot |. 
          CorrelationPlot.Lens.selectedPoints
        
        let sel = AnnotationApp.getSelectedPoints' model.annotationApp
       
        { model with annotationApp = updateAnnotationApp m } |> Lenses.set _selectedPoints sel 
      | CorrelationDrawingMessage m, _ -> // used for drawing annotations
        let (drawingApp, annoApp) =               
            match m with
              | CorrelationDrawing.AddPoint p -> 
                let (drawingApp, annoApp) =
                  match CorrelationDrawing.isDone model.drawingApp model.semanticApp with
                    | true  -> 
                      let correlationDrawing = CorrelationDrawing.addPoint model.drawingApp model.semanticApp p
                      let annotationModel = updateAnnotationApp (AnnotationApp.AddAnnotation correlationDrawing.working.Value) //TODO safe but  maybe do this differently

                      //clear corr drawing state
                      let correlationDrawing = { correlationDrawing with working = None; isDrawing = false }

                      (correlationDrawing, annotationModel)
                    | false -> 
                      (updateCorrelationDrawing m, model.annotationApp)
                (                 
                  drawingApp,
                  annoApp
                ) 
              | _  -> 
                (                
                  updateCorrelationDrawing m,
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
            {model.corrPlot with 
              correlationPlot = {model.corrPlot.correlationPlot with selectedPoints = sel}}
          let _corrPlot = 
            CorrelationPlotApp.update model.annotationApp updModel m
          _corrPlot

        let (annoApp, corrPlotApp) =
          match m with 
            | CorrelationPlotApp.CorrelationPlotMessage cpa ->
              match cpa with
                | CorrelationPlot.SelectLog id ->
                  let selPoints = CorrelationPlot.getPointsOfLog model.corrPlot.correlationPlot id
                  let upd =
                    AnnotationApp.update model.annotationApp AnnotationApp.DeselectAllPoints
                  let annoApp = 
                    AnnotationApp.update upd (AnnotationApp.SelectPoints selPoints)
                  let cPlot =
                    CorrelationPlot.update model.annotationApp model.corrPlot.correlationPlot (CorrelationPlot.SelectLog id)
                  (annoApp, {model.corrPlot with correlationPlot = cPlot})
                | _ -> (model.annotationApp, updCorrPlotApp m)
            | _ -> (model.annotationApp, updCorrPlotApp m)
        
                //model.annotationApp.annotations
                //model.semanticApp m
        {model with corrPlot = corrPlotApp; annotationApp = annoApp }
      | ColourMapMessage m, _ -> 
        let _cp = CorrelationPlot.update 
                    model.annotationApp
                    model.corrPlot.correlationPlot 
                    (CorrelationPlot.ColourMapMessage m)
        {model with 
          corrPlot = 
            {model.corrPlot with
              correlationPlot = _cp
            }
        }
            
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
            { model with camera = updateCamera m }   

      | TopLevelEvent, _ -> 
            model
      | _   -> model
        


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
                  (onMouseClick (fun _ -> CorrelationPlot.Action.FinishLog)) 
                 ] 
                 [text "New Log"]
          )
            |> UI.map CorrelationPlotApp.CorrelationPlotMessage
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
        //  Flags.toButtonGroup typeof<AppFlags> ToggleAppFlag //TODO css
        //  Flags.toButtonGroup typeof<SgFlags> ToggleSgFlag
        //  (Flags.toButtonGroup typeof<SvgFlags> CorrelationPlot.ToggleFlag) 
        //    |> UI.map CorrelationPlotApp.CorrelationPlotMessage 
        //    |> UI.map Action.CorrPlotMessage

        //  div[clazz "ui label"] 
        //      [
        //        text "SecondaryLevel"
        //        div[clazz "detail"] [Html.SemUi.dropDown' 
        //                NodeLevel.availableLevels
        //                model.corrPlotApp.correlationPlot.secondaryLvl 
        //                CorrelationPlot.SetSecondaryLevel 
        //                (fun (x : NodeLevel) -> sprintf "%i" x.level)
        //                |> UI.map CorrelationPlotApp.Action.CorrelationPlotMessage
        //                |> UI.map CorrPlotMessage]
        //      ]
        ] 

      body [style "width: 100%; height:100%; background: transparent; overflow: auto"] [
        div [
              clazz "ui vertical inverted menu"; 
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
                                    onKeyDown (KeyDown)
                                    onKeyUp (KeyUp)
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
                      body [attribute "overflow-x" "hidden";
                            attribute "overflow-y" "hidden"; 
                            (onMouseDown (fun b p -> MouseDown (b,p)))
                            (onMouseUp (fun b p -> MouseUp (b,p)))
                            (onMouseMove (fun p -> MouseMove (V2d p)))
                            onLayoutChanged UpdateConfig
                           ] [
                            CorrelationPlotApp.viewSvg model.annotationApp.annotations model.corrPlot
                              |> (UI.map CorrPlotMessage)
                      ]
                    )

                  | Some "logs" -> //DEBUG
                      CorrelationPlotApp.view model.corrPlot model.annotationApp.annotations model.semanticApp
                        |> UI.map CorrPlotMessage
                  //| Some "Debug" ->
                  //    CorrelationPlotApp.View.view model.corrPlotApp model.annotationApp model.semanticApp
                  //      |> UI.map CorrPlotMessage
                  | Some "semantics" ->
                      SemanticApp.View.expertGUI model.semanticApp 
                        |> UI.map SemanticAppMessage

                  | Some "semanticsMini" ->
                      SemanticApp.View.simpleView model.semanticApp 
                        |> UI.map SemanticAppMessage
                  | Some "mappings" ->
                     let domNode = 
                       (ColourMap.view model.corrPlot.correlationPlot.colourMapApp)
                         |> UI.map Action.ColourMapMessage

                     require (GUI.CSS.myCss) (
                       body [style "overflow: auto"] [
                         div [] [
                           // menu |> ui.map correlationplotmessage
                           Incremental.div (AttributeMap.ofList [clazz "ui inverted segment"])
                                           (AList.single domNode)
                             
                         ]
                       ]
                     )

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
        |> ThreadPool.union (CorrelationPlotApp.threads model.corrPlot)
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
