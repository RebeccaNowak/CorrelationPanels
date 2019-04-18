namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlot =
    open System
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UIPlus
    open Aardvark.UI.Primitives
    open Svgplus.DA
    open Svgplus
    open Svgplus.RectangleStackTypes
    open Svgplus.RectangleType
    open UIPlus.KeyboardTypes
    open Svgplus.DiagramItemType
    open SimpleTypes

    open CorrelationDrawing.Types
    open CorrelationDrawing.AnnotationTypes
    open CorrelationDrawing.SemanticTypes
    open CorrelationDrawing.LogNodeTypes
    open CorrelationDrawing.LogTypes
    open CorrelationDrawing.CorrelationTypes
    open CorrelationDrawing.CorrelationPlotTypes

 
      

    //let logOffset (index : int) =
    //  float (index * 10 + index * 250)

    let tryFindLog (model : CorrelationPlot) (logId : DiagramItemId) =
      HMap.tryFind logId model.logs

    let tryFindNodeFromRectangleId (model : CorrelationPlot) 
                                   (rid   : RectangleId)
                                   (stackid : DiagramItemId) =
      let log = tryFindLog model stackid
      Option.bind (fun lo -> 
                    let on = GeologicalLog.findNodeFromRectangleId lo rid
                    Option.map (fun n -> (n, lo)) on
                  ) log

    let tryFindNode (model : CorrelationPlot) 
                    (logId : DiagramItemId) 
                    (nodeId : LogNodeId) =
      let optLog = tryFindLog model logId
      Option.bind
        (fun log -> GeologicalLog.findNode log (fun n -> n.id = nodeId)) optLog

    let tryFindNodeFromRectId (model  : CorrelationPlot) 
                              (logId  : DiagramItemId) 
                              (rectId : RectangleId) =
      let optLog = tryFindLog model logId
      Option.bind
        (fun log -> GeologicalLog.findNode log (fun n -> n.rectangleId = rectId)) optLog
        
    let getPointsOfLog (model : CorrelationPlot) (logId : DiagramItemId) =
      let opt = HMap.tryFind logId model.logs
      match opt with
        | Some log -> log.annoPoints
        | None     -> HMap.empty
      
    
    //let yToSvg' (model : MCorrelationPlot) (y : float)  =
    //  adaptive {  
    //    let! lp =  model.svgOptions.logPadding
    //    let! lh = model.svgOptions.logHeight
    //    let! yRange = model.yRange
    //    match yRange.range with
    //      | 0.0 -> Log.line "Divide by zero: Log range is %.2f-%.2f" yRange.min yRange.max
    //      | _ -> ()
    //    return lp + (yRange.max - y) * (lh / yRange.range)
    //    //opt.logPadding + (y - yRange.min) * (opt.logHeight / yRange.range)
    //  }

    //let svgXAxisYOffset (model : MCorrelationPlot) = //TODO refactor
    //  let ypos' logHeight = SvgOptions.xAxisYPosition' model.svgOptions logHeight
    //  let ypos (logHeight : float) (opt : SvgOptions) = SvgOptions.xAxisYPosition opt logHeight

    //  adaptive {  
    //    let! yRange = model.yRange
    //    let! map = model.currrentYMapping
    //    let! opt = model.svgOptions.Current
    //    let! h = model.svgOptions.logHeight
    //    return match map with 
    //            | None -> 
    //              ypos h opt
    //            | Some m -> 
    //              ypos (yRange.range * m ) opt
    //  }

    //let yToSvg (model : CorrelationPlot) (y : float)  =
    //  let opt =  model.svgOptions
    //  let yRange = model.yRange
    //  match yRange.range with
    //    | 0.0 -> Log.line "Divide by zero: Log range is %.2f-%.2f" yRange.min yRange.max
    //    | _ -> ()
    //  let foo = opt.logPadding + (yRange.max - y) * (opt.logHeight / yRange.range)
    //  foo

    let createNewLog (model : CorrelationPlot) 
                     (annoApp : AnnotationApp) 
                     (semanticApp : SemanticApp)
                     (colourMap : ColourMap) =
      let dataRange = AnnotationApp.elevationRange annoApp

      match annoApp.annotations.IsEmptyOrNull () with
      | true -> 
        Log.error "Creating log failed. There are no annotations."
        model
      | false ->
        let (item, newLog) = 
          GeologicalLog.initial 
              model.selectedPoints  //selected points
              semanticApp
              annoApp
              model.xToSvg
              model.yToSvg
              model.defaultWidth
              colourMap
              model.elevationZeroHeight



        let diagram =
          Diagram.update model.diagram (Diagram.AddItem item)

        {
          model with 
            diagram          = diagram
            logs             = (model.logs.Add (newLog.diagramRef.itemId, newLog))
            selectedPoints   = hmap<AnnotationId, V3d>.Empty
        }


      

///////////////////////////////////////////////////////////// UPDATE ////////////////////////////////////////////////////
    
    
    let deleteLog (id : DiagramItemId) (model : CorrelationPlot) =
      let _logs = (HMap.remove id model.logs)
      {model with logs = _logs}

    let updateLog  index (message : GeologicalLog.Action) (logs : hmap<DiagramItemId, GeologicalLog>) =
       HMap.update index (fun (x : option<GeologicalLog>) -> GeologicalLog.update x.Value message) logs//hack

    let selectLog (model    : CorrelationPlot) (id       : DiagramItemId) =
      let hasNew = not (model.logs |> HMap.forall (fun id x -> x.state <> State.New))
      if hasNew then
        model
      else
        let (_logs, _sel) =
          match model.selectedLog with
            | None -> 
              let _logs = 
                model.logs
                  |> updateLog id (GeologicalLog.Action.SetState State.Edit)
              (_logs, Some id)
            | Some logId when logId = id ->
              let _logs = 
                model.logs
                  |> updateLog id (GeologicalLog.Action.SetState State.Display)
              (_logs, None)
            | Some logId ->
              let _logs = 
                model.logs
                    |> updateLog logId (GeologicalLog.Action.SetState State.Display)
                    |> updateLog id (GeologicalLog.Action.SetState State.Edit)
              (_logs, Some id)
        {model with logs        = _logs
                    selectedLog = _sel}

    let updateYToSvg (g : float -> float) (model : CorrelationPlot)  =
      let _yToSvg = g model.yToSvg
      match _yToSvg with
        | _yToSvg when _yToSvg <= 1.0 -> model
        | _ ->
          let f size =
            (size * _yToSvg) / model.yToSvg
          {model with yToSvg   = _yToSvg
                      diagram  = Diagram.update model.diagram (Diagram.UpdateYSizes f)}

    let selectUpperBorder (model : CorrelationPlot)  
                          (rstackId : DiagramItemId)
                          (rectangleId : RectangleId) =
      let opt = 
        tryFindNodeFromRectangleId model rectangleId rstackId
      let optBorder =
        Option.bind (fun (n, l) -> n.uBorder) opt
      {model with selectedBorder = optBorder}

    let selectLowerBorder (model : CorrelationPlot)  
                          (rstackId : DiagramItemId)
                          (rectangleId : RectangleId) =
      let opt = 
        tryFindNodeFromRectangleId model rectangleId rstackId
      let optBorder =
        Option.bind (fun (n, l) -> n.lBorder) opt
      {model with selectedBorder = optBorder}

    let keyboard =
      let keyboard = Keyboard.init ()
      let _keyboard = 
        keyboard
          |> (Keyboard.register
                {
                  update = (updateYToSvg (fun x -> x - 1.0))
                  key    = Keys.Y
                  ctrl   = false
                  alt    = false
                })
          |> (Keyboard.register
                {
                  update = (updateYToSvg (fun x -> x + 1.0))
                  key    = Keys.Y
                  ctrl   = true
                  alt    = false
                })
      _keyboard

    let update (annoApp  : AnnotationApp)
               (semApp   : SemanticApp)
               (model    : CorrelationPlot) 
               (action   : Action) = 
      let updateColoursFromCMap r =
        let opt = ColourMap.svgValueToColourPicker model.colourMapApp r.dim.width 
        match opt with
          | Some c -> {r with colour = c; overwriteColour = None}
          | None   -> r
      
      match action with
        | Clear                    ->
          let _diagram = 
            {
              model.diagram with  items      =  HMap.empty
                                  order = PList.empty
                                  connectionApp   = ConnectionApp.init
                                  selectedRectangle = None}
          {model with logs             = HMap.empty
                      selectedPoints   = hmap<AnnotationId, V3d>.Empty
                      selectedLog      = None
                      correlations     = plist.Empty
                      //annotations      = hmap<AnnotationId, Annotation>.Empty
                      currrentYMapping = None
                      selectedBorder   = None
                      diagram          = _diagram
          }
        | SvgCameraMessage m ->
          let _svgCamera = SvgCamera.update model.svgCamera m
          {model with svgCamera = _svgCamera}
        | KeyboardMessage m ->
          //Log.line "CorrelationPlot received kbmsg %A" m
          let (_kb, _model) = Keyboard.update model.keyboard model m
          {_model with keyboard = _kb}
        | LogMessage (logId, logmsg) ->
          let log = tryFindLog model logId
          match log with
          | Some log ->
            let _dApp =
              match logmsg with
                | GeologicalLog.TextInputMessage (sid, textMessage) ->
                  let itemMessage = DiagramItem.ChangeLabel textMessage
                  Diagram.updateItemFromId model.diagram log.diagramRef.itemId itemMessage
                | GeologicalLog.MoveDown id ->
                  Diagram.update model.diagram (Diagram.MoveRight log.diagramRef.itemId)
                | GeologicalLog.MoveUp id ->
                  Diagram.update model.diagram (Diagram.MoveLeft log.diagramRef.itemId)
                | _ -> model.diagram

            let _model = 
              match logmsg with
                | GeologicalLog.SelectLogNode nid ->
                  let optNode = tryFindNode model logId nid
                  let _cmap = 
                    match optNode with
                      | Some node -> 
                          ColourMap.update model.colourMapApp 
                                           (ColourMap.SelectItemFromSvg (Rectangle.Lens.width.Get node.mainBody))
                      | None -> model.colourMapApp
                  let _model = selectLog model logId
                  {_model with colourMapApp = _cmap}    
                | _ -> model

            {_model with logs       = updateLog logId logmsg _model.logs
                         diagram = _dApp
            }
          | None -> model
        | SelectLog id -> selectLog model id
        //| NewLog             -> 
        //  {model with creatingNew     = true
        //              selectedPoints  = List<(V3d * Annotation)>.Empty}
        | FinishLog ->
          let _model =
            match model.selectedPoints.IsEmpty with
              | true      -> 
                Log.line "no points in list for creating log"
                model //TODO create empty log
              | false ->
                let updLogs =
                  model.logs
                    |> HMap.map (fun id log -> {log with state = State.Display})
                (createNewLog {model with logs = updLogs} annoApp semApp model.colourMapApp)
          _model
        | DeleteLog id        -> deleteLog id model
          
        //| LogAxisAppMessage m -> 
        //  {model with logAxisApp  = (LogAxisApp.update model.logAxisApp m)}
        //| ChangeView m          -> {model with viewType = m}
        | ToggleEditCorrelations  -> 
          {model with editCorrelations = not model.editCorrelations}
        | SetSecondaryLevel lvl  -> 
          {model with secondaryLvl = lvl}
        //| ToggleFlag f ->
        //  {model with svgFlags = Flags.toggle f model.svgFlags}
        | MouseMove m       -> 
          let _d =
            Diagram.update model.diagram (Diagram.MouseMove m)
          {model with diagram = _d}
        | DiagramMessage m       -> 
          let _d =
            Diagram.update model.diagram m

          let unpackchain = 
            (Diagram.Action.unpack m 
                                  Diagram.UnpackAction.OnLeftClick
                                  (fun stackid _ -> selectLog model stackid))
            >> (Diagram.Action.unpack m 
                                  Diagram.UnpackAction.SelectLowerBorder
                                  (fun stackid rid -> selectLowerBorder model stackid rid))
            >> (Diagram.Action.unpack m 
                                  Diagram.UnpackAction.SelectUpperBorder
                                  (fun stackid rid -> selectUpperBorder model stackid rid))                                  

          let _cp =
            unpackchain model
          let selectMap stackid rectId = 
            let optnode = tryFindNodeFromRectId model stackid rectId
            match optnode with
              | Some node -> 
                let width = (Rectangle.Lens.width.Get node.mainBody)
                let _m  = ColourMap.SelectItemFromSvg width
                ColourMap.update model.colourMapApp _m
              | None      ->
                model.colourMapApp
          let _cmap =
            Diagram.Action.unpack m 
                                  Diagram.UnpackAction.SelectRectangle
                                  selectMap
                                  model.colourMapApp

          {_cp with diagram   = _d |> Diagram.layout
                    colourMapApp = _cmap}

        | GrainSizeTypeMessage m -> 
          let _cmap = ColourMap.update model.colourMapApp m
          let optselid = model.diagram.selectedRectangle 
          let (_logs, _diagram) = 
            match m, optselid.IsSome with
              | ColourMap.SelectItem cmitemid, true ->
                let selRect = optselid.Value
                let optsel = 
                  Diagram.tryFindRectangle model.diagram selRect
                match optsel with
                  | Some r ->
                    let width = ColourMap.svgValueFromItemId model.colourMapApp cmitemid
                    let _optn = tryFindNodeFromRectangleId model selRect.rid selRect.diagramItemId
                    match _optn with
                      | Some (n, log) ->
                        let logM1 = 
                              (GeologicalLog.LogNodeMessage 
                                (n.id, LogNodes.RectangleMessage (Rectangle.UpdateColour updateColoursFromCMap))
                              )
                        let logM2 = 
                              (GeologicalLog.LogNodeMessage 
                                (n.id, LogNodes.RectangleMessage (Rectangle.SetWidth (width)))
                              )
                        let _logs  = 
                          (updateLog log.diagramRef.itemId logM1 model.logs)
                            |> (updateLog log.diagramRef.itemId logM2)
                        let rectIds = {stackid = selRect.stackid; rid = selRect.rid; diagramItemId = selRect.diagramItemId}
                        let rectMessage1 = Rectangle.SetWidth width
                        let rectMessage2 = Rectangle.UpdateColour updateColoursFromCMap
                        let diagr = Diagram.update model.diagram (Diagram.UpdateRectangle (rectIds, rectMessage1))
                        let _diagr = Diagram.update diagr (Diagram.UpdateRectangle (rectIds, rectMessage2))
                        (_logs, _diagr)
                      | None -> (model.logs, model.diagram)
                  | None   -> (model.logs, model.diagram)
               | ColourMap.ItemMessage (id, a), _ -> 
                 let diagr = Diagram.update model.diagram (Diagram.UpdateColour updateColoursFromCMap) //(_cmap, id))
                 (model.logs, diagr)

        //| OnKeyUp k ->
        //  let a = (CorrelationPlot.KeyboardMessage (Keyboard.Action.KeyUp k))
        //  {model with correlationPlot =
        //                CorrelationPlot.update annoApp model.correlationPlot a
        //  }
              
        //| OnKeyDown k ->
        //  let a = (CorrelationPlot.KeyboardMessage (Keyboard.Action.KeyDown k))
        //  {model with correlationPlot =
        //                CorrelationPlot.update annoApp model.correlationPlot a
        //  }
               | _,_ -> (model.logs, model.diagram)
               
          {model with colourMapApp = _cmap
                      diagram      = _diagram |> Diagram.layout
                      logs         = _logs}

    let update' (annos  : hmap<AnnotationId, Annotation>)
                (semApp : SemanticApp)
                (model  : CorrelationPlot) 
                (action : Action) = 
      let annoApp : AnnotationApp =
        {
          annotations        = annos
          selectedAnnotation = None
          keyboard           = AnnotationApp.keyboard
        }
      update annoApp semApp model action
        


    let viewSvg (annoApp : amap<AnnotationId, MAnnotation>) (model : MCorrelationPlot)  = //TODO refactor
      let svgNode =
        let attsRoot = 
          [
            clazz "svgRoot"
            style "border: 1px solid black"
            //attribute "viewBox" "0 0 600 400"
            attribute "preserveAspectRatio" "xMinYMin meet"
            attribute "height" "100%"
            attribute "width" "100%"
          ]

        let attsGroup = SvgCamera.transformationAttributes model.svgCamera

        let logSvgList =
          (Diagram.view model.diagram)
            |> AList.map (fun x -> x |> UI.map DiagramMessage)

        Svg.svg attsRoot [
          (
            Incremental.Svg.g (AttributeMap.ofAMap attsGroup) 
                              logSvgList
          )
        ]
      require (GUI.CSS.myCss) (
        body [attribute "overflow-x" "auto";
              attribute "overflow-y" "auto"; 
              (onMouseDown (fun b p -> SvgCameraMessage 
                                          (SvgCamera.Action.MouseDown (b,p)))) 
              (onMouseUp   (fun b p -> SvgCameraMessage 
                                           (SvgCamera.Action.MouseUp (b,p))))
              (onMouseMove (fun p   -> 
                 SvgCameraMessage 
                    (SvgCamera.Action.MouseMove (V2d p))))
              
              onKeyDown (fun k -> KeyboardMessage (Keyboard.Action.KeyDown k))
              onKeyUp   (fun k -> KeyboardMessage (Keyboard.Action.KeyUp k))
             ] 
             [
                div [attribute "overflow-x" "auto";attribute "overflow-y" "auto"] [
                        //menu
                        svgNode
                        ]
             ]
        )   


                                       
    let listView  (model   : MCorrelationPlot) 
                  (annoApp : amap<AnnotationId, MAnnotation>)
                  (semApp  : MSemanticApp) =
      let logList = 
        DS.AMap.toOrderedAList model.logs model.diagram.order
      
      let itemList = 
        DS.AMap.toOrderedAList model.diagram.items model.diagram.order

      let tview =
          Table.view model.logsTable itemList logList
        
      //Tables.toTableView (div[][]) rows ["Log Name";"Order"]

      let domnode =
        let attsBody =
            [
              style "overflow: auto"
            ]
        let attsDiv = 
          AttributeMap.ofList [clazz "ui inverted segment"]
        require (GUI.CSS.myCss) (
          body attsBody [
            Incremental.div attsDiv (AList.single tview)
          ]
        )

      domnode     
      


    let initial : CorrelationPlot  =
      let xToSvg              = fun x -> (21.0 + Math.Log(x,2.0)) * 10.0
      let svgToX              = fun x -> (Math.Pow (2.0, (x * 0.1 - 21.0)))
      let yToSvg              = 25.0
      let defaultWidth        = xToSvg UIPlus.ColourMapItem.vfGravel.defaultMiddle
      let actionMapping (log       : MGeologicalLog)
                        (domNode   : DomNode<GeologicalLog.Action>) =
         
        UI.map (fun a -> Action.LogMessage (log.diagramRef.itemId, a)) domNode 
           

      let uitable = 
        Table.init (GeologicalLog.logToRow actionMapping) GeologicalLog.headings
      {
        diagram             = Svgplus.Diagram.init (fun x -> x * yToSvg) (fun x -> x / yToSvg)
        logs                = HMap.empty
        correlations        = PList.empty
        
        editCorrelations    = false
        colourMapApp        = ColourMap.initial ColourMapItem.vfGravel.defaultMiddle xToSvg svgToX 
        selectedPoints      = hmap<AnnotationId, V3d>.Empty
        selectedLog         = None
        selectedNode        = None
        selectedBorder      = None
        secondaryLvl        = NodeLevel.init 1
        logsTable           = uitable
        //creatingNew         = false

        //svgFlags            = SvgFlags.None
        //svgOptions          = SvgOptions.init
        svgCamera           = SvgCamera.init

        keyboard            = keyboard
        xAxis               = SemanticId.invalid
        yRange              = Rangef.init
        currrentYMapping    = None

        xToSvg              = xToSvg      
        yToSvg              = yToSvg      
        defaultWidth        = defaultWidth
        elevationZeroHeight = 2982748.0
      }

        //    div [clazz "item"][
        //      div [clazz "content"] [
        //        div [clazz "header"; 
        //              style "text-align: center"; 
        //              onMouseClick (fun _ -> Action.SelectLog log.id)
        //            ] 
        //            [
        //              i [clazz "yellow arrow alternate circle down icon"] [] |> ui.tooltips.wraptooltip "select"
        //            ] |> ui.map (action.correlationplotmessage)
        //        div [] 
        //            [
        //              (
        //                  loglist model semapp annoapp
        //              )
        //            ]        
        //      ]
        //    ]
        //}   

    let threads (model : CorrelationPlot) =
      match model.logs.Count = 0 with
        | true  -> ThreadPool.empty
        | false ->
            model.logs |> HMap.map (fun id lo -> GeologicalLog.threads lo)
                       |> DS.HMap.values
                       |> List.reduce ThreadPool.union

    let app (annoApp : AnnotationApp) 
            (semApp  : SemanticApp)
            (mAnnoApp : MAnnotationApp) : App<CorrelationPlot,MCorrelationPlot,Action> =
    // let app (annoApp : amap<AnnotationId, MAnnotation>) (mAnnoApp : AnnotationModel) : App<CorrelationPlot,MCorrelationPlot,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = (update annoApp semApp)
              view = (viewSvg mAnnoApp.annotations)
          }

    //let start (annoApp : AnnotationApp) (mAnnoApp : MAnnotationApp) =
    //          update = (update mAnnoApp)
    //          view = (viewSvg annoApp)
    //      }

    let start annoApp semApp mAnnoApp =
      App.start (app annoApp semApp mAnnoApp)