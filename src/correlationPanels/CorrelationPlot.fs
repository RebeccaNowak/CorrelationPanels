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


    type Action = 
      | Clear
      | SvgCameraMessage of SvgCamera.Action
     // | ToggleSelectLog        of option<RectangleStackId>
      | SelectLog              of RectangleStackId
      //| NewLog                 
   //   | TogglePoint            of (V3d * AnnotationId)
      | FinishLog              
   //   | SaveLog                of RectangleStackId              
      | DeleteLog              of RectangleStackId
      | LogMessage             of (RectangleStackId * Log.Action)
      | ChangeView             of CorrelationPlotViewType
      //| ChangeXAxis            of (AnnotationApp * SemanticId)
      //| LogAxisAppMessage      of LogAxisApp.Action
     // | NoMessage              of obj
      | ToggleEditCorrelations
      | SetSecondaryLevel      of NodeLevel
      | ToggleFlag             of SvgFlags
      | DiagramMessage         of Diagram.Action
      | MouseMove              of V2d
      | ColourMapMessage       of ColourMap.Action
      



    //let logOffset (index : int) =
    //  float (index * 10 + index * 250)

    let initial : CorrelationPlot  = 
      let xToSvg              = fun x -> (21.0 + Math.Log(x,2.0)) * 10.0
      let svgToX              = fun x -> (Math.Pow (2.0, (x * 0.1 - 21.0)))
      let yToSvg              = 25.0
      let defaultWidth        = 20.0

      {
        diagramApp          = Svgplus.Diagram.init
        logs                = HMap.empty
        correlations        = PList.empty
        
        editCorrelations    = false
        colourMapApp        = ColourMap.initial xToSvg svgToX
        selectedPoints      = hmap<AnnotationId, V3d>.Empty
        selectedLog         = None
        selectedNode        = None
        selectedBorder      = None
        secondaryLvl        = NodeLevel.init 1

        //creatingNew         = false
        viewType            = CorrelationPlotViewType.LogView

        svgFlags            = SvgFlags.None
        svgOptions          = SvgOptions.init
        svgCamera           = SvgCamera.init

        //logAxisApp          = LogAxisApp.initial
        xAxis               = SemanticId.invalid
        semanticApp         = SemanticApp.getInitialWithSamples
        //annotations         = hmap<AnnotationId, Annotation>.Empty
        yRange              = Rangef.init
        currrentYMapping    = None

        xToSvg              = xToSvg      
        yToSvg              = yToSvg      
        defaultWidth        = defaultWidth
      }

    let tryFindLog (model : CorrelationPlot) (logId : RectangleStackId) =
      HMap.tryFind logId model.logs

    let tryFindNodeFromRectangleId (model : CorrelationPlot) 
                                   (selRect : RectangleIdentification) =
      let log = tryFindLog model selRect.stackid
      Option.bind (fun lo -> 
                    let on = Log.findNodeFromRectangleId lo selRect.rid
                    Option.map (fun n -> (n, lo)) on
                  ) log

    let tryFindNode (model : CorrelationPlot) 
                    (logId : RectangleStackId) 
                    (nodeId : LogNodeId) =
      let optLog = tryFindLog model logId
      Option.bind
        (fun log -> Log.findNode log (fun n -> n.id = nodeId)) optLog

    let tryFindNodeFromRectId (model  : CorrelationPlot) 
                              (logId  : RectangleStackId) 
                              (rectId : RectangleId) =
      let optLog = tryFindLog model logId
      Option.bind
        (fun log -> Log.findNode log (fun n -> n.id.rectangleId = rectId)) optLog
        
      

    let getPointsOfLog (model : CorrelationPlot) (logId : RectangleStackId) =
      let opt = HMap.tryFind logId model.logs
      match opt with
        | Some log -> log.annoPoints
        | None     -> HMap.empty
      
    
    let yToSvg' (model : MCorrelationPlot) (y : float)  =
      adaptive {  
        let! lp =  model.svgOptions.logPadding
        let! lh = model.svgOptions.logHeight
        let! yRange = model.yRange
        match yRange.range with
          | 0.0 -> printf "Divide by zero: Log range is %.2f-%.2f" yRange.min yRange.max
          | _ -> ()
        return lp + (yRange.max - y) * (lh / yRange.range)
        //opt.logPadding + (y - yRange.min) * (opt.logHeight / yRange.range)
      }

    let svgXAxisYOffset (model : MCorrelationPlot) = //TODO refactor
      let ypos' logHeight = SvgOptions.xAxisYPosition' model.svgOptions logHeight
      let ypos (logHeight : float) (opt : SvgOptions) = SvgOptions.xAxisYPosition opt logHeight

      adaptive {  
        let! yRange = model.yRange
        let! map = model.currrentYMapping
        let! opt = model.svgOptions.Current
        let! h = model.svgOptions.logHeight
        return match map with 
                | None -> 
                  ypos h opt
                | Some m -> 
                  ypos (yRange.range * m ) opt
      }

    let yToSvg (model : CorrelationPlot) (y : float)  =
      let opt =  model.svgOptions
      let yRange = model.yRange
      match yRange.range with
        | 0.0 -> printf "Divide by zero: Log range is %.2f-%.2f" yRange.min yRange.max
        | _ -> ()
      let foo = opt.logPadding + (yRange.max - y) * (opt.logHeight / yRange.range)
      foo

    let createNewLog (model : CorrelationPlot) (annoApp : AnnotationModel) (colourMap : ColourMap) =
      let (stack, newLog) = 
        Log.initial 
            model.selectedPoints  //selected points
            model.semanticApp     //selected
            annoApp
            model.xToSvg
            model.yToSvg
            model.defaultWidth
            colourMap

      let diagram =
        Diagram.update model.diagramApp (Diagram.AddStack stack)

      {
        model with 
          diagramApp       = diagram
          logs             = (model.logs.Add (newLog.id, newLog))
          selectedPoints   = hmap<AnnotationId, V3d>.Empty
      }


      

///////////////////////////////////////////////////////////// UPDATE ////////////////////////////////////////////////////
    
    
    let deleteLog (id : RectangleStackId) (model : CorrelationPlot) =
      let _logs = (HMap.remove id model.logs)
      {model with logs = _logs}

    let updateLog  index (message : Log.Action) (logs : hmap<RectangleStackId, GeologicalLog>) =
       HMap.update index (fun (x : option<GeologicalLog>) -> Log.update x.Value message) logs//hack

    let selectLog (model    : CorrelationPlot) (id       : RectangleStackId) =
      let hasNew = not (model.logs |> HMap.forall (fun id x -> x.state <> State.New))
      if hasNew then
        model
      else
        let (_logs, _sel) =
          match model.selectedLog with
            | None -> 
              let _logs = 
                model.logs
                  |> updateLog id (Log.Action.SetState State.Edit)
              (_logs, Some id)
            | Some logId when logId = id ->
              let _logs = 
                model.logs
                  |> updateLog id (Log.Action.SetState State.Display)
              (_logs, None)
            | Some logId ->
              let _logs = 
                model.logs
                    |> updateLog logId (Log.Action.SetState State.Display)
                    |> updateLog id (Log.Action.SetState State.Edit)
              (_logs, Some id)
        {model with logs        = _logs
                    selectedLog = _sel}

    let update (annoApp  : AnnotationModel)
               (model    : CorrelationPlot) 
               (action   : Action) = 
      
      match action with
        | Clear                    ->
          {model with logs             = HMap.empty
                      selectedPoints   = hmap<AnnotationId, V3d>.Empty
                      selectedLog      = None
                      correlations     = plist.Empty
                      //annotations      = hmap<AnnotationId, Annotation>.Empty
                      currrentYMapping = None
                      selectedBorder   = None
                      diagramApp       = Diagram.init
          }
        | SvgCameraMessage m ->
          let _svgCamera = SvgCamera.update model.svgCamera m
          {model with svgCamera = _svgCamera}
        | LogMessage (logId, logmsg) ->
          let _dApp =
            match logmsg with
              | Log.TextInputMessage (sid, m) ->
                 Diagram.update model.diagramApp (Diagram.RectStackMessage (sid, RectangleStack.ChangeLabel m))
              | Log.MoveDown id ->
                Diagram.update model.diagramApp (Diagram.MoveRight id)
              | Log.MoveUp id ->
                Diagram.update model.diagramApp (Diagram.MoveLeft id)
              | _ -> model.diagramApp

          let _model = 
            match logmsg with
              | Log.SelectLogNode nid ->
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
                       diagramApp = _dApp
          }
        | SelectLog id -> selectLog model id
        //| NewLog             -> 
        //  {model with creatingNew     = true
        //              selectedPoints  = List<(V3d * Annotation)>.Empty}
        | FinishLog ->
          let _model =
            match model.selectedPoints.IsEmpty with
              | true      -> 
                printf "no points in list for creating log"
                model //TODO create empty log
              | false ->
                let updLogs =
                  model.logs
                    |> HMap.map (fun id log -> {log with state = State.Display})
                (createNewLog {model with logs = updLogs} annoApp model.colourMapApp)
          _model
        | DeleteLog id        -> deleteLog id model
          
        //| LogAxisAppMessage m -> 
        //  {model with logAxisApp  = (LogAxisApp.update model.logAxisApp m)}
        | ChangeView m          -> {model with viewType = m}
        | ToggleEditCorrelations  -> 
          {model with editCorrelations = not model.editCorrelations}
        | SetSecondaryLevel lvl  -> 
          {model with secondaryLvl = lvl}
        | ToggleFlag f ->
          {model with svgFlags = Flags.toggle f model.svgFlags}
        | MouseMove m       -> 
          let _d =
            Diagram.update model.diagramApp (Diagram.MouseMove m)
          {model with diagramApp = _d}
        | DiagramMessage m       -> 
          let _d =
            Diagram.update model.diagramApp m

          let _cp = 
            Diagram.Action.unpack m 
                                  Diagram.UnpackAction.OnLeftClick
                                  (fun stackid _ -> selectLog model stackid) 
                                  model
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
          {_cp with diagramApp   = _d
                    colourMapApp = _cmap}

        | ColourMapMessage m -> 
          let _cmap = ColourMap.update model.colourMapApp m
          let optselid = model.diagramApp.selectedRectangle 
          let (_logs, _diagram) = 
            match m, optselid.IsSome with
              | ColourMap.SelectItem cmitemid, true ->
                let selRect = optselid.Value
                let optsel = 
                  Diagram.tryFindRectangle model.diagramApp selRect
                match optsel with
                  | Some r ->
                    let width = ColourMap.svgValueFromItemId model.colourMapApp cmitemid
                    let _optn = tryFindNodeFromRectangleId model selRect
                    match _optn with
                      | Some (n, log) ->
                        let m = 
                              (Log.LogNodeMessage 
                                (n.id, LogNodes.RectangleMessage (Rectangle.SetWidth (width, _cmap)))
                              )
                        let _logs  = updateLog log.id m model.logs
                        let rectIds = {stackid = optselid.Value.stackid; rid = optselid.Value.rid}
                        let rectMessage = Rectangle.SetWidth (width, _cmap)
                        let diagr = Diagram.update model.diagramApp (Diagram.UpdateRectangle (rectIds, rectMessage))
                        (_logs, diagr)
                      | None -> (model.logs, model.diagramApp)
                  | None   -> (model.logs, model.diagramApp)
               | ColourMap.ItemMessage cmitemid, _ -> 
                 let diagr = Diagram.update model.diagramApp (Diagram.UpdateColour _cmap)
                 (model.logs, diagr)
               | _,_ -> (model.logs, model.diagramApp)
          
          {model with colourMapApp = _cmap
                      diagramApp   = _diagram
                      logs         = _logs}


        


    let viewSvg (annoApp : MAnnotationModel) (model : MCorrelationPlot)  = //TODO refactor
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
        (Diagram.view model.diagramApp)
          |> AList.map (fun x -> x |> UI.map DiagramMessage)

      Svg.svg attsRoot [
        (
          Incremental.Svg.g (AttributeMap.ofAMap attsGroup) 
                            logSvgList
        )
      ]

                                       
    let listView  (model : MCorrelationPlot) 
                  (semApp : MSemanticApp)
                  (annoApp : MAnnotationModel) =
      let mapper (log : MGeologicalLog) = (fun a -> Action.LogMessage (log.id, a))
      
      let logList =
        let rows = 
          let stacks = model.diagramApp.rectangleStacks
          alist {
            for id in model.diagramApp.order do
              let! stack = AMap.find id stacks
              let! log   = AMap.find id model.logs
              let! tmp = 
                Log.View.listView log semApp annoApp 
                                  (Action.SelectLog log.id) 
                                  (mapper log)        
                                  stack
               //|> List.map (UI.map Action.SelectLog)
              //let! state = log.state
              for row in tmp do
                yield row
              //  if state = State.New then
              //    let menu = 
              //      (UIPlus.Menus.saveCancelMenu 
              //        (Action.SaveLog log.id)
              //        (Action.DeleteLog log.id) 
              //      )
              //    yield Table.intoTr [(Table.intoTd' menu tmp.Length)]     
          }
        
        Table.toTableView (div[][]) rows ["Log Name";"Order"]
      logList
      



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
        



    //////////////////////
    //let getLogConnectionSgs 
    //      (model : MCorrelationPlot)
    //      (semanticApp : MSemanticApp) 
    //      (camera : MCameraControllerState) =

    //  adaptive {
    //    let! logIdOpt = model.selectedLog
    //    return match logIdOpt with
    //            | None      -> Sg.empty
    //            | Some logId  ->
    //              let sgs = 
    //                model.logs
    //                  |> AList.map (fun (x : MGeologicalLog) -> 
    //                                  (Log.getLogConnectionSg x semanticApp (x.id = logId) camera |> Sg.noEvents))
    //                  |> ASet.ofAList
    //                  |> Sg.set
    //              sgs
    //  }
    //  |> Sg.dynamic

    let threads (model : CorrelationPlot) =
      match model.logs.Count = 0 with
        | true  -> ThreadPool.empty
        | false ->
            model.logs |> HMap.map (fun id lo -> Log.threads lo)
                       |> DS.HMap.values
                       |> List.reduce ThreadPool.union





    let app (annoApp : AnnotationModel) (mAnnoApp : MAnnotationModel) : App<CorrelationPlot,MCorrelationPlot,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = (update annoApp)
              view = (viewSvg mAnnoApp)
          }

    let start (annoApp : AnnotationModel) (mAnnoApp : MAnnotationModel) =
      App.start (app annoApp mAnnoApp)