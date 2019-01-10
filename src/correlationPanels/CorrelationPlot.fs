namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlot =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UIPlus
    open Aardvark.UI.Primitives
    open Svgplus.DA
    open Svgplus
    open Svgplus.RS

    type Action = 
      | Clear
     // | ToggleSelectLog        of option<LogId>
      | SelectLog              of LogId
      //| NewLog                 
   //   | TogglePoint            of (V3d * AnnotationId)
      | FinishLog              
   //   | SaveLog                of LogId              
      | DeleteLog              of LogId
      | LogMessage             of (LogId * Log.Action)
      | ChangeView             of CorrelationPlotViewType
      //| ChangeXAxis            of (AnnotationApp * SemanticId)
      //| LogAxisAppMessage      of LogAxisApp.Action
     // | NoMessage              of obj
      | ToggleEditCorrelations
      | SetSecondaryLevel      of NodeLevel
      | ToggleFlag             of SvgFlags
      | DiagramMessage         of DiagramApp.Action
      | MouseMove              of V2d



    //let logOffset (index : int) =
    //  float (index * 10 + index * 250)

    let initial : CorrelationPlot  = 
      {
        diagramApp          = Svgplus.DiagramApp.init
        logs                = HMap.empty
        correlations        = PList.empty
        selectedBorder      = None
        editCorrelations    = false

        selectedPoints      = hmap<AnnotationId, V3d>.Empty
        selectedLog         = None
        secondaryLvl        = NodeLevel.init 1

        //creatingNew         = false
        viewType            = CorrelationPlotViewType.LogView

        svgFlags            = SvgFlags.None
        svgOptions          = SvgOptions.init

        logAxisApp          = LogAxisApp.initial
        xAxis               = SemanticId.invalid
        semanticApp         = SemanticApp.getInitialWithSamples
        //annotations         = hmap<AnnotationId, Annotation>.Empty
        yRange              = Rangef.init
        currrentYMapping    = None

      }

    let getPointsOfLog (model : CorrelationPlot) (logId : LogId) =
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

    let createNewLog (model : CorrelationPlot) (annoApp : AnnotationApp) =
      let (stack, newLog) = 
        Log.initial 
            model.selectedPoints
            model.semanticApp 
            annoApp
            10.0
            10.0
            50.0

      let diagram =
        DiagramApp.update model.diagramApp (DiagramApp.AddStack stack)
      {
        model with 
          diagramApp       = diagram
          logs             = (model.logs.Add (newLog.id, newLog))
          selectedPoints   = hmap<AnnotationId, V3d>.Empty
      }


///////////////////////////////////////////////////////////// UPDATE ////////////////////////////////////////////////////
    
    
    let deleteLog (id : LogId) (model : CorrelationPlot) =
      let _logs = (HMap.remove id model.logs)
      {model with logs = _logs}

      

    let updateLog  index (message : Log.Action) (logs : hmap<LogId, GeologicalLog>) =
       HMap.update index (fun (x : option<GeologicalLog>) -> Log.update x.Value message) logs//hack

    let update (annoApp  : AnnotationApp)
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
          }
        | LogMessage (id, logmsg) ->
          let _dApp =
            match logmsg with
              | Log.TextInputMessage (sid, m) ->
                 DiagramApp.update model.diagramApp (DiagramApp.RectStackMessage (sid, RectangleStack.ChangeLabel m))
              | Log.MoveDown id ->
                DiagramApp.update model.diagramApp (DiagramApp.MoveRight id)
              | Log.MoveUp id ->
                DiagramApp.update model.diagramApp (DiagramApp.MoveLeft id)
              | _ -> model.diagramApp

          {model with logs       = updateLog id logmsg model.logs
                      diagramApp = _dApp
          }
        | SelectLog id ->
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
                (createNewLog {model with logs = updLogs} annoApp)
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
            DiagramApp.update model.diagramApp (DiagramApp.MouseMove m)
          {model with diagramApp = _d}
        | DiagramMessage m       -> 
          let _d =
            DiagramApp.update model.diagramApp m
          {model with diagramApp = _d}

        


    let viewSvg (annoApp : MAnnotationApp) (model : MCorrelationPlot)  = //TODO refactor
      let attsRoot = 
        [
          clazz "svgRoot"
          style "border: 1px solid black"
          //attribute "viewBox" "0 0 600 400"
          attribute "preserveAspectRatio" "xMinYMin meet"
          attribute "height" "100%"
          attribute "width" "100%"
        ]

      let attsGroup =
        amap {
          let! zf = model.svgOptions.zoom
          let! offset = model.svgOptions.offset
          let! fs = model.svgOptions.fontSize
          let transform = sprintf "scale(%f) translate(%f %f)" zf.zoomFactor offset.X offset.Y
          yield attribute "transform" transform
          yield attribute "font-size" (sprintf "%ipx" fs.fontSize)
        }

      let logSvgList =
        (DiagramApp.view model.diagramApp)
          |> AList.map (fun x -> x |> UI.map DiagramMessage)

      Svg.svg attsRoot [
        (
          Incremental.Svg.g (AttributeMap.ofAMap attsGroup) 
                            logSvgList
        )
      ]

                                       
    let listView  (model : MCorrelationPlot) 
                  (semApp : MSemanticApp)
                  (annoApp : MAnnotationApp) =

      let mapper (log : MGeologicalLog) = (fun a -> Action.LogMessage (log.id, a))
      
      let logList =
        let rows = 
          let logs = DS.AMap.valuesToAList model.logs
          let stacks = model.diagramApp.rectangleStacks
          alist {
            for log in logs do
              let! stack =  (AMap.find log.stackId stacks)
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
        
        Table.toTableView (div[][]) rows ["label";"order"]
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





    let app (annoApp : AnnotationApp) (mAnnoApp : MAnnotationApp) : App<CorrelationPlot,MCorrelationPlot,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = (update annoApp)
              view = (viewSvg mAnnoApp)
          }

    let start (annoApp : AnnotationApp) (mAnnoApp : MAnnotationApp) =
      App.start (app annoApp mAnnoApp)