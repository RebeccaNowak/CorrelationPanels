namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlot =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UIPlus
    open Aardvark.UI.Primitives

    type Action = 
      | Clear
     // | ToggleSelectLog        of option<LogId>
      | SelectLog              of LogId
      //| NewLog                 
      | TogglePoint            of (V3d * AnnotationId)
      | FinishLog              
      | SaveLog                of LogId              
      | DeleteLog              of LogId
      | LogMessage             of (LogId * Log.Action)
      | ChangeView             of CorrelationPlotViewType
      | ChangeXAxis            of (AnnotationApp * SemanticId)
      | LogAxisAppMessage      of LogAxisApp.Action
      | NoMessage              of obj
      | ToggleEditCorrelations
      | SetSecondaryLevel      of NodeLevel
      | ToggleFlag             of SvgFlags



    //let logOffset (index : int) =
    //  float (index * 10 + index * 250)

    let initial : CorrelationPlot  = 
      {
        logs                = PList.empty
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

    let getPointsOfLog  (model : CorrelationPlot) 
                        (logId : LogId) =
      let optLog =
        model.logs
          |> PList.toList
          |> List.tryFind (fun x -> x.id = logId)
      match optLog with
        | Some log -> log.annoPoints
        | None     -> hmap<AnnotationId, V3d>.Empty


    let findBorder (model : CorrelationPlot) 
                   (logId : LogId)
                   (nodeId : LogNodeId)
                   (borderId : BorderId) = 
      let log = 
        model.logs
          |> PList.toList
          |> List.tryFind (fun x -> x.id = logId)

      let node =         
        match log with
          | None -> None
          | Some l ->
            Log.findNode l nodeId        
            
      let border = 
        match log with
          | None -> None
          | Some l ->
            let n = Log.findNode' l borderId
            match n with
              | None -> None
              | Some n ->
                (LogNodes.Helper.findBorder n borderId)
      (log, node, border)


    let logXOffset (model : CorrelationPlot) (i : int) =
      let offset =
        match i = 0 with
          | true ->
            SvgOptions.firstLogOffset model.svgOptions
          | false ->
            let offs =
              model.logs
                |> PList.toList
                |> List.filter (fun (log : GeologicalLog) -> (log.index < i))
                |> List.map (fun log -> log.svgMaxX + 2.0 * model.svgOptions.logPadding) //(float log.index) * 
                |> List.reduce (fun x y -> x + y) //TODO unsafe
            if i = 1 then 
              SvgOptions.secLogOffset model.svgOptions offs 
              else offs //TODO find problem with 0 and 1 offset
      offset

    let logXOffset' (model : MCorrelationPlot) (i : int) =
      let offset = 
        match (i=0) with //TODO assuming log with 0 index exists
          | true ->
            SvgOptions.firstLogOffset' model.svgOptions
          | false ->
            adaptive {
              let! logs = model.logs.Content
              let opt = model.svgOptions
              let filtered = 
                logs
                  |> PList.toList
                  |> List.filter (fun (log : MGeologicalLog) -> (Mod.force log.index < i))
              let offsets : alist<float> = 
                alist {
                  for log in filtered do
                    let! max = log.svgMaxX
                    let! p = opt.logPadding
                    yield max + 2.0 * p //(float log.index) * 
                }
              let! offsets = offsets.Content
              return offsets
                      |> PList.toList
                      |> List.reduce (fun x y -> x + y) //TODO unsafe
            }

      SvgOptions.secLogOffset' model.svgOptions offset
      
    let xAxisXPosition (model : CorrelationPlot) (i : int) =
      ((logXOffset model i) + 2.0 * model.svgOptions.secLevelWidth)

    let xAxisXPosition' (model : MCorrelationPlot) (i : int) =
      adaptive {
        let! sw = model.svgOptions.secLevelWidth
        let! offset = (logXOffset' model i)
        return (offset + 2.0 * sw)
      }
      //let i = float i
      //i * this.logPadding + i * this.logMaxWidth //TODO hardcoded log width

    let tryCalcAllLogsRange (model : CorrelationPlot) : option<Rangef> =
      match model.logs.IsEmptyOrNull() with
        | true  -> None
        | false ->
            let logs = model.logs
            let min =
              logs 
                |> PList.map (fun log -> log.nativeYRange.min)
                |> DS.PList.tryMinBy (fun x -> x)
            let max =
              logs 
                |> PList.map (fun log -> log.nativeYRange.max)
                |> DS.PList.tryMaxBy (fun x -> x)
            Option.map2 (fun min max -> {min = min; max = max}) min max
               
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

    let correlate (model                : CorrelationPlot) 
                  (newLogId             : LogId)
                  (newNodeId            : LogNodeId)
                  (newBorderId          : BorderId)
                  (newPosLogCoordinates : V2d) =
      //let logXAxisXPos i = (model.svgOptions.xAxisPosition i).X
      let calcSvgPosFrom log = 
        (new V2d (newPosLogCoordinates.X + (logXOffset model log.index),
                    newPosLogCoordinates.Y + log.yOffset))
      let calcSvgPosTo toLog toNode = 
        (new V2d (toNode.mainBody.pos.X  //TODO content of svgPos.X is 0 for hierarchical nodes
                      + (logXOffset model toLog.index) 
                      + 2.0 *  model.svgOptions.secLevelWidth, //2* so correlation line extends through secondary level
                  newPosLogCoordinates.Y 
                    + toLog.yOffset
                 )
        )

      match model.selectedBorder with
        | Some selectedBorder ->
          match selectedBorder.logId = newLogId, selectedBorder.id = newBorderId with
            | true, true   -> model
            | true, false  -> //same log, different border -> toggle
              // toggle the prev Border
              let (l,_,b) = (findBorder model newLogId newNodeId newBorderId)
              match b with //TODO refactor
                | None -> model
                | Some b ->
                  match l with
                    | None -> model
                    | Some l ->
                      {
                        model with selectedBorder = // 
                                     Some ({b with svgPosition = calcSvgPosFrom l}) //WIP correlation positions are off
                                          
                      } //TODO toggleselect Action
            | false, true  -> //TODO debug output: this shouldn't happen
              model
            | false, false -> //different log, different border -> correlate
                //TODO check index: sort borders
                let (toLo, toNo, toBo) = (findBorder model newLogId newNodeId newBorderId)
                match toBo, toLo, toNo with
                  | Some toBo, Some toLo, Some toNo ->
                      let newCorrelation : Correlation = 
                        {
                          fromBorder    = selectedBorder
                          toBorder      = 
                            {toBo with svgPosition = calcSvgPosTo toLo toNo}
                        }
                      {
                        model with correlations   = model.correlations.Append newCorrelation
                                   selectedBorder = None
                      }
                  | _,_,_     -> model
        | None   ->
            let (lo,_,b) = (findBorder model newLogId newNodeId newBorderId)
            match lo with
              | None -> model
              | Some lo ->
                {model with selectedBorder = 
                              b |> Option.map 
                                    (fun x -> {x with svgPosition = calcSvgPosFrom lo})
                }
            

    let tryCorrelate (model : CorrelationPlot) (a : Action) =
      match a with
        | LogMessage (logId, b) ->
          match b with
            | Log.LogNodeMessage (nodeId, c) ->
              match c with
                | LogNodes.BorderMessage d ->
                  match d with
                    | Border.ToggleSelect (borderId, pos) ->
                      correlate model logId nodeId borderId pos
                    | _ -> model
                | _ -> model
            | _ -> model
        | _ -> model
                  


    let createNewLog (model : CorrelationPlot) (annoApp : AnnotationApp) =
      let xAxis = 
        match model.xAxis with
          | x when  x = SemanticId.invalid -> 
            let optS = SemanticApp.getMetricId model.semanticApp
            match optS with
              | Some o -> o
              | None   -> SemanticId.invalid
          | _ -> model.xAxis
      let yRangeNewLog = AnnotationPoint.tryCalcRange (DS.HMap.toPList annoApp.annotations) //TODO taking all annos in the model > filter?
      let yRangePrev = (tryCalcAllLogsRange model)
      let (model, yOffset) = 
        match yRangeNewLog, yRangePrev with
          | None, None -> (model, 0.0)
          | None, Some pr -> 
            let model = {model with yRange = pr}
            (model, yToSvg model pr.max)
          | Some lr, None ->
            let model = {model with yRange = lr}
            (model, yToSvg model lr.max)
          | Some lr, Some pr -> 
            let newRange = lr.outer(pr)
            let diff = newRange.max - model.yRange.max
            let model = {model with yRange = newRange}
            let newYOffset log = (yToSvg model log.nativeYRange.max)
            let model =
              {
                model with
                  logs   = model.logs 
                            |> PList.map (fun log ->  ////////////////TODO BUG
                                            (Log.update log (Log.UpdateYOffset ((newYOffset log)))) //TODO refactor
                                          ) 
                  correlations = 
                    if diff <= 0.0 then model.correlations else model.correlations |> PList.map (Correlation.moveDown (diff * model.currrentYMapping.Value)) //TODO do proper checks before using option.value
              }
            (model, yToSvg model newRange.max)

      let (newLog, mapping) = 
        (Log.generate 
            model.logs.Count
            model.selectedPoints
            model.semanticApp 
            annoApp
            xAxis 
            yOffset
            (model.svgOptions.logHeight - yOffset)
            model.currrentYMapping
            model.svgOptions)
      let mapping =
        match model.logs.Count with
          | 0 -> Some mapping
          | _ -> model.currrentYMapping
      {
        model with 
          //creatingNew      = false
          xAxis            = xAxis
          logs             = (model.logs.Append newLog)
          selectedPoints   = hmap<AnnotationId, V3d>.Empty
          currrentYMapping = mapping
      }


///////////////////////////////////////////////////////////// UPDATE ////////////////////////////////////////////////////
    let deleteCorrelations (logId : LogId) (model : CorrelationPlot) =
      let f (x : Correlation) =
        (x.fromBorder.logId = logId) 
          || (x.toBorder.logId = logId)
      let updCorr = 
        model.correlations
          |> DS.PList.deleteAll f
      {model with correlations = updCorr}

    let indexOf (logs : plist<GeologicalLog>) id =
      logs.FirstIndexOf (fun (x : GeologicalLog) -> x.id = id)

    let tryGetLog (logs : plist<GeologicalLog>) id =
      let ind = logs.FirstIndexOf (fun (x : GeologicalLog) -> x.id = id)
      (ind, logs.TryGet ind)

    let updateLog id message logs =
      let (ind, opt) = tryGetLog logs id
      match opt with
        | Some log ->
          logs.Update (ind, (fun x -> Log.update x message))
        | None    -> logs

    
    
    
    let deleteLog id model =
      let del id model = 
        let ind = indexOf model.logs id
        model.logs.RemoveAt ind
      let updLogs =
        model
          |> deleteCorrelations id
          |> del id
      {model with logs = updLogs}

      

    let updateLog' message index (logs : plist<GeologicalLog>) =
       logs.Update (index, (fun x -> Log.update x message))

    let moveUp (logs : plist<GeologicalLog>) id =
      let (ind, log) = tryGetLog logs id
      match (ind > 0) with 
        | true  -> 
          let above = logs.Item (ind-1)
          let updLogs =
            logs
              |> updateLog' (Log.MoveUp id) ind
              |> updateLog' (Log.MoveDown above.id) (ind-1)
          let above = logs.Item (ind-1)
          let current = logs.Item ind
          let updLogs = updLogs.Update (ind,(fun x -> above))
          let updLogs = updLogs.Update (ind-1,(fun x -> current))
          updLogs
        | false -> logs

    let moveDown (logs : plist<GeologicalLog>) id =
      let (ind, log) = tryGetLog logs id
      match (ind < (logs.Count - 1)) with 
        | true  -> 
          let below = logs.Item (ind+1)
          let updLogs =
            logs
              |> updateLog' (Log.MoveDown id) ind
              |> updateLog' (Log.MoveUp below.id) (ind+1)
          let below = logs.Item (ind+1)
          let current = logs.Item ind
          let updLogs = updLogs.Update (ind,(fun x -> below))
          let updLogs = updLogs.Update (ind+1,(fun x -> current))
          updLogs
        | false -> logs

    let update (annoApp  : AnnotationApp)
               (model    : CorrelationPlot) 
               (action   : Action) = 
               
      match action with
        | Clear                    ->
          {model with logs             = PList.empty
                      selectedPoints   = hmap<AnnotationId, V3d>.Empty
                      selectedLog      = None
                      correlations     = plist.Empty
                      //annotations      = hmap<AnnotationId, Annotation>.Empty
                      currrentYMapping = None
                      selectedBorder   = None
          }

        | SelectLog id ->
          let hasNew = model.logs |> DS.PList.contains (fun x -> x.state = State.New)
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
                        //|> PList.map (fun log -> {log with state = State.Display})
                        |> updateLog id (Log.Action.SetState State.Edit)
                  (_logs, Some id)
            {model with logs        = _logs
                        selectedLog = _sel}
          
          
        //| NewLog             -> 
        //  {model with creatingNew     = true
        //              selectedPoints  = List<(V3d * Annotation)>.Empty}
        | FinishLog ->
          match model.selectedPoints.IsEmpty with
            | true      -> 
              printf "no points in list for creating log"
              model //TODO create empty log
            | false ->
              let updLogs =
                model.logs
                  |> PList.map (fun log -> {log with state = State.Display})
              (createNewLog {model with logs = updLogs} annoApp)

        | SaveLog   id      ->
          let ind = indexOf model.logs id
          let updLogs = model.logs.Update (ind, (fun x -> {x with state = State.Display}))
          {model with logs = updLogs}
          //match model.selectedPoints with
          //  | []      -> 
          //    printf "no points in list for creating log"
          //    model
          //  | working ->
          //    createNewLog model
              
              //let logYOffset = model.svgOptions.toSvg (lRange.min) allLogsRange
        | DeleteLog id        -> deleteLog id model
          
        | LogMessage (id, m)    -> 
          let logs =
            match m with
              | Log.Action.MoveUp id ->
                moveUp model.logs id
                
              | Log.Action.MoveDown id ->
                moveDown model.logs id
              | _ ->
                model.logs |> updateLog id m
          let updatedModel = 
            {model with logs = logs}
          tryCorrelate updatedModel action

        | LogAxisAppMessage m -> 
          {model with logAxisApp  = (LogAxisApp.update model.logAxisApp m)}
        | ChangeView m          -> {model with viewType = m}
        //| ChangeXAxis (annoApp, id)     -> 
        //  let updLogs = model.logs 
        //                  |> PList.map (fun log -> 
        //                      Log.update log (Log.ChangeXAxis (annoApp, id, model.svgOptions.xAxisScaleFactor, model.svgOptions))
        //                               )
        //  {model with xAxis    = id
        //              logs     = updLogs
        //  }
        | ToggleEditCorrelations  -> 
          {model with editCorrelations = not model.editCorrelations}
        | SetSecondaryLevel lvl  -> 
          {model with secondaryLvl = lvl}
        | ToggleFlag f ->
          {model with svgFlags = Flags.toggle f model.svgFlags}
        | _                         -> model
        


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
        alist {          //TODO more elegant
          let! length   = (AList.count model.logs)
          let! logs     = model.logs.Content
          let! xAxis    = model.xAxis
          let semLabel  = SemanticApp.getLabel model.semanticApp model.xAxis
          let! flags = model.svgFlags
          let! sel = model.selectedLog
          /// LOGS
          match length with
           | le when le = 0 -> ()
           | _ ->
            for i in [0..length - 1] do //log in model.logs do
              let isSelected = 
                match sel with
                  | Some s  -> s = (logs.Item i).id
                  | None    -> false              
              let log = logs.Item i
              let! yOffset = log.yOffset 
              let! lRange = log.nativeYRange
              let! lxo = (logXOffset' model i)
              //let! logYOffset = yToSvg model (lRange.min)
              let attributes =
                match isSelected with
                  | true  -> [style "border: 2px solid yellow";]
                  | false -> []
                @ [
                    attribute "x" (sprintf "%0.2f" lxo)
                    attribute "y" (sprintf "%0.2f" yOffset)
                  //  onMouseClick (fun _ -> ToggleSelectLog (Some log.id))
                  ]
                |> AttributeMap.ofList 

              let mapper (a : DomNode<Log.Action>) =
                a |> UI.map (fun m -> LogMessage (log.id, m))
              let! xAxisScaleFactor = model.svgOptions.xAxisScaleFactor
              let logDomNode =
                (Log.View.svgView 
                                log annoApp model.svgFlags model.svgOptions model.secondaryLvl 
                                (LogAxisApp.getStyle model.logAxisApp xAxisScaleFactor)
                ) |> AList.map mapper
              let logView =
                Incremental.Svg.svg 
                      (
                        attributes
                      )   
                      logDomNode
                              
              yield logView
            
              /// X AXIS
              if (Flags.isSet SvgFlags.XAxis flags) then
                let! svgMaxX = log.svgMaxX
                let! yPos = (svgXAxisYOffset model)
                let! xPos = (xAxisXPosition' model i)
                let! xAxisSvg = (LogAxisApp.svgXAxis 
                                  model.logAxisApp 
                                  (new V2d(xPos, yPos))
                                  model.svgOptions
                                  svgMaxX
                                  semLabel
                                ) 
                yield xAxisSvg 
              ///

            ///// Y AXIS FOR EACH LOG
            //if (LogSvgFlags.isSet LogSvgFlags.YAxis flags) then
            //  let! nativeRange = log.nativeYRange
            //  let! yMapping = model.currrentYMapping
            //  yield LogAxisApp.svgYAxis 
            //          (new V2d(lxo, yOffset))
            //          nativeRange
            //          svgOptions.axisWeight
            //          yMapping.Value //TODO using .Value
            //          svgOptions.yAxisStep
            //          "elevation" //TODO hardcoded
            /////

            if (Flags.isSet SvgFlags.YAxis flags) then
              let! yAxis =
                LogAxisApp.svgYAxis' model.svgOptions
                                      model.yRange
                                      model.currrentYMapping
                                      (Mod.constant "elevation")
              yield yAxis
                                     
              //let! nativeRange = model.yRange
              //let! yMapping = model.currrentYMapping
              //yield LogAxisApp.svgYAxis 
              //        (new V2d((SvgOptions.firstLogOffset svgOptions) * 0.8, svgOptions.logPadding))
              //        nativeRange
              //        svgOptions.axisWeight
              //        yMapping.Value //TODO using .Value
              //        svgOptions.yAxisStep
              //        svgOptions.fontSize.fontSize
              //        "elevation" //TODO hardcoded


            /// CORRELATIONS
            let correlations = 
              model.correlations |> AList.map (fun x -> Correlation.Svg.view x)
            for c in correlations do
              let! c = c
              yield c
           ///
        } 
     //////////////////////
      //let labelDomNode =
      //  alist {
      //    for log in model.logs do
      //      let! label = log.label.text
      //      let! maxX = log.svgMaxX
      //      let! opt = model.svgOptions
      //      let! fontSize = model.svgFontSize
      //      yield (Svg.drawText (V2d (log., opt.logPadding - float fontSize.fontSize)) label Orientation.Horizontal)
      //  }

      Svg.svg attsRoot [
        (Incremental.Svg.g (AttributeMap.ofAMap attsGroup) logSvgList)
      ]

                                        
                           

    //////////////////////
    let getLogConnectionSgs 
          (model : MCorrelationPlot)
          (semanticApp : MSemanticApp) 
          (camera : MCameraControllerState) =

      adaptive {
        let! logIdOpt = model.selectedLog
        return match logIdOpt with
                | None      -> Sg.empty
                | Some logId  ->
                  let sgs = 
                    model.logs
                      |> AList.map (fun (x : MGeologicalLog) -> 
                                      (Log.getLogConnectionSg x semanticApp (x.id = logId) camera |> Sg.noEvents))
                      |> ASet.ofAList
                      |> Sg.set
                  sgs
      }
      |> Sg.dynamic

    let threads (model : CorrelationPlot) =
      match model.logs.IsEmpty() with
        | true  -> ThreadPool.empty
        | false ->
            model.logs |> PList.map (fun lo -> Log.threads lo)
                       |> PList.toList
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