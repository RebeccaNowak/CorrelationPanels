namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlot =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UtilitiesGUI
    open Aardvark.UI.Primitives

    type Action = 
      | Clear
      | ToggleSelectLog        of option<LogId>
      | NewLog                 
      | TogglePoint            of (V3d * Annotation)
      | FinishLog              
      | DeleteLog              
      | LogMessage             of (LogId * GeologicalLog.Action)
      | ChangeView             of CorrelationPlotViewType
      | ChangeXAxis            of SemanticId
      | LogAxisAppMessage of LogAxisApp.Action
      | NoMessage              of obj
      | ToggleEditCorrelations
      | SetSecondaryLevel      of int
      | ToggleFlag             of LogSvgFlags


    //let logOffset (index : int) =
    //  float (index * 10 + index * 250)

    let initial : CorrelationPlot  = 
      {
        logs                = PList.empty
        correlations        = PList.empty
        selectedBorder      = None
        editCorrelations    = false
        selectedPoints      = List<(V3d * Annotation)>.Empty
        selectedLog         = None
        secondaryLvl        = 1
        creatingNew         = false
        viewType            = CorrelationPlotViewType.LogView
        svgFlags            = LogSvgFlags.None
        svgOptions          = SvgOptions.init
        logAxisApp          = LogAxisApp.initial
        xAxis               = SemanticId.invalid
        semanticApp         = SemanticApp.getInitialWithSamples
        annotations         = PList.empty
        yRange              = Rangef.init
        currrentYMapping    = None
      }



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
            GeologicalLog.findNode l nodeId        
            
      let border = 
        match log with
          | None -> None
          | Some l ->
            let n = GeologicalLog.findNode' l borderId
            match n with
              | None -> None
              | Some n ->
                (LogNode.findBorder n borderId)
      (log, node, border)

    let tryCalcAllLogsRange (model : CorrelationPlot) : option<Rangef> =
      match model.logs.IsEmptyOrNull() with
        | true  -> None
        | false ->
            let logs = model.logs
            let min =
              logs 
                |> PList.map (fun log -> log.range.min)
                |> PList.tryMinBy (fun x -> x)
            let max =
              logs 
                |> PList.map (fun log -> log.range.max)
                |> PList.tryMaxBy (fun x -> x)
            Option.map2 (fun min max -> {min = min; max = max}) min max
               
    let yToSvg' (model : MCorrelationPlot) (y : float)  =
      adaptive {  
        let! opt =  model.svgOptions
        let! yRange = model.yRange
        return  opt.logPadding + (y - yRange.min) * (opt.logHeight / yRange.range)
      }

    let yToSvg (model : CorrelationPlot) (y : float)  =
      let opt =  model.svgOptions
      let yRange = model.yRange
      match yRange.range with
        | 0.0 -> printf "Divide by zero: Log range is %.2f-%.2f" yRange.min yRange.max
        | _ -> ()
      let foo = opt.logPadding + (y - yRange.min) * (opt.logHeight / yRange.range)
      foo

    let correlate (model        : CorrelationPlot) 
                  (newLogId     : LogId)
                  (newNodeId    : LogNodeId)
                  (newBorderId  : BorderId)
                  (newPos       : V2d) =
      //let logXAxisXPos i = (model.svgOptions.xAxisPosition i).X
      let logOffset i = (model.svgOptions.logXOffset i)
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
                                     Some ({b with svgPosition = (new V2d (newPos.X + (logOffset l.index), newPos.Y))})
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
                          toBorder      = {toBo with svgPosition = 
                                                      (new V2d (
                                                        toNo.svgPos.X 
                                                          + (logOffset toLo.index) 
                                                          + 2.0 *  model.svgOptions.secLevelWidth, //2* so correlation line extends through secondary level
                                                        newPos.Y)
                                                      )
                                          }
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
                              (Option.map (fun x -> 
                                ({x with svgPosition = (new V2d (newPos.X + (logOffset lo.index), newPos.Y))})) b)}
            

    let tryCorrelate (model : CorrelationPlot) (a : Action) =
      match a with
        | LogMessage (logId, b) ->
          match b with
            | GeologicalLog.LogNodeMessage (nodeId, c) ->
              match c with
                | LogNode.BorderMessage d ->
                  match d with
                    | Border.ToggleSelect (borderId, pos) ->
                      correlate model logId nodeId borderId pos
                    | _ -> model
                | _ -> model
            | _ -> model
        | _ -> model
                  


    let update (model : CorrelationPlot) 
               (action : Action) = 
               
      match action, model.creatingNew with
        | Clear, _                     ->
          {model with logs = PList.empty
                      selectedPoints      = List<(V3d * Annotation)>.Empty
                      selectedLog         = None
                      creatingNew         = false
          }
        | ToggleSelectLog oStr, false  -> 
          match (oStr = model.selectedLog) with
            | true  -> {model with selectedLog = None}
            | false -> {model with selectedLog = oStr}
        | NewLog, false                -> 
          {model with creatingNew     = true
                      selectedPoints  = List<(V3d * Annotation)>.Empty}
        | FinishLog ,  true              ->
          match model.selectedPoints with
            | []      -> 
              printf "no points in list for creating log"
              model
            | working ->
              let xAxis = 
                match model.xAxis with
                  | x when  x = SemanticId.invalid -> 
                    let optS = SemanticApp.getMetricId model.semanticApp
                    match optS with
                     | Some o -> o
                     | None   -> SemanticId.invalid
                  | _ -> model.xAxis
              let yRangeNewLog = AnnotationPoint.tryCalcRange model.annotations //TODO taking all annos in the model > filter?
              let yRangePrev = (tryCalcAllLogsRange model)
              let (model, yOffset) = 
                match yRangeNewLog, yRangePrev with
                  | None, None -> (model, 0.0)
                  | None, Some pr -> 
                    let model = {model with yRange = pr}
                    (model, yToSvg model pr.min)
                  | Some lr, None ->
                    let model = {model with yRange = lr}
                    (model, yToSvg model lr.min)
                  | Some lr, Some pr -> 
                    let newRange = lr.outer(pr)
                    let model = 
                      {
                        model with 
                          yRange = newRange
                      }
                    let model =
                      {
                        model with
                          logs   = model.logs 
                                    |> PList.map (fun log -> 
                                                    (GeologicalLog.update log (GeologicalLog.UpdateYOffset (yToSvg model log.range.min))) //TODO refactor
                                                 ) 
                      }
                    (model, yToSvg model newRange.min)

              let (newLog, mapping) = 
                (GeologicalLog.generate 
                    model.logs.Count
                    working
                    model.annotations  //TODO taking all annos in the system > filter?
                    model.semanticApp 
                    xAxis 
                    yOffset
                    (model.svgOptions.logHeight - yOffset)
                    model.currrentYMapping)
              let mapping =
                match model.logs.Count with
                  | 0 -> Some mapping
                  | _ -> model.currrentYMapping
              {
                model with 
                  creatingNew      = false
                  xAxis            = xAxis
                  logs             = (model.logs.Append newLog)
                  selectedPoints   = List<(V3d * Annotation)>.Empty
                  currrentYMapping = mapping
              }
              //let logYOffset = model.svgOptions.toSvg (lRange.min) allLogsRange
        | DeleteLog, false             -> model
        | LogMessage (id, m), _        -> 
            let ind = model.logs.FirstIndexOf (fun (x : GeologicalLog) -> x.id = id)
            let log = (model.logs.TryGet ind)
            match log with
              | Some lo ->
                  let upd (l) = GeologicalLog.update l m
                  let updatedModel = 
                    {model with logs = model.logs.Update (ind, (fun x -> upd x))}
                  tryCorrelate updatedModel action
              | None -> model

        | LogAxisAppMessage m, _  -> 
          {model with logAxisApp  = (LogAxisApp.update model.logAxisApp m)}
        | ChangeView m, _              -> {model with viewType = m}
        | ChangeXAxis id, _            -> 
          let updLogs = model.logs 
                          |> PList.map (fun log -> 
                              GeologicalLog.update log (GeologicalLog.ChangeXAxis id))
          {model with xAxis    = id
                      logs     = updLogs
          }
        | ToggleEditCorrelations, _   -> 
          {model with editCorrelations = not model.editCorrelations}
        | SetSecondaryLevel lvl, _    -> 
          {model with secondaryLvl = lvl}
        | ToggleFlag f, _ ->
          {model with svgFlags = LogSvgFlags.toggle f model.svgFlags}
        | _,_                         -> model
        


    let viewSvg (model : MCorrelationPlot) = //TODO refactor
      let atts = 
        AttributeMap.ofList 
          [
            clazz "svgRoot"
            style "border: 1px solid black"
            //attribute "viewBox" "0 0 600 400"
            attribute "preserveAspectRatio" "xMinYMin meet"
            attribute "height" "100%"
            attribute "width" "100%"

          ]
      

      let logSvgList =
        alist {          //TODO more elegant
          let! length   = (AList.count model.logs)
          let! logs     = model.logs.Content
          let! xAxis    = model.xAxis
          let semLabel  = SemanticApp.getLabel model.semanticApp model.xAxis
          let! svgOptions = model.svgOptions
          /// LOGS
          
          for i in [0..length - 1] do //log in model.logs do
            let! sel = model.selectedLog
            
            let isSelected = 
              match sel with
                | Some s  -> s = (logs.Item i).id
                | None    -> false              
            let log = logs.Item i
            let! yOffset = log.yOffset 
            //let! lRange = log.range
            //let! logYOffset = yToSvg model (lRange.min)
            let attributes =
              match isSelected with
                | true  -> [style "border: 2px solid yellow";]
                | false -> []
              @ [
                  attribute "x" (sprintf "%0.2f" (svgOptions.logXOffset i))
                  attribute "y" (sprintf "%0.2f" yOffset)
                //  onMouseClick (fun _ -> ToggleSelectLog (Some log.id))
                ]
              |> AttributeMap.ofList 

            let mapper (a : DomNode<GeologicalLog.Action>) =
              a |> UI.map (fun m -> LogMessage (log.id, m))
            
            let logView =
              Incremental.Svg.svg 
                    (
                      attributes
                    )   
                    (GeologicalLog.svgView 
                      log model.svgFlags svgOptions model.secondaryLvl 
                      (LogAxisApp.getStyle model.logAxisApp)
                      
                    ) 
            yield (logView |>  mapper)
            ///

            /// X AXIS
            let! xAxisSvg = (LogAxisApp.svgXAxis 
                              model.logAxisApp 
                              (svgOptions.xAxisPosition i)
                              svgOptions.logMaxWidth
                              svgOptions.axisWeight
                              semLabel) 
            yield xAxisSvg 
            ///

          /// CORRELATIONS
          let correlations = 
            model.correlations |> AList.map (fun x -> Correlation.Svg.view x)
          for c in correlations do
            let! c = c
            yield c
         ///
        } 
      Incremental.Svg.svg atts logSvgList

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
                                      (GeologicalLog.getLogConnectionSg x semanticApp (x.id = logId) camera |> Sg.noEvents))
                      |> ASet.ofAList
                      |> Sg.set
                  sgs
      }
      |> Sg.dynamic


    let threads (model : CorrelationPlot) =
      match model.logs.IsEmpty() with
        | true  -> ThreadPool.empty
        | false ->
            model.logs |> PList.map (fun lo -> GeologicalLog.threads lo)
                       |> PList.toList
                       |> List.reduce ThreadPool.union

        
        
    let app : App<CorrelationPlot,MCorrelationPlot,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = update
              view = viewSvg
          }

    let start = App.start app