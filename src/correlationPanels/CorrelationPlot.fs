namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlot =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UI
    open Aardvark.UI.Primitives

    type Action = 
      | Clear
      | ToggleSelectLog        of option<LogId>
      //| NewLog                 
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
      | ToggleFlag             of SvgFlags


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

        //creatingNew         = false
        viewType            = CorrelationPlotViewType.LogView

        svgFlags            = SvgFlags.None
        svgOptions          = SvgOptions.init
        svgOffset           = V2d.OO
        svgZoom             = SvgZoom.defaultZoom
        svgFontSize         = FontSize.defaultSize

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


    let logXOffset (model : CorrelationPlot) (i : int) =
      let offset =
        match i = 0 with
          | true ->
            model.svgOptions.firstLogOffset
          | false ->
            let offs =
              model.logs
                |> PList.toList
                |> List.filter (fun (log : GeologicalLog) -> (log.index < i))
                |> List.map (fun log -> log.svgMaxX + 2.0 * model.svgOptions.logPadding) //(float log.index) * 
                |> List.reduce (fun x y -> x + y) //TODO unsafe
            if i = 1 then model.svgOptions.secLogOffset offs else offs //TODO find problem with 0 and 1 offset
      offset

    let logXOffset' (model : MCorrelationPlot) (i : int) =
      let offset = 
        match (i=0) with //TODO assuming log with 0 index exists
          | true ->
            adaptive {
              let! opts = model.svgOptions
              return opts.firstLogOffset
            }
          | false ->
            adaptive {
              let! logs = model.logs.Content
              let! opt = model.svgOptions
              //let! smax (log : MGeologicalLog) = 
              let filtered = 
                logs
                  |> PList.toList
                  |> List.filter (fun (log : MGeologicalLog) -> (log.index < i))
              let offsets : alist<float> = 
                alist {
                  for log in filtered do
                    let! max = log.svgMaxX
                    yield max + 2.0 * opt.logPadding //(float log.index) * 
                }
              let! offsets = offsets.Content
              return offsets
                      |> PList.toList
                      |> List.reduce (fun x y -> x + y) //TODO unsafe
            }
      offset 
        |> Mod.map2 (fun (opts : SvgOptions) x -> 
                      if i = 1 then opts.secLogOffset x else x
                    ) model.svgOptions
      
    let xAxisXPosition (model : CorrelationPlot) (i : int) =
      ((logXOffset model i) + 2.0 * model.svgOptions.secLevelWidth)

    let xAxisXPosition' (model : MCorrelationPlot) (i : int) =
      adaptive {
        let! opt = model.svgOptions
        let! offset = (logXOffset' model i)
        return (offset + 2.0 * opt.secLevelWidth)
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
                |> PList.tryMinBy (fun x -> x)
            let max =
              logs 
                |> PList.map (fun log -> log.nativeYRange.max)
                |> PList.tryMaxBy (fun x -> x)
            Option.map2 (fun min max -> {min = min; max = max}) min max
               
    let yToSvg' (model : MCorrelationPlot) (y : float)  =
      adaptive {  
        let! opt =  model.svgOptions
        let! yRange = model.yRange
        match yRange.range with
          | 0.0 -> printf "Divide by zero: Log range is %.2f-%.2f" yRange.min yRange.max
          | _ -> ()
        return opt.logPadding + (yRange.max - y) * (opt.logHeight / yRange.range)
        //opt.logPadding + (y - yRange.min) * (opt.logHeight / yRange.range)
      }

    let svgXAxisYOffset (model : MCorrelationPlot) =
      adaptive {  
        let! opt =  model.svgOptions
        let! yRange = model.yRange
        let! map = model.currrentYMapping
        return match map with 
                | None -> 
                  opt.xAxisYPosition opt.logHeight
                | Some m -> 
                  opt.xAxisYPosition (yRange.range * m )
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
        (new V2d (toNode.svgPos.X 
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
                  


    let createNewLog (model : CorrelationPlot) =
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
                                            (GeologicalLog.update log (GeologicalLog.UpdateYOffset ((newYOffset log)))) //TODO refactor
                                          ) 
                  correlations = 
                    if diff <= 0.0 then model.correlations else model.correlations |> PList.map (Correlation.moveDown (diff * model.currrentYMapping.Value)) //TODO do proper checks before using option.value
              }
            (model, yToSvg model newRange.max)

      let (newLog, mapping) = 
        (GeologicalLog.generate 
            model.logs.Count
            model.selectedPoints
            model.annotations  //TODO taking all annos in the system > filter?
            model.semanticApp 
            xAxis 
            yOffset
            (model.svgOptions.logHeight - yOffset)
            (model.svgOptions.xAxisScaleFactor)
            model.currrentYMapping)
      let mapping =
        match model.logs.Count with
          | 0 -> Some mapping
          | _ -> model.currrentYMapping
      {
        model with 
          //creatingNew      = false
          xAxis            = xAxis
          logs             = (model.logs.Append newLog)
          selectedPoints   = List<(V3d * Annotation)>.Empty
          currrentYMapping = mapping
      }


///////////////////////////////////////////////////////////// UPDATE ////////////////////////////////////////////////////
    let update (model : CorrelationPlot) 
               (action : Action) = 
               
      match action with
        | Clear                    ->
          {model with logs             = PList.empty
                      selectedPoints   = List<(V3d * Annotation)>.Empty
                      selectedLog      = None
                      correlations     = plist.Empty
                      annotations      = plist.Empty
                      currrentYMapping = None
                      selectedBorder   = None
          }
        | ToggleSelectLog oStr -> 
          match (oStr = model.selectedLog) with
            | true  -> {model with selectedLog = None}
            | false -> {model with selectedLog = oStr}
        //| NewLog             -> 
        //  {model with creatingNew     = true
        //              selectedPoints  = List<(V3d * Annotation)>.Empty}
        | FinishLog         ->
          match model.selectedPoints with
            | []      -> 
              printf "no points in list for creating log"
              model
            | working ->
              createNewLog model
              
              //let logYOffset = model.svgOptions.toSvg (lRange.min) allLogsRange
        | DeleteLog         -> model
        | LogMessage (id, m)    -> 
            let ind = model.logs.FirstIndexOf (fun (x : GeologicalLog) -> x.id = id)
            let log = (model.logs.TryGet ind)
            match log with
              | Some lo ->
                  let upd (l) = GeologicalLog.update l m
                  let updatedModel = 
                    {model with logs = model.logs.Update (ind, (fun x -> upd x))}
                  tryCorrelate updatedModel action
              | None -> model

        | LogAxisAppMessage m -> 
          {model with logAxisApp  = (LogAxisApp.update model.logAxisApp m)}
        | ChangeView m          -> {model with viewType = m}
        | ChangeXAxis id          -> 
          let updLogs = model.logs 
                          |> PList.map (fun log -> 
                              GeologicalLog.update log (GeologicalLog.ChangeXAxis (id, model.svgOptions.xAxisScaleFactor))
                                       )
          {model with xAxis    = id
                      logs     = updLogs
          }
        | ToggleEditCorrelations  -> 
          {model with editCorrelations = not model.editCorrelations}
        | SetSecondaryLevel lvl  -> 
          {model with secondaryLvl = lvl}
        | ToggleFlag f ->
          {model with svgFlags = Flags.toggle f model.svgFlags}
        | _                         -> model
        


    let viewSvg (model : MCorrelationPlot) = //TODO refactor
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
          let! offset = model.svgOffset
          let! zoom   = model.svgZoom
          let! fontSize = model.svgFontSize
          let transform = sprintf "scale(%f) translate(%f %f)" zoom.zoomFactor offset.X offset.Y
          yield attribute "transform" transform
          yield attribute "font-size" (sprintf "%ipx" fontSize.fontSize)

        }

      let logSvgList =
        alist {          //TODO more elegant
          let! length   = (AList.count model.logs)
          let! logs     = model.logs.Content
          let! xAxis    = model.xAxis
          let semLabel  = SemanticApp.getLabel model.semanticApp model.xAxis
          let! svgOptions = model.svgOptions
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

              let mapper (a : DomNode<GeologicalLog.Action>) =
                a |> UI.map (fun m -> LogMessage (log.id, m))
            
              let logView =
                Incremental.Svg.svg 
                      (
                        attributes
                      )   
                      (GeologicalLog.svgView 
                        log model.svgFlags svgOptions model.secondaryLvl 
                        (LogAxisApp.getStyle model.logAxisApp svgOptions.xAxisScaleFactor)
                      
                      ) 
              yield (logView |>  mapper)
            
              /// X AXIS
              if (Flags.isSet SvgFlags.XAxis flags) then
                let! svgMaxX = log.svgMaxX
                let! yPos = (svgXAxisYOffset model)
                let! xPos = (xAxisXPosition' model i)
                let! fontSize = model.svgFontSize
                let! xAxisSvg = (LogAxisApp.svgXAxis 
                                  model.logAxisApp 
                                  (new V2d(xPos, yPos))
                                  svgMaxX
                                  svgOptions.axisWeight
                                  svgOptions.xAxisScaleFactor
                                  fontSize.fontSize
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
              let! nativeRange = model.yRange
              let! yMapping = model.currrentYMapping
              let! fontSize = model.svgFontSize
              yield LogAxisApp.svgYAxis 
                      (new V2d(svgOptions.firstLogOffset * 0.8, svgOptions.logPadding))
                      nativeRange
                      svgOptions.axisWeight
                      yMapping.Value //TODO using .Value
                      svgOptions.yAxisStep
                      fontSize.fontSize
                      "elevation" //TODO hardcoded


            /// CORRELATIONS
            let correlations = 
              model.correlations |> AList.map (fun x -> Correlation.Svg.view x)
            for c in correlations do
              let! c = c
              yield c
           ///
        } 
     //////////////////////

      Svg.svg attsRoot [(Incremental.Svg.g (AttributeMap.ofAMap attsGroup) logSvgList)]

                                        
                           

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