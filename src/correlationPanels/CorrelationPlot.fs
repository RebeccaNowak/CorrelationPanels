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
      | LogNodeStyleAppMessage of LogNodeStyleApp.Action
      | NoMessage              of obj
      | ToggleEditCorrelations
      | SetSecondaryLevel      of int


    let logOffset (index : int) =
      float (index * 10 + index * 250)

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
        logNodeStyleApp     = LogNodeStyleApp.initial
        xAxis               = SemanticId.invalid
        semanticApp         = SemanticApp.getInitialWithSamples
        annotations         = PList.empty
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

    let correlate (model : CorrelationPlot) 
                  (logId : LogId)
                  (nodeId : LogNodeId)
                  (borderId : BorderId)
                  (pos      : V2d) =
      match model.selectedBorder with
        | Some border ->
          match border.logId = logId, border.id = borderId with
            | true, true   -> model
            | true, false  -> 
              // toggle the prev Border
              let (l,_,b) = (findBorder model logId nodeId borderId)
              match b with //TODO refactor
                | None -> model
                | Some b ->
                  match l with
                    | None -> model
                    | Some l ->
                      {
                        model with selectedBorder = 
                                     Some ({b with svgPosition = (new V2d (pos.X + (logOffset l.index), pos.Y))})
                      } //TODO toggleselect Action
            | false, true  -> //TODO debug output: this shouldn't happen
              model
            | false, false ->
                let (toLo, toNo, toBo) = (findBorder model logId nodeId borderId)
                match toBo, toLo, toNo with
                  | Some toBo, Some toLo, Some toNo ->
                      let newCorrelation : Correlation = 
                        {
                            fromBorder    = border
                            toBorder      = {toBo with svgPosition = (new V2d (toNo.svgPos.X + (logOffset toLo.index), pos.Y))}
                        }
                      {
                        model with correlations   = model.correlations.Append newCorrelation
                                   selectedBorder = None
                      }
                  | _,_,_     -> model
        | None   ->
            let (lo,_,b) = (findBorder model logId nodeId borderId)
            match lo with
              | None -> model
              | Some lo ->
                {model with selectedBorder = 
                              (Option.map (fun x -> 
                                ({x with svgPosition = (new V2d (pos.X + (logOffset lo.index), pos.Y))})) b)}
            

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
               //(annos : plist<Annotation>) 
               //(semApp : SemanticApp) 
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
              let guid = System.Guid.NewGuid().ToString()
              let xAxis = 
                match model.xAxis with
                  | x when  x = SemanticId.invalid -> 
                    let optS = SemanticApp.getMetricId model.semanticApp
                    match optS with
                     | Some o -> o
                     | None   -> SemanticId.invalid
                  | _ -> model.xAxis
              {model with 
                creatingNew        = false
                xAxis              = xAxis
                logs               = (model.logs.Append 
                                        (GeologicalLog.generate 
                                                (model.logs.Count)
                                                working model.annotations
                                                model.semanticApp xAxis 300.0))
                selectedPoints     = List<(V3d * Annotation)>.Empty}
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

        | LogNodeStyleAppMessage m, _  -> 
          {model with logNodeStyleApp  = (LogNodeStyleApp.update model.logNodeStyleApp m)}
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
        | _,_                         -> model
        


    let viewSvg (model : MCorrelationPlot) (semApp : MSemanticApp) =
      let atts = 
        AttributeMap.ofList 
          [
            clazz "svgRoot"
            style "border: 1px solid black"
            //attribute "viewBox" "0 0 600 400"
            attribute "preserveAspectRatio" "xMinYMin meet"
            attribute "overflow-x" "scroll";attribute "overflow-y" "scroll"
            attribute "height" "100%"
            attribute "width" "100%"
          ]
      

      let svgList =
        alist {          //TODO more elegant
          let! length   = (AList.count model.logs)
          let! logs     = model.logs.Content
          let! xAxis    = model.xAxis
          let semLabel  = SemanticApp.getLabel model.semanticApp model.xAxis

          /// LOGS
          for i in [0..length - 1] do //log in model.logs do
            let! sel = model.selectedLog
            
            let isSelected = 
              match sel with
                | Some s  -> s = (logs.Item i).id
                | None    -> false              
            let log = logs.Item i
            let attributes =
              match isSelected with
                | true  -> [style "border: 2px solid yellow";]
                | false -> []
              @ [
                  attribute "x" (sprintf "%0.2f" (logOffset i))
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
                      log model.viewType model.secondaryLvl
                      (LogNodeStyleApp.getStyle model.logNodeStyleApp)
                    ) 
            yield (logView |>  mapper)
            ///

            /// X AXIS
            let! xAxisSvg = (LogNodeStyleApp.svgXAxis 
                              model.logNodeStyleApp 
                              (new V2d((logOffset i), 350.0)) 
                              (250.0) 
                              2.0
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
      
     
      let menu = 
          let viewSelection = 
            alist {
              for sem in semApp.semanticsList do
                let! sType = sem.semanticType
                if sType = SemanticType.Metric then
                      yield getColourIconButton' sem.style.color.c
                                                    //sem.label.text
                                                    (fun _ -> ChangeXAxis sem.id)
            }
          div [clazz "ui horizontal menu";
               style "float:right; vertical-align: top"][
            div [clazz "item"]
                [button [clazz "ui small icon button"; onMouseClick (fun _ -> ChangeView CorrelationPlotViewType.LineView)] 
                        [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Line view"];
            div [clazz "item"]
                [button [clazz "ui small icon button"; onMouseClick (fun _ -> ChangeView CorrelationPlotViewType.LogView)] 
                        [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Log view"];
            div [clazz "item"]
                [button [clazz "ui small icon button"; onMouseClick (fun _ -> ChangeView CorrelationPlotViewType.CorrelationView)] 
                        [i [clazz "small exchange icon"] [] ] |> UtilitiesGUI.wrapToolTip "edit correlations"];
            Incremental.div (AttributeMap.ofList [clazz "item"])
                            viewSelection
            Incremental.div (AttributeMap.ofList [clazz "item"])
                            (LogNodeStyleApp.view model.logNodeStyleApp) |> UI.map Action.LogNodeStyleAppMessage
            div []
                [Html.SemUi.dropDown' 
                  (AList.ofList Semantic.levels) 
                  model.secondaryLvl 
                  SetSecondaryLevel 
                  (fun x -> sprintf "%i" x)]
          ]
              
        

      div [] [//[style "width:100%; height: 100%"] [
              menu
              Incremental.Svg.svg atts svgList
             ]


    let view  (model : MCorrelationPlot) =
      let menu =
        let icon =
          alist {
            let! ic =
              (model.creatingNew |> Mod.map (fun n -> 
                                              match n with
                                                | true  -> i [clazz "small yellow plus icon"] [] 
                                                | false -> i [clazz "small plus icon"] []
                                            ))
            yield ic
          }
        div [clazz "ui horizontal inverted menu";
              style "float:top"]
            [
              div [clazz "item"]
                  [Incremental.button (AttributeMap.ofList [clazz "ui small icon button"; onMouseClick (fun _ -> NewLog)]) 
                                        icon
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> FinishLog)] 
                          [i [clazz "small check icon"] [] ] |> UtilitiesGUI.wrapToolTip "done"
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> DeleteLog)] 
                          [i [clazz "small minus icon"] [] ] |> UtilitiesGUI.wrapToolTip "delete"
                  ]; 
            ]


      let domList =
         alist {            
            let! xAxis = model.xAxis
            for log in model.logs do
              let! sel = model.selectedLog
              let isSelected = 
                match sel with
                  | Some s  -> s = log.id
                  | None    -> false
              
              yield
                        div [clazz "item"][
                          div [clazz "content"] [
                            div [clazz "header"; style "text-align: center"; onMouseClick (fun _ -> ToggleSelectLog (Some log.id))] [
                              i [clazz "yellow arrow alternate circle down icon"] [] |> UtilitiesGUI.wrapToolTip "select"
                            ]
                            div [] 
                                [
                                  (GeologicalLog.view 
                                    log 
                                  )
                                ]        
                          ]
                        ]
          }     

      let myCss = [
          { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
          { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
          { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]

      require (myCss) (
        body [] [
          div [] [
            menu
            div [clazz "ui inverted segment"]
                [Incremental.div (AttributeMap.ofList [clazz "ui inverted divided list"])
                                 domList
                ]           
          ]
        ]
      )

    

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
              view = view
          }

    let start = App.start app