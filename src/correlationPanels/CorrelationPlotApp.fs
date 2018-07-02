namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
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
      | CorrelationPlotMessage of CorrelationPlot.Action

    let logOffset (index : int) =
      float (index * 10 + index * 250)

    let initial : CorrelationPlotApp  = 
      {
        correlationPlot     = CorrelationPlot.initial
        semApp              = SemanticApp.initial
      }

    let update (model : CorrelationPlotApp)
               (action : Action) = 
               
      match action with
        | CorrelationPlotMessage lm -> 
          {model with correlationPlot = CorrelationPlot.update model.correlationPlot lm}
        | _                         -> model
        


    let view' (model : MCorrelationPlotApp) (semApp : MSemanticApp) =
     
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
                            (LogNodeStyleApp.view model.correlationPlot.logNodeStyleApp) |> UI.map Action.LogNodeStyleAppMessage
            div []
                [Html.SemUi.dropDown' 
                  (AList.ofList Semantic.levels) 
                  model.correlationPlot.secondaryLvl 
                  SetSecondaryLevel 
                  (fun x -> sprintf "%i" x)]
          ]
              
        

      div [] [//[style "width:100%; height: 100%"] [
              menu
              CorrelationPlot.viewSvg model.correlationPlot semApp |> (UI.map CorrelationPlotMessage)
             ]


    let view  (model : MCorrelationPlotApp)  =
      let menu =
        let icon =
          alist {
            let! ic =
              (model.correlationPlot.creatingNew |> Mod.map (fun n -> 
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
                ((CorrelationPlot.viewSvg model.correlationPlot semApp) |> UI.map CorrelationPlotMessage)
          ]
        ]
      )

    



    let threads (model : CorrelationPlotApp) =
      CorrelationPlot.threads model.correlationPlot

        
        
    let app : App<CorrelationPlotApp,MCorrelationPlotApp,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = update
              view = view
          }

    let start = App.start app