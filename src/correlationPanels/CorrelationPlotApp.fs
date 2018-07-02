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
      | CorrelationPlotMessage of CorrelationPlot.Action
      | Clear

    let logOffset (index : int) =
      float (index * 10 + index * 250)

    let initial : CorrelationPlotApp  = 
      {
        correlationPlot     = CorrelationPlot.initial
        semanticApp         = SemanticApp.initial
      }

    let update (model : CorrelationPlotApp)
               (action : Action) = 
               
      match action with
        | Clear -> 
          {model with correlationPlot =
                        CorrelationPlot.update model.correlationPlot CorrelationPlot.Clear}
        | CorrelationPlotMessage lm -> 
          {model with correlationPlot = CorrelationPlot.update model.correlationPlot lm}
        


    let viewSvg (model : MCorrelationPlotApp) =
     
      let menu = 
          let viewSelection = 
            alist {
              for sem in model.semanticApp.semanticsList do
                let! sType = sem.semanticType
                if sType = SemanticType.Metric then
                      yield getColourIconButton' sem.style.color.c
                                                    (fun _ -> CorrelationPlot.ChangeXAxis sem.id)
            }
          div [clazz "ui horizontal menu";
               style "float:right; vertical-align: top"
               attribute "position" "fixed"
              ]
              [
                div [clazz "item"]
                    [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.ChangeView CorrelationPlotViewType.LineView)] 
                            [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Line view"];
                div [clazz "item"]
                    [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.ChangeView CorrelationPlotViewType.LogView)] 
                            [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Log view"];
                div [clazz "item"]
                    [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.ChangeView CorrelationPlotViewType.CorrelationView)] 
                            [i [clazz "small exchange icon"] [] ] |> UtilitiesGUI.wrapToolTip "edit correlations"];
                Incremental.div (AttributeMap.ofList [clazz "item"])
                                viewSelection
                Incremental.div (AttributeMap.ofList [clazz "item"])
                                (LogNodeStyleApp.view model.correlationPlot.logNodeStyleApp) |> UI.map CorrelationPlot.LogNodeStyleAppMessage
                div []
                    [Html.SemUi.dropDown' 
                      (AList.ofList Semantic.levels) 
                      model.correlationPlot.secondaryLvl 
                      CorrelationPlot.SetSecondaryLevel 
                      (fun x -> sprintf "%i" x)]
              ]
              
        
      let domNode = 
        div [] [
                menu
                CorrelationPlot.viewSvg model.correlationPlot 
               ]
      domNode |> UI.map CorrelationPlotMessage


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
                  [Incremental.button (AttributeMap.ofList [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.NewLog)]) 
                                        icon
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.FinishLog)] 
                          [i [clazz "small check icon"] [] ] |> UtilitiesGUI.wrapToolTip "done"
                  ];
              div [clazz "item"]
                  [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.DeleteLog)] 
                          [i [clazz "small minus icon"] [] ] |> UtilitiesGUI.wrapToolTip "delete"
                  ]; 
            ]


      let domList =
        alist {            
          let! xAxis = model.correlationPlot.xAxis
          for log in model.correlationPlot.logs do
            let! sel = model.correlationPlot.selectedLog
            let isSelected = 
              match sel with
                | Some s  -> s = log.id
                | None    -> false
              
            yield
                      div [clazz "item"][
                        div [clazz "content"] [
                          div [clazz "header"; style "text-align: center"; onMouseClick (fun _ -> CorrelationPlot.ToggleSelectLog (Some log.id))] [
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

      let domNode =
        require (myCss) (
          body [] [
            div [] [
              menu
              Incremental.div (AttributeMap.ofList [clazz "ui inverted segment"])
                              domList
            ]
          ]
        )

      domNode |> UI.map CorrelationPlotMessage
    



    let threads (model : CorrelationPlotApp) =
      CorrelationPlot.threads model.correlationPlot

        
        
    let app : App<CorrelationPlotApp,MCorrelationPlotApp,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = update
              view = viewSvg
          }

    let start = App.start app