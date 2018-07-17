namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UtilitiesGUI

    type Action =
      | CorrelationPlotMessage of CorrelationPlot.Action
      | AxisMessage of LogAxisApp.Action
      | Clear



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
        | AxisMessage m -> 
          {model with correlationPlot = CorrelationPlot.update model.correlationPlot (m |> CorrelationPlot.LogAxisAppMessage)} //TODO refactor


    let viewSvg (model : MCorrelationPlotApp) =
     
      let menu = 
          //let viewSelection = 
          //  alist {
          //    for sem in model.semanticApp.semanticsList do
          //      let! sType = sem.semanticType
          //      if sType = SemanticType.Metric then
          //            yield getColourIconButton' sem.style.color.c
          //                                          (fun _ -> CorrelationPlot.ChangeXAxis sem.id)
          //  }

          let flagButtons =
            let names = System.Enum.GetNames(typeof<LogSvgFlags>)
            seq {
              for str in names.[1..names.Length-1] do
                let e = LogSvgFlags.parse str
                yield (toggleButton str (fun p -> CorrelationPlot.ToggleFlag e)) //|> UI.map CorrelationPlotMessage
            } |> List.ofSeq

          //let foo = (viewSelection |> AList.map (UI.map CorrelationPlotMessage))
          let axisSel = ((LogAxisApp.view model.correlationPlot.logAxisApp) |> AList.map (UI.map AxisMessage))
            
          div [//clazz "ui horizontal menu";
               style "float:right; vertical-align: top"
               attribute "position" "sticky"
               attribute "top" "5"
              ]
              [
                div [style "display:inline"] flagButtons |> UI.map CorrelationPlotMessage
                    //[div [clazz "ui horizontal buttons"] flagButtons]
                div []
                    [
                      //button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.ChangeView CorrelationPlotViewType.LineView)] 
                      //       [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Line view" |> UI.map CorrelationPlotMessage
                      //button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.ChangeView CorrelationPlotViewType.LogView)] 
                      //       [i [clazz "small align left icon"] [] ] |> UtilitiesGUI.wrapToolTip "Log view" |> UI.map CorrelationPlotMessage
                      //button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.ChangeView CorrelationPlotViewType.CorrelationView)] 
                      //       [i [clazz "small exchange icon"] [] ] |> UtilitiesGUI.wrapToolTip "edit correlations" |> UI.map CorrelationPlotMessage
                      

                      Incremental.div (AttributeMap.ofList [style "display:inline"])
                                      axisSel
                                            
                
                    
                      div [style "display:inline"]
                          [Html.SemUi.dropDown' 
                            (AList.ofList Semantic.levels) 
                            model.correlationPlot.secondaryLvl 
                            CorrelationPlot.SetSecondaryLevel 
                            (fun x -> sprintf "%i" x)
                            |> UI.map CorrelationPlotMessage]
                ];
              ]
              
        
      let domNode = 
        div [attribute "overflow-x" "hidden";attribute "overflow-y" "hidden"] [
                menu
                CorrelationPlot.viewSvg model.correlationPlot |> UI.map CorrelationPlotMessage
               ]
      domNode


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