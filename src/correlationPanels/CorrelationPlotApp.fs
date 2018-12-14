namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UIPlus
    open System

    type Action =
      | MouseDown of (MouseButtons * V2i)
      | MouseUp of (MouseButtons * V2i)
      | MouseMove of V2i
      | CorrelationPlotMessage of CorrelationPlot.Action
      | AxisMessage of LogAxisApp.Action
      | Clear

    let defaultZoomFactor = 1.0
    let defaultOffset     = V2d.OO

    let initial : CorrelationPlotApp  = 
      {
        correlationPlot     = CorrelationPlot.initial
        semanticApp         = SemanticApp.initial
        zooming             = false
        dragging            = false
        lastMousePos        = V2i.OO
      }

    let update (annoApp : AnnotationApp)
               (model : CorrelationPlotApp)
               (action : Action) = 
               
      match action with
        | MouseDown (b,p) ->
          match b with
            | MouseButtons.Right -> {model with dragging = true
                                                lastMousePos = p}
            | MouseButtons.Middle -> {model with zooming = true
                                                 lastMousePos = p}
            | _ -> model
          
        | MouseUp (b,p) -> 
          match b with
            | MouseButtons.Right -> {model with dragging = false
                                                lastMousePos = p}
            | MouseButtons.Middle -> {model with zooming = false
                                                 lastMousePos = p}
              
            | _ -> model
        | MouseMove p ->
          match model.dragging, model.zooming with //TODO refactor
            | true, false ->
              let offset = model.correlationPlot.svgOptions.offset + V2d(p - model.lastMousePos)
              {model with 
                correlationPlot = 
                  {model.correlationPlot with
                            svgOptions = {model.correlationPlot.svgOptions with
                                            offset = offset}
                  }
                lastMousePos = p
              }            
            | false, true  -> 
              let diff = (V2d(p - model.lastMousePos))
              let factor = diff.OY.Length * 0.01 //TODO hardcoded zoom speed
              let signum =
                match diff.Y with
                  | a when a <= 0.0  -> -1.0
                  | b when b >  0.0  -> 1.0
                  | _                -> 1.0
              let deltaZoom = factor * signum
              let zoom = (model.correlationPlot.svgOptions.zoom + deltaZoom)
              //let deltaFontSIze = int -signum
              let fontSize = 
                match (zoom.zoomFactor) with
                  | z when z < 1.0 -> 
                    FontSize.defaultSize.fontSize + (int (Math.Round ((1.0 - z) * 10.0)))
                  | z when z > 1.0 -> 
                    FontSize.defaultSize.fontSize - int (Math.Round z)
                  | _ -> FontSize.defaultSize.fontSize
              {model with  //TODO refactor
                correlationPlot = 
                  {model.correlationPlot with //vector richtung ausrechnen
                            svgOptions = 
                              {model.correlationPlot.svgOptions with
                                zoom = zoom
                                fontSize = FontSize.init fontSize
                              }
                              //(model.correlationPlot.svgFontSize + deltaFontSize)
                  }
                lastMousePos = p
              }
            | true, true -> {model with dragging = false
                                        zooming  = false}
            | false, false -> model
        | Clear -> 
          {model with correlationPlot =
                        CorrelationPlot.update annoApp model.correlationPlot CorrelationPlot.Clear
          }
        | CorrelationPlotMessage lm -> 
          {model with correlationPlot = CorrelationPlot.update annoApp model.correlationPlot lm}
        | AxisMessage m -> 
          {model with correlationPlot = CorrelationPlot.update annoApp model.correlationPlot (m |> CorrelationPlot.LogAxisAppMessage)} //TODO refactor


    let viewSvg (annoApp : MAnnotationApp) (model : MCorrelationPlotApp) =
     
      let menu = 
          let axisSel = ((LogAxisApp.view model.correlationPlot.logAxisApp) |> AList.map (UI.map AxisMessage))
            
          div [
               style "float:right; vertical-align: top"
               attribute "position" "sticky"
               attribute "top" "5"
              ]
              [
                div []
                    [
                      Incremental.div (AttributeMap.ofList [style "display:inline"])
                                      axisSel
                ];
              ]
              
        
      let domNode = 
        div [attribute "overflow-x" "hidden";attribute "overflow-y" "hidden"] [
                //menu
                CorrelationPlot.viewSvg annoApp model.correlationPlot 
                  |> UI.map CorrelationPlotMessage
               ]
      domNode

    // Log Debug View
    module View =
      let mapper (log : MGeologicalLog) = (fun a -> CorrelationPlot.LogMessage (log.id, a))
      
      let logList (model : MCorrelationPlotApp) 
                  (semApp : MSemanticApp)
                  (annoApp : MAnnotationApp) =
        let rows = 
          alist {
            for log in model.correlationPlot.logs do
              let! tmp = 
                Log.View.listView log semApp annoApp 
                                  (CorrelationPlot.Action.SelectLog log.id) 
                                  (mapper log)
              let tmp = tmp
                          |> List.map (UI.map (fun a -> Action.CorrelationPlotMessage a))
                                                      
              let! state = log.state
              for row in tmp do
                let dNodeRow = 
                  row
                    //|> UI.map (fun m -> (CorrelationPlot.LogMessage (log.id, m)))
                    //|> UI.map CorrelationPlotMessage
                yield dNodeRow
                if state = State.New then
                  let menu = 
                    (UIPlus.Menus.saveCancelMenu 
                      (CorrelationPlot.Action.SaveLog log.id)
                      (CorrelationPlot.Action.DeleteLog log.id) 
                       |> UI.map CorrelationPlotMessage)
                  yield Table.intoTr [(Table.intoTd' menu tmp.Length)]
                                  
          }

        Table.toTableView (div[][]) rows ["Label";"Order"]


      //let view  (model    : MCorrelationPlotApp) 
      //          (annoApp  : MAnnotationApp) 
      //          (semApp   : MSemanticApp) =
        //let domList =
        //  alist {            
        //    let! xAxis = model.correlationPlot.xAxis
        //    for log in model.correlationPlot.logs do
        //      let! sel = model.correlationPlot.selectedLog
        //      let isSelected = 
        //        match sel with
        //          | Some s  -> s = log.id
        //          | None    -> false
              
        //      yield
        //        div [clazz "item"][
        //          div [clazz "content"] [
        //            div [clazz "header"; 
        //                 style "text-align: center"; 
        //                 onMouseClick (fun _ -> CorrelationPlot.ToggleSelectLog (Some log.id))] 
        //                [
        //                  i [clazz "yellow arrow alternate circle down icon"] [] |> UI.ToolTips.wrapToolTip "select"
        //                ] |> UI.map (Action.CorrelationPlotMessage)
        //            div [] 
        //                [
        //                  (
        //                      logList model semApp annoApp
        //                  )
        //                ]        
        //          ]
        //        ]
        //  }   


        //let domNode =
        //  require (GUI.CSS.myCss) (
        //    body [style "overflow: auto"] [
        //      div [] [
        //       // menu |> UI.map CorrelationPlotMessage
        //        Incremental.div (AttributeMap.ofList [clazz "ui inverted segment"])
        //                        domList
        //      ]
        //    ]
        //  )

        //domNode
    



    let threads (model : CorrelationPlotApp) =
      CorrelationPlot.threads model.correlationPlot
        
    let app (annoApp : MAnnotationApp) (mAnnoApp : AnnotationApp) : App<CorrelationPlotApp,MCorrelationPlotApp,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = update mAnnoApp
              view = viewSvg annoApp
          }

    let start annoApp mAnnoApp = App.start (app annoApp mAnnoApp)


            //let menu =
        //  let icon =
        //    alist {
        //      let! ic =
        //        (model.correlationPlot.creatingNew |> Mod.map (fun n -> 
        //                                        match n with
        //                                          | true  -> i [clazz "small yellow plus icon"] [] 
        //                                          | false -> i [clazz "small plus icon"] []
        //                                      ))
        //      yield ic
        //    }
        //  div [clazz "ui horizontal inverted menu";
        //        style "float:top"]
        //      [
        //        div [clazz "item"]
        //            [Incremental.button (AttributeMap.ofList [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.NewLog)]) 
        //                                  icon
        //            ];
        //        div [clazz "item"]
        //            [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.FinishLog)] 
        //                    [i [clazz "small check icon"] [] ] |> UI.wrapToolTip "done"
        //            ];
        //        div [clazz "item"]
        //            [button [clazz "ui small icon button"; onMouseClick (fun _ -> CorrelationPlot.DeleteLog)] 
        //                    [i [clazz "small minus icon"] [] ] |> UI.wrapToolTip "delete"
        //            ]; 
        //      ]

                  //let viewSelection = 
          //  alist {
          //    for sem in model.semanticApp.semanticsList do
          //      let! sType = sem.semanticType
          //      if sType = SemanticType.Metric then
          //            yield getColourIconButton' sem.style.color.c
          //                                          (fun _ -> CorrelationPlot.ChangeXAxis sem.id)
          //  }