namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UI
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

    let update (model : CorrelationPlotApp)
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
          match model.dragging, model.zooming with
            | true, false ->
              {model with 
                correlationPlot = 
                  {model.correlationPlot with
                            svgOffset = model.correlationPlot.svgOffset + V2d(p - model.lastMousePos)
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
              let zoom = (model.correlationPlot.svgZoom + deltaZoom)
              //let deltaFontSIze = int -signum
              let fontSize = 
                match (zoom.zoomFactor) with
                  | z when z < 1.0 -> 
                    FontSize.defaultSize.fontSize + (int (Math.Round ((1.0 - z) * 10.0)))
                  | z when z > 1.0 -> 
                    FontSize.defaultSize.fontSize - int (Math.Round z)
                  | _ -> FontSize.defaultSize.fontSize
              {model with 
                correlationPlot = 
                  {model.correlationPlot with //vector richtung ausrechnen
                            svgZoom = zoom
                            svgFontSize = FontSize.init fontSize
                              //(model.correlationPlot.svgFontSize + deltaFontSize)
                  }
                lastMousePos = p
              }
            | true, true -> {model with dragging = false
                                        zooming  = false}
            | false, false -> model
        | Clear -> 
          {model with correlationPlot =
                        CorrelationPlot.update model.correlationPlot CorrelationPlot.Clear
          }
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
                CorrelationPlot.viewSvg model.correlationPlot |> UI.map CorrelationPlotMessage
               ]
      domNode

    // Log Debug View
    let view  (model : MCorrelationPlotApp)  =
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
                            i [clazz "yellow arrow alternate circle down icon"] [] |> UI.wrapToolTip "select"
                          ]
                          div [] 
                              [
                                (GeologicalLog.View.debug 
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
          body [style "overflow: auto"] [
            div [] [
             // menu |> UI.map CorrelationPlotMessage
              Incremental.div (AttributeMap.ofList [clazz "ui inverted segment"])
                              domList |> UI.map CorrelationPlotMessage
            ]
          ]
        )

      domNode
    



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