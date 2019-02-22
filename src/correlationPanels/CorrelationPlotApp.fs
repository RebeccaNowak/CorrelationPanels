namespace CorrelationDrawing

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CorrelationPlotApp =
    open Aardvark.Base.Incremental
    open Aardvark.Base
    open Aardvark.Application
    open Aardvark.UI
    open UIPlus
    open System
    open Svgplus

    type Action =
      //| MouseDown     of (MouseButtons * V2i)
      //| MouseUp       of (MouseButtons * V2i)
      //| MouseMove     of V2d
      | CorrelationPlotMessage of CorrelationPlot.Action
      | Clear

    let defaultZoomFactor = 1.0
    let defaultOffset     = V2d.OO

    let initial : CorrelationPlotModel  = 
      {
        correlationPlot     = CorrelationPlot.initial
        semanticApp         = SemanticApp.getInitialWithSamples //SemanticApp.initial
        //zooming             = false
        //dragging            = false
        //lastMousePos        = V2d.OO
      }

    let update (annoApp : AnnotationModel)
               (model : CorrelationPlotModel)
               (action : Action) = 
               
      match action with

          
        | Clear -> 
          {model with correlationPlot =
                        CorrelationPlot.update annoApp model.correlationPlot CorrelationPlot.Clear
          }
        | CorrelationPlotMessage lm -> 
          {model with correlationPlot = CorrelationPlot.update annoApp model.correlationPlot lm}
        //| MouseDown (b,p) ->
        //  let p = V2d p
        //  match b with
        //    | MouseButtons.Right -> {model with dragging = true
        //                                        lastMousePos = p}
        //    | MouseButtons.Middle -> {model with zooming = true
        //                                         lastMousePos = p}
        //    | _ -> model
          
        //| MouseUp (b,p) -> 
        //  let p = V2d p
        //  match b with
        //    | MouseButtons.Right -> {model with dragging = false
        //                                        lastMousePos = p}
        //    | MouseButtons.Middle -> {model with zooming = false
        //                                         lastMousePos = p}
              
        //    | _ -> model
        //| MouseMove p ->
        //  match model.dragging, model.zooming with //TODO refactor
        //    | true, false ->
        //      let offset = model.correlationPlot.svgOptions.offset + V2d(p - model.lastMousePos)
        //      let _cp = 
        //          {model.correlationPlot with
        //                    svgOptions = {model.correlationPlot.svgOptions with
        //                                    offset = offset}
        //          }
        //      let _cp =
        //        CorrelationPlot.update annoApp model.correlationPlot (CorrelationPlot.MouseMove p)
        //      {model with 
        //        correlationPlot = _cp
        //        lastMousePos = p
        //      }            

        //    | false, true  -> 
        //      let diff = (V2d(p - model.lastMousePos))
        //      let factor = diff.OY.Length * 0.01 //TODO hardcoded zoom speed
        //      let signum =
        //        match diff.Y with
        //          | a when a <= 0.0  -> -1.0
        //          | b when b >  0.0  -> 1.0
        //          | _                -> 1.0
        //      let deltaZoom = factor * signum
        //      let zoom = (model.correlationPlot.svgOptions.zoom + deltaZoom)
        //      //let deltaFontSIze = int -signum
        //      let fontSize = 
        //        match (zoom.zoomFactor) with
        //          | z when z < 1.0 -> 
        //            FontSize.defaultSize.fontSize + (int (Math.Round ((1.0 - z) * 10.0)))
        //          | z when z > 1.0 -> 
        //            FontSize.defaultSize.fontSize - int (Math.Round z)
        //          | _ -> FontSize.defaultSize.fontSize
        //      {model with  //TODO refactor
        //        correlationPlot = 
        //          {model.correlationPlot with //vector richtung ausrechnen
        //                    svgOptions = 
        //                      {model.correlationPlot.svgOptions with
        //                        zoom = zoom
        //                        fontSize = FontSize.init fontSize
        //                      }
        //                      //(model.correlationPlot.svgFontSize + deltaFontSize)
        //          }
        //        lastMousePos = p
        //      }
        //    | true, true -> {model with dragging = false
        //                                zooming  = false}
        //    | false, false -> 
        //      let _p = V2d(p) / model.correlationPlot.svgOptions.zoom.factor  
        //      let _cpapp = CorrelationPlot.update annoApp model.correlationPlot (CorrelationPlot.MouseMove _p)
        //      {model with correlationPlot = _cpapp}
        //| AxisMessage m -> 
        //  {model with correlationPlot = CorrelationPlot.update annoApp model.correlationPlot (m |> CorrelationPlot.LogAxisAppMessage)} //TODO refactor

    let update' (annos : hmap<AnnotationId, Annotation>) (model : CorrelationPlotModel) (action : Action) = 
      update { annotations = annos; selectedAnnotation = None } model action

    let viewSvg (annoApp : MAnnotationModel) (model : MCorrelationPlotModel) =
     
      //let menu = 
      //    let axisSel = ((LogAxisApp.view model.correlationPlot.logAxisApp) |> AList.map (UI.map AxisMessage))
            
      //    div [
      //         style "float:right; vertical-align: top"
      //         attribute "position" "sticky"
      //         attribute "top" "5"
      //        ]
      //        [
      //          div []
      //              [
      //                Incremental.div (AttributeMap.ofList [style "display:inline"])
      //                                axisSel
      //          ];
      //        ]
              
        
      let domNode = 
        div [attribute "overflow-x" "hidden";attribute "overflow-y" "hidden"] [
                //menu
                CorrelationPlot.viewSvg annoApp model.correlationPlot 
                  |> UI.map CorrelationPlotMessage
               ]
      domNode

    // Log Debug View




    let view  (model    : MCorrelationPlotModel) 
              (annoApp  : MAnnotationModel) 
              (semApp   : MSemanticApp) =



      let domnode =
        let domNode = (CorrelationPlot.listView model.correlationPlot semApp annoApp) 
                        |> UI.map Action.CorrelationPlotMessage

        require (GUI.CSS.myCss) (
          body [style "overflow: auto"] [
            div [] [
              // menu |> ui.map correlationplotmessage
              Incremental.div (AttributeMap.ofList [clazz "ui inverted segment"])
                              (AList.single domNode)
                              
            ]
          ]
        )

      domnode
    



    let threads (model : CorrelationPlotModel) =
      CorrelationPlot.threads model.correlationPlot
        
    let app (annoApp : MAnnotationModel) (mAnnoApp : AnnotationModel) : App<CorrelationPlotModel,MCorrelationPlotModel,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = update mAnnoApp
              view = viewSvg annoApp
          }

    let start annoApp mAnnoApp = App.start (app annoApp mAnnoApp)

