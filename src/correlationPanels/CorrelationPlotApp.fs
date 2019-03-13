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
      | CorrelationPlotMessage of CorrelationPlot.Action
      | Clear
      | OnKeyUp of Keys
      | OnKeyDown of Keys

    let defaultZoomFactor = 1.0
    let defaultOffset     = V2d.OO

    let initial : CorrelationPlotModel  = 
      {
        correlationPlot     = CorrelationPlot.initial
        semanticApp         = SemanticApp.getInitialWithSamples 
      }

    let update (annoApp : AnnotationApp)
               (model : CorrelationPlotModel)
               (action : Action) = 
               
      match action with
        | Clear -> 
          {model with correlationPlot =
                        CorrelationPlot.update annoApp model.correlationPlot CorrelationPlot.Clear
          }
        | CorrelationPlotMessage lm -> 
          {model with correlationPlot = CorrelationPlot.update annoApp model.correlationPlot lm}
        | OnKeyUp k ->
          let a = (CorrelationPlot.KeyboardMessage (Keyboard.Action.KeyUp k))
          {model with correlationPlot =
                        CorrelationPlot.update annoApp model.correlationPlot a
          }
              
        | OnKeyDown k ->
          let a = (CorrelationPlot.KeyboardMessage (Keyboard.Action.KeyDown k))
          {model with correlationPlot =
                        CorrelationPlot.update annoApp model.correlationPlot a
          }




    //let update' (annos : hmap<AnnotationId, Annotation>) (model : CorrelationPlotModel) (action : Action) = 
    //  update { annotations = annos; selectedAnnotation = None } model action

    // let viewSvg (annoApp : MAnnotationApp) (model : MCorrelationPlotModel) =
    //let viewSvg (annoApp  : amap<AnnotationId, MAnnotation>) (model : MCorrelationPlotModel) =
     

    let update' (annos : hmap<AnnotationId, Annotation>) 
                (model : CorrelationPlotModel) 
                (action : Action) = 
      update 
        { 
          annotations = annos; 
          selectedAnnotation = None 
          keyboard = AnnotationApp.keyboard // ???
        } model action

    let viewSvg (annoApp : amap<AnnotationId, MAnnotation>) (model : MCorrelationPlotModel) =

      require (GUI.CSS.myCss) (
        body [attribute "overflow-x" "auto";
              attribute "overflow-y" "auto"; 
              (onMouseDown (fun b p -> CorrelationPlot.SvgCameraMessage 
                                          (SvgCamera.Action.MouseDown (b,p)))) 
              (onMouseUp   (fun b p -> CorrelationPlot.SvgCameraMessage 
                                           (SvgCamera.Action.MouseUp (b,p))))
              (onMouseMove (fun p   -> 
                 CorrelationPlot.SvgCameraMessage 
                    (SvgCamera.Action.MouseMove (V2d p))))
              //onLayoutChanged UpdateConfig
             ] 
             [
                div [attribute "overflow-x" "auto";attribute "overflow-y" "auto"] [
                        //menu
                        CorrelationPlot.viewSvg annoApp model.correlationPlot
                        ]
             ]
      ) |> UI.map Action.CorrelationPlotMessage
        


    // Log Debug View




    let view  (model    : MCorrelationPlotModel) 
              (annoApp  : amap<AnnotationId, MAnnotation>)
              (semApp   : MSemanticApp) =

      let domnode =
        let domNode = (CorrelationPlot.listView model.correlationPlot semApp annoApp) 
                        |> UI.map Action.CorrelationPlotMessage
        let attsBody =
            [
              style "overflow: auto"
            ]
        let attsDiv = 
          AttributeMap.ofList [clazz "ui inverted segment"]
        require (GUI.CSS.myCss) (
          body attsBody [
            div [] [
              // menu |> ui.map correlationplotmessage
              Incremental.div attsDiv (AList.single domNode)
                              
            ]
          ]
        )

      domnode        

    let threads (model : CorrelationPlotModel) =
      CorrelationPlot.threads model.correlationPlot
        
    let app (annoApp : amap<AnnotationId, MAnnotation>) (mAnnoApp : AnnotationApp) : App<CorrelationPlotModel,MCorrelationPlotModel,Action> =
          {
              unpersist = Unpersist.instance
              threads = threads
              initial = initial
              update = update mAnnoApp
              view = viewSvg annoApp
          }

    let start annoApp mAnnoApp = App.start (app annoApp mAnnoApp)

