namespace CorrelationDrawing

//  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//  module CorrelationPlotApp =
//    open Aardvark.Base.Incremental
//    open Aardvark.Base
//    open Aardvark.Application
//    open Aardvark.UI
//    open UIPlus
//    open System
//    open Svgplus

//    type Action =
//      | CorrelationPlotMessage of CorrelationPlot.Action
//      | Clear


//    let defaultZoomFactor = 1.0
//    let defaultOffset     = V2d.OO

//    let initial : CorrelationPlotModel  = 
//      {
//        correlationPlot     = CorrelationPlot.initial
//        semanticApp         = SemanticApp.getInitialWithSamples 
//      }

//    let update (annoApp : AnnotationApp)
//               (model : CorrelationPlotModel)
//               (action : Action) = 
               
//      match action with
//        | Clear -> 
//          {model with correlationPlot =
//                        CorrelationPlot.update annoApp model.correlationPlot CorrelationPlot.Clear
//          }
//        | CorrelationPlotMessage lm -> 
//          {model with correlationPlot = CorrelationPlot.update annoApp model.correlationPlot lm}





//    //let update' (annos : hmap<AnnotationId, Annotation>) (model : CorrelationPlotModel) (action : Action) = 
//    //  update { annotations = annos; selectedAnnotation = None } model action

//    // let viewSvg (annoApp : MAnnotationApp) (model : MCorrelationPlotModel) =
//    //let viewSvg (annoApp  : amap<AnnotationId, MAnnotation>) (model : MCorrelationPlotModel) =
     

//    //let update' (annos : hmap<AnnotationId, Annotation>) 
//    //            (model : CorrelationPlotModel) 
//    //            (action : Action) = 
//    //  update 
//    //    { 
//    //      annotations = annos; 
//    //      selectedAnnotation = None 
//    //      keyboard = AnnotationApp.keyboard // ???
//    //    } model action

//    let viewSvg (annoApp : amap<AnnotationId, MAnnotation>) (model : MCorrelationPlotModel) =

      
        


//    // Log Debug View




   

//    //let threads (model : CorrelationPlotModel) =
//    //  CorrelationPlot.threads model.correlationPlot
        
//    //let app (annoApp : amap<AnnotationId, MAnnotation>) (mAnnoApp : AnnotationApp) : App<CorrelationPlotModel,MCorrelationPlotModel,Action> =
//    //      {
//    //          unpersist = Unpersist.instance
//    //          threads = threads
//    //          initial = initial
//    //          update = update mAnnoApp
//    //          view = viewSvg annoApp
//    //      }

//    //let start annoApp mAnnoApp = App.start (app annoApp mAnnoApp)

