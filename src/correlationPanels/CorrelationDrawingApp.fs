namespace CorrelationDrawing


open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering.Text


//module Serialization =
//    open MBrace.FsPickler
//    open System.IO

//    let binarySerializer = FsPickler.CreateBinarySerializer()


    
//    let save (model : CorrelationAppModel) path = 
//        let arr = binarySerializer.Pickle model
//        //let info = System.IO.Directory.CreateDirectory "./saved"
//        //let success = info.Exists
//        File.WriteAllBytes("./saved/model", arr);
//        let success = File.Exists path
//        success

//    let load (path : CorrelationAppModel) = 
//        let arr = File.ReadAllBytes("./saved/model");
//        let app = binarySerializer.UnPickle arr
//        app

//    let writeToFile path (contents : string) =
//        System.IO.File.WriteAllText(path, contents)

//module CorrelationDrawingApp = 
//    open Newtonsoft.Json
            
//    type Action =
//        | CameraMessage             of CameraController.Message
//        | DrawingMessage            of CorrelationDrawing.Action
//        | AnnotationMessage         of CorrelationDrawing.Action
//        | Save
//        | Load

                       
//    let update (model             : CorrelationAppModel)  
//               (semanticApp       : SemanticApp)
//               (act               : Action) =
//        match act, model.drawing.draw with   
//            | DrawingMessage m, _ ->
//                { model with drawing = CorrelationDrawing.update model.drawing semanticApp m}      
////            | DrawingSemanticMessage m, _ ->
////                {model with drawing = CorrelationDrawing.update model.drawing semanticApp m}          
//            | AnnotationMessage m, _ ->
//                {model with drawing = CorrelationDrawing.update model.drawing semanticApp m}          
////            | ToggleSelectMessage m, _ ->
////                {model with drawing = CorrelationDrawing.update model.drawing semanticApp m}     

//                             //camera   = CameraController.update model.camera (CameraController.Message.KeyUp k)}
//            | Save , _ -> (Serialization.save model "./savedModel") |> ignore
//                          model
//            | Load , _ -> (Serialization.load model "./savedModel") |> ignore
//                          model
//            | _ -> model
                       
//    let myCss = [
//                  { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
//                  { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
//                  { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
//                ]
    
//    //let view (model         : MCorrelationAppModel) 
//    //         (camera        : MCameraControllerState)
//    //         (semanticApp   : MSemanticApp) 
//    //         (annoSg        : ISg<CorrelationDrawing.Action>)
//    //         (additionalSg  : ISg) =


                 


            
//    //        //  div [clazz "scrolling content"; style "width:30%; height: 100%; float: right; overflow-y: scroll"] [
//    //              //CorrelationDrawing.UI.viewAnnotations model.drawing semanticApp |> UI.map AnnotationMessage   
//    //              //CorrelationDrawing.SemanticApp.viewSemantics model. |> UI.map DrawingSemanticMessage

//    //        //]


////    let view' (model : MCorrelationAppModel) =
////      view model SemanticApp.initial

//    let initial : CorrelationAppModel =
//        {
//           // camera    = { ArcBallController.initial with view = CameraView.lookAt (23.0 * V3d.OIO) V3d.Zero Mars.Terrain.up}
//            rendering = RenderingPars.initial
//            drawing   = CorrelationDrawing.initial 
//        }






////    let app : App<CorrelationAppModel,MCorrelationAppModel,Action> =
////        {
////            unpersist = Unpersist.instance
////            threads = fun model -> ArcBallController.threads model.camera |> ThreadPool.map CameraMessage
////            initial = initial
////            update = update
////            view = view'
////        }
////
////    let start () = App.start app

