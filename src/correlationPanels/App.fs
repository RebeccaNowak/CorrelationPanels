namespace Test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Svgplus
open Svgplus.CA
open Svgplus.DA



type Action =
    | ToggleModel
    | ButtonMessage   of Button.Action
    | CameraMessage   of CameraControllerMessage
    | RDMessage       of RoseDiagram.Action
    | DiagramMessage  of DiagramApp.Action
    | MouseMove       of V2i

module App =
    open Svgplus.Mutable
    open Svgplus
    open Aardvark.Base.IL.Frontend
    
    let initial : TestModel =
      {
        currentModel  = Primitive.Box
        cameraState   = CameraController.initial
        svgButton     = {Button.init with pos = V2d (10.0)}
        roseDiagram   = {RoseDiagram.init with centre = V2d (100.0)}
        diagramApp    = DiagramApp.init
      }

    let updateConnections model msg =
      ConnectionApp.update model.connectionApp 
                           (ConnectionApp.Action.ButtonMessage msg)
      

    let update (model : TestModel) (msg : Action) =
        match msg with
            | ToggleModel -> 
                match model.currentModel with
                    | Box -> { model with currentModel = Sphere }
                    | Sphere -> { model with currentModel = Primitive.Box }

            | CameraMessage msg ->
                { model with cameraState = CameraController.update model.cameraState msg }

            | ButtonMessage msg -> 
              match msg with
                | Svgplus.Button.Action.OnLeftClick p -> 
                  {model with svgButton = (Svgplus.Button.update model.svgButton msg)}
                | _ -> {model with svgButton = (Svgplus.Button.update model.svgButton msg)}
            
            | RDMessage msg -> {model with roseDiagram = Svgplus.RoseDiagram.update model.roseDiagram msg}
            | DiagramMessage msg -> 
              {model with diagramApp = DiagramApp.update model.diagramApp msg}
              
            | MouseMove p -> 
              {model with diagramApp = 
                            DiagramApp.update model.diagramApp 
                                              (DiagramApp.Action.MouseMove p)
              }
    let view (model : MTestModel) =
        let svgAtts = 
          [
            clazz "svgRoot"
            style "border: 1px solid black"
            //attribute "viewBox" "0 0 600 400"
            attribute "preserveAspectRatio" "xMinYMin meet"
            attribute "height" "100%"
            attribute "width" "100%"
            (onMouseMove (fun p -> MouseMove p))
          ] |> AttributeMap.ofList

        let button =
          alist {
            yield (Svgplus.Button.view model.svgButton) |> UI.map ButtonMessage
          } 
        let rose = ((Svgplus.RoseDiagram.view model.roseDiagram) 
                      |> AList.map (UI.map RDMessage))
      

        let content = 
          (DiagramApp.view model.diagramApp) 
            |> AList.map (fun d -> d |> UI.map Action.DiagramMessage)

        require (GUI.CSS.myCss) (
          body [] [
              // CameraController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
                  
              //div [style "position: fixed; left: 20px; top: 20px"] [
              //    button [onClick (fun _ -> ToggleModel)] [text "Toggle Model"]
              //]

              Incremental.Svg.svg svgAtts content
          ]
        )

    let app =
        {
            initial = initial
            update = update
            view = view
            threads = fun _ -> ThreadPool.empty
            unpersist = Unpersist.instance
        }