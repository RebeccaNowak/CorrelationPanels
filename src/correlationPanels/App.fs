namespace Test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives


type Message =
    | ToggleModel
    | ButtonMessage   of Svgplus.Button.Action
    | CameraMessage   of CameraControllerMessage
    | RDMessage       of Svgplus.RoseDiagram.Action
    | RectMessage     of Svgplus.Rectangle.Action

module App =
    open Svgplus.Mutable
    
    let initial : TestModel = {
      currentModel = Primitive.Box
      cameraState  = CameraController.initial
      svgButton    = {Svgplus.Button.init with pos = V2d (10.0)}
      roseDiagram  = {Svgplus.RoseDiagram.init with centre = V2d (100.0)}
      rectangle    = {Svgplus.Rectangle.init with pos = V2d (200.00)}
    }

    let update (model : TestModel) (msg : Message) =
        match msg with
            | ToggleModel -> 
                match model.currentModel with
                    | Box -> { model with currentModel = Sphere }
                    | Sphere -> { model with currentModel = Primitive.Box }

            | CameraMessage msg ->
                { model with cameraState = CameraController.update model.cameraState msg }

            | ButtonMessage msg -> 
              match msg with
                | Svgplus.Button.Action.OnLeftClick -> 
                  {model with svgButton = (Svgplus.Button.update model.svgButton msg)
                              roseDiagram = {model.roseDiagram with 
                                                centre = model.roseDiagram.centre + 50.0}
                  }
                | _ -> {model with svgButton = (Svgplus.Button.update model.svgButton msg)}
            
            | RDMessage msg -> {model with roseDiagram = Svgplus.RoseDiagram.update model.roseDiagram msg}
            | RectMessage msg -> {model with rectangle = Svgplus.Rectangle.update model.rectangle msg}

    let view (model : MTestModel) =
        let svgAtts = 
          [
            clazz "svgRoot"
            style "border: 1px solid black"
            //attribute "viewBox" "0 0 600 400"
            attribute "preserveAspectRatio" "xMinYMin meet"
            attribute "height" "100%"
            attribute "width" "100%"
          ] |> AttributeMap.ofList

        let button =
          alist {
            yield (Svgplus.Button.view model.svgButton) |> UI.map ButtonMessage
          } 
        let rose = ((Svgplus.RoseDiagram.view model.roseDiagram) 
                      |> AList.map (UI.map RDMessage))

        let rect = (Svgplus.Rectangle.view model.rectangle)
                      |> AList.map (UI.map RectMessage)

        let content = button 
                        |> AList.append rose
                        |> AList.append rect

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