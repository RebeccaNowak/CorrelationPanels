namespace Test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Svgplus.RoseDiagramModel

type Primitive =
    | Box
    | Sphere


[<DomainType>]
type TestModel =
    {
        currentModel    : Primitive
        cameraState     : CameraControllerState
        svgButton       : Svgplus.Button
        roseDiagram     : RoseDiagram
        diagramApp      : Svgplus.DA.Diagram
    }