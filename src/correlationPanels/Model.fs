namespace Test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Svgplus

type Primitive =
    | Box
    | Sphere


[<DomainType>]
type TestModel =
    {
        currentModel    : Primitive
        cameraState     : CameraControllerState
        svgButton       : Svgplus.Button
        roseDiagram     : Svgplus.RoseDiagram
        diagramApp      : Svgplus.DiagramApp
    }