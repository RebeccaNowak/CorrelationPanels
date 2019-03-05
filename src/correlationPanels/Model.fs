namespace Test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Svgplus.RoseDiagramModel
open Svgplus.ArrowType

type Primitive =
    | Box
    | Sphere


[<DomainType>]
type TestModel =
    {
        currentModel    : Primitive
        cameraState     : CameraControllerState
        svgButton       : Svgplus.Button
        arrow           : Arrow
        roseDiagram     : RoseDiagram
        diagramApp      : Svgplus.DA.Diagram
    }