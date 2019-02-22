namespace Svgplus.CameraType

open Aardvark.Base
open Aardvark.Base.Incremental

type Zoom = 
  {
    factor : float
  } with
      static member (+) (a,b) : Zoom =
        let newZoom = (a.factor + b.factor)
        let checkedZoom =
          match newZoom with
            | a when a <= 0.1 -> 0.1
            | b when b >= 10.0 -> 10.0
            | _ -> newZoom
        {factor = checkedZoom}
      static member (+) (a : Zoom, b : float) : Zoom =
        let newZoom = (a.factor + b)
        let checkedZoom =
          match newZoom with
            | a when a <= 0.1 -> 0.1
            | b when b >= 10.0 -> 10.0
            | _ -> newZoom
        {factor = checkedZoom}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Zoom =
  let defaultZoom = {factor = 1.0}

  let init d : Zoom = 
    let z =
      match d with
        | a when a <= 0.1 -> 0.1
        | b when b >= 10.0 -> 10.0
        | _ -> 1.0
    {factor = z}

  let add (z : Zoom) (d : float) : Zoom =
    init (z.factor + d)

type FontSize = {
  fontSize : int 
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FontSize =
  let defaultSize = {fontSize = 12}
  let min = 10
  let max = 30

  let init d : FontSize = 
    let s =
      match d with
        | a when a <= min -> min
        | b when b >= max -> max
        | _ -> d
    {fontSize = s}

  let add (s : FontSize) (d : int) : FontSize =
    init (s.fontSize + d)
    

[<DomainType>]
type SvgCamera = {
  zoomFactorX          : Zoom
  zoomFactorY          : Zoom
  dragging             : bool
  zoomingX             : bool
  zoomingY             : bool
  lastMousePos         : V2d
  offset               : V2d
  fontSize             : FontSize
  transformedMousePos  : V2d
}