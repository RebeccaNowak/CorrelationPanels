namespace Svgplus.RectangleStackTypes

open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus
open Svgplus.RectangleType

  type RectangleStackId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module RectangleStackId = 
    let invalid = {id = ""}
    let newId () : RectangleStackId  = 
      let id = System.Guid.NewGuid ()
      {id = id.ToString () }

  [<DomainType>]
  type RectangleStack = {
    [<NonIncremental>]
    id              : RectangleStackId

    needsLayouting  : bool

    rectangles      : hmap<RectangleId, Rectangle>
    header          : HeaderType.Header
    order           : plist<RectangleId>
    pos             : V2d
    yAxis           : Svgplus.AxesTypes.AxisApp
    yAxisMargin     : float
  } with
    member this.maxWidth = 
      let maxRectangleWidth =
        this.rectangles 
          |> DS.HMap.values
          |> List.map (fun r -> r.maxWidth)
          |> List.max
      maxRectangleWidth