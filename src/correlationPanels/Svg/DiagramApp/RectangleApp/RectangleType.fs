namespace Svgplus.RectangleType

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open Svgplus
  open Svgplus.Base
  open SimpleTypes
  open UIPlus

  type RectangleId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module RectangleId = 
    let invalid = {id = ""}
    let newId () : RectangleId  = 
      let id = System.Guid.NewGuid ()
      {id = id.ToString () }

  //[<DomainType>]
  //type RectangleBorder = {
  //  [<NonIncremental>]
  //  id             : RectangleId
    
  //  colour         : C4b
  //}




  [<DomainType>]
  type Rectangle = {
    [<NonIncremental>]
    id             : RectangleId

    needsLayoutingX : bool
    needsLayoutingY : bool

    drawLabel     : bool
    label         : UIPlus.TextInput
    pos           : V2d
    dim           : Size2D
    fixedWidth    : option<float>

    colour        : Aardvark.UI.ColorInput
    lowerBorderColour  : C4b
    upperBorderColour : C4b
    overwriteColour : option<C4b>

    isToggled     : bool
    colChange     : V3i
    isHovering    : bool
    dottedBorder  : bool
    draw          : bool

    drawButtons     : bool
    northWestButton : Button
    northEastButton : Button
    southWestButton : Button
    southEastButton : Button
    svgYAxisLabel   : Svgplus.TextType.Text
  } with 
    member this.maxWidth =
      let labelWidth =
        match this.drawLabel with
        | true  -> Text.preferredWidth this.svgYAxisLabel
        | false -> 0.0
      match this.fixedWidth with
      | Some w  -> w + labelWidth
      | None    -> this.dim.width + labelWidth



