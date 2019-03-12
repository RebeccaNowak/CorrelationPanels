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

  [<DomainType>]
  type Rectangle = {
    [<NonIncremental>]
    id             : RectangleId

    needsLayoutingX : bool
    needsLayoutingY : bool

    label         : UIPlus.TextInput
    pos           : V2d
    dim           : Size2D
    colour        : Aardvark.UI.ColorInput
    borderColour  : C4b
    overwriteColour : option<C4b>
    isToggled     : bool
    colChange     : V3i
    isHovering    : bool
    dottedBorder  : bool
    draw          : bool

    northWestButton : Button
    northEastButton : Button
    southWestButton : Button
    southEastButton : Button
  }


