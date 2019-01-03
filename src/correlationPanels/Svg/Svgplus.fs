namespace Svgplus

open Aardvark.Base
open Aardvark.Base.Incremental
open SimpleTypes

  type Bin = {
    number : int
    value  : int
    colour : C4b
  }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Bin =
    let init number value colour =
      {
        number = number
        value  = value
        colour = colour
      }

  type Bin16x6 = {
    angularBin : int
    radius     : float
    colour     : C4b
  }

  type RoseDiagramId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module RoseDiagramId = 
    let invalid = {id = ""}
    let newId () : RoseDiagramId  = 
      {id = System.Guid.NewGuid().ToString()}

  [<DomainType>]
  type RoseDiagram = {
    [<NonIncremental>]
    id            : RoseDiagramId

    centre        : V2d
    outerRadius   : float
    innerRadius   : float
    colour        : list<C4b>
    nrCircles     : int
    weight        : float
    countPerBin   : plist<Bin>
  }

  type ButtonId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module ButtonId = 
    let invalid = {id = ""}
    let newId () : ButtonId  = 
      {id = System.Guid.NewGuid().ToString ()}

  type ConnectionStatus =
    | NoConnection
    | InProgress
    | Connected


  [<DomainType>]
  type Button = {
    [<NonIncremental>]
    id              : ButtonId

    pos             : V2d
    radius          : float
    rHoverChange    : float
    stroke          : float
    color           : C4b
    colChange       : V3i
    fill            : bool
    isToggled       : bool
    isHovering      : bool
    transitionSec   : float
  }

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
    id            : RectangleId

    pos           : V2d
    dim           : Size2D
    colour        : Aardvark.UI.ColorInput
    borderColour  : C4b
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

  [<DomainType>]
  type Header = {
    label       : string
  }

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
    id            : RectangleStackId
    rectangles    : hmap<RectangleId, Rectangle>
    order         : plist<RectangleId>
    pos           : V2d

  }



