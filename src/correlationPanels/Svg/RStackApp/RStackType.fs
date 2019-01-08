namespace Svgplus.RS

open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

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
    header        : HeaderType.Header
    order         : plist<RectangleId>
    pos           : V2d

  }