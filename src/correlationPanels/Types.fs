namespace SimpleTypes

open Aardvark.Base
open Aardvark.Base.Incremental

type Orientation  = Horizontal | Vertical

type Alignment = LEFT | RIGHT

type Direction =
| Up
| Down
| Left
| Right
with
  member this.toString =
    match this with
      | Up    -> "up"
      | Down  -> "down"
      | Left  -> "left"
      | Right -> "right"

type Size =
  | Mini
  | Tiny
  | Small
  | Normal
  | Large
  | Big
  | Huge
  | Massive
  with
    member this.toString =
      match this with
        | Mini      -> "mini"
        | Tiny      -> "tiny"
        | Small     -> "small"
        | Normal    -> ""
        | Large     -> "large"
        | Big       -> "big"
        | Huge      -> "huge"
        | Massive   -> "massive"

type Size2D = {
  width  : float
  height : float
} with 
    member this.X = this.width
    member this.Y = this.height

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Size2D =
  let init = {width = 0.0; height = 0.0}

type SvgWeight = {
  value : float
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SvgWeight =
  let init = {value = 2.0}


type BorderColors = {
  upper : C4b
  lower : C4b
}



module Math = 
  open Aardvark.UI.Static

  let radToDeg = 180.0 / System.Math.PI
  let degToRad = System.Math.PI / 180.0

  let twoPi = System.Math.PI * 2.0
  let halfPi = System.Math.PI / 2.0
  let quarterPi = System.Math.PI / 4.0
  let eigthPi = System.Math.PI / 8.0
  let sixteenthPi = System.Math.PI / 16.0

  ///////////////////////////////////////////////////////////



  ///////////////////////////////////////////////////////////

  type Angle = {
    radians : float
  } with
      static member (*)  (this : Angle, other : Angle) : Angle =
        {radians = (this.radians * other.radians) % twoPi}
      static member (*)  (this : Angle, other : float) : Angle =
        {radians = (this.radians * other) % twoPi}
      static member (+)  (this : Angle, other : Angle) : Angle =
        {radians = (this.radians + other.radians) % twoPi}
      static member (+)  (this : Angle, other : float) : Angle =
        {radians = (this.radians + other) % twoPi}
      member this.degrees =
        this.radians * 57.29577951


  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Angle =
    let twoPi = {radians = twoPi}
    let halfPi = {radians = halfPi}
    let quarterPi = {radians = quarterPi}
    let eigthPi = {radians = eigthPi}
    let sixteenthPi = {radians = sixteenthPi}

    let init radians =
      {radians = radians % twoPi.radians}

type Rangef = { //TODO move to math
  min     : float
  max     : float
} with 
    member this.range   = this.max - this.min
    member this.mapRange (other : Rangef) = 
      fun (x : float) ->
        x *  (other.range / this.range)
    member this.outer (other : Rangef) : Rangef =
      {
        min = min this.min other.min
        max = max this.max other.max
      }
    member this.average =
      (this.min + this.max) * 0.5
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rangef =
  let init : Rangef = {
      min     = 0.0
      max     = 0.0
    }

  let calcRange (r : Rangef) =
    r.max - r.min

  let calcRangeNoInf (r : Rangef) =
    match r.max with 
      | a when a = Operators.infinity -> r.min * 1.01 //TODO HACK
      | _ -> r.max - r.min
