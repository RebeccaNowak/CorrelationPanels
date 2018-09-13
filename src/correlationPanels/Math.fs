namespace CorrelationDrawing

module Math = 
  let radToDeg =  180.0 / System.Math.PI
  let degToRad = System.Math.PI / 180.0

  let twoPi = System.Math.PI * 2.0
  let halfPi = System.Math.PI / 2.0
  let quarterPi = System.Math.PI / 4.0
  let eigthPi = System.Math.PI / 8.0


  type Angle = {
    radians : float
  } with
      static member (*)  (this : Angle, other : Angle) : Angle =
        {radians = (this.radians * other.radians) % twoPi}
      static member (*)  (this : Angle, other : float) : Angle =
        {radians = (this.radians * other) % twoPi}
      member this.degrees =
        this.radians * 57.29577951


  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Angle =
    let twoPi = {radians = twoPi}
    let halfPi = {radians = halfPi}
    let quarterPi = {radians = quarterPi}
    let eigthPi = {radians = eigthPi}

    let init radians =
      {radians = radians % twoPi.radians}