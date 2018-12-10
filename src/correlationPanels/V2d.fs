namespace CorrelationDrawing

open Aardvark.Base

  module V2d = 
    let OY (y : float) : V2d = 
      new V2d (0.0, y)

    let OX (x : float) : V2d = 
      new V2d (x, 0.0)

    let withX (v : V2d) (newX : float) : V2d =
      new V2d (newX, v.Y)

    let withY (v : V2d) (newY : float) : V2d =
      new V2d (v.X, newY)