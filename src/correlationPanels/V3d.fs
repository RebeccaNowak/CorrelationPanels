namespace CorrelationDrawing

open Aardvark.Base

  module V3d = 
    let inline elevation (v : V3d) =
      v.Length //TODO PRO3D elevation

    let inline isElevationBetween (v : V3d) (lower : V3d) (upper : V3d) =
       (elevation lower < elevation v) && (elevation upper > elevation v)