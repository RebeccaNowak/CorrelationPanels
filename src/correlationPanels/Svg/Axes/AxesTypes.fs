namespace Svgplus.AxesTypes

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open SimpleTypes
  open Svgplus


  [<DomainType>]
  type AxisApp = {
    positionTop        : V2d
    weight             : float
    step               : float
    label              : string
    defaultGranularity : float
    yMapping           : float -> float
    nativeRange        : Rangef
    fontSize           : FontSize
    draw               : bool
  }