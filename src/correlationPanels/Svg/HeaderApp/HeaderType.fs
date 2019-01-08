namespace Svgplus.HeaderType
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open SimpleTypes
  open Svgplus

  [<DomainType>]
  type Header = {
    pos         : V2d
    dim         : Size2D
    label       : string
    leftButton  : Button
    rightButton : Button
    
  }

