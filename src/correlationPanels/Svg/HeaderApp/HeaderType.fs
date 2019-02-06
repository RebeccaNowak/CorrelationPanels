namespace Svgplus.HeaderType
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open SimpleTypes
  open Svgplus
  open UIPlus

  [<DomainType>]
  type Header = {
    pos         : V2d
    dim         : Size2D
    label       : TextInput
    leftButton  : Button
    rightButton : Button
    
  }

