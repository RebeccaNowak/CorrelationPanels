namespace Svgplus.ArrowType

  open Aardvark.Base
  open Aardvark.Base.Incremental
  open SimpleTypes
  
  [<DomainType>]
  type Arrow = {
    direction     : Direction
    length        : float
    colour        : C4b
  }


