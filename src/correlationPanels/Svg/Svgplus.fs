namespace Svgplus

open Aardvark.Base
open Aardvark.Base.Incremental

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





[<DomainType>]
type RoseDiagram = {
  centre        : V2d
  outerRadius   : float
  innerRadius   : float
  colour        : list<C4b>
  nrCircles     : int
  weight        : float
  countPerBin   : plist<Bin>
}





[<DomainType>]
type Button = {
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