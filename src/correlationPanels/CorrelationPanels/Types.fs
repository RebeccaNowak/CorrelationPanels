namespace CorrelationDrawing.Types

open Aardvark.Base.Incremental
open Aardvark.UI



[<DomainType>]
type Style = {
    color     : ColorInput
    thickness : NumericInput
 } 



    
type State = New | Edit | Display
type Projection   = Linear = 0 | Viewpoint = 1 | Sky = 2

type LogNodeType             = Hierarchical | HierarchicalLeaf | Metric | Angular | PosInfinity | NegInfinity | Infinity | Empty
type LogNodeBoxType          = SimpleBox | TwoColorBox | FancyBox

type NodeLevel = {
  level : int
} with 
    member this.weight =
      ((8.0 - (float this.level)) * 0.3)

module NodeLevel =
  let LEVEL_MAX = 8
  let INVALID = {level = -1}

  let init integer : NodeLevel = 
    match integer with
      | i when i < 0         -> {level = i}
      | i when i > LEVEL_MAX -> {level = LEVEL_MAX}
      | i                    -> {level = i}

  let isInvalid nodeLevel =
    nodeLevel = INVALID
  
  let availableLevels =
    alist {
      for i in 0..LEVEL_MAX do
        yield {level = i}
    }