namespace CorrelationDrawing.CorrelationTypes
open Aardvark.Base.Incremental
open CorrelationDrawing.LogNodeTypes

[<DomainType>]
type Correlation = {
  fromBorder    : Border
  toBorder      : Border
}