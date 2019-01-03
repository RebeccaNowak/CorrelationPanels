namespace Svgplus.DA
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.CA
open Svgplus


  [<DomainType>]
  type DiagramApp = {
    rectangleStacks : hmap<RectangleStackId, RectangleStack>
    connectionApp   : ConnectionApp
  }

