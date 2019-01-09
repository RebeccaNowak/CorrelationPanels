namespace Svgplus.DA
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.CA
open Svgplus
open Svgplus.HeaderType



  [<DomainType>]
  type DiagramApp = {
    rectangleStacks : hmap<RS.RectangleStackId, RS.RectangleStack>
    order           : plist<RS.RectangleStackId>
    connectionApp   : ConnectionApp
    rstackGap       : float
    marginLeft      : float
    marginTop       : float

  }

