namespace Svgplus.DA
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.CA
open Svgplus
open Svgplus.HeaderType



  [<DomainType>]
  type DiagramApp = {
    rectangleStacks : hmap<RS.RectangleStackId, RS.RectangleStack>
    //headers         : hmap<RS.RectangleStackId, Header>
    order           : plist<RS.RectangleStackId>
    connectionApp   : ConnectionApp
  }

