namespace Svgplus.DA
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

open Svgplus.RectangleType
open Svgplus.RectangleStackTypes
open Svgplus.CA
open Svgplus
open Svgplus.HeaderType
open UIPlus


  [<DomainType>]
  type DiagramApp = {
    rectangleStacks   : hmap<RectangleStackTypes.RectangleStackId, RectangleStackTypes.RectangleStack>
    order             : plist<RectangleStackTypes.RectangleStackId>
    connectionApp     : ConnectionApp
    rstackGap         : float
    marginLeft        : float
    marginTop         : float
    selectedRectangle : option<RectangleId * RectangleStackId>
  }

  

