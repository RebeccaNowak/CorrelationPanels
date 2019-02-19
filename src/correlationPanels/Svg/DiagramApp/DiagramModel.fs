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

  type RectangleIdentification = {
    rid         : RectangleId
    stackid     : RectangleStackId
  }


  [<DomainType>]
  type Diagram = {
    rectangleStacks   : hmap<RectangleStackTypes.RectangleStackId, RectangleStackTypes.RectangleStack>
    order             : plist<RectangleStackTypes.RectangleStackId>
    connectionApp     : ConnectionApp
    rstackGap         : float
    marginLeft        : float
    marginTop         : float
    selectedRectangle : option<RectangleIdentification>
  }

  

