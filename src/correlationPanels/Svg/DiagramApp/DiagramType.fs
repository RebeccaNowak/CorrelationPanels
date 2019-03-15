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
open Svgplus.AxesTypes
open SimpleTypes
open Svgplus.DiagramItemType


  [<DomainType>]
  type Diagram = {
    //rectangleStacks   : hmap<RectangleStackTypes.RectangleStackId, RectangleStackTypes.RectangleStack>
    items             : hmap<DiagramItemId, DiagramItem>
    //order             : plist<RectangleStackTypes.RectangleStackId>
    order             : plist<DiagramItemId>
    connectionApp     : ConnectionApp
    itemGap         : float
    marginLeft        : float
    marginTop         : float
    selectedRectangle : option<RectangleIdentification>
    yToData           : float -> float        
    dataToY           : float -> float        
    dataRange         : Rangef
  }

  

