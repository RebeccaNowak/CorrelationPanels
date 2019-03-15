namespace Svgplus.DiagramItemType
  
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




  type DiagramItemId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module DiagramItemId = 
    let invalid = {id = ""}
    let newId () : DiagramItemId  = 
      let id = System.Guid.NewGuid ()
      {id = id.ToString () }

  type RectangleIdentification = {
    rid           : RectangleId
    stackid       : RectangleStackId
    diagramItemId : DiagramItemId
  }

  [<DomainType>]
  type DiagramItem = {
    id                : DiagramItemId
    
    pos               : V2d
    header            : HeaderType.Header

    rectangleStacks   : hmap<RectangleStackTypes.RectangleStackId, RectangleStackTypes.RectangleStack>
    order             : plist<RectangleStackTypes.RectangleStackId>
    rightGaps         : hmap<RectangleStackTypes.RectangleStackId, float>
    
    marginLeft        : float
    marginRight       : float
    marginTop         : float
  } with
    member this.maxWidth =
      let widths = 
        this.rectangleStacks
          |> HMap.values
          |> Seq.map (fun s -> s.maxWidth)
      let max =
        match Seq.isEmpty widths with
        | true -> 0.0
        | false -> Seq.max widths
      max
