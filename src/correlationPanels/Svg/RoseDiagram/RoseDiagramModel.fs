namespace Svgplus.RoseDiagramModel
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Svgplus

  type RoseDiagramId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module RoseDiagramId = 
    let invalid = {id = ""}
    let newId () : RoseDiagramId  = 
      {id = System.Guid.NewGuid().ToString()}

  [<DomainType>]
  type RoseDiagram = {
    [<NonIncremental>]
    id            : RoseDiagramId

    centre        : V2d
    outerRadius   : float
    innerRadius   : float
    colour        : list<C4b>
    nrCircles     : int
    weight        : float
    countPerBin   : plist<Bin>
  }