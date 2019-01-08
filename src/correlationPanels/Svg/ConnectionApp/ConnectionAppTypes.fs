namespace Svgplus.CA
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

  type ConnectionId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module ConnectionId = 
    let invalid = {id = ""}
    let newId () : ConnectionId  = 
      {id = System.Guid.NewGuid().ToString ()}

  [<DomainType>]
  type Connection = {
    [<NonIncremental>]
    id          : ConnectionId
    bFrom       : IMod<V2d>
    bTo         : IMod<V2d>
    dotted      : bool
    colour      : C4b
    weight      : float
    dashLength  : float
    dashDist    : float
    //bFrom     : Lens<'a,V2d>
    //bTo       : Lens<'a,V2d>
    //mFrom     : 'ma -> IMod<V2d>
    //mTo       : 'ma -> IMod<V2d>
  }


  [<DomainType>]
  type ConnectionApp = {
    connections   : hmap<ConnectionId, Connection>
    connecting    : option<IMod<V2d>>
    mouseposition : V2i
  }
