namespace Svgplus.CA
  
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

    type Connection = {
      bFrom     : IMod<V2d>
      bTo       : IMod<V2d>
      //bFrom     : Lens<'a,V2d>
      //bTo       : Lens<'a,V2d>
      //mFrom     : 'ma -> IMod<V2d>
      //mTo       : 'ma -> IMod<V2d>
    }

    module Connection =
      let contains (model : Connection) (v : IMod<V2d>) =
        (model.bTo == v) || (model.bFrom == v)
        



    [<DomainType>]
    type ConnectionApp = {
      connections   : plist<Connection>
      connecting    : option<IMod<V2d>>
      mouseposition : V2i
    }
