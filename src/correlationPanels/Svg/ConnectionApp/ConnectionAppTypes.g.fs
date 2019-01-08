namespace Svgplus.CA

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.CA

[<AutoOpen>]
module Mutable =

    
    
    type MConnection(__initial : Svgplus.CA.Connection) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.CA.Connection> = Aardvark.Base.Incremental.EqModRef<Svgplus.CA.Connection>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.CA.Connection>
        let _bFrom = ResetMod.Create(__initial.bFrom)
        let _bTo = ResetMod.Create(__initial.bTo)
        let _dotted = ResetMod.Create(__initial.dotted)
        let _colour = ResetMod.Create(__initial.colour)
        let _weight = ResetMod.Create(__initial.weight)
        
        member x.id = __current.Value.id
        member x.bFrom = _bFrom :> IMod<_>
        member x.bTo = _bTo :> IMod<_>
        member x.dotted = _dotted :> IMod<_>
        member x.colour = _colour :> IMod<_>
        member x.weight = _weight :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.CA.Connection) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_bFrom,v.bFrom)
                ResetMod.Update(_bTo,v.bTo)
                ResetMod.Update(_dotted,v.dotted)
                ResetMod.Update(_colour,v.colour)
                ResetMod.Update(_weight,v.weight)
                
        
        static member Create(__initial : Svgplus.CA.Connection) : MConnection = MConnection(__initial)
        static member Update(m : MConnection, v : Svgplus.CA.Connection) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.CA.Connection> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Connection =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.CA.Connection, Svgplus.CA.ConnectionId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let bFrom =
                { new Lens<Svgplus.CA.Connection, Aardvark.Base.Incremental.IMod<Aardvark.Base.V2d>>() with
                    override x.Get(r) = r.bFrom
                    override x.Set(r,v) = { r with bFrom = v }
                    override x.Update(r,f) = { r with bFrom = f r.bFrom }
                }
            let bTo =
                { new Lens<Svgplus.CA.Connection, Aardvark.Base.Incremental.IMod<Aardvark.Base.V2d>>() with
                    override x.Get(r) = r.bTo
                    override x.Set(r,v) = { r with bTo = v }
                    override x.Update(r,f) = { r with bTo = f r.bTo }
                }
            let dotted =
                { new Lens<Svgplus.CA.Connection, System.Boolean>() with
                    override x.Get(r) = r.dotted
                    override x.Set(r,v) = { r with dotted = v }
                    override x.Update(r,f) = { r with dotted = f r.dotted }
                }
            let colour =
                { new Lens<Svgplus.CA.Connection, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let weight =
                { new Lens<Svgplus.CA.Connection, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
    
    
    type MConnectionApp(__initial : Svgplus.CA.ConnectionApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.CA.ConnectionApp> = Aardvark.Base.Incremental.EqModRef<Svgplus.CA.ConnectionApp>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.CA.ConnectionApp>
        let _connections = MMap.Create(__initial.connections, (fun v -> MConnection.Create(v)), (fun (m,v) -> MConnection.Update(m, v)), (fun v -> v))
        let _connecting = MOption.Create(__initial.connecting)
        let _mouseposition = ResetMod.Create(__initial.mouseposition)
        
        member x.connections = _connections :> amap<_,_>
        member x.connecting = _connecting :> IMod<_>
        member x.mouseposition = _mouseposition :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.CA.ConnectionApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_connections, v.connections)
                MOption.Update(_connecting, v.connecting)
                ResetMod.Update(_mouseposition,v.mouseposition)
                
        
        static member Create(__initial : Svgplus.CA.ConnectionApp) : MConnectionApp = MConnectionApp(__initial)
        static member Update(m : MConnectionApp, v : Svgplus.CA.ConnectionApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.CA.ConnectionApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ConnectionApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let connections =
                { new Lens<Svgplus.CA.ConnectionApp, Aardvark.Base.hmap<Svgplus.CA.ConnectionId,Svgplus.CA.Connection>>() with
                    override x.Get(r) = r.connections
                    override x.Set(r,v) = { r with connections = v }
                    override x.Update(r,f) = { r with connections = f r.connections }
                }
            let connecting =
                { new Lens<Svgplus.CA.ConnectionApp, Microsoft.FSharp.Core.Option<Aardvark.Base.Incremental.IMod<Aardvark.Base.V2d>>>() with
                    override x.Get(r) = r.connecting
                    override x.Set(r,v) = { r with connecting = v }
                    override x.Update(r,f) = { r with connecting = f r.connecting }
                }
            let mouseposition =
                { new Lens<Svgplus.CA.ConnectionApp, Aardvark.Base.V2i>() with
                    override x.Get(r) = r.mouseposition
                    override x.Set(r,v) = { r with mouseposition = v }
                    override x.Update(r,f) = { r with mouseposition = f r.mouseposition }
                }
