namespace Svgplus.CA

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.CA

[<AutoOpen>]
module Mutable =

    
    
    type MConnectionApp(__initial : Svgplus.CA.ConnectionApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.CA.ConnectionApp> = Aardvark.Base.Incremental.EqModRef<Svgplus.CA.ConnectionApp>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.CA.ConnectionApp>
        let _connections = MList.Create(__initial.connections)
        let _connecting = MOption.Create(__initial.connecting)
        let _mouseposition = ResetMod.Create(__initial.mouseposition)
        
        member x.connections = _connections :> alist<_>
        member x.connecting = _connecting :> IMod<_>
        member x.mouseposition = _mouseposition :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.CA.ConnectionApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_connections, v.connections)
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
                { new Lens<Svgplus.CA.ConnectionApp, Aardvark.Base.plist<Svgplus.CA.Connection>>() with
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
