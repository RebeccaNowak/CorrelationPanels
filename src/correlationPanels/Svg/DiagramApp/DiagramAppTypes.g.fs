namespace Svgplus.DA

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.DA

[<AutoOpen>]
module Mutable =

    
    
    type MDiagramApp(__initial : Svgplus.DA.DiagramApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.DA.DiagramApp> = Aardvark.Base.Incremental.EqModRef<Svgplus.DA.DiagramApp>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.DA.DiagramApp>
        let _rectangleStacks = MMap.Create(__initial.rectangleStacks, (fun v -> Svgplus.Mutable.MRectangleStack.Create(v)), (fun (m,v) -> Svgplus.Mutable.MRectangleStack.Update(m, v)), (fun v -> v))
        let _connectionApp = Svgplus.CA.Mutable.MConnectionApp.Create(__initial.connectionApp)
        
        member x.rectangleStacks = _rectangleStacks :> amap<_,_>
        member x.connectionApp = _connectionApp
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.DA.DiagramApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_rectangleStacks, v.rectangleStacks)
                Svgplus.CA.Mutable.MConnectionApp.Update(_connectionApp, v.connectionApp)
                
        
        static member Create(__initial : Svgplus.DA.DiagramApp) : MDiagramApp = MDiagramApp(__initial)
        static member Update(m : MDiagramApp, v : Svgplus.DA.DiagramApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.DA.DiagramApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DiagramApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rectangleStacks =
                { new Lens<Svgplus.DA.DiagramApp, Aardvark.Base.hmap<Svgplus.RectangleStackId,Svgplus.RectangleStack>>() with
                    override x.Get(r) = r.rectangleStacks
                    override x.Set(r,v) = { r with rectangleStacks = v }
                    override x.Update(r,f) = { r with rectangleStacks = f r.rectangleStacks }
                }
            let connectionApp =
                { new Lens<Svgplus.DA.DiagramApp, Svgplus.CA.ConnectionApp>() with
                    override x.Get(r) = r.connectionApp
                    override x.Set(r,v) = { r with connectionApp = v }
                    override x.Update(r,f) = { r with connectionApp = f r.connectionApp }
                }
