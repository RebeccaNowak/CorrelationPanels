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
        let _rectangleStacks = MMap.Create(__initial.rectangleStacks, (fun v -> Svgplus.RS.Mutable.MRectangleStack.Create(v)), (fun (m,v) -> Svgplus.RS.Mutable.MRectangleStack.Update(m, v)), (fun v -> v))
        let _order = MList.Create(__initial.order)
        let _connectionApp = Svgplus.CA.Mutable.MConnectionApp.Create(__initial.connectionApp)
        let _rstackGap = ResetMod.Create(__initial.rstackGap)
        let _marginLeft = ResetMod.Create(__initial.marginLeft)
        let _marginTop = ResetMod.Create(__initial.marginTop)
        
        member x.rectangleStacks = _rectangleStacks :> amap<_,_>
        member x.order = _order :> alist<_>
        member x.connectionApp = _connectionApp
        member x.rstackGap = _rstackGap :> IMod<_>
        member x.marginLeft = _marginLeft :> IMod<_>
        member x.marginTop = _marginTop :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.DA.DiagramApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_rectangleStacks, v.rectangleStacks)
                MList.Update(_order, v.order)
                Svgplus.CA.Mutable.MConnectionApp.Update(_connectionApp, v.connectionApp)
                ResetMod.Update(_rstackGap,v.rstackGap)
                ResetMod.Update(_marginLeft,v.marginLeft)
                ResetMod.Update(_marginTop,v.marginTop)
                
        
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
                { new Lens<Svgplus.DA.DiagramApp, Aardvark.Base.hmap<Svgplus.RS.RectangleStackId,Svgplus.RS.RectangleStack>>() with
                    override x.Get(r) = r.rectangleStacks
                    override x.Set(r,v) = { r with rectangleStacks = v }
                    override x.Update(r,f) = { r with rectangleStacks = f r.rectangleStacks }
                }
            let order =
                { new Lens<Svgplus.DA.DiagramApp, Aardvark.Base.plist<Svgplus.RS.RectangleStackId>>() with
                    override x.Get(r) = r.order
                    override x.Set(r,v) = { r with order = v }
                    override x.Update(r,f) = { r with order = f r.order }
                }
            let connectionApp =
                { new Lens<Svgplus.DA.DiagramApp, Svgplus.CA.ConnectionApp>() with
                    override x.Get(r) = r.connectionApp
                    override x.Set(r,v) = { r with connectionApp = v }
                    override x.Update(r,f) = { r with connectionApp = f r.connectionApp }
                }
            let rstackGap =
                { new Lens<Svgplus.DA.DiagramApp, System.Double>() with
                    override x.Get(r) = r.rstackGap
                    override x.Set(r,v) = { r with rstackGap = v }
                    override x.Update(r,f) = { r with rstackGap = f r.rstackGap }
                }
            let marginLeft =
                { new Lens<Svgplus.DA.DiagramApp, System.Double>() with
                    override x.Get(r) = r.marginLeft
                    override x.Set(r,v) = { r with marginLeft = v }
                    override x.Update(r,f) = { r with marginLeft = f r.marginLeft }
                }
            let marginTop =
                { new Lens<Svgplus.DA.DiagramApp, System.Double>() with
                    override x.Get(r) = r.marginTop
                    override x.Set(r,v) = { r with marginTop = v }
                    override x.Update(r,f) = { r with marginTop = f r.marginTop }
                }
