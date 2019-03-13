namespace Svgplus.DA

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.DA

[<AutoOpen>]
module Mutable =

    
    
    type MDiagram(__initial : Svgplus.DA.Diagram) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.DA.Diagram> = Aardvark.Base.Incremental.EqModRef<Svgplus.DA.Diagram>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.DA.Diagram>
        let _rectangleStacks = MMap.Create(__initial.rectangleStacks, (fun v -> Svgplus.RectangleStackTypes.Mutable.MRectangleStack.Create(v)), (fun (m,v) -> Svgplus.RectangleStackTypes.Mutable.MRectangleStack.Update(m, v)), (fun v -> v))
        let _order = MList.Create(__initial.order)
        let _connectionApp = Svgplus.CA.Mutable.MConnectionApp.Create(__initial.connectionApp)
        let _rstackGap = ResetMod.Create(__initial.rstackGap)
        let _marginLeft = ResetMod.Create(__initial.marginLeft)
        let _marginTop = ResetMod.Create(__initial.marginTop)
        let _selectedRectangle = MOption.Create(__initial.selectedRectangle)
        let _yToData = ResetMod.Create(__initial.yToData)
        let _dataToY = ResetMod.Create(__initial.dataToY)
        let _dataRange = ResetMod.Create(__initial.dataRange)
        
        member x.rectangleStacks = _rectangleStacks :> amap<_,_>
        member x.order = _order :> alist<_>
        member x.connectionApp = _connectionApp
        member x.rstackGap = _rstackGap :> IMod<_>
        member x.marginLeft = _marginLeft :> IMod<_>
        member x.marginTop = _marginTop :> IMod<_>
        member x.selectedRectangle = _selectedRectangle :> IMod<_>
        member x.yToData = _yToData :> IMod<_>
        member x.dataToY = _dataToY :> IMod<_>
        member x.dataRange = _dataRange :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.DA.Diagram) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_rectangleStacks, v.rectangleStacks)
                MList.Update(_order, v.order)
                Svgplus.CA.Mutable.MConnectionApp.Update(_connectionApp, v.connectionApp)
                ResetMod.Update(_rstackGap,v.rstackGap)
                ResetMod.Update(_marginLeft,v.marginLeft)
                ResetMod.Update(_marginTop,v.marginTop)
                MOption.Update(_selectedRectangle, v.selectedRectangle)
                ResetMod.Update(_yToData,v.yToData)
                ResetMod.Update(_dataToY,v.dataToY)
                ResetMod.Update(_dataRange,v.dataRange)
                
        
        static member Create(__initial : Svgplus.DA.Diagram) : MDiagram = MDiagram(__initial)
        static member Update(m : MDiagram, v : Svgplus.DA.Diagram) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.DA.Diagram> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Diagram =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let rectangleStacks =
                { new Lens<Svgplus.DA.Diagram, Aardvark.Base.hmap<Svgplus.RectangleStackTypes.RectangleStackId,Svgplus.RectangleStackTypes.RectangleStack>>() with
                    override x.Get(r) = r.rectangleStacks
                    override x.Set(r,v) = { r with rectangleStacks = v }
                    override x.Update(r,f) = { r with rectangleStacks = f r.rectangleStacks }
                }
            let order =
                { new Lens<Svgplus.DA.Diagram, Aardvark.Base.plist<Svgplus.RectangleStackTypes.RectangleStackId>>() with
                    override x.Get(r) = r.order
                    override x.Set(r,v) = { r with order = v }
                    override x.Update(r,f) = { r with order = f r.order }
                }
            let connectionApp =
                { new Lens<Svgplus.DA.Diagram, Svgplus.CA.ConnectionApp>() with
                    override x.Get(r) = r.connectionApp
                    override x.Set(r,v) = { r with connectionApp = v }
                    override x.Update(r,f) = { r with connectionApp = f r.connectionApp }
                }
            let rstackGap =
                { new Lens<Svgplus.DA.Diagram, System.Double>() with
                    override x.Get(r) = r.rstackGap
                    override x.Set(r,v) = { r with rstackGap = v }
                    override x.Update(r,f) = { r with rstackGap = f r.rstackGap }
                }
            let marginLeft =
                { new Lens<Svgplus.DA.Diagram, System.Double>() with
                    override x.Get(r) = r.marginLeft
                    override x.Set(r,v) = { r with marginLeft = v }
                    override x.Update(r,f) = { r with marginLeft = f r.marginLeft }
                }
            let marginTop =
                { new Lens<Svgplus.DA.Diagram, System.Double>() with
                    override x.Get(r) = r.marginTop
                    override x.Set(r,v) = { r with marginTop = v }
                    override x.Update(r,f) = { r with marginTop = f r.marginTop }
                }
            let selectedRectangle =
                { new Lens<Svgplus.DA.Diagram, Microsoft.FSharp.Core.Option<Svgplus.DA.RectangleIdentification>>() with
                    override x.Get(r) = r.selectedRectangle
                    override x.Set(r,v) = { r with selectedRectangle = v }
                    override x.Update(r,f) = { r with selectedRectangle = f r.selectedRectangle }
                }
            let yToData =
                { new Lens<Svgplus.DA.Diagram, System.Double -> System.Double>() with
                    override x.Get(r) = r.yToData
                    override x.Set(r,v) = { r with yToData = v }
                    override x.Update(r,f) = { r with yToData = f r.yToData }
                }
            let dataToY =
                { new Lens<Svgplus.DA.Diagram, System.Double -> System.Double>() with
                    override x.Get(r) = r.dataToY
                    override x.Set(r,v) = { r with dataToY = v }
                    override x.Update(r,f) = { r with dataToY = f r.dataToY }
                }
            let dataRange =
                { new Lens<Svgplus.DA.Diagram, SimpleTypes.Rangef>() with
                    override x.Get(r) = r.dataRange
                    override x.Set(r,v) = { r with dataRange = v }
                    override x.Update(r,f) = { r with dataRange = f r.dataRange }
                }
