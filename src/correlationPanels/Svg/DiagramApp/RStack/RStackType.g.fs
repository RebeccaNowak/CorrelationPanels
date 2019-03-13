namespace Svgplus.RectangleStackTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.RectangleStackTypes

[<AutoOpen>]
module Mutable =

    
    
    type MRectangleStack(__initial : Svgplus.RectangleStackTypes.RectangleStack) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.RectangleStackTypes.RectangleStack> = Aardvark.Base.Incremental.EqModRef<Svgplus.RectangleStackTypes.RectangleStack>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.RectangleStackTypes.RectangleStack>
        let _needsLayouting = ResetMod.Create(__initial.needsLayouting)
        let _rectangles = MMap.Create(__initial.rectangles, (fun v -> Svgplus.RectangleType.Mutable.MRectangle.Create(v)), (fun (m,v) -> Svgplus.RectangleType.Mutable.MRectangle.Update(m, v)), (fun v -> v))
        let _header = Svgplus.HeaderType.Mutable.MHeader.Create(__initial.header)
        let _order = MList.Create(__initial.order)
        let _pos = ResetMod.Create(__initial.pos)
        let _yAxis = Svgplus.AxesTypes.Mutable.MAxisApp.Create(__initial.yAxis)
        let _yAxisMargin = ResetMod.Create(__initial.yAxisMargin)
        
        member x.id = __current.Value.id
        member x.needsLayouting = _needsLayouting :> IMod<_>
        member x.rectangles = _rectangles :> amap<_,_>
        member x.header = _header
        member x.order = _order :> alist<_>
        member x.pos = _pos :> IMod<_>
        member x.yAxis = _yAxis
        member x.yAxisMargin = _yAxisMargin :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.RectangleStackTypes.RectangleStack) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_needsLayouting,v.needsLayouting)
                MMap.Update(_rectangles, v.rectangles)
                Svgplus.HeaderType.Mutable.MHeader.Update(_header, v.header)
                MList.Update(_order, v.order)
                ResetMod.Update(_pos,v.pos)
                Svgplus.AxesTypes.Mutable.MAxisApp.Update(_yAxis, v.yAxis)
                ResetMod.Update(_yAxisMargin,v.yAxisMargin)
                
        
        static member Create(__initial : Svgplus.RectangleStackTypes.RectangleStack) : MRectangleStack = MRectangleStack(__initial)
        static member Update(m : MRectangleStack, v : Svgplus.RectangleStackTypes.RectangleStack) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.RectangleStackTypes.RectangleStack> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RectangleStack =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, Svgplus.RectangleStackTypes.RectangleStackId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let needsLayouting =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, System.Boolean>() with
                    override x.Get(r) = r.needsLayouting
                    override x.Set(r,v) = { r with needsLayouting = v }
                    override x.Update(r,f) = { r with needsLayouting = f r.needsLayouting }
                }
            let rectangles =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, Aardvark.Base.hmap<Svgplus.RectangleType.RectangleId,Svgplus.RectangleType.Rectangle>>() with
                    override x.Get(r) = r.rectangles
                    override x.Set(r,v) = { r with rectangles = v }
                    override x.Update(r,f) = { r with rectangles = f r.rectangles }
                }
            let header =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, Svgplus.HeaderType.Header>() with
                    override x.Get(r) = r.header
                    override x.Set(r,v) = { r with header = v }
                    override x.Update(r,f) = { r with header = f r.header }
                }
            let order =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, Aardvark.Base.plist<Svgplus.RectangleType.RectangleId>>() with
                    override x.Get(r) = r.order
                    override x.Set(r,v) = { r with order = v }
                    override x.Update(r,f) = { r with order = f r.order }
                }
            let pos =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let yAxis =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, Svgplus.AxesTypes.AxisApp>() with
                    override x.Get(r) = r.yAxis
                    override x.Set(r,v) = { r with yAxis = v }
                    override x.Update(r,f) = { r with yAxis = f r.yAxis }
                }
            let yAxisMargin =
                { new Lens<Svgplus.RectangleStackTypes.RectangleStack, System.Double>() with
                    override x.Get(r) = r.yAxisMargin
                    override x.Set(r,v) = { r with yAxisMargin = v }
                    override x.Update(r,f) = { r with yAxisMargin = f r.yAxisMargin }
                }
