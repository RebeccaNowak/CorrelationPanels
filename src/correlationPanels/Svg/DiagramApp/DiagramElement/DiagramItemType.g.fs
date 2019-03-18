namespace Svgplus.DiagramItemType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.DiagramItemType

[<AutoOpen>]
module Mutable =

    
    
    type MDiagramItem(__initial : Svgplus.DiagramItemType.DiagramItem) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.DiagramItemType.DiagramItem> = Aardvark.Base.Incremental.EqModRef<Svgplus.DiagramItemType.DiagramItem>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.DiagramItemType.DiagramItem>
        let _id = ResetMod.Create(__initial.id)
        let _pos = ResetMod.Create(__initial.pos)
        let _header = Svgplus.HeaderType.Mutable.MHeader.Create(__initial.header)
        let _rectangleStacks = MMap.Create(__initial.rectangleStacks, (fun v -> Svgplus.RectangleStackTypes.Mutable.MRectangleStack.Create(v)), (fun (m,v) -> Svgplus.RectangleStackTypes.Mutable.MRectangleStack.Update(m, v)), (fun v -> v))
        let _order = MList.Create(__initial.order)
        let _rightGaps = MMap.Create(__initial.rightGaps)
        let _marginLeft = ResetMod.Create(__initial.marginLeft)
        let _marginRight = ResetMod.Create(__initial.marginRight)
        let _marginTop = ResetMod.Create(__initial.marginTop)
        
        member x.id = _id :> IMod<_>
        member x.pos = _pos :> IMod<_>
        member x.header = _header
        member x.rectangleStacks = _rectangleStacks :> amap<_,_>
        member x.order = _order :> alist<_>
        member x.rightGaps = _rightGaps :> amap<_,_>
        member x.marginLeft = _marginLeft :> IMod<_>
        member x.marginRight = _marginRight :> IMod<_>
        member x.marginTop = _marginTop :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.DiagramItemType.DiagramItem) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_id,v.id)
                ResetMod.Update(_pos,v.pos)
                Svgplus.HeaderType.Mutable.MHeader.Update(_header, v.header)
                MMap.Update(_rectangleStacks, v.rectangleStacks)
                MList.Update(_order, v.order)
                MMap.Update(_rightGaps, v.rightGaps)
                ResetMod.Update(_marginLeft,v.marginLeft)
                ResetMod.Update(_marginRight,v.marginRight)
                ResetMod.Update(_marginTop,v.marginTop)
                
        
        static member Create(__initial : Svgplus.DiagramItemType.DiagramItem) : MDiagramItem = MDiagramItem(__initial)
        static member Update(m : MDiagramItem, v : Svgplus.DiagramItemType.DiagramItem) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.DiagramItemType.DiagramItem> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DiagramItem =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, Svgplus.DiagramItemType.DiagramItemId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let pos =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let header =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, Svgplus.HeaderType.Header>() with
                    override x.Get(r) = r.header
                    override x.Set(r,v) = { r with header = v }
                    override x.Update(r,f) = { r with header = f r.header }
                }
            let rectangleStacks =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, Aardvark.Base.hmap<Svgplus.RectangleStackTypes.RectangleStackId,Svgplus.RectangleStackTypes.RectangleStack>>() with
                    override x.Get(r) = r.rectangleStacks
                    override x.Set(r,v) = { r with rectangleStacks = v }
                    override x.Update(r,f) = { r with rectangleStacks = f r.rectangleStacks }
                }
            let order =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, Aardvark.Base.plist<Svgplus.RectangleStackTypes.RectangleStackId>>() with
                    override x.Get(r) = r.order
                    override x.Set(r,v) = { r with order = v }
                    override x.Update(r,f) = { r with order = f r.order }
                }
            let rightGaps =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, Aardvark.Base.hmap<Svgplus.RectangleStackTypes.RectangleStackId,System.Double>>() with
                    override x.Get(r) = r.rightGaps
                    override x.Set(r,v) = { r with rightGaps = v }
                    override x.Update(r,f) = { r with rightGaps = f r.rightGaps }
                }
            let marginLeft =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, System.Double>() with
                    override x.Get(r) = r.marginLeft
                    override x.Set(r,v) = { r with marginLeft = v }
                    override x.Update(r,f) = { r with marginLeft = f r.marginLeft }
                }
            let marginRight =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, System.Double>() with
                    override x.Get(r) = r.marginRight
                    override x.Set(r,v) = { r with marginRight = v }
                    override x.Update(r,f) = { r with marginRight = f r.marginRight }
                }
            let marginTop =
                { new Lens<Svgplus.DiagramItemType.DiagramItem, System.Double>() with
                    override x.Get(r) = r.marginTop
                    override x.Set(r,v) = { r with marginTop = v }
                    override x.Update(r,f) = { r with marginTop = f r.marginTop }
                }
