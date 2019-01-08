namespace Svgplus.RS

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.RS

[<AutoOpen>]
module Mutable =

    
    
    type MRectangleStack(__initial : Svgplus.RS.RectangleStack) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.RS.RectangleStack> = Aardvark.Base.Incremental.EqModRef<Svgplus.RS.RectangleStack>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.RS.RectangleStack>
        let _rectangles = MMap.Create(__initial.rectangles, (fun v -> Svgplus.Mutable.MRectangle.Create(v)), (fun (m,v) -> Svgplus.Mutable.MRectangle.Update(m, v)), (fun v -> v))
        let _header = Svgplus.HeaderType.Mutable.MHeader.Create(__initial.header)
        let _order = MList.Create(__initial.order)
        let _pos = ResetMod.Create(__initial.pos)
        
        member x.id = __current.Value.id
        member x.rectangles = _rectangles :> amap<_,_>
        member x.header = _header
        member x.order = _order :> alist<_>
        member x.pos = _pos :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.RS.RectangleStack) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_rectangles, v.rectangles)
                Svgplus.HeaderType.Mutable.MHeader.Update(_header, v.header)
                MList.Update(_order, v.order)
                ResetMod.Update(_pos,v.pos)
                
        
        static member Create(__initial : Svgplus.RS.RectangleStack) : MRectangleStack = MRectangleStack(__initial)
        static member Update(m : MRectangleStack, v : Svgplus.RS.RectangleStack) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.RS.RectangleStack> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RectangleStack =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.RS.RectangleStack, Svgplus.RS.RectangleStackId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let rectangles =
                { new Lens<Svgplus.RS.RectangleStack, Aardvark.Base.hmap<Svgplus.RectangleId,Svgplus.Rectangle>>() with
                    override x.Get(r) = r.rectangles
                    override x.Set(r,v) = { r with rectangles = v }
                    override x.Update(r,f) = { r with rectangles = f r.rectangles }
                }
            let header =
                { new Lens<Svgplus.RS.RectangleStack, Svgplus.HeaderType.Header>() with
                    override x.Get(r) = r.header
                    override x.Set(r,v) = { r with header = v }
                    override x.Update(r,f) = { r with header = f r.header }
                }
            let order =
                { new Lens<Svgplus.RS.RectangleStack, Aardvark.Base.plist<Svgplus.RectangleId>>() with
                    override x.Get(r) = r.order
                    override x.Set(r,v) = { r with order = v }
                    override x.Update(r,f) = { r with order = f r.order }
                }
            let pos =
                { new Lens<Svgplus.RS.RectangleStack, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
