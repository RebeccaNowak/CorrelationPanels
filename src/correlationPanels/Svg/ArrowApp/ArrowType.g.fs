namespace Svgplus.ArrowType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.ArrowType

[<AutoOpen>]
module Mutable =

    
    
    type MArrow(__initial : Svgplus.ArrowType.Arrow) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.ArrowType.Arrow> = Aardvark.Base.Incremental.EqModRef<Svgplus.ArrowType.Arrow>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.ArrowType.Arrow>
        let _centre = ResetMod.Create(__initial.centre)
        let _direction = ResetMod.Create(__initial.direction)
        let _length = ResetMod.Create(__initial.length)
        let _height = ResetMod.Create(__initial.height)
        let _horz = ResetMod.Create(__initial.horz)
        let _vert = ResetMod.Create(__initial.vert)
        let _stroke = ResetMod.Create(__initial.stroke)
        let _fill = ResetMod.Create(__initial.fill)
        let _colour = ResetMod.Create(__initial.colour)
        let _onEnter = ResetMod.Create(__initial.onEnter)
        let _onLeave = ResetMod.Create(__initial.onLeave)
        
        member x.centre = _centre :> IMod<_>
        member x.direction = _direction :> IMod<_>
        member x.length = _length :> IMod<_>
        member x.height = _height :> IMod<_>
        member x.horz = _horz :> IMod<_>
        member x.vert = _vert :> IMod<_>
        member x.stroke = _stroke :> IMod<_>
        member x.fill = _fill :> IMod<_>
        member x.colour = _colour :> IMod<_>
        member x.onEnter = _onEnter :> IMod<_>
        member x.onLeave = _onLeave :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.ArrowType.Arrow) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_centre,v.centre)
                ResetMod.Update(_direction,v.direction)
                ResetMod.Update(_length,v.length)
                ResetMod.Update(_height,v.height)
                ResetMod.Update(_horz,v.horz)
                ResetMod.Update(_vert,v.vert)
                ResetMod.Update(_stroke,v.stroke)
                ResetMod.Update(_fill,v.fill)
                ResetMod.Update(_colour,v.colour)
                ResetMod.Update(_onEnter,v.onEnter)
                ResetMod.Update(_onLeave,v.onLeave)
                
        
        static member Create(__initial : Svgplus.ArrowType.Arrow) : MArrow = MArrow(__initial)
        static member Update(m : MArrow, v : Svgplus.ArrowType.Arrow) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.ArrowType.Arrow> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Arrow =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let centre =
                { new Lens<Svgplus.ArrowType.Arrow, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.centre
                    override x.Set(r,v) = { r with centre = v }
                    override x.Update(r,f) = { r with centre = f r.centre }
                }
            let direction =
                { new Lens<Svgplus.ArrowType.Arrow, SimpleTypes.Direction>() with
                    override x.Get(r) = r.direction
                    override x.Set(r,v) = { r with direction = v }
                    override x.Update(r,f) = { r with direction = f r.direction }
                }
            let length =
                { new Lens<Svgplus.ArrowType.Arrow, System.Double>() with
                    override x.Get(r) = r.length
                    override x.Set(r,v) = { r with length = v }
                    override x.Update(r,f) = { r with length = f r.length }
                }
            let height =
                { new Lens<Svgplus.ArrowType.Arrow, System.Double>() with
                    override x.Get(r) = r.height
                    override x.Set(r,v) = { r with height = v }
                    override x.Update(r,f) = { r with height = f r.height }
                }
            let horz =
                { new Lens<Svgplus.ArrowType.Arrow, System.Double>() with
                    override x.Get(r) = r.horz
                    override x.Set(r,v) = { r with horz = v }
                    override x.Update(r,f) = { r with horz = f r.horz }
                }
            let vert =
                { new Lens<Svgplus.ArrowType.Arrow, System.Double>() with
                    override x.Get(r) = r.vert
                    override x.Set(r,v) = { r with vert = v }
                    override x.Update(r,f) = { r with vert = f r.vert }
                }
            let stroke =
                { new Lens<Svgplus.ArrowType.Arrow, System.Double>() with
                    override x.Get(r) = r.stroke
                    override x.Set(r,v) = { r with stroke = v }
                    override x.Update(r,f) = { r with stroke = f r.stroke }
                }
            let fill =
                { new Lens<Svgplus.ArrowType.Arrow, System.Boolean>() with
                    override x.Get(r) = r.fill
                    override x.Set(r,v) = { r with fill = v }
                    override x.Update(r,f) = { r with fill = f r.fill }
                }
            let colour =
                { new Lens<Svgplus.ArrowType.Arrow, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let onEnter =
                { new Lens<Svgplus.ArrowType.Arrow, Svgplus.ArrowType.Arrow -> Svgplus.ArrowType.Arrow>() with
                    override x.Get(r) = r.onEnter
                    override x.Set(r,v) = { r with onEnter = v }
                    override x.Update(r,f) = { r with onEnter = f r.onEnter }
                }
            let onLeave =
                { new Lens<Svgplus.ArrowType.Arrow, Svgplus.ArrowType.Arrow -> Svgplus.ArrowType.Arrow>() with
                    override x.Get(r) = r.onLeave
                    override x.Set(r,v) = { r with onLeave = v }
                    override x.Update(r,f) = { r with onLeave = f r.onLeave }
                }
