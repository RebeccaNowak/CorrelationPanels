namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

[<AutoOpen>]
module Mutable =

    
    
    type MButton(__initial : Svgplus.Button) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.Button> = Aardvark.Base.Incremental.EqModRef<Svgplus.Button>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.Button>
        let _pos = ResetMod.Create(__initial.pos)
        let _radius = ResetMod.Create(__initial.radius)
        let _rHoverChange = ResetMod.Create(__initial.rHoverChange)
        let _stroke = ResetMod.Create(__initial.stroke)
        let _color = ResetMod.Create(__initial.color)
        let _colChange = ResetMod.Create(__initial.colChange)
        let _fill = ResetMod.Create(__initial.fill)
        let _isToggled = ResetMod.Create(__initial.isToggled)
        let _isHovering = ResetMod.Create(__initial.isHovering)
        let _transitionSec = ResetMod.Create(__initial.transitionSec)
        
        member x.id = __current.Value.id
        member x.pos = _pos :> IMod<_>
        member x.radius = _radius :> IMod<_>
        member x.rHoverChange = _rHoverChange :> IMod<_>
        member x.stroke = _stroke :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.colChange = _colChange :> IMod<_>
        member x.fill = _fill :> IMod<_>
        member x.isToggled = _isToggled :> IMod<_>
        member x.isHovering = _isHovering :> IMod<_>
        member x.transitionSec = _transitionSec :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.Button) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_pos,v.pos)
                ResetMod.Update(_radius,v.radius)
                ResetMod.Update(_rHoverChange,v.rHoverChange)
                ResetMod.Update(_stroke,v.stroke)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_colChange,v.colChange)
                ResetMod.Update(_fill,v.fill)
                ResetMod.Update(_isToggled,v.isToggled)
                ResetMod.Update(_isHovering,v.isHovering)
                ResetMod.Update(_transitionSec,v.transitionSec)
                
        
        static member Create(__initial : Svgplus.Button) : MButton = MButton(__initial)
        static member Update(m : MButton, v : Svgplus.Button) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.Button> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Button =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.Button, Svgplus.ButtonId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let pos =
                { new Lens<Svgplus.Button, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let radius =
                { new Lens<Svgplus.Button, System.Double>() with
                    override x.Get(r) = r.radius
                    override x.Set(r,v) = { r with radius = v }
                    override x.Update(r,f) = { r with radius = f r.radius }
                }
            let rHoverChange =
                { new Lens<Svgplus.Button, System.Double>() with
                    override x.Get(r) = r.rHoverChange
                    override x.Set(r,v) = { r with rHoverChange = v }
                    override x.Update(r,f) = { r with rHoverChange = f r.rHoverChange }
                }
            let stroke =
                { new Lens<Svgplus.Button, System.Double>() with
                    override x.Get(r) = r.stroke
                    override x.Set(r,v) = { r with stroke = v }
                    override x.Update(r,f) = { r with stroke = f r.stroke }
                }
            let color =
                { new Lens<Svgplus.Button, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let colChange =
                { new Lens<Svgplus.Button, Aardvark.Base.V3i>() with
                    override x.Get(r) = r.colChange
                    override x.Set(r,v) = { r with colChange = v }
                    override x.Update(r,f) = { r with colChange = f r.colChange }
                }
            let fill =
                { new Lens<Svgplus.Button, System.Boolean>() with
                    override x.Get(r) = r.fill
                    override x.Set(r,v) = { r with fill = v }
                    override x.Update(r,f) = { r with fill = f r.fill }
                }
            let isToggled =
                { new Lens<Svgplus.Button, System.Boolean>() with
                    override x.Get(r) = r.isToggled
                    override x.Set(r,v) = { r with isToggled = v }
                    override x.Update(r,f) = { r with isToggled = f r.isToggled }
                }
            let isHovering =
                { new Lens<Svgplus.Button, System.Boolean>() with
                    override x.Get(r) = r.isHovering
                    override x.Set(r,v) = { r with isHovering = v }
                    override x.Update(r,f) = { r with isHovering = f r.isHovering }
                }
            let transitionSec =
                { new Lens<Svgplus.Button, System.Double>() with
                    override x.Get(r) = r.transitionSec
                    override x.Set(r,v) = { r with transitionSec = v }
                    override x.Update(r,f) = { r with transitionSec = f r.transitionSec }
                }
