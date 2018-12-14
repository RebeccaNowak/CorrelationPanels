namespace Svgplus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus

[<AutoOpen>]
module Mutable =

    
    
    type MRoseDiagram(__initial : Svgplus.RoseDiagram) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.RoseDiagram> = Aardvark.Base.Incremental.EqModRef<Svgplus.RoseDiagram>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.RoseDiagram>
        let _centre = ResetMod.Create(__initial.centre)
        let _outerRadius = ResetMod.Create(__initial.outerRadius)
        let _innerRadius = ResetMod.Create(__initial.innerRadius)
        let _colour = ResetMod.Create(__initial.colour)
        let _nrCircles = ResetMod.Create(__initial.nrCircles)
        let _weight = ResetMod.Create(__initial.weight)
        let _countPerBin = MList.Create(__initial.countPerBin)
        
        member x.centre = _centre :> IMod<_>
        member x.outerRadius = _outerRadius :> IMod<_>
        member x.innerRadius = _innerRadius :> IMod<_>
        member x.colour = _colour :> IMod<_>
        member x.nrCircles = _nrCircles :> IMod<_>
        member x.weight = _weight :> IMod<_>
        member x.countPerBin = _countPerBin :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.RoseDiagram) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_centre,v.centre)
                ResetMod.Update(_outerRadius,v.outerRadius)
                ResetMod.Update(_innerRadius,v.innerRadius)
                ResetMod.Update(_colour,v.colour)
                ResetMod.Update(_nrCircles,v.nrCircles)
                ResetMod.Update(_weight,v.weight)
                MList.Update(_countPerBin, v.countPerBin)
                
        
        static member Create(__initial : Svgplus.RoseDiagram) : MRoseDiagram = MRoseDiagram(__initial)
        static member Update(m : MRoseDiagram, v : Svgplus.RoseDiagram) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.RoseDiagram> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RoseDiagram =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let centre =
                { new Lens<Svgplus.RoseDiagram, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.centre
                    override x.Set(r,v) = { r with centre = v }
                    override x.Update(r,f) = { r with centre = f r.centre }
                }
            let outerRadius =
                { new Lens<Svgplus.RoseDiagram, System.Double>() with
                    override x.Get(r) = r.outerRadius
                    override x.Set(r,v) = { r with outerRadius = v }
                    override x.Update(r,f) = { r with outerRadius = f r.outerRadius }
                }
            let innerRadius =
                { new Lens<Svgplus.RoseDiagram, System.Double>() with
                    override x.Get(r) = r.innerRadius
                    override x.Set(r,v) = { r with innerRadius = v }
                    override x.Update(r,f) = { r with innerRadius = f r.innerRadius }
                }
            let colour =
                { new Lens<Svgplus.RoseDiagram, Microsoft.FSharp.Collections.List<Aardvark.Base.C4b>>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let nrCircles =
                { new Lens<Svgplus.RoseDiagram, System.Int32>() with
                    override x.Get(r) = r.nrCircles
                    override x.Set(r,v) = { r with nrCircles = v }
                    override x.Update(r,f) = { r with nrCircles = f r.nrCircles }
                }
            let weight =
                { new Lens<Svgplus.RoseDiagram, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
            let countPerBin =
                { new Lens<Svgplus.RoseDiagram, Aardvark.Base.plist<Svgplus.Bin>>() with
                    override x.Get(r) = r.countPerBin
                    override x.Set(r,v) = { r with countPerBin = v }
                    override x.Update(r,f) = { r with countPerBin = f r.countPerBin }
                }
    
    
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
    
    
    type MRectangle(__initial : Svgplus.Rectangle) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.Rectangle> = Aardvark.Base.Incremental.EqModRef<Svgplus.Rectangle>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.Rectangle>
        let _pos = ResetMod.Create(__initial.pos)
        let _dim = ResetMod.Create(__initial.dim)
        let _colour = Aardvark.UI.Mutable.MColorInput.Create(__initial.colour)
        let _borderColour = ResetMod.Create(__initial.borderColour)
        let _isToggled = ResetMod.Create(__initial.isToggled)
        let _colChange = ResetMod.Create(__initial.colChange)
        let _isHovering = ResetMod.Create(__initial.isHovering)
        let _dottedBorder = ResetMod.Create(__initial.dottedBorder)
        let _draw = ResetMod.Create(__initial.draw)
        
        member x.pos = _pos :> IMod<_>
        member x.dim = _dim :> IMod<_>
        member x.colour = _colour
        member x.borderColour = _borderColour :> IMod<_>
        member x.isToggled = _isToggled :> IMod<_>
        member x.colChange = _colChange :> IMod<_>
        member x.isHovering = _isHovering :> IMod<_>
        member x.dottedBorder = _dottedBorder :> IMod<_>
        member x.draw = _draw :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.Rectangle) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_pos,v.pos)
                ResetMod.Update(_dim,v.dim)
                Aardvark.UI.Mutable.MColorInput.Update(_colour, v.colour)
                ResetMod.Update(_borderColour,v.borderColour)
                ResetMod.Update(_isToggled,v.isToggled)
                ResetMod.Update(_colChange,v.colChange)
                ResetMod.Update(_isHovering,v.isHovering)
                ResetMod.Update(_dottedBorder,v.dottedBorder)
                ResetMod.Update(_draw,v.draw)
                
        
        static member Create(__initial : Svgplus.Rectangle) : MRectangle = MRectangle(__initial)
        static member Update(m : MRectangle, v : Svgplus.Rectangle) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.Rectangle> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rectangle =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let pos =
                { new Lens<Svgplus.Rectangle, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let dim =
                { new Lens<Svgplus.Rectangle, SimpleTypes.Size2D>() with
                    override x.Get(r) = r.dim
                    override x.Set(r,v) = { r with dim = v }
                    override x.Update(r,f) = { r with dim = f r.dim }
                }
            let colour =
                { new Lens<Svgplus.Rectangle, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let borderColour =
                { new Lens<Svgplus.Rectangle, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.borderColour
                    override x.Set(r,v) = { r with borderColour = v }
                    override x.Update(r,f) = { r with borderColour = f r.borderColour }
                }
            let isToggled =
                { new Lens<Svgplus.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.isToggled
                    override x.Set(r,v) = { r with isToggled = v }
                    override x.Update(r,f) = { r with isToggled = f r.isToggled }
                }
            let colChange =
                { new Lens<Svgplus.Rectangle, Aardvark.Base.V3i>() with
                    override x.Get(r) = r.colChange
                    override x.Set(r,v) = { r with colChange = v }
                    override x.Update(r,f) = { r with colChange = f r.colChange }
                }
            let isHovering =
                { new Lens<Svgplus.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.isHovering
                    override x.Set(r,v) = { r with isHovering = v }
                    override x.Update(r,f) = { r with isHovering = f r.isHovering }
                }
            let dottedBorder =
                { new Lens<Svgplus.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.dottedBorder
                    override x.Set(r,v) = { r with dottedBorder = v }
                    override x.Update(r,f) = { r with dottedBorder = f r.dottedBorder }
                }
            let draw =
                { new Lens<Svgplus.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
