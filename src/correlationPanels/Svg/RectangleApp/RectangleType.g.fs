namespace Svgplus.RectangleType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.RectangleType

[<AutoOpen>]
module Mutable =

    
    
    type MRectangle(__initial : Svgplus.RectangleType.Rectangle) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.RectangleType.Rectangle> = Aardvark.Base.Incremental.EqModRef<Svgplus.RectangleType.Rectangle>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.RectangleType.Rectangle>
        let _label = UIPlus.Mutable.MTextInput.Create(__initial.label)
        let _pos = ResetMod.Create(__initial.pos)
        let _dim = ResetMod.Create(__initial.dim)
        let _colour = Aardvark.UI.Mutable.MColorInput.Create(__initial.colour)
        let _borderColour = ResetMod.Create(__initial.borderColour)
        let _isToggled = ResetMod.Create(__initial.isToggled)
        let _colChange = ResetMod.Create(__initial.colChange)
        let _isHovering = ResetMod.Create(__initial.isHovering)
        let _dottedBorder = ResetMod.Create(__initial.dottedBorder)
        let _draw = ResetMod.Create(__initial.draw)
        let _northWestButton = Svgplus.Mutable.MButton.Create(__initial.northWestButton)
        let _northEastButton = Svgplus.Mutable.MButton.Create(__initial.northEastButton)
        let _southWestButton = Svgplus.Mutable.MButton.Create(__initial.southWestButton)
        let _southEastButton = Svgplus.Mutable.MButton.Create(__initial.southEastButton)
        
        member x.id = __current.Value.id
        member x.label = _label
        member x.pos = _pos :> IMod<_>
        member x.dim = _dim :> IMod<_>
        member x.colour = _colour
        member x.borderColour = _borderColour :> IMod<_>
        member x.isToggled = _isToggled :> IMod<_>
        member x.colChange = _colChange :> IMod<_>
        member x.isHovering = _isHovering :> IMod<_>
        member x.dottedBorder = _dottedBorder :> IMod<_>
        member x.draw = _draw :> IMod<_>
        member x.northWestButton = _northWestButton
        member x.northEastButton = _northEastButton
        member x.southWestButton = _southWestButton
        member x.southEastButton = _southEastButton
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.RectangleType.Rectangle) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                UIPlus.Mutable.MTextInput.Update(_label, v.label)
                ResetMod.Update(_pos,v.pos)
                ResetMod.Update(_dim,v.dim)
                Aardvark.UI.Mutable.MColorInput.Update(_colour, v.colour)
                ResetMod.Update(_borderColour,v.borderColour)
                ResetMod.Update(_isToggled,v.isToggled)
                ResetMod.Update(_colChange,v.colChange)
                ResetMod.Update(_isHovering,v.isHovering)
                ResetMod.Update(_dottedBorder,v.dottedBorder)
                ResetMod.Update(_draw,v.draw)
                Svgplus.Mutable.MButton.Update(_northWestButton, v.northWestButton)
                Svgplus.Mutable.MButton.Update(_northEastButton, v.northEastButton)
                Svgplus.Mutable.MButton.Update(_southWestButton, v.southWestButton)
                Svgplus.Mutable.MButton.Update(_southEastButton, v.southEastButton)
                
        
        static member Create(__initial : Svgplus.RectangleType.Rectangle) : MRectangle = MRectangle(__initial)
        static member Update(m : MRectangle, v : Svgplus.RectangleType.Rectangle) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.RectangleType.Rectangle> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rectangle =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.RectangleType.Rectangle, Svgplus.RectangleType.RectangleId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let label =
                { new Lens<Svgplus.RectangleType.Rectangle, UIPlus.TextInput>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let pos =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let dim =
                { new Lens<Svgplus.RectangleType.Rectangle, SimpleTypes.Size2D>() with
                    override x.Get(r) = r.dim
                    override x.Set(r,v) = { r with dim = v }
                    override x.Update(r,f) = { r with dim = f r.dim }
                }
            let colour =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let borderColour =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.borderColour
                    override x.Set(r,v) = { r with borderColour = v }
                    override x.Update(r,f) = { r with borderColour = f r.borderColour }
                }
            let isToggled =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.isToggled
                    override x.Set(r,v) = { r with isToggled = v }
                    override x.Update(r,f) = { r with isToggled = f r.isToggled }
                }
            let colChange =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.Base.V3i>() with
                    override x.Get(r) = r.colChange
                    override x.Set(r,v) = { r with colChange = v }
                    override x.Update(r,f) = { r with colChange = f r.colChange }
                }
            let isHovering =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.isHovering
                    override x.Set(r,v) = { r with isHovering = v }
                    override x.Update(r,f) = { r with isHovering = f r.isHovering }
                }
            let dottedBorder =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.dottedBorder
                    override x.Set(r,v) = { r with dottedBorder = v }
                    override x.Update(r,f) = { r with dottedBorder = f r.dottedBorder }
                }
            let draw =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
            let northWestButton =
                { new Lens<Svgplus.RectangleType.Rectangle, Svgplus.Button>() with
                    override x.Get(r) = r.northWestButton
                    override x.Set(r,v) = { r with northWestButton = v }
                    override x.Update(r,f) = { r with northWestButton = f r.northWestButton }
                }
            let northEastButton =
                { new Lens<Svgplus.RectangleType.Rectangle, Svgplus.Button>() with
                    override x.Get(r) = r.northEastButton
                    override x.Set(r,v) = { r with northEastButton = v }
                    override x.Update(r,f) = { r with northEastButton = f r.northEastButton }
                }
            let southWestButton =
                { new Lens<Svgplus.RectangleType.Rectangle, Svgplus.Button>() with
                    override x.Get(r) = r.southWestButton
                    override x.Set(r,v) = { r with southWestButton = v }
                    override x.Update(r,f) = { r with southWestButton = f r.southWestButton }
                }
            let southEastButton =
                { new Lens<Svgplus.RectangleType.Rectangle, Svgplus.Button>() with
                    override x.Get(r) = r.southEastButton
                    override x.Set(r,v) = { r with southEastButton = v }
                    override x.Update(r,f) = { r with southEastButton = f r.southEastButton }
                }
