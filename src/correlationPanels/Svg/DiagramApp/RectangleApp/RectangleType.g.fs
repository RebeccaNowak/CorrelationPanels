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
        let _needsLayoutingX = ResetMod.Create(__initial.needsLayoutingX)
        let _needsLayoutingY = ResetMod.Create(__initial.needsLayoutingY)
        let _drawLabel = ResetMod.Create(__initial.drawLabel)
        let _label = UIPlus.Mutable.MTextInput.Create(__initial.label)
        let _pos = ResetMod.Create(__initial.pos)
        let _dim = ResetMod.Create(__initial.dim)
        let _fixedWidth = MOption.Create(__initial.fixedWidth)
        let _colour = Aardvark.UI.Mutable.MColorInput.Create(__initial.colour)
        let _lowerBorderColour = ResetMod.Create(__initial.lowerBorderColour)
        let _upperBorderColour = ResetMod.Create(__initial.upperBorderColour)
        let _overwriteColour = MOption.Create(__initial.overwriteColour)
        let _isToggled = ResetMod.Create(__initial.isToggled)
        let _colChange = ResetMod.Create(__initial.colChange)
        let _isHovering = ResetMod.Create(__initial.isHovering)
        let _dottedBorder = ResetMod.Create(__initial.dottedBorder)
        let _draw = ResetMod.Create(__initial.draw)
        let _drawButtons = ResetMod.Create(__initial.drawButtons)
        let _northWestButton = Svgplus.Mutable.MButton.Create(__initial.northWestButton)
        let _northEastButton = Svgplus.Mutable.MButton.Create(__initial.northEastButton)
        let _southWestButton = Svgplus.Mutable.MButton.Create(__initial.southWestButton)
        let _southEastButton = Svgplus.Mutable.MButton.Create(__initial.southEastButton)
        let _svgYAxisLabel = Svgplus.TextType.Mutable.MText.Create(__initial.svgYAxisLabel)
        
        member x.id = __current.Value.id
        member x.needsLayoutingX = _needsLayoutingX :> IMod<_>
        member x.needsLayoutingY = _needsLayoutingY :> IMod<_>
        member x.drawLabel = _drawLabel :> IMod<_>
        member x.label = _label
        member x.pos = _pos :> IMod<_>
        member x.dim = _dim :> IMod<_>
        member x.fixedWidth = _fixedWidth :> IMod<_>
        member x.colour = _colour
        member x.lowerBorderColour = _lowerBorderColour :> IMod<_>
        member x.upperBorderColour = _upperBorderColour :> IMod<_>
        member x.overwriteColour = _overwriteColour :> IMod<_>
        member x.isToggled = _isToggled :> IMod<_>
        member x.colChange = _colChange :> IMod<_>
        member x.isHovering = _isHovering :> IMod<_>
        member x.dottedBorder = _dottedBorder :> IMod<_>
        member x.draw = _draw :> IMod<_>
        member x.drawButtons = _drawButtons :> IMod<_>
        member x.northWestButton = _northWestButton
        member x.northEastButton = _northEastButton
        member x.southWestButton = _southWestButton
        member x.southEastButton = _southEastButton
        member x.svgYAxisLabel = _svgYAxisLabel
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.RectangleType.Rectangle) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_needsLayoutingX,v.needsLayoutingX)
                ResetMod.Update(_needsLayoutingY,v.needsLayoutingY)
                ResetMod.Update(_drawLabel,v.drawLabel)
                UIPlus.Mutable.MTextInput.Update(_label, v.label)
                ResetMod.Update(_pos,v.pos)
                ResetMod.Update(_dim,v.dim)
                MOption.Update(_fixedWidth, v.fixedWidth)
                Aardvark.UI.Mutable.MColorInput.Update(_colour, v.colour)
                ResetMod.Update(_lowerBorderColour,v.lowerBorderColour)
                ResetMod.Update(_upperBorderColour,v.upperBorderColour)
                MOption.Update(_overwriteColour, v.overwriteColour)
                ResetMod.Update(_isToggled,v.isToggled)
                ResetMod.Update(_colChange,v.colChange)
                ResetMod.Update(_isHovering,v.isHovering)
                ResetMod.Update(_dottedBorder,v.dottedBorder)
                ResetMod.Update(_draw,v.draw)
                ResetMod.Update(_drawButtons,v.drawButtons)
                Svgplus.Mutable.MButton.Update(_northWestButton, v.northWestButton)
                Svgplus.Mutable.MButton.Update(_northEastButton, v.northEastButton)
                Svgplus.Mutable.MButton.Update(_southWestButton, v.southWestButton)
                Svgplus.Mutable.MButton.Update(_southEastButton, v.southEastButton)
                Svgplus.TextType.Mutable.MText.Update(_svgYAxisLabel, v.svgYAxisLabel)
                
        
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
            let needsLayoutingX =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.needsLayoutingX
                    override x.Set(r,v) = { r with needsLayoutingX = v }
                    override x.Update(r,f) = { r with needsLayoutingX = f r.needsLayoutingX }
                }
            let needsLayoutingY =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.needsLayoutingY
                    override x.Set(r,v) = { r with needsLayoutingY = v }
                    override x.Update(r,f) = { r with needsLayoutingY = f r.needsLayoutingY }
                }
            let drawLabel =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.drawLabel
                    override x.Set(r,v) = { r with drawLabel = v }
                    override x.Update(r,f) = { r with drawLabel = f r.drawLabel }
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
            let fixedWidth =
                { new Lens<Svgplus.RectangleType.Rectangle, Microsoft.FSharp.Core.Option<System.Double>>() with
                    override x.Get(r) = r.fixedWidth
                    override x.Set(r,v) = { r with fixedWidth = v }
                    override x.Update(r,f) = { r with fixedWidth = f r.fixedWidth }
                }
            let colour =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let lowerBorderColour =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.lowerBorderColour
                    override x.Set(r,v) = { r with lowerBorderColour = v }
                    override x.Update(r,f) = { r with lowerBorderColour = f r.lowerBorderColour }
                }
            let upperBorderColour =
                { new Lens<Svgplus.RectangleType.Rectangle, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.upperBorderColour
                    override x.Set(r,v) = { r with upperBorderColour = v }
                    override x.Update(r,f) = { r with upperBorderColour = f r.upperBorderColour }
                }
            let overwriteColour =
                { new Lens<Svgplus.RectangleType.Rectangle, Microsoft.FSharp.Core.Option<Aardvark.Base.C4b>>() with
                    override x.Get(r) = r.overwriteColour
                    override x.Set(r,v) = { r with overwriteColour = v }
                    override x.Update(r,f) = { r with overwriteColour = f r.overwriteColour }
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
            let drawButtons =
                { new Lens<Svgplus.RectangleType.Rectangle, System.Boolean>() with
                    override x.Get(r) = r.drawButtons
                    override x.Set(r,v) = { r with drawButtons = v }
                    override x.Update(r,f) = { r with drawButtons = f r.drawButtons }
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
            let svgYAxisLabel =
                { new Lens<Svgplus.RectangleType.Rectangle, Svgplus.TextType.Text>() with
                    override x.Get(r) = r.svgYAxisLabel
                    override x.Set(r,v) = { r with svgYAxisLabel = v }
                    override x.Update(r,f) = { r with svgYAxisLabel = f r.svgYAxisLabel }
                }
