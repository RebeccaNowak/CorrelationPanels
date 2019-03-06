namespace Svgplus.TextType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.TextType

[<AutoOpen>]
module Mutable =

    
    
    type MText(__initial : Svgplus.TextType.Text) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.TextType.Text> = Aardvark.Base.Incremental.EqModRef<Svgplus.TextType.Text>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.TextType.Text>
        let _centre = ResetMod.Create(__initial.centre)
        let _dim = ResetMod.Create(__initial.dim)
        let _textInput = UIPlus.Mutable.MTextInput.Create(__initial.textInput)
        let _bold = ResetMod.Create(__initial.bold)
        let _onEnter = ResetMod.Create(__initial.onEnter)
        let _onLeave = ResetMod.Create(__initial.onLeave)
        let _fontSize = ResetMod.Create(__initial.fontSize)
        
        member x.centre = _centre :> IMod<_>
        member x.dim = _dim :> IMod<_>
        member x.textInput = _textInput
        member x.bold = _bold :> IMod<_>
        member x.onEnter = _onEnter :> IMod<_>
        member x.onLeave = _onLeave :> IMod<_>
        member x.fontSize = _fontSize :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.TextType.Text) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_centre,v.centre)
                ResetMod.Update(_dim,v.dim)
                UIPlus.Mutable.MTextInput.Update(_textInput, v.textInput)
                ResetMod.Update(_bold,v.bold)
                ResetMod.Update(_onEnter,v.onEnter)
                ResetMod.Update(_onLeave,v.onLeave)
                ResetMod.Update(_fontSize,v.fontSize)
                
        
        static member Create(__initial : Svgplus.TextType.Text) : MText = MText(__initial)
        static member Update(m : MText, v : Svgplus.TextType.Text) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.TextType.Text> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Text =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let centre =
                { new Lens<Svgplus.TextType.Text, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.centre
                    override x.Set(r,v) = { r with centre = v }
                    override x.Update(r,f) = { r with centre = f r.centre }
                }
            let dim =
                { new Lens<Svgplus.TextType.Text, SimpleTypes.Size2D>() with
                    override x.Get(r) = r.dim
                    override x.Set(r,v) = { r with dim = v }
                    override x.Update(r,f) = { r with dim = f r.dim }
                }
            let textInput =
                { new Lens<Svgplus.TextType.Text, UIPlus.TextInput>() with
                    override x.Get(r) = r.textInput
                    override x.Set(r,v) = { r with textInput = v }
                    override x.Update(r,f) = { r with textInput = f r.textInput }
                }
            let bold =
                { new Lens<Svgplus.TextType.Text, System.Boolean>() with
                    override x.Get(r) = r.bold
                    override x.Set(r,v) = { r with bold = v }
                    override x.Update(r,f) = { r with bold = f r.bold }
                }
            let onEnter =
                { new Lens<Svgplus.TextType.Text, Svgplus.TextType.Text -> Svgplus.TextType.Text>() with
                    override x.Get(r) = r.onEnter
                    override x.Set(r,v) = { r with onEnter = v }
                    override x.Update(r,f) = { r with onEnter = f r.onEnter }
                }
            let onLeave =
                { new Lens<Svgplus.TextType.Text, Svgplus.TextType.Text -> Svgplus.TextType.Text>() with
                    override x.Get(r) = r.onLeave
                    override x.Set(r,v) = { r with onLeave = v }
                    override x.Update(r,f) = { r with onLeave = f r.onLeave }
                }
            let fontSize =
                { new Lens<Svgplus.TextType.Text, Svgplus.FontSize>() with
                    override x.Get(r) = r.fontSize
                    override x.Set(r,v) = { r with fontSize = v }
                    override x.Update(r,f) = { r with fontSize = f r.fontSize }
                }
