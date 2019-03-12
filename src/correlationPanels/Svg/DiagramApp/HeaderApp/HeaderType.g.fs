namespace Svgplus.HeaderType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.HeaderType

[<AutoOpen>]
module Mutable =

    
    
    type MHeader(__initial : Svgplus.HeaderType.Header) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.HeaderType.Header> = Aardvark.Base.Incremental.EqModRef<Svgplus.HeaderType.Header>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.HeaderType.Header>
        let _centre = ResetMod.Create(__initial.centre)
        let _dim = ResetMod.Create(__initial.dim)
        let _label = Svgplus.TextType.Mutable.MText.Create(__initial.label)
        let _leftButton = Svgplus.ArrowType.Mutable.MArrow.Create(__initial.leftButton)
        let _rightButton = Svgplus.ArrowType.Mutable.MArrow.Create(__initial.rightButton)
        
        member x.centre = _centre :> IMod<_>
        member x.dim = _dim :> IMod<_>
        member x.label = _label
        member x.leftButton = _leftButton
        member x.rightButton = _rightButton
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.HeaderType.Header) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_centre,v.centre)
                ResetMod.Update(_dim,v.dim)
                Svgplus.TextType.Mutable.MText.Update(_label, v.label)
                Svgplus.ArrowType.Mutable.MArrow.Update(_leftButton, v.leftButton)
                Svgplus.ArrowType.Mutable.MArrow.Update(_rightButton, v.rightButton)
                
        
        static member Create(__initial : Svgplus.HeaderType.Header) : MHeader = MHeader(__initial)
        static member Update(m : MHeader, v : Svgplus.HeaderType.Header) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.HeaderType.Header> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Header =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let centre =
                { new Lens<Svgplus.HeaderType.Header, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.centre
                    override x.Set(r,v) = { r with centre = v }
                    override x.Update(r,f) = { r with centre = f r.centre }
                }
            let dim =
                { new Lens<Svgplus.HeaderType.Header, SimpleTypes.Size2D>() with
                    override x.Get(r) = r.dim
                    override x.Set(r,v) = { r with dim = v }
                    override x.Update(r,f) = { r with dim = f r.dim }
                }
            let label =
                { new Lens<Svgplus.HeaderType.Header, Svgplus.TextType.Text>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let leftButton =
                { new Lens<Svgplus.HeaderType.Header, Svgplus.ArrowType.Arrow>() with
                    override x.Get(r) = r.leftButton
                    override x.Set(r,v) = { r with leftButton = v }
                    override x.Update(r,f) = { r with leftButton = f r.leftButton }
                }
            let rightButton =
                { new Lens<Svgplus.HeaderType.Header, Svgplus.ArrowType.Arrow>() with
                    override x.Get(r) = r.rightButton
                    override x.Set(r,v) = { r with rightButton = v }
                    override x.Update(r,f) = { r with rightButton = f r.rightButton }
                }
