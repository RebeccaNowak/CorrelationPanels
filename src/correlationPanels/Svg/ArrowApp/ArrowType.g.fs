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
        let _direction = ResetMod.Create(__initial.direction)
        let _length = ResetMod.Create(__initial.length)
        let _colour = ResetMod.Create(__initial.colour)
        
        member x.direction = _direction :> IMod<_>
        member x.length = _length :> IMod<_>
        member x.colour = _colour :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.ArrowType.Arrow) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_direction,v.direction)
                ResetMod.Update(_length,v.length)
                ResetMod.Update(_colour,v.colour)
                
        
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
            let colour =
                { new Lens<Svgplus.ArrowType.Arrow, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
