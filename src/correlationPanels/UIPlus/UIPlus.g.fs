namespace UIPlus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open UIPlus

[<AutoOpen>]
module Mutable =

    
    
    type MArrowButton(__initial : UIPlus.ArrowButton) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.ArrowButton> = Aardvark.Base.Incremental.EqModRef<UIPlus.ArrowButton>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.ArrowButton>
        let _direction = ResetMod.Create(__initial.direction)
        let _size = ResetMod.Create(__initial.size)
        
        member x.id = __current.Value.id
        member x.direction = _direction :> IMod<_>
        member x.size = _size :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.ArrowButton) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_direction,v.direction)
                ResetMod.Update(_size,v.size)
                
        
        static member Create(__initial : UIPlus.ArrowButton) : MArrowButton = MArrowButton(__initial)
        static member Update(m : MArrowButton, v : UIPlus.ArrowButton) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.ArrowButton> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArrowButton =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<UIPlus.ArrowButton, UIPlus.ArrowButtonId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let direction =
                { new Lens<UIPlus.ArrowButton, UIPlus.Direction>() with
                    override x.Get(r) = r.direction
                    override x.Set(r,v) = { r with direction = v }
                    override x.Update(r,f) = { r with direction = f r.direction }
                }
            let size =
                { new Lens<UIPlus.ArrowButton, UIPlus.Size>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
