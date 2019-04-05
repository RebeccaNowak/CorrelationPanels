namespace CorrelationDrawing.Types

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.Types

[<AutoOpen>]
module Mutable =

    
    
    type MStyle(__initial : CorrelationDrawing.Types.Style) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Types.Style> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Types.Style>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Types.Style>
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        
        member x.color = _color
        member x.thickness = _thickness
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Types.Style) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                Aardvark.UI.Mutable.MNumericInput.Update(_thickness, v.thickness)
                
        
        static member Create(__initial : CorrelationDrawing.Types.Style) : MStyle = MStyle(__initial)
        static member Update(m : MStyle, v : CorrelationDrawing.Types.Style) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Types.Style> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Style =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let color =
                { new Lens<CorrelationDrawing.Types.Style, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let thickness =
                { new Lens<CorrelationDrawing.Types.Style, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.thickness
                    override x.Set(r,v) = { r with thickness = v }
                    override x.Update(r,f) = { r with thickness = f r.thickness }
                }
