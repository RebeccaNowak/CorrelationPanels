namespace CorrelationDrawing.CorrelationTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.CorrelationTypes

[<AutoOpen>]
module Mutable =

    
    
    type MCorrelation(__initial : CorrelationDrawing.CorrelationTypes.Correlation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationTypes.Correlation> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationTypes.Correlation>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationTypes.Correlation>
        let _fromBorder = CorrelationDrawing.LogNodeTypes.Mutable.MBorder.Create(__initial.fromBorder)
        let _toBorder = CorrelationDrawing.LogNodeTypes.Mutable.MBorder.Create(__initial.toBorder)
        
        member x.fromBorder = _fromBorder
        member x.toBorder = _toBorder
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationTypes.Correlation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                CorrelationDrawing.LogNodeTypes.Mutable.MBorder.Update(_fromBorder, v.fromBorder)
                CorrelationDrawing.LogNodeTypes.Mutable.MBorder.Update(_toBorder, v.toBorder)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationTypes.Correlation) : MCorrelation = MCorrelation(__initial)
        static member Update(m : MCorrelation, v : CorrelationDrawing.CorrelationTypes.Correlation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationTypes.Correlation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Correlation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let fromBorder =
                { new Lens<CorrelationDrawing.CorrelationTypes.Correlation, CorrelationDrawing.LogNodeTypes.Border>() with
                    override x.Get(r) = r.fromBorder
                    override x.Set(r,v) = { r with fromBorder = v }
                    override x.Update(r,f) = { r with fromBorder = f r.fromBorder }
                }
            let toBorder =
                { new Lens<CorrelationDrawing.CorrelationTypes.Correlation, CorrelationDrawing.LogNodeTypes.Border>() with
                    override x.Get(r) = r.toBorder
                    override x.Set(r,v) = { r with toBorder = v }
                    override x.Update(r,f) = { r with toBorder = f r.toBorder }
                }
