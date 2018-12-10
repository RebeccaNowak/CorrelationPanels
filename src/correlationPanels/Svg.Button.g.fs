namespace CorrelationDrawing.Svg

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.Svg

[<AutoOpen>]
module Mutable =

    
    
    type MButton(__initial : CorrelationDrawing.Svg.Button) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Svg.Button> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Svg.Button>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Svg.Button>
        let _pos = ResetMod.Create(__initial.pos)
        let _radius = ResetMod.Create(__initial.radius)
        let _stroke = ResetMod.Create(__initial.stroke)
        let _color = ResetMod.Create(__initial.color)
        let _fill = ResetMod.Create(__initial.fill)
        
        member x.pos = _pos :> IMod<_>
        member x.radius = _radius :> IMod<_>
        member x.stroke = _stroke :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.fill = _fill :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Svg.Button) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_pos,v.pos)
                ResetMod.Update(_radius,v.radius)
                ResetMod.Update(_stroke,v.stroke)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_fill,v.fill)
                
        
        static member Create(__initial : CorrelationDrawing.Svg.Button) : MButton = MButton(__initial)
        static member Update(m : MButton, v : CorrelationDrawing.Svg.Button) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Svg.Button> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Button =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let pos =
                { new Lens<CorrelationDrawing.Svg.Button, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.pos
                    override x.Set(r,v) = { r with pos = v }
                    override x.Update(r,f) = { r with pos = f r.pos }
                }
            let radius =
                { new Lens<CorrelationDrawing.Svg.Button, System.Double>() with
                    override x.Get(r) = r.radius
                    override x.Set(r,v) = { r with radius = v }
                    override x.Update(r,f) = { r with radius = f r.radius }
                }
            let stroke =
                { new Lens<CorrelationDrawing.Svg.Button, System.Double>() with
                    override x.Get(r) = r.stroke
                    override x.Set(r,v) = { r with stroke = v }
                    override x.Update(r,f) = { r with stroke = f r.stroke }
                }
            let color =
                { new Lens<CorrelationDrawing.Svg.Button, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let fill =
                { new Lens<CorrelationDrawing.Svg.Button, System.Boolean>() with
                    override x.Get(r) = r.fill
                    override x.Set(r,v) = { r with fill = v }
                    override x.Update(r,f) = { r with fill = f r.fill }
                }
