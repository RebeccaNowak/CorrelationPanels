namespace Svgplus.AxesTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.AxesTypes

[<AutoOpen>]
module Mutable =

    
    
    type MAxisApp(__initial : Svgplus.AxesTypes.AxisApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.AxesTypes.AxisApp> = Aardvark.Base.Incremental.EqModRef<Svgplus.AxesTypes.AxisApp>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.AxesTypes.AxisApp>
        let _positionTop = ResetMod.Create(__initial.positionTop)
        let _weight = ResetMod.Create(__initial.weight)
        let _step = ResetMod.Create(__initial.step)
        let _label = ResetMod.Create(__initial.label)
        let _defaultGranularity = ResetMod.Create(__initial.defaultGranularity)
        let _yMapping = ResetMod.Create(__initial.yMapping)
        let _nativeRange = ResetMod.Create(__initial.nativeRange)
        let _fontSize = ResetMod.Create(__initial.fontSize)
        let _draw = ResetMod.Create(__initial.draw)
        
        member x.positionTop = _positionTop :> IMod<_>
        member x.weight = _weight :> IMod<_>
        member x.step = _step :> IMod<_>
        member x.label = _label :> IMod<_>
        member x.defaultGranularity = _defaultGranularity :> IMod<_>
        member x.yMapping = _yMapping :> IMod<_>
        member x.nativeRange = _nativeRange :> IMod<_>
        member x.fontSize = _fontSize :> IMod<_>
        member x.draw = _draw :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.AxesTypes.AxisApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_positionTop,v.positionTop)
                ResetMod.Update(_weight,v.weight)
                ResetMod.Update(_step,v.step)
                ResetMod.Update(_label,v.label)
                ResetMod.Update(_defaultGranularity,v.defaultGranularity)
                ResetMod.Update(_yMapping,v.yMapping)
                ResetMod.Update(_nativeRange,v.nativeRange)
                ResetMod.Update(_fontSize,v.fontSize)
                ResetMod.Update(_draw,v.draw)
                
        
        static member Create(__initial : Svgplus.AxesTypes.AxisApp) : MAxisApp = MAxisApp(__initial)
        static member Update(m : MAxisApp, v : Svgplus.AxesTypes.AxisApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.AxesTypes.AxisApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AxisApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let positionTop =
                { new Lens<Svgplus.AxesTypes.AxisApp, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.positionTop
                    override x.Set(r,v) = { r with positionTop = v }
                    override x.Update(r,f) = { r with positionTop = f r.positionTop }
                }
            let weight =
                { new Lens<Svgplus.AxesTypes.AxisApp, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
            let step =
                { new Lens<Svgplus.AxesTypes.AxisApp, System.Double>() with
                    override x.Get(r) = r.step
                    override x.Set(r,v) = { r with step = v }
                    override x.Update(r,f) = { r with step = f r.step }
                }
            let label =
                { new Lens<Svgplus.AxesTypes.AxisApp, System.String>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let defaultGranularity =
                { new Lens<Svgplus.AxesTypes.AxisApp, System.Double>() with
                    override x.Get(r) = r.defaultGranularity
                    override x.Set(r,v) = { r with defaultGranularity = v }
                    override x.Update(r,f) = { r with defaultGranularity = f r.defaultGranularity }
                }
            let yMapping =
                { new Lens<Svgplus.AxesTypes.AxisApp, System.Double -> System.Double>() with
                    override x.Get(r) = r.yMapping
                    override x.Set(r,v) = { r with yMapping = v }
                    override x.Update(r,f) = { r with yMapping = f r.yMapping }
                }
            let nativeRange =
                { new Lens<Svgplus.AxesTypes.AxisApp, SimpleTypes.Rangef>() with
                    override x.Get(r) = r.nativeRange
                    override x.Set(r,v) = { r with nativeRange = v }
                    override x.Update(r,f) = { r with nativeRange = f r.nativeRange }
                }
            let fontSize =
                { new Lens<Svgplus.AxesTypes.AxisApp, Svgplus.FontSize>() with
                    override x.Get(r) = r.fontSize
                    override x.Set(r,v) = { r with fontSize = v }
                    override x.Update(r,f) = { r with fontSize = f r.fontSize }
                }
            let draw =
                { new Lens<Svgplus.AxesTypes.AxisApp, System.Boolean>() with
                    override x.Get(r) = r.draw
                    override x.Set(r,v) = { r with draw = v }
                    override x.Update(r,f) = { r with draw = f r.draw }
                }
