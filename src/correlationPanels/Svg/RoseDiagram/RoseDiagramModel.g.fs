namespace Svgplus.RoseDiagramModel

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.RoseDiagramModel

[<AutoOpen>]
module Mutable =

    
    
    type MRoseDiagram(__initial : Svgplus.RoseDiagramModel.RoseDiagram) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.RoseDiagramModel.RoseDiagram> = Aardvark.Base.Incremental.EqModRef<Svgplus.RoseDiagramModel.RoseDiagram>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.RoseDiagramModel.RoseDiagram>
        let _centre = ResetMod.Create(__initial.centre)
        let _outerRadius = ResetMod.Create(__initial.outerRadius)
        let _innerRadius = ResetMod.Create(__initial.innerRadius)
        let _colour = ResetMod.Create(__initial.colour)
        let _nrCircles = ResetMod.Create(__initial.nrCircles)
        let _weight = ResetMod.Create(__initial.weight)
        let _countPerBin = MList.Create(__initial.countPerBin)
        
        member x.id = __current.Value.id
        member x.centre = _centre :> IMod<_>
        member x.outerRadius = _outerRadius :> IMod<_>
        member x.innerRadius = _innerRadius :> IMod<_>
        member x.colour = _colour :> IMod<_>
        member x.nrCircles = _nrCircles :> IMod<_>
        member x.weight = _weight :> IMod<_>
        member x.countPerBin = _countPerBin :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.RoseDiagramModel.RoseDiagram) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_centre,v.centre)
                ResetMod.Update(_outerRadius,v.outerRadius)
                ResetMod.Update(_innerRadius,v.innerRadius)
                ResetMod.Update(_colour,v.colour)
                ResetMod.Update(_nrCircles,v.nrCircles)
                ResetMod.Update(_weight,v.weight)
                MList.Update(_countPerBin, v.countPerBin)
                
        
        static member Create(__initial : Svgplus.RoseDiagramModel.RoseDiagram) : MRoseDiagram = MRoseDiagram(__initial)
        static member Update(m : MRoseDiagram, v : Svgplus.RoseDiagramModel.RoseDiagram) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.RoseDiagramModel.RoseDiagram> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RoseDiagram =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, Svgplus.RoseDiagramModel.RoseDiagramId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let centre =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.centre
                    override x.Set(r,v) = { r with centre = v }
                    override x.Update(r,f) = { r with centre = f r.centre }
                }
            let outerRadius =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, System.Double>() with
                    override x.Get(r) = r.outerRadius
                    override x.Set(r,v) = { r with outerRadius = v }
                    override x.Update(r,f) = { r with outerRadius = f r.outerRadius }
                }
            let innerRadius =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, System.Double>() with
                    override x.Get(r) = r.innerRadius
                    override x.Set(r,v) = { r with innerRadius = v }
                    override x.Update(r,f) = { r with innerRadius = f r.innerRadius }
                }
            let colour =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, Microsoft.FSharp.Collections.List<Aardvark.Base.C4b>>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let nrCircles =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, System.Int32>() with
                    override x.Get(r) = r.nrCircles
                    override x.Set(r,v) = { r with nrCircles = v }
                    override x.Update(r,f) = { r with nrCircles = f r.nrCircles }
                }
            let weight =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
            let countPerBin =
                { new Lens<Svgplus.RoseDiagramModel.RoseDiagram, Aardvark.Base.plist<Svgplus.Bin>>() with
                    override x.Get(r) = r.countPerBin
                    override x.Set(r,v) = { r with countPerBin = v }
                    override x.Update(r,f) = { r with countPerBin = f r.countPerBin }
                }
