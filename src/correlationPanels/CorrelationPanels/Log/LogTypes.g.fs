namespace CorrelationDrawing.LogTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.LogTypes

[<AutoOpen>]
module Mutable =

    
    
    type MGeologicalLog(__initial : CorrelationDrawing.LogTypes.GeologicalLog) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogTypes.GeologicalLog> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogTypes.GeologicalLog>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogTypes.GeologicalLog>
        let _state = ResetMod.Create(__initial.state)
        let _defaultWidth = ResetMod.Create(__initial.defaultWidth)
        let _nodes = MList.Create(__initial.nodes, (fun v -> CorrelationDrawing.LogNodeTypes.Mutable.MLogNode.Create(v)), (fun (m,v) -> CorrelationDrawing.LogNodeTypes.Mutable.MLogNode.Update(m, v)), (fun v -> v))
        let _annoPoints = MMap.Create(__initial.annoPoints)
        
        member x.id = __current.Value.id
        member x.diagramRef = __current.Value.diagramRef
        member x.state = _state :> IMod<_>
        member x.defaultWidth = _defaultWidth :> IMod<_>
        member x.nodes = _nodes :> alist<_>
        member x.annoPoints = _annoPoints :> amap<_,_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogTypes.GeologicalLog) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_state,v.state)
                ResetMod.Update(_defaultWidth,v.defaultWidth)
                MList.Update(_nodes, v.nodes)
                MMap.Update(_annoPoints, v.annoPoints)
                
        
        static member Create(__initial : CorrelationDrawing.LogTypes.GeologicalLog) : MGeologicalLog = MGeologicalLog(__initial)
        static member Update(m : MGeologicalLog, v : CorrelationDrawing.LogTypes.GeologicalLog) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogTypes.GeologicalLog> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GeologicalLog =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.LogTypes.GeologicalLog, Svgplus.RectangleStackTypes.RectangleStackId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let diagramRef =
                { new Lens<CorrelationDrawing.LogTypes.GeologicalLog, CorrelationDrawing.LogTypes.LogDiagramReferences>() with
                    override x.Get(r) = r.diagramRef
                    override x.Set(r,v) = { r with diagramRef = v }
                    override x.Update(r,f) = { r with diagramRef = f r.diagramRef }
                }
            let state =
                { new Lens<CorrelationDrawing.LogTypes.GeologicalLog, CorrelationDrawing.Types.State>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let defaultWidth =
                { new Lens<CorrelationDrawing.LogTypes.GeologicalLog, System.Double>() with
                    override x.Get(r) = r.defaultWidth
                    override x.Set(r,v) = { r with defaultWidth = v }
                    override x.Update(r,f) = { r with defaultWidth = f r.defaultWidth }
                }
            let nodes =
                { new Lens<CorrelationDrawing.LogTypes.GeologicalLog, Aardvark.Base.plist<CorrelationDrawing.LogNodeTypes.LogNode>>() with
                    override x.Get(r) = r.nodes
                    override x.Set(r,v) = { r with nodes = v }
                    override x.Update(r,f) = { r with nodes = f r.nodes }
                }
            let annoPoints =
                { new Lens<CorrelationDrawing.LogTypes.GeologicalLog, Aardvark.Base.hmap<CorrelationDrawing.AnnotationTypes.AnnotationId,Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.annoPoints
                    override x.Set(r,v) = { r with annoPoints = v }
                    override x.Update(r,f) = { r with annoPoints = f r.annoPoints }
                }
