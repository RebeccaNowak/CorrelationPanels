namespace CorrelationDrawing.LogNodeTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.LogNodeTypes

[<AutoOpen>]
module Mutable =

    
    
    type MBorder(__initial : CorrelationDrawing.LogNodeTypes.Border) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNodeTypes.Border> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogNodeTypes.Border>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNodeTypes.Border>
        let _nodeId = ResetMod.Create(__initial.nodeId)
        let _logId = ResetMod.Create(__initial.logId)
        let _isSelected = ResetMod.Create(__initial.isSelected)
        let _correlation = MOption.Create(__initial.correlation)
        let _annotationId = ResetMod.Create(__initial.annotationId)
        let _point = ResetMod.Create(__initial.point)
        let _color = ResetMod.Create(__initial.color)
        let _weight = ResetMod.Create(__initial.weight)
        let _svgPosition = ResetMod.Create(__initial.svgPosition)
        
        member x.id = __current.Value.id
        member x.nodeId = _nodeId :> IMod<_>
        member x.logId = _logId :> IMod<_>
        member x.isSelected = _isSelected :> IMod<_>
        member x.correlation = _correlation :> IMod<_>
        member x.annotationId = _annotationId :> IMod<_>
        member x.point = _point :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.weight = _weight :> IMod<_>
        member x.svgPosition = _svgPosition :> IMod<_>
        member x.borderType = __current.Value.borderType
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogNodeTypes.Border) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_nodeId,v.nodeId)
                ResetMod.Update(_logId,v.logId)
                ResetMod.Update(_isSelected,v.isSelected)
                MOption.Update(_correlation, v.correlation)
                ResetMod.Update(_annotationId,v.annotationId)
                ResetMod.Update(_point,v.point)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_weight,v.weight)
                ResetMod.Update(_svgPosition,v.svgPosition)
                
        
        static member Create(__initial : CorrelationDrawing.LogNodeTypes.Border) : MBorder = MBorder(__initial)
        static member Update(m : MBorder, v : CorrelationDrawing.LogNodeTypes.Border) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogNodeTypes.Border> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Border =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, CorrelationDrawing.LogNodeTypes.BorderId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let nodeId =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, CorrelationDrawing.LogNodeTypes.LogNodeId>() with
                    override x.Get(r) = r.nodeId
                    override x.Set(r,v) = { r with nodeId = v }
                    override x.Update(r,f) = { r with nodeId = f r.nodeId }
                }
            let logId =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, Svgplus.RectangleStackTypes.RectangleStackId>() with
                    override x.Get(r) = r.logId
                    override x.Set(r,v) = { r with logId = v }
                    override x.Update(r,f) = { r with logId = f r.logId }
                }
            let isSelected =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, System.Boolean>() with
                    override x.Get(r) = r.isSelected
                    override x.Set(r,v) = { r with isSelected = v }
                    override x.Update(r,f) = { r with isSelected = f r.isSelected }
                }
            let correlation =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, Microsoft.FSharp.Core.Option<CorrelationDrawing.LogNodeTypes.BorderId>>() with
                    override x.Get(r) = r.correlation
                    override x.Set(r,v) = { r with correlation = v }
                    override x.Update(r,f) = { r with correlation = f r.correlation }
                }
            let annotationId =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, CorrelationDrawing.AnnotationTypes.AnnotationId>() with
                    override x.Get(r) = r.annotationId
                    override x.Set(r,v) = { r with annotationId = v }
                    override x.Update(r,f) = { r with annotationId = f r.annotationId }
                }
            let point =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.point
                    override x.Set(r,v) = { r with point = v }
                    override x.Update(r,f) = { r with point = f r.point }
                }
            let color =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let weight =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
            let svgPosition =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.svgPosition
                    override x.Set(r,v) = { r with svgPosition = v }
                    override x.Update(r,f) = { r with svgPosition = f r.svgPosition }
                }
            let borderType =
                { new Lens<CorrelationDrawing.LogNodeTypes.Border, CorrelationDrawing.LogNodeTypes.BorderType>() with
                    override x.Get(r) = r.borderType
                    override x.Set(r,v) = { r with borderType = v }
                    override x.Update(r,f) = { r with borderType = f r.borderType }
                }
    
    
    type MLogNode(__initial : CorrelationDrawing.LogNodeTypes.LogNode) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNodeTypes.LogNode> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogNodeTypes.LogNode>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNodeTypes.LogNode>
        let _logId = ResetMod.Create(__initial.logId)
        let _nodeType = ResetMod.Create(__initial.nodeType)
        let _level = ResetMod.Create(__initial.level)
        let _lBorder = MOption.Create(__initial.lBorder, (fun v -> MBorder.Create(v)), (fun (m,v) -> MBorder.Update(m, v)), (fun v -> v))
        let _uBorder = MOption.Create(__initial.uBorder, (fun v -> MBorder.Create(v)), (fun (m,v) -> MBorder.Update(m, v)), (fun v -> v))
        let _annotation = MOption.Create(__initial.annotation)
        let _children = MList.Create(__initial.children, (fun v -> MLogNode.Create(v)), (fun (m,v) -> MLogNode.Update(m, v)), (fun v -> v))
        let _mainBody = Svgplus.RectangleType.Mutable.MRectangle.Create(__initial.mainBody)
        
        member x.id = __current.Value.id
        member x.rectangleId = __current.Value.rectangleId
        member x.logId = _logId :> IMod<_>
        member x.nodeType = _nodeType :> IMod<_>
        member x.level = _level :> IMod<_>
        member x.lBorder = _lBorder :> IMod<_>
        member x.uBorder = _uBorder :> IMod<_>
        member x.annotation = _annotation :> IMod<_>
        member x.children = _children :> alist<_>
        member x.mainBody = _mainBody
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogNodeTypes.LogNode) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_logId,v.logId)
                ResetMod.Update(_nodeType,v.nodeType)
                ResetMod.Update(_level,v.level)
                MOption.Update(_lBorder, v.lBorder)
                MOption.Update(_uBorder, v.uBorder)
                MOption.Update(_annotation, v.annotation)
                MList.Update(_children, v.children)
                Svgplus.RectangleType.Mutable.MRectangle.Update(_mainBody, v.mainBody)
                
        
        static member Create(__initial : CorrelationDrawing.LogNodeTypes.LogNode) : MLogNode = MLogNode(__initial)
        static member Update(m : MLogNode, v : CorrelationDrawing.LogNodeTypes.LogNode) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogNodeTypes.LogNode> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LogNode =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, CorrelationDrawing.LogNodeTypes.LogNodeId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let rectangleId =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Svgplus.RectangleType.RectangleId>() with
                    override x.Get(r) = r.rectangleId
                    override x.Set(r,v) = { r with rectangleId = v }
                    override x.Update(r,f) = { r with rectangleId = f r.rectangleId }
                }
            let logId =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Svgplus.RectangleStackTypes.RectangleStackId>() with
                    override x.Get(r) = r.logId
                    override x.Set(r,v) = { r with logId = v }
                    override x.Update(r,f) = { r with logId = f r.logId }
                }
            let nodeType =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, CorrelationDrawing.Types.LogNodeType>() with
                    override x.Get(r) = r.nodeType
                    override x.Set(r,v) = { r with nodeType = v }
                    override x.Update(r,f) = { r with nodeType = f r.nodeType }
                }
            let level =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, CorrelationDrawing.Types.NodeLevel>() with
                    override x.Get(r) = r.level
                    override x.Set(r,v) = { r with level = v }
                    override x.Update(r,f) = { r with level = f r.level }
                }
            let lBorder =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Microsoft.FSharp.Core.Option<CorrelationDrawing.LogNodeTypes.Border>>() with
                    override x.Get(r) = r.lBorder
                    override x.Set(r,v) = { r with lBorder = v }
                    override x.Update(r,f) = { r with lBorder = f r.lBorder }
                }
            let uBorder =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Microsoft.FSharp.Core.Option<CorrelationDrawing.LogNodeTypes.Border>>() with
                    override x.Get(r) = r.uBorder
                    override x.Set(r,v) = { r with uBorder = v }
                    override x.Update(r,f) = { r with uBorder = f r.uBorder }
                }
            let annotation =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Microsoft.FSharp.Core.Option<CorrelationDrawing.AnnotationTypes.AnnotationId>>() with
                    override x.Get(r) = r.annotation
                    override x.Set(r,v) = { r with annotation = v }
                    override x.Update(r,f) = { r with annotation = f r.annotation }
                }
            let children =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Aardvark.Base.plist<CorrelationDrawing.LogNodeTypes.LogNode>>() with
                    override x.Get(r) = r.children
                    override x.Set(r,v) = { r with children = v }
                    override x.Update(r,f) = { r with children = f r.children }
                }
            let mainBody =
                { new Lens<CorrelationDrawing.LogNodeTypes.LogNode, Svgplus.RectangleType.Rectangle>() with
                    override x.Get(r) = r.mainBody
                    override x.Set(r,v) = { r with mainBody = v }
                    override x.Update(r,f) = { r with mainBody = f r.mainBody }
                }
