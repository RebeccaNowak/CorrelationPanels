namespace CorrelationDrawing.AnnotationTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.AnnotationTypes

[<AutoOpen>]
module Mutable =

    
    
    type MAnnotationPoint(__initial : CorrelationDrawing.AnnotationTypes.AnnotationPoint) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationTypes.AnnotationPoint> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.AnnotationTypes.AnnotationPoint>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationTypes.AnnotationPoint>
        let _selected = ResetMod.Create(__initial.selected)
        
        member x.point = __current.Value.point
        member x.selected = _selected :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.AnnotationTypes.AnnotationPoint) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_selected,v.selected)
                
        
        static member Create(__initial : CorrelationDrawing.AnnotationTypes.AnnotationPoint) : MAnnotationPoint = MAnnotationPoint(__initial)
        static member Update(m : MAnnotationPoint, v : CorrelationDrawing.AnnotationTypes.AnnotationPoint) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.AnnotationTypes.AnnotationPoint> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationPoint =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let point =
                { new Lens<CorrelationDrawing.AnnotationTypes.AnnotationPoint, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.point
                    override x.Set(r,v) = { r with point = v }
                    override x.Update(r,f) = { r with point = f r.point }
                }
            let selected =
                { new Lens<CorrelationDrawing.AnnotationTypes.AnnotationPoint, System.Boolean>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
    
    
    type MAnnotation(__initial : CorrelationDrawing.AnnotationTypes.Annotation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationTypes.Annotation> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.AnnotationTypes.Annotation>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationTypes.Annotation>
        let _selected = ResetMod.Create(__initial.selected)
        let _hovered = ResetMod.Create(__initial.hovered)
        let _semanticId = ResetMod.Create(__initial.semanticId)
        let _points = MList.Create(__initial.points, (fun v -> MAnnotationPoint.Create(v)), (fun (m,v) -> MAnnotationPoint.Update(m, v)), (fun v -> v))
        let _segments = MList.Create(__initial.segments, (fun v -> MList.Create(v)), (fun (m,v) -> MList.Update(m, v)), (fun v -> v :> alist<_>))
        let _visible = ResetMod.Create(__initial.visible)
        let _text = ResetMod.Create(__initial.text)
        let _overrideStyle = MOption.Create(__initial.overrideStyle, (fun v -> CorrelationDrawing.Types.Mutable.MStyle.Create(v)), (fun (m,v) -> CorrelationDrawing.Types.Mutable.MStyle.Update(m, v)), (fun v -> v))
        
        member x.id = __current.Value.id
        member x.geometry = __current.Value.geometry
        member x.projection = __current.Value.projection
        member x.semanticType = __current.Value.semanticType
        member x.elevation = __current.Value.elevation
        member x.selected = _selected :> IMod<_>
        member x.hovered = _hovered :> IMod<_>
        member x.semanticId = _semanticId :> IMod<_>
        member x.points = _points :> alist<_>
        member x.segments = _segments :> alist<_>
        member x.visible = _visible :> IMod<_>
        member x.text = _text :> IMod<_>
        member x.overrideStyle = _overrideStyle :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.AnnotationTypes.Annotation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_selected,v.selected)
                ResetMod.Update(_hovered,v.hovered)
                ResetMod.Update(_semanticId,v.semanticId)
                MList.Update(_points, v.points)
                MList.Update(_segments, v.segments)
                ResetMod.Update(_visible,v.visible)
                ResetMod.Update(_text,v.text)
                MOption.Update(_overrideStyle, v.overrideStyle)
                
        
        static member Create(__initial : CorrelationDrawing.AnnotationTypes.Annotation) : MAnnotation = MAnnotation(__initial)
        static member Update(m : MAnnotation, v : CorrelationDrawing.AnnotationTypes.Annotation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.AnnotationTypes.Annotation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Annotation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, CorrelationDrawing.AnnotationTypes.AnnotationId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let geometry =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, CorrelationDrawing.SemanticTypes.GeometryType>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let projection =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, CorrelationDrawing.Types.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let semanticType =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, CorrelationDrawing.SemanticTypes.SemanticType>() with
                    override x.Get(r) = r.semanticType
                    override x.Set(r,v) = { r with semanticType = v }
                    override x.Update(r,f) = { r with semanticType = f r.semanticType }
                }
            let elevation =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, Aardvark.Base.V3d -> System.Double>() with
                    override x.Get(r) = r.elevation
                    override x.Set(r,v) = { r with elevation = v }
                    override x.Update(r,f) = { r with elevation = f r.elevation }
                }
            let selected =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, System.Boolean>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let hovered =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, System.Boolean>() with
                    override x.Get(r) = r.hovered
                    override x.Set(r,v) = { r with hovered = v }
                    override x.Update(r,f) = { r with hovered = f r.hovered }
                }
            let semanticId =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, CorrelationDrawing.SemanticTypes.SemanticId>() with
                    override x.Get(r) = r.semanticId
                    override x.Set(r,v) = { r with semanticId = v }
                    override x.Update(r,f) = { r with semanticId = f r.semanticId }
                }
            let points =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, Aardvark.Base.plist<CorrelationDrawing.AnnotationTypes.AnnotationPoint>>() with
                    override x.Get(r) = r.points
                    override x.Set(r,v) = { r with points = v }
                    override x.Update(r,f) = { r with points = f r.points }
                }
            let segments =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, Aardvark.Base.plist<Aardvark.Base.plist<Aardvark.Base.V3d>>>() with
                    override x.Get(r) = r.segments
                    override x.Set(r,v) = { r with segments = v }
                    override x.Update(r,f) = { r with segments = f r.segments }
                }
            let visible =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, System.Boolean>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let text =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let overrideStyle =
                { new Lens<CorrelationDrawing.AnnotationTypes.Annotation, Microsoft.FSharp.Core.Option<CorrelationDrawing.Types.Style>>() with
                    override x.Get(r) = r.overrideStyle
                    override x.Set(r,v) = { r with overrideStyle = v }
                    override x.Update(r,f) = { r with overrideStyle = f r.overrideStyle }
                }
    
    
    type MAnnotationApp(__initial : CorrelationDrawing.AnnotationTypes.AnnotationApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationTypes.AnnotationApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.AnnotationTypes.AnnotationApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationTypes.AnnotationApp>
        let _annotations = MMap.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _selectedAnnotation = MOption.Create(__initial.selectedAnnotation)
        let _keyboard = ResetMod.Create(__initial.keyboard)
        
        member x.annotations = _annotations :> amap<_,_>
        member x.selectedAnnotation = _selectedAnnotation :> IMod<_>
        member x.keyboard = _keyboard :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.AnnotationTypes.AnnotationApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_annotations, v.annotations)
                MOption.Update(_selectedAnnotation, v.selectedAnnotation)
                ResetMod.Update(_keyboard,v.keyboard)
                
        
        static member Create(__initial : CorrelationDrawing.AnnotationTypes.AnnotationApp) : MAnnotationApp = MAnnotationApp(__initial)
        static member Update(m : MAnnotationApp, v : CorrelationDrawing.AnnotationTypes.AnnotationApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.AnnotationTypes.AnnotationApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let annotations =
                { new Lens<CorrelationDrawing.AnnotationTypes.AnnotationApp, Aardvark.Base.hmap<CorrelationDrawing.AnnotationTypes.AnnotationId,CorrelationDrawing.AnnotationTypes.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let selectedAnnotation =
                { new Lens<CorrelationDrawing.AnnotationTypes.AnnotationApp, Microsoft.FSharp.Core.Option<CorrelationDrawing.AnnotationTypes.AnnotationId>>() with
                    override x.Get(r) = r.selectedAnnotation
                    override x.Set(r,v) = { r with selectedAnnotation = v }
                    override x.Update(r,f) = { r with selectedAnnotation = f r.selectedAnnotation }
                }
            let keyboard =
                { new Lens<CorrelationDrawing.AnnotationTypes.AnnotationApp, UIPlus.KeyboardTypes.Keyboard<CorrelationDrawing.AnnotationTypes.AnnotationApp>>() with
                    override x.Get(r) = r.keyboard
                    override x.Set(r,v) = { r with keyboard = v }
                    override x.Update(r,f) = { r with keyboard = f r.keyboard }
                }
