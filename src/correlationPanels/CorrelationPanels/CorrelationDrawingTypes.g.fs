namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing

[<AutoOpen>]
module Mutable =

    
    
    type MRenderingParameters(__initial : CorrelationDrawing.RenderingParameters) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.RenderingParameters> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.RenderingParameters>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.RenderingParameters>
        let _fillMode = ResetMod.Create(__initial.fillMode)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        
        member x.fillMode = _fillMode :> IMod<_>
        member x.cullMode = _cullMode :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.RenderingParameters) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_fillMode,v.fillMode)
                ResetMod.Update(_cullMode,v.cullMode)
                
        
        static member Create(__initial : CorrelationDrawing.RenderingParameters) : MRenderingParameters = MRenderingParameters(__initial)
        static member Update(m : MRenderingParameters, v : CorrelationDrawing.RenderingParameters) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.RenderingParameters> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RenderingParameters =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let fillMode =
                { new Lens<CorrelationDrawing.RenderingParameters, Aardvark.Base.Rendering.FillMode>() with
                    override x.Get(r) = r.fillMode
                    override x.Set(r,v) = { r with fillMode = v }
                    override x.Update(r,f) = { r with fillMode = f r.fillMode }
                }
            let cullMode =
                { new Lens<CorrelationDrawing.RenderingParameters, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
    
    
    type MSvgOptions(__initial : CorrelationDrawing.SvgOptions) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SvgOptions> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.SvgOptions>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SvgOptions>
        let _logPadding = ResetMod.Create(__initial.logPadding)
        let _logHeight = ResetMod.Create(__initial.logHeight)
        let _logMaxWidth = ResetMod.Create(__initial.logMaxWidth)
        let _cpWidth = ResetMod.Create(__initial.cpWidth)
        let _secLevelWidth = ResetMod.Create(__initial.secLevelWidth)
        let _xAxisScaleFactor = ResetMod.Create(__initial.xAxisScaleFactor)
        let _yAxisScaleFactor = ResetMod.Create(__initial.yAxisScaleFactor)
        let _xAxisPadding = ResetMod.Create(__initial.xAxisPadding)
        let _yAxisPadding = ResetMod.Create(__initial.yAxisPadding)
        let _yAxisStep = ResetMod.Create(__initial.yAxisStep)
        let _axisWeight = ResetMod.Create(__initial.axisWeight)
        
        member x.logPadding = _logPadding :> IMod<_>
        member x.logHeight = _logHeight :> IMod<_>
        member x.logMaxWidth = _logMaxWidth :> IMod<_>
        member x.cpWidth = _cpWidth :> IMod<_>
        member x.secLevelWidth = _secLevelWidth :> IMod<_>
        member x.xAxisScaleFactor = _xAxisScaleFactor :> IMod<_>
        member x.yAxisScaleFactor = _yAxisScaleFactor :> IMod<_>
        member x.xAxisPadding = _xAxisPadding :> IMod<_>
        member x.yAxisPadding = _yAxisPadding :> IMod<_>
        member x.yAxisStep = _yAxisStep :> IMod<_>
        member x.axisWeight = _axisWeight :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.SvgOptions) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_logPadding,v.logPadding)
                ResetMod.Update(_logHeight,v.logHeight)
                ResetMod.Update(_logMaxWidth,v.logMaxWidth)
                ResetMod.Update(_cpWidth,v.cpWidth)
                ResetMod.Update(_secLevelWidth,v.secLevelWidth)
                ResetMod.Update(_xAxisScaleFactor,v.xAxisScaleFactor)
                ResetMod.Update(_yAxisScaleFactor,v.yAxisScaleFactor)
                ResetMod.Update(_xAxisPadding,v.xAxisPadding)
                ResetMod.Update(_yAxisPadding,v.yAxisPadding)
                ResetMod.Update(_yAxisStep,v.yAxisStep)
                ResetMod.Update(_axisWeight,v.axisWeight)
                
        
        static member Create(__initial : CorrelationDrawing.SvgOptions) : MSvgOptions = MSvgOptions(__initial)
        static member Update(m : MSvgOptions, v : CorrelationDrawing.SvgOptions) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.SvgOptions> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SvgOptions =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let logPadding =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.logPadding
                    override x.Set(r,v) = { r with logPadding = v }
                    override x.Update(r,f) = { r with logPadding = f r.logPadding }
                }
            let logHeight =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.logHeight
                    override x.Set(r,v) = { r with logHeight = v }
                    override x.Update(r,f) = { r with logHeight = f r.logHeight }
                }
            let logMaxWidth =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.logMaxWidth
                    override x.Set(r,v) = { r with logMaxWidth = v }
                    override x.Update(r,f) = { r with logMaxWidth = f r.logMaxWidth }
                }
            let cpWidth =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.cpWidth
                    override x.Set(r,v) = { r with cpWidth = v }
                    override x.Update(r,f) = { r with cpWidth = f r.cpWidth }
                }
            let secLevelWidth =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.secLevelWidth
                    override x.Set(r,v) = { r with secLevelWidth = v }
                    override x.Update(r,f) = { r with secLevelWidth = f r.secLevelWidth }
                }
            let xAxisScaleFactor =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.xAxisScaleFactor
                    override x.Set(r,v) = { r with xAxisScaleFactor = v }
                    override x.Update(r,f) = { r with xAxisScaleFactor = f r.xAxisScaleFactor }
                }
            let yAxisScaleFactor =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.yAxisScaleFactor
                    override x.Set(r,v) = { r with yAxisScaleFactor = v }
                    override x.Update(r,f) = { r with yAxisScaleFactor = f r.yAxisScaleFactor }
                }
            let xAxisPadding =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.xAxisPadding
                    override x.Set(r,v) = { r with xAxisPadding = v }
                    override x.Update(r,f) = { r with xAxisPadding = f r.xAxisPadding }
                }
            let yAxisPadding =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.yAxisPadding
                    override x.Set(r,v) = { r with yAxisPadding = v }
                    override x.Update(r,f) = { r with yAxisPadding = f r.yAxisPadding }
                }
            let yAxisStep =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.yAxisStep
                    override x.Set(r,v) = { r with yAxisStep = v }
                    override x.Update(r,f) = { r with yAxisStep = f r.yAxisStep }
                }
            let axisWeight =
                { new Lens<CorrelationDrawing.SvgOptions, System.Double>() with
                    override x.Get(r) = r.axisWeight
                    override x.Set(r,v) = { r with axisWeight = v }
                    override x.Update(r,f) = { r with axisWeight = f r.axisWeight }
                }
    
    
    type MCorrelationDrawingModel(__initial : CorrelationDrawing.CorrelationDrawingModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationDrawingModel> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationDrawingModel>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationDrawingModel>
        let _keyboard = ResetMod.Create(__initial.keyboard)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> CorrelationDrawing.AnnotationTypes.Mutable.MAnnotation.Create(v)), (fun (m,v) -> CorrelationDrawing.AnnotationTypes.Mutable.MAnnotation.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _exportPath = ResetMod.Create(__initial.exportPath)
        
        member x.keyboard = _keyboard :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.exportPath = _exportPath :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationDrawingModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_keyboard,v.keyboard)
                MOption.Update(_hoverPosition, v.hoverPosition)
                MOption.Update(_working, v.working)
                ResetMod.Update(_projection,v.projection)
                ResetMod.Update(_exportPath,v.exportPath)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationDrawingModel) : MCorrelationDrawingModel = MCorrelationDrawingModel(__initial)
        static member Update(m : MCorrelationDrawingModel, v : CorrelationDrawing.CorrelationDrawingModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationDrawingModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationDrawingModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let keyboard =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, UIPlus.KeyboardTypes.Keyboard<CorrelationDrawing.CorrelationDrawingModel>>() with
                    override x.Get(r) = r.keyboard
                    override x.Set(r,v) = { r with keyboard = v }
                    override x.Update(r,f) = { r with keyboard = f r.keyboard }
                }
            let hoverPosition =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.Option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
            let working =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.Option<CorrelationDrawing.AnnotationTypes.Annotation>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let projection =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, CorrelationDrawing.Types.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let exportPath =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, System.String>() with
                    override x.Get(r) = r.exportPath
                    override x.Set(r,v) = { r with exportPath = v }
                    override x.Update(r,f) = { r with exportPath = f r.exportPath }
                }
    
    
    type MPages(__initial : CorrelationDrawing.Pages) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Pages> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Pages>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Pages>
        let _saveIndices = ResetMod.Create(__initial.saveIndices)
        let _keyboard = ResetMod.Create(__initial.keyboard)
        let _appFlags = ResetMod.Create(__initial.appFlags)
        let _sgFlags = ResetMod.Create(__initial.sgFlags)
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _rendering = MRenderingParameters.Create(__initial.rendering)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        let _fill = ResetMod.Create(__initial.fill)
        let _dockConfig = ResetMod.Create(__initial.dockConfig)
        let _drawingApp = MCorrelationDrawingModel.Create(__initial.drawingApp)
        let _annotationApp = CorrelationDrawing.AnnotationTypes.Mutable.MAnnotationApp.Create(__initial.annotationApp)
        let _semanticApp = CorrelationDrawing.SemanticTypes.Mutable.MSemanticApp.Create(__initial.semanticApp)
        let _corrPlot = CorrelationDrawing.CorrelationPlotTypes.Mutable.MCorrelationPlot.Create(__initial.corrPlot)
        
        member x.past = __current.Value.past
        member x.saveIndices = _saveIndices :> IMod<_>
        member x.keyboard = _keyboard :> IMod<_>
        member x.future = __current.Value.future
        member x.appFlags = _appFlags :> IMod<_>
        member x.sgFlags = _sgFlags :> IMod<_>
        member x.camera = _camera
        member x.rendering = _rendering
        member x.cullMode = _cullMode :> IMod<_>
        member x.fill = _fill :> IMod<_>
        member x.dockConfig = _dockConfig :> IMod<_>
        member x.drawingApp = _drawingApp
        member x.annotationApp = _annotationApp
        member x.semanticApp = _semanticApp
        member x.corrPlot = _corrPlot
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Pages) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_saveIndices,v.saveIndices)
                ResetMod.Update(_keyboard,v.keyboard)
                ResetMod.Update(_appFlags,v.appFlags)
                ResetMod.Update(_sgFlags,v.sgFlags)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                MRenderingParameters.Update(_rendering, v.rendering)
                ResetMod.Update(_cullMode,v.cullMode)
                ResetMod.Update(_fill,v.fill)
                ResetMod.Update(_dockConfig,v.dockConfig)
                MCorrelationDrawingModel.Update(_drawingApp, v.drawingApp)
                CorrelationDrawing.AnnotationTypes.Mutable.MAnnotationApp.Update(_annotationApp, v.annotationApp)
                CorrelationDrawing.SemanticTypes.Mutable.MSemanticApp.Update(_semanticApp, v.semanticApp)
                CorrelationDrawing.CorrelationPlotTypes.Mutable.MCorrelationPlot.Update(_corrPlot, v.corrPlot)
                
        
        static member Create(__initial : CorrelationDrawing.Pages) : MPages = MPages(__initial)
        static member Update(m : MPages, v : CorrelationDrawing.Pages) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Pages> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Pages =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let past =
                { new Lens<CorrelationDrawing.Pages, Microsoft.FSharp.Core.Option<CorrelationDrawing.Pages>>() with
                    override x.Get(r) = r.past
                    override x.Set(r,v) = { r with past = v }
                    override x.Update(r,f) = { r with past = f r.past }
                }
            let saveIndices =
                { new Lens<CorrelationDrawing.Pages, Microsoft.FSharp.Collections.List<CorrelationDrawing.SaveIndex>>() with
                    override x.Get(r) = r.saveIndices
                    override x.Set(r,v) = { r with saveIndices = v }
                    override x.Update(r,f) = { r with saveIndices = f r.saveIndices }
                }
            let keyboard =
                { new Lens<CorrelationDrawing.Pages, UIPlus.KeyboardTypes.Keyboard<CorrelationDrawing.Pages>>() with
                    override x.Get(r) = r.keyboard
                    override x.Set(r,v) = { r with keyboard = v }
                    override x.Update(r,f) = { r with keyboard = f r.keyboard }
                }
            let future =
                { new Lens<CorrelationDrawing.Pages, Microsoft.FSharp.Core.Option<CorrelationDrawing.Pages>>() with
                    override x.Get(r) = r.future
                    override x.Set(r,v) = { r with future = v }
                    override x.Update(r,f) = { r with future = f r.future }
                }
            let appFlags =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.AppFlags>() with
                    override x.Get(r) = r.appFlags
                    override x.Set(r,v) = { r with appFlags = v }
                    override x.Update(r,f) = { r with appFlags = f r.appFlags }
                }
            let sgFlags =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.SgFlags>() with
                    override x.Get(r) = r.sgFlags
                    override x.Set(r,v) = { r with sgFlags = v }
                    override x.Update(r,f) = { r with sgFlags = f r.sgFlags }
                }
            let camera =
                { new Lens<CorrelationDrawing.Pages, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
            let rendering =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.RenderingParameters>() with
                    override x.Get(r) = r.rendering
                    override x.Set(r,v) = { r with rendering = v }
                    override x.Update(r,f) = { r with rendering = f r.rendering }
                }
            let cullMode =
                { new Lens<CorrelationDrawing.Pages, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
            let fill =
                { new Lens<CorrelationDrawing.Pages, System.Boolean>() with
                    override x.Get(r) = r.fill
                    override x.Set(r,v) = { r with fill = v }
                    override x.Update(r,f) = { r with fill = f r.fill }
                }
            let dockConfig =
                { new Lens<CorrelationDrawing.Pages, Aardvark.UI.Primitives.DockConfig>() with
                    override x.Get(r) = r.dockConfig
                    override x.Set(r,v) = { r with dockConfig = v }
                    override x.Update(r,f) = { r with dockConfig = f r.dockConfig }
                }
            let drawingApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.CorrelationDrawingModel>() with
                    override x.Get(r) = r.drawingApp
                    override x.Set(r,v) = { r with drawingApp = v }
                    override x.Update(r,f) = { r with drawingApp = f r.drawingApp }
                }
            let annotationApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.AnnotationTypes.AnnotationApp>() with
                    override x.Get(r) = r.annotationApp
                    override x.Set(r,v) = { r with annotationApp = v }
                    override x.Update(r,f) = { r with annotationApp = f r.annotationApp }
                }
            let semanticApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.SemanticTypes.SemanticApp>() with
                    override x.Get(r) = r.semanticApp
                    override x.Set(r,v) = { r with semanticApp = v }
                    override x.Update(r,f) = { r with semanticApp = f r.semanticApp }
                }
            let corrPlot =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot>() with
                    override x.Get(r) = r.corrPlot
                    override x.Set(r,v) = { r with corrPlot = v }
                    override x.Update(r,f) = { r with corrPlot = f r.corrPlot }
                }
