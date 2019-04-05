namespace CorrelationDrawing.CorrelationPlotTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.CorrelationPlotTypes

[<AutoOpen>]
module Mutable =

    
    
    type MCorrelationPlot(__initial : CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot>
        let _diagram = Svgplus.DA.Mutable.MDiagram.Create(__initial.diagram)
        let _colourMapApp = UIPlus.Mutable.MColourMap.Create(__initial.colourMapApp)
        let _svgCamera = Svgplus.CameraType.Mutable.MSvgCamera.Create(__initial.svgCamera)
        let _keyboard = ResetMod.Create(__initial.keyboard)
        let _logs = MMap.Create(__initial.logs, (fun v -> CorrelationDrawing.LogTypes.Mutable.MGeologicalLog.Create(v)), (fun (m,v) -> CorrelationDrawing.LogTypes.Mutable.MGeologicalLog.Update(m, v)), (fun v -> v))
        let _correlations = MList.Create(__initial.correlations, (fun v -> CorrelationDrawing.CorrelationTypes.Mutable.MCorrelation.Create(v)), (fun (m,v) -> CorrelationDrawing.CorrelationTypes.Mutable.MCorrelation.Update(m, v)), (fun v -> v))
        let _selectedBorder = MOption.Create(__initial.selectedBorder, (fun v -> CorrelationDrawing.LogNodeTypes.Mutable.MBorder.Create(v)), (fun (m,v) -> CorrelationDrawing.LogNodeTypes.Mutable.MBorder.Update(m, v)), (fun v -> v))
        let _editCorrelations = ResetMod.Create(__initial.editCorrelations)
        let _selectedPoints = MMap.Create(__initial.selectedPoints)
        let _selectedNode = MOption.Create(__initial.selectedNode)
        let _selectedLog = MOption.Create(__initial.selectedLog)
        let _secondaryLvl = ResetMod.Create(__initial.secondaryLvl)
        let _xAxis = ResetMod.Create(__initial.xAxis)
        let _currrentYMapping = MOption.Create(__initial.currrentYMapping)
        let _yRange = ResetMod.Create(__initial.yRange)
        let _xToSvg = ResetMod.Create(__initial.xToSvg)
        let _yToSvg = ResetMod.Create(__initial.yToSvg)
        let _defaultWidth = ResetMod.Create(__initial.defaultWidth)
        let _elevationZeroHeight = ResetMod.Create(__initial.elevationZeroHeight)
        
        member x.diagram = _diagram
        member x.colourMapApp = _colourMapApp
        member x.svgCamera = _svgCamera
        member x.keyboard = _keyboard :> IMod<_>
        member x.logs = _logs :> amap<_,_>
        member x.correlations = _correlations :> alist<_>
        member x.selectedBorder = _selectedBorder :> IMod<_>
        member x.editCorrelations = _editCorrelations :> IMod<_>
        member x.selectedPoints = _selectedPoints :> amap<_,_>
        member x.selectedNode = _selectedNode :> IMod<_>
        member x.selectedLog = _selectedLog :> IMod<_>
        member x.secondaryLvl = _secondaryLvl :> IMod<_>
        member x.xAxis = _xAxis :> IMod<_>
        member x.currrentYMapping = _currrentYMapping :> IMod<_>
        member x.yRange = _yRange :> IMod<_>
        member x.xToSvg = _xToSvg :> IMod<_>
        member x.yToSvg = _yToSvg :> IMod<_>
        member x.defaultWidth = _defaultWidth :> IMod<_>
        member x.elevationZeroHeight = _elevationZeroHeight :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Svgplus.DA.Mutable.MDiagram.Update(_diagram, v.diagram)
                UIPlus.Mutable.MColourMap.Update(_colourMapApp, v.colourMapApp)
                Svgplus.CameraType.Mutable.MSvgCamera.Update(_svgCamera, v.svgCamera)
                ResetMod.Update(_keyboard,v.keyboard)
                MMap.Update(_logs, v.logs)
                MList.Update(_correlations, v.correlations)
                MOption.Update(_selectedBorder, v.selectedBorder)
                ResetMod.Update(_editCorrelations,v.editCorrelations)
                MMap.Update(_selectedPoints, v.selectedPoints)
                MOption.Update(_selectedNode, v.selectedNode)
                MOption.Update(_selectedLog, v.selectedLog)
                ResetMod.Update(_secondaryLvl,v.secondaryLvl)
                ResetMod.Update(_xAxis,v.xAxis)
                MOption.Update(_currrentYMapping, v.currrentYMapping)
                ResetMod.Update(_yRange,v.yRange)
                ResetMod.Update(_xToSvg,v.xToSvg)
                ResetMod.Update(_yToSvg,v.yToSvg)
                ResetMod.Update(_defaultWidth,v.defaultWidth)
                ResetMod.Update(_elevationZeroHeight,v.elevationZeroHeight)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot) : MCorrelationPlot = MCorrelationPlot(__initial)
        static member Update(m : MCorrelationPlot, v : CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationPlot =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let diagram =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Svgplus.DA.Diagram>() with
                    override x.Get(r) = r.diagram
                    override x.Set(r,v) = { r with diagram = v }
                    override x.Update(r,f) = { r with diagram = f r.diagram }
                }
            let colourMapApp =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, UIPlus.ColourMap>() with
                    override x.Get(r) = r.colourMapApp
                    override x.Set(r,v) = { r with colourMapApp = v }
                    override x.Update(r,f) = { r with colourMapApp = f r.colourMapApp }
                }
            let svgCamera =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Svgplus.CameraType.SvgCamera>() with
                    override x.Get(r) = r.svgCamera
                    override x.Set(r,v) = { r with svgCamera = v }
                    override x.Update(r,f) = { r with svgCamera = f r.svgCamera }
                }
            let keyboard =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, UIPlus.KeyboardTypes.Keyboard<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot>>() with
                    override x.Get(r) = r.keyboard
                    override x.Set(r,v) = { r with keyboard = v }
                    override x.Update(r,f) = { r with keyboard = f r.keyboard }
                }
            let logs =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Aardvark.Base.hmap<Svgplus.DiagramItemType.DiagramItemId,CorrelationDrawing.LogTypes.GeologicalLog>>() with
                    override x.Get(r) = r.logs
                    override x.Set(r,v) = { r with logs = v }
                    override x.Update(r,f) = { r with logs = f r.logs }
                }
            let correlations =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Aardvark.Base.plist<CorrelationDrawing.CorrelationTypes.Correlation>>() with
                    override x.Get(r) = r.correlations
                    override x.Set(r,v) = { r with correlations = v }
                    override x.Update(r,f) = { r with correlations = f r.correlations }
                }
            let selectedBorder =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Microsoft.FSharp.Core.Option<CorrelationDrawing.LogNodeTypes.Border>>() with
                    override x.Get(r) = r.selectedBorder
                    override x.Set(r,v) = { r with selectedBorder = v }
                    override x.Update(r,f) = { r with selectedBorder = f r.selectedBorder }
                }
            let editCorrelations =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, System.Boolean>() with
                    override x.Get(r) = r.editCorrelations
                    override x.Set(r,v) = { r with editCorrelations = v }
                    override x.Update(r,f) = { r with editCorrelations = f r.editCorrelations }
                }
            let selectedPoints =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Aardvark.Base.hmap<CorrelationDrawing.AnnotationTypes.AnnotationId,Aardvark.Base.V3d>>() with
                    override x.Get(r) = r.selectedPoints
                    override x.Set(r,v) = { r with selectedPoints = v }
                    override x.Update(r,f) = { r with selectedPoints = f r.selectedPoints }
                }
            let selectedNode =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Microsoft.FSharp.Core.Option<CorrelationDrawing.LogNodeTypes.LogNodeId>>() with
                    override x.Get(r) = r.selectedNode
                    override x.Set(r,v) = { r with selectedNode = v }
                    override x.Update(r,f) = { r with selectedNode = f r.selectedNode }
                }
            let selectedLog =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Microsoft.FSharp.Core.Option<Svgplus.DiagramItemType.DiagramItemId>>() with
                    override x.Get(r) = r.selectedLog
                    override x.Set(r,v) = { r with selectedLog = v }
                    override x.Update(r,f) = { r with selectedLog = f r.selectedLog }
                }
            let secondaryLvl =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, CorrelationDrawing.Types.NodeLevel>() with
                    override x.Get(r) = r.secondaryLvl
                    override x.Set(r,v) = { r with secondaryLvl = v }
                    override x.Update(r,f) = { r with secondaryLvl = f r.secondaryLvl }
                }
            let xAxis =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, CorrelationDrawing.SemanticTypes.SemanticId>() with
                    override x.Get(r) = r.xAxis
                    override x.Set(r,v) = { r with xAxis = v }
                    override x.Update(r,f) = { r with xAxis = f r.xAxis }
                }
            let currrentYMapping =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, Microsoft.FSharp.Core.Option<System.Double>>() with
                    override x.Get(r) = r.currrentYMapping
                    override x.Set(r,v) = { r with currrentYMapping = v }
                    override x.Update(r,f) = { r with currrentYMapping = f r.currrentYMapping }
                }
            let yRange =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, SimpleTypes.Rangef>() with
                    override x.Get(r) = r.yRange
                    override x.Set(r,v) = { r with yRange = v }
                    override x.Update(r,f) = { r with yRange = f r.yRange }
                }
            let xToSvg =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, System.Double -> System.Double>() with
                    override x.Get(r) = r.xToSvg
                    override x.Set(r,v) = { r with xToSvg = v }
                    override x.Update(r,f) = { r with xToSvg = f r.xToSvg }
                }
            let yToSvg =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, System.Double>() with
                    override x.Get(r) = r.yToSvg
                    override x.Set(r,v) = { r with yToSvg = v }
                    override x.Update(r,f) = { r with yToSvg = f r.yToSvg }
                }
            let defaultWidth =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, System.Double>() with
                    override x.Get(r) = r.defaultWidth
                    override x.Set(r,v) = { r with defaultWidth = v }
                    override x.Update(r,f) = { r with defaultWidth = f r.defaultWidth }
                }
            let elevationZeroHeight =
                { new Lens<CorrelationDrawing.CorrelationPlotTypes.CorrelationPlot, System.Double>() with
                    override x.Get(r) = r.elevationZeroHeight
                    override x.Set(r,v) = { r with elevationZeroHeight = v }
                    override x.Update(r,f) = { r with elevationZeroHeight = f r.elevationZeroHeight }
                }
