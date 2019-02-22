namespace Svgplus.CameraType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Svgplus.CameraType

[<AutoOpen>]
module Mutable =

    
    
    type MSvgCamera(__initial : Svgplus.CameraType.SvgCamera) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Svgplus.CameraType.SvgCamera> = Aardvark.Base.Incremental.EqModRef<Svgplus.CameraType.SvgCamera>(__initial) :> Aardvark.Base.Incremental.IModRef<Svgplus.CameraType.SvgCamera>
        let _zoomFactorX = ResetMod.Create(__initial.zoomFactorX)
        let _zoomFactorY = ResetMod.Create(__initial.zoomFactorY)
        let _dragging = ResetMod.Create(__initial.dragging)
        let _zoomingX = ResetMod.Create(__initial.zoomingX)
        let _zoomingY = ResetMod.Create(__initial.zoomingY)
        let _lastMousePos = ResetMod.Create(__initial.lastMousePos)
        let _offset = ResetMod.Create(__initial.offset)
        let _fontSize = ResetMod.Create(__initial.fontSize)
        let _transformedMousePos = ResetMod.Create(__initial.transformedMousePos)
        
        member x.zoomFactorX = _zoomFactorX :> IMod<_>
        member x.zoomFactorY = _zoomFactorY :> IMod<_>
        member x.dragging = _dragging :> IMod<_>
        member x.zoomingX = _zoomingX :> IMod<_>
        member x.zoomingY = _zoomingY :> IMod<_>
        member x.lastMousePos = _lastMousePos :> IMod<_>
        member x.offset = _offset :> IMod<_>
        member x.fontSize = _fontSize :> IMod<_>
        member x.transformedMousePos = _transformedMousePos :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Svgplus.CameraType.SvgCamera) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_zoomFactorX,v.zoomFactorX)
                ResetMod.Update(_zoomFactorY,v.zoomFactorY)
                ResetMod.Update(_dragging,v.dragging)
                ResetMod.Update(_zoomingX,v.zoomingX)
                ResetMod.Update(_zoomingY,v.zoomingY)
                ResetMod.Update(_lastMousePos,v.lastMousePos)
                ResetMod.Update(_offset,v.offset)
                ResetMod.Update(_fontSize,v.fontSize)
                ResetMod.Update(_transformedMousePos,v.transformedMousePos)
                
        
        static member Create(__initial : Svgplus.CameraType.SvgCamera) : MSvgCamera = MSvgCamera(__initial)
        static member Update(m : MSvgCamera, v : Svgplus.CameraType.SvgCamera) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Svgplus.CameraType.SvgCamera> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SvgCamera =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let zoomFactorX =
                { new Lens<Svgplus.CameraType.SvgCamera, Svgplus.CameraType.Zoom>() with
                    override x.Get(r) = r.zoomFactorX
                    override x.Set(r,v) = { r with zoomFactorX = v }
                    override x.Update(r,f) = { r with zoomFactorX = f r.zoomFactorX }
                }
            let zoomFactorY =
                { new Lens<Svgplus.CameraType.SvgCamera, Svgplus.CameraType.Zoom>() with
                    override x.Get(r) = r.zoomFactorY
                    override x.Set(r,v) = { r with zoomFactorY = v }
                    override x.Update(r,f) = { r with zoomFactorY = f r.zoomFactorY }
                }
            let dragging =
                { new Lens<Svgplus.CameraType.SvgCamera, System.Boolean>() with
                    override x.Get(r) = r.dragging
                    override x.Set(r,v) = { r with dragging = v }
                    override x.Update(r,f) = { r with dragging = f r.dragging }
                }
            let zoomingX =
                { new Lens<Svgplus.CameraType.SvgCamera, System.Boolean>() with
                    override x.Get(r) = r.zoomingX
                    override x.Set(r,v) = { r with zoomingX = v }
                    override x.Update(r,f) = { r with zoomingX = f r.zoomingX }
                }
            let zoomingY =
                { new Lens<Svgplus.CameraType.SvgCamera, System.Boolean>() with
                    override x.Get(r) = r.zoomingY
                    override x.Set(r,v) = { r with zoomingY = v }
                    override x.Update(r,f) = { r with zoomingY = f r.zoomingY }
                }
            let lastMousePos =
                { new Lens<Svgplus.CameraType.SvgCamera, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.lastMousePos
                    override x.Set(r,v) = { r with lastMousePos = v }
                    override x.Update(r,f) = { r with lastMousePos = f r.lastMousePos }
                }
            let offset =
                { new Lens<Svgplus.CameraType.SvgCamera, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.offset
                    override x.Set(r,v) = { r with offset = v }
                    override x.Update(r,f) = { r with offset = f r.offset }
                }
            let fontSize =
                { new Lens<Svgplus.CameraType.SvgCamera, Svgplus.CameraType.FontSize>() with
                    override x.Get(r) = r.fontSize
                    override x.Set(r,v) = { r with fontSize = v }
                    override x.Update(r,f) = { r with fontSize = f r.fontSize }
                }
            let transformedMousePos =
                { new Lens<Svgplus.CameraType.SvgCamera, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.transformedMousePos
                    override x.Set(r,v) = { r with transformedMousePos = v }
                    override x.Update(r,f) = { r with transformedMousePos = f r.transformedMousePos }
                }
