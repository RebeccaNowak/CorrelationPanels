namespace Test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Test

[<AutoOpen>]
module Mutable =

    
    
    type MTestModel(__initial : Test.TestModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Test.TestModel> = Aardvark.Base.Incremental.EqModRef<Test.TestModel>(__initial) :> Aardvark.Base.Incremental.IModRef<Test.TestModel>
        let _currentModel = ResetMod.Create(__initial.currentModel)
        let _cameraState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraState)
        let _svgButton = Svgplus.Mutable.MButton.Create(__initial.svgButton)
        let _roseDiagram = Svgplus.Mutable.MRoseDiagram.Create(__initial.roseDiagram)
        let _diagramApp = Svgplus.Mutable.MDiagramApp.Create(__initial.diagramApp)
        
        member x.currentModel = _currentModel :> IMod<_>
        member x.cameraState = _cameraState
        member x.svgButton = _svgButton
        member x.roseDiagram = _roseDiagram
        member x.diagramApp = _diagramApp
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Test.TestModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_currentModel,v.currentModel)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                Svgplus.Mutable.MButton.Update(_svgButton, v.svgButton)
                Svgplus.Mutable.MRoseDiagram.Update(_roseDiagram, v.roseDiagram)
                Svgplus.Mutable.MDiagramApp.Update(_diagramApp, v.diagramApp)
                
        
        static member Create(__initial : Test.TestModel) : MTestModel = MTestModel(__initial)
        static member Update(m : MTestModel, v : Test.TestModel) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Test.TestModel> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TestModel =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let currentModel =
                { new Lens<Test.TestModel, Test.Primitive>() with
                    override x.Get(r) = r.currentModel
                    override x.Set(r,v) = { r with currentModel = v }
                    override x.Update(r,f) = { r with currentModel = f r.currentModel }
                }
            let cameraState =
                { new Lens<Test.TestModel, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraState
                    override x.Set(r,v) = { r with cameraState = v }
                    override x.Update(r,f) = { r with cameraState = f r.cameraState }
                }
            let svgButton =
                { new Lens<Test.TestModel, Svgplus.Button>() with
                    override x.Get(r) = r.svgButton
                    override x.Set(r,v) = { r with svgButton = v }
                    override x.Update(r,f) = { r with svgButton = f r.svgButton }
                }
            let roseDiagram =
                { new Lens<Test.TestModel, Svgplus.RoseDiagram>() with
                    override x.Get(r) = r.roseDiagram
                    override x.Set(r,v) = { r with roseDiagram = v }
                    override x.Update(r,f) = { r with roseDiagram = f r.roseDiagram }
                }
            let diagramApp =
                { new Lens<Test.TestModel, Svgplus.DiagramApp>() with
                    override x.Get(r) = r.diagramApp
                    override x.Set(r,v) = { r with diagramApp = v }
                    override x.Update(r,f) = { r with diagramApp = f r.diagramApp }
                }
