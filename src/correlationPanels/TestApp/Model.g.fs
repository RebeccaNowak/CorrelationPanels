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
        let _svgButton = Svgplus.Mutable.MButton.Create(__initial.svgButton)
        let _arrow = Svgplus.ArrowType.Mutable.MArrow.Create(__initial.arrow)
        let _header = Svgplus.HeaderType.Mutable.MHeader.Create(__initial.header)
        let _roseDiagram = Svgplus.RoseDiagramModel.Mutable.MRoseDiagram.Create(__initial.roseDiagram)
        
        member x.currentModel = _currentModel :> IMod<_>
        member x.svgButton = _svgButton
        member x.arrow = _arrow
        member x.header = _header
        member x.roseDiagram = _roseDiagram
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Test.TestModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_currentModel,v.currentModel)
                Svgplus.Mutable.MButton.Update(_svgButton, v.svgButton)
                Svgplus.ArrowType.Mutable.MArrow.Update(_arrow, v.arrow)
                Svgplus.HeaderType.Mutable.MHeader.Update(_header, v.header)
                Svgplus.RoseDiagramModel.Mutable.MRoseDiagram.Update(_roseDiagram, v.roseDiagram)
                
        
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
            let svgButton =
                { new Lens<Test.TestModel, Svgplus.Button>() with
                    override x.Get(r) = r.svgButton
                    override x.Set(r,v) = { r with svgButton = v }
                    override x.Update(r,f) = { r with svgButton = f r.svgButton }
                }
            let arrow =
                { new Lens<Test.TestModel, Svgplus.ArrowType.Arrow>() with
                    override x.Get(r) = r.arrow
                    override x.Set(r,v) = { r with arrow = v }
                    override x.Update(r,f) = { r with arrow = f r.arrow }
                }
            let header =
                { new Lens<Test.TestModel, Svgplus.HeaderType.Header>() with
                    override x.Get(r) = r.header
                    override x.Set(r,v) = { r with header = v }
                    override x.Update(r,f) = { r with header = f r.header }
                }
            let roseDiagram =
                { new Lens<Test.TestModel, Svgplus.RoseDiagramModel.RoseDiagram>() with
                    override x.Get(r) = r.roseDiagram
                    override x.Set(r,v) = { r with roseDiagram = v }
                    override x.Update(r,f) = { r with roseDiagram = f r.roseDiagram }
                }
