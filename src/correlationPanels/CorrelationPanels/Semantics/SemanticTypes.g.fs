namespace CorrelationDrawing.SemanticTypes

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing.SemanticTypes

[<AutoOpen>]
module Mutable =

    
    
    type MSemantic(__initial : CorrelationDrawing.SemanticTypes.Semantic) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticTypes.Semantic> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.SemanticTypes.Semantic>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticTypes.Semantic>
        let _state = ResetMod.Create(__initial.state)
        let _label = UIPlus.Mutable.MTextInput.Create(__initial.label)
        let _style = CorrelationDrawing.Types.Mutable.MStyle.Create(__initial.style)
        let _semanticType = ResetMod.Create(__initial.semanticType)
        let _geometryType = ResetMod.Create(__initial.geometryType)
        let _level = ResetMod.Create(__initial.level)
        
        member x.id = __current.Value.id
        member x.timestamp = __current.Value.timestamp
        member x.state = _state :> IMod<_>
        member x.label = _label
        member x.style = _style
        member x.semanticType = _semanticType :> IMod<_>
        member x.geometryType = _geometryType :> IMod<_>
        member x.level = _level :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.SemanticTypes.Semantic) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_state,v.state)
                UIPlus.Mutable.MTextInput.Update(_label, v.label)
                CorrelationDrawing.Types.Mutable.MStyle.Update(_style, v.style)
                ResetMod.Update(_semanticType,v.semanticType)
                ResetMod.Update(_geometryType,v.geometryType)
                ResetMod.Update(_level,v.level)
                
        
        static member Create(__initial : CorrelationDrawing.SemanticTypes.Semantic) : MSemantic = MSemantic(__initial)
        static member Update(m : MSemantic, v : CorrelationDrawing.SemanticTypes.Semantic) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.SemanticTypes.Semantic> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Semantic =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, CorrelationDrawing.SemanticTypes.SemanticId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let timestamp =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, System.String>() with
                    override x.Get(r) = r.timestamp
                    override x.Set(r,v) = { r with timestamp = v }
                    override x.Update(r,f) = { r with timestamp = f r.timestamp }
                }
            let state =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, CorrelationDrawing.Types.State>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let label =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, UIPlus.TextInput>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let style =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, CorrelationDrawing.Types.Style>() with
                    override x.Get(r) = r.style
                    override x.Set(r,v) = { r with style = v }
                    override x.Update(r,f) = { r with style = f r.style }
                }
            let semanticType =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, CorrelationDrawing.SemanticTypes.SemanticType>() with
                    override x.Get(r) = r.semanticType
                    override x.Set(r,v) = { r with semanticType = v }
                    override x.Update(r,f) = { r with semanticType = f r.semanticType }
                }
            let geometryType =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, CorrelationDrawing.SemanticTypes.GeometryType>() with
                    override x.Get(r) = r.geometryType
                    override x.Set(r,v) = { r with geometryType = v }
                    override x.Update(r,f) = { r with geometryType = f r.geometryType }
                }
            let level =
                { new Lens<CorrelationDrawing.SemanticTypes.Semantic, CorrelationDrawing.Types.NodeLevel>() with
                    override x.Get(r) = r.level
                    override x.Set(r,v) = { r with level = v }
                    override x.Update(r,f) = { r with level = f r.level }
                }
    
    
    type MSemanticApp(__initial : CorrelationDrawing.SemanticTypes.SemanticApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticTypes.SemanticApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.SemanticTypes.SemanticApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticTypes.SemanticApp>
        let _semantics = MMap.Create(__initial.semantics, (fun v -> MSemantic.Create(v)), (fun (m,v) -> MSemantic.Update(m, v)), (fun v -> v))
        let _semanticsList = MList.Create(__initial.semanticsList, (fun v -> MSemantic.Create(v)), (fun (m,v) -> MSemantic.Update(m, v)), (fun v -> v))
        let _selectedSemantic = ResetMod.Create(__initial.selectedSemantic)
        let _sortBy = ResetMod.Create(__initial.sortBy)
        let _creatingNew = ResetMod.Create(__initial.creatingNew)
        
        member x.semantics = _semantics :> amap<_,_>
        member x.semanticsList = _semanticsList :> alist<_>
        member x.selectedSemantic = _selectedSemantic :> IMod<_>
        member x.sortBy = _sortBy :> IMod<_>
        member x.creatingNew = _creatingNew :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.SemanticTypes.SemanticApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_semantics, v.semantics)
                MList.Update(_semanticsList, v.semanticsList)
                ResetMod.Update(_selectedSemantic,v.selectedSemantic)
                ResetMod.Update(_sortBy,v.sortBy)
                ResetMod.Update(_creatingNew,v.creatingNew)
                
        
        static member Create(__initial : CorrelationDrawing.SemanticTypes.SemanticApp) : MSemanticApp = MSemanticApp(__initial)
        static member Update(m : MSemanticApp, v : CorrelationDrawing.SemanticTypes.SemanticApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.SemanticTypes.SemanticApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SemanticApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let semantics =
                { new Lens<CorrelationDrawing.SemanticTypes.SemanticApp, Aardvark.Base.hmap<CorrelationDrawing.SemanticTypes.SemanticId,CorrelationDrawing.SemanticTypes.Semantic>>() with
                    override x.Get(r) = r.semantics
                    override x.Set(r,v) = { r with semantics = v }
                    override x.Update(r,f) = { r with semantics = f r.semantics }
                }
            let semanticsList =
                { new Lens<CorrelationDrawing.SemanticTypes.SemanticApp, Aardvark.Base.plist<CorrelationDrawing.SemanticTypes.Semantic>>() with
                    override x.Get(r) = r.semanticsList
                    override x.Set(r,v) = { r with semanticsList = v }
                    override x.Update(r,f) = { r with semanticsList = f r.semanticsList }
                }
            let selectedSemantic =
                { new Lens<CorrelationDrawing.SemanticTypes.SemanticApp, CorrelationDrawing.SemanticTypes.SemanticId>() with
                    override x.Get(r) = r.selectedSemantic
                    override x.Set(r,v) = { r with selectedSemantic = v }
                    override x.Update(r,f) = { r with selectedSemantic = f r.selectedSemantic }
                }
            let sortBy =
                { new Lens<CorrelationDrawing.SemanticTypes.SemanticApp, CorrelationDrawing.SemanticTypes.SemanticsSortingOption>() with
                    override x.Get(r) = r.sortBy
                    override x.Set(r,v) = { r with sortBy = v }
                    override x.Update(r,f) = { r with sortBy = f r.sortBy }
                }
            let creatingNew =
                { new Lens<CorrelationDrawing.SemanticTypes.SemanticApp, System.Boolean>() with
                    override x.Get(r) = r.creatingNew
                    override x.Set(r,v) = { r with creatingNew = v }
                    override x.Update(r,f) = { r with creatingNew = f r.creatingNew }
                }
