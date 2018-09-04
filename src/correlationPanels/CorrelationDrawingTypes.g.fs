namespace CorrelationDrawing

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open CorrelationDrawing

[<AutoOpen>]
module Mutable =

    
    
    type MTextInput(__initial : CorrelationDrawing.TextInput) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.TextInput> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.TextInput>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.TextInput>
        let _text = ResetMod.Create(__initial.text)
        let _disabled = ResetMod.Create(__initial.disabled)
        let _bgColor = ResetMod.Create(__initial.bgColor)
        let _size = MOption.Create(__initial.size)
        
        member x.text = _text :> IMod<_>
        member x.disabled = _disabled :> IMod<_>
        member x.bgColor = _bgColor :> IMod<_>
        member x.size = _size :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.TextInput) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_text,v.text)
                ResetMod.Update(_disabled,v.disabled)
                ResetMod.Update(_bgColor,v.bgColor)
                MOption.Update(_size, v.size)
                
        
        static member Create(__initial : CorrelationDrawing.TextInput) : MTextInput = MTextInput(__initial)
        static member Update(m : MTextInput, v : CorrelationDrawing.TextInput) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.TextInput> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TextInput =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let text =
                { new Lens<CorrelationDrawing.TextInput, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let disabled =
                { new Lens<CorrelationDrawing.TextInput, System.Boolean>() with
                    override x.Get(r) = r.disabled
                    override x.Set(r,v) = { r with disabled = v }
                    override x.Update(r,f) = { r with disabled = f r.disabled }
                }
            let bgColor =
                { new Lens<CorrelationDrawing.TextInput, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.bgColor
                    override x.Set(r,v) = { r with bgColor = v }
                    override x.Update(r,f) = { r with bgColor = f r.bgColor }
                }
            let size =
                { new Lens<CorrelationDrawing.TextInput, Microsoft.FSharp.Core.Option<System.Int32>>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
    [<AbstractClass; StructuredFormatDisplay("{AsString}")>]
    type MDropdownList<'va,'na>() = 
        abstract member valueList : Aardvark.Base.Incremental.alist<'na>
        abstract member selected : Aardvark.Base.Incremental.IMod<Microsoft.FSharp.Core.option<'na>>
        abstract member color : Aardvark.Base.Incremental.IMod<Aardvark.Base.C4b>
        abstract member searchable : Aardvark.Base.Incremental.IMod<System.Boolean>
        abstract member AsString : string
    
    
    and private MDropdownListD<'a,'ma,'va>(__initial : CorrelationDrawing.DropdownList<'a>, __ainit : 'a -> 'ma, __aupdate : 'ma * 'a -> unit, __aview : 'ma -> 'va) =
        inherit MDropdownList<'va,'va>()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.DropdownList<'a>>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>>
        let _valueList = MList.Create(__initial.valueList, (fun v -> __ainit(v)), (fun (m,v) -> __aupdate(m, v)), (fun v -> __aview(v)))
        let _selected = MOption.Create(__initial.selected, (fun v -> __ainit(v)), (fun (m,v) -> __aupdate(m, v)), (fun v -> __aview(v)))
        let _color = ResetMod.Create(__initial.color)
        let _searchable = ResetMod.Create(__initial.searchable)
        
        override x.valueList = _valueList :> alist<_>
        override x.selected = _selected :> IMod<_>
        override x.color = _color :> IMod<_>
        override x.searchable = _searchable :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.DropdownList<'a>) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_valueList, v.valueList)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_searchable,v.searchable)
                
        
        static member Update(m : MDropdownListD<'a,'ma,'va>, v : CorrelationDrawing.DropdownList<'a>) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        override x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.DropdownList<'a>> with
            member x.Update v = x.Update v
    
    and private MDropdownListV<'a>(__initial : CorrelationDrawing.DropdownList<'a>) =
        inherit MDropdownList<IMod<'a>,'a>()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.DropdownList<'a>>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.DropdownList<'a>>
        let _valueList = MList.Create(__initial.valueList)
        let _selected = MOption.Create(__initial.selected)
        let _color = ResetMod.Create(__initial.color)
        let _searchable = ResetMod.Create(__initial.searchable)
        
        override x.valueList = _valueList :> alist<_>
        override x.selected = _selected :> IMod<_>
        override x.color = _color :> IMod<_>
        override x.searchable = _searchable :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.DropdownList<'a>) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_valueList, v.valueList)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_searchable,v.searchable)
                
        
        static member Update(m : MDropdownListV<'a>, v : CorrelationDrawing.DropdownList<'a>) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        override x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.DropdownList<'a>> with
            member x.Update v = x.Update v
    
    and [<AbstractClass; Sealed>] MDropdownList private() =
        static member Create<'a,'ma,'va>(__initial : CorrelationDrawing.DropdownList<'a>, __ainit : 'a -> 'ma, __aupdate : 'ma * 'a -> unit, __aview : 'ma -> 'va) : MDropdownList<'va,'va> = MDropdownListD<'a,'ma,'va>(__initial, __ainit, __aupdate, __aview) :> MDropdownList<'va,'va>
        static member Create<'a>(__initial : CorrelationDrawing.DropdownList<'a>) : MDropdownList<IMod<'a>,'a> = MDropdownListV<'a>(__initial) :> MDropdownList<IMod<'a>,'a>
        static member Update<'a,'xva,'xna>(m : MDropdownList<'xva,'xna>, v : CorrelationDrawing.DropdownList<'a>) : unit = 
            match m :> obj with
            | :? IUpdatable<CorrelationDrawing.DropdownList<'a>> as m -> m.Update(v)
            | _ -> failwith "cannot update"
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DropdownList =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let valueList<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Aardvark.Base.plist<'a>>() with
                    override x.Get(r) = r.valueList
                    override x.Set(r,v) = { r with valueList = v }
                    override x.Update(r,f) = { r with valueList = f r.valueList }
                }
            let selected<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Microsoft.FSharp.Core.Option<'a>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let color<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let searchable<'a> =
                { new Lens<CorrelationDrawing.DropdownList<'a>, System.Boolean>() with
                    override x.Get(r) = r.searchable
                    override x.Set(r,v) = { r with searchable = v }
                    override x.Update(r,f) = { r with searchable = f r.searchable }
                }
    
    
    type MStyle(__initial : CorrelationDrawing.Style) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Style> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Style>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Style>
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        let _thickness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.thickness)
        
        member x.color = _color
        member x.thickness = _thickness
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Style) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                Aardvark.UI.Mutable.MNumericInput.Update(_thickness, v.thickness)
                
        
        static member Create(__initial : CorrelationDrawing.Style) : MStyle = MStyle(__initial)
        static member Update(m : MStyle, v : CorrelationDrawing.Style) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Style> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Style =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let color =
                { new Lens<CorrelationDrawing.Style, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let thickness =
                { new Lens<CorrelationDrawing.Style, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.thickness
                    override x.Set(r,v) = { r with thickness = v }
                    override x.Update(r,f) = { r with thickness = f r.thickness }
                }
    
    
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
    
    
    type MSemantic(__initial : CorrelationDrawing.Semantic) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Semantic> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Semantic>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Semantic>
        let _state = ResetMod.Create(__initial.state)
        let _label = MTextInput.Create(__initial.label)
        let _size = ResetMod.Create(__initial.size)
        let _style = MStyle.Create(__initial.style)
        let _semanticType = ResetMod.Create(__initial.semanticType)
        let _geometryType = ResetMod.Create(__initial.geometryType)
        let _level = ResetMod.Create(__initial.level)
        
        member x.id = __current.Value.id
        member x.timestamp = __current.Value.timestamp
        member x.state = _state :> IMod<_>
        member x.label = _label
        member x.size = _size :> IMod<_>
        member x.style = _style
        member x.semanticType = _semanticType :> IMod<_>
        member x.geometryType = _geometryType :> IMod<_>
        member x.level = _level :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Semantic) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_state,v.state)
                MTextInput.Update(_label, v.label)
                ResetMod.Update(_size,v.size)
                MStyle.Update(_style, v.style)
                ResetMod.Update(_semanticType,v.semanticType)
                ResetMod.Update(_geometryType,v.geometryType)
                ResetMod.Update(_level,v.level)
                
        
        static member Create(__initial : CorrelationDrawing.Semantic) : MSemantic = MSemantic(__initial)
        static member Update(m : MSemantic, v : CorrelationDrawing.Semantic) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Semantic> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Semantic =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.SemanticId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let timestamp =
                { new Lens<CorrelationDrawing.Semantic, System.String>() with
                    override x.Get(r) = r.timestamp
                    override x.Set(r,v) = { r with timestamp = v }
                    override x.Update(r,f) = { r with timestamp = f r.timestamp }
                }
            let state =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.State>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let label =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.TextInput>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let size =
                { new Lens<CorrelationDrawing.Semantic, System.Double>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
            let style =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.Style>() with
                    override x.Get(r) = r.style
                    override x.Set(r,v) = { r with style = v }
                    override x.Update(r,f) = { r with style = f r.style }
                }
            let semanticType =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.SemanticType>() with
                    override x.Get(r) = r.semanticType
                    override x.Set(r,v) = { r with semanticType = v }
                    override x.Update(r,f) = { r with semanticType = f r.semanticType }
                }
            let geometryType =
                { new Lens<CorrelationDrawing.Semantic, CorrelationDrawing.GeometryType>() with
                    override x.Get(r) = r.geometryType
                    override x.Set(r,v) = { r with geometryType = v }
                    override x.Update(r,f) = { r with geometryType = f r.geometryType }
                }
            let level =
                { new Lens<CorrelationDrawing.Semantic, System.Int32>() with
                    override x.Get(r) = r.level
                    override x.Set(r,v) = { r with level = v }
                    override x.Update(r,f) = { r with level = f r.level }
                }
    
    
    type MSemanticApp(__initial : CorrelationDrawing.SemanticApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.SemanticApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.SemanticApp>
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
        member x.Update(v : CorrelationDrawing.SemanticApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MMap.Update(_semantics, v.semantics)
                MList.Update(_semanticsList, v.semanticsList)
                ResetMod.Update(_selectedSemantic,v.selectedSemantic)
                ResetMod.Update(_sortBy,v.sortBy)
                ResetMod.Update(_creatingNew,v.creatingNew)
                
        
        static member Create(__initial : CorrelationDrawing.SemanticApp) : MSemanticApp = MSemanticApp(__initial)
        static member Update(m : MSemanticApp, v : CorrelationDrawing.SemanticApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.SemanticApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SemanticApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let semantics =
                { new Lens<CorrelationDrawing.SemanticApp, Aardvark.Base.hmap<CorrelationDrawing.SemanticId,CorrelationDrawing.Semantic>>() with
                    override x.Get(r) = r.semantics
                    override x.Set(r,v) = { r with semantics = v }
                    override x.Update(r,f) = { r with semantics = f r.semantics }
                }
            let semanticsList =
                { new Lens<CorrelationDrawing.SemanticApp, Aardvark.Base.plist<CorrelationDrawing.Semantic>>() with
                    override x.Get(r) = r.semanticsList
                    override x.Set(r,v) = { r with semanticsList = v }
                    override x.Update(r,f) = { r with semanticsList = f r.semanticsList }
                }
            let selectedSemantic =
                { new Lens<CorrelationDrawing.SemanticApp, CorrelationDrawing.SemanticId>() with
                    override x.Get(r) = r.selectedSemantic
                    override x.Set(r,v) = { r with selectedSemantic = v }
                    override x.Update(r,f) = { r with selectedSemantic = f r.selectedSemantic }
                }
            let sortBy =
                { new Lens<CorrelationDrawing.SemanticApp, CorrelationDrawing.SemanticsSortingOption>() with
                    override x.Get(r) = r.sortBy
                    override x.Set(r,v) = { r with sortBy = v }
                    override x.Update(r,f) = { r with sortBy = f r.sortBy }
                }
            let creatingNew =
                { new Lens<CorrelationDrawing.SemanticApp, System.Boolean>() with
                    override x.Get(r) = r.creatingNew
                    override x.Set(r,v) = { r with creatingNew = v }
                    override x.Update(r,f) = { r with creatingNew = f r.creatingNew }
                }
    
    
    type MAnnotationPoint(__initial : CorrelationDrawing.AnnotationPoint) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationPoint> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.AnnotationPoint>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationPoint>
        let _selected = ResetMod.Create(__initial.selected)
        
        member x.point = __current.Value.point
        member x.selected = _selected :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.AnnotationPoint) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_selected,v.selected)
                
        
        static member Create(__initial : CorrelationDrawing.AnnotationPoint) : MAnnotationPoint = MAnnotationPoint(__initial)
        static member Update(m : MAnnotationPoint, v : CorrelationDrawing.AnnotationPoint) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.AnnotationPoint> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationPoint =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let point =
                { new Lens<CorrelationDrawing.AnnotationPoint, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.point
                    override x.Set(r,v) = { r with point = v }
                    override x.Update(r,f) = { r with point = f r.point }
                }
            let selected =
                { new Lens<CorrelationDrawing.AnnotationPoint, System.Boolean>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
    
    
    type MAnnotation(__initial : CorrelationDrawing.Annotation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Annotation> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Annotation>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Annotation>
        let _selected = ResetMod.Create(__initial.selected)
        let _hovered = ResetMod.Create(__initial.hovered)
        let _semanticId = ResetMod.Create(__initial.semanticId)
        let _points = MList.Create(__initial.points, (fun v -> MAnnotationPoint.Create(v)), (fun (m,v) -> MAnnotationPoint.Update(m, v)), (fun v -> v))
        let _segments = MList.Create(__initial.segments, (fun v -> MList.Create(v)), (fun (m,v) -> MList.Update(m, v)), (fun v -> v :> alist<_>))
        let _visible = ResetMod.Create(__initial.visible)
        let _text = ResetMod.Create(__initial.text)
        let _overrideStyle = MOption.Create(__initial.overrideStyle, (fun v -> MStyle.Create(v)), (fun (m,v) -> MStyle.Update(m, v)), (fun v -> v))
        
        member x.id = __current.Value.id
        member x.geometry = __current.Value.geometry
        member x.projection = __current.Value.projection
        member x.semanticType = __current.Value.semanticType
        member x.selected = _selected :> IMod<_>
        member x.hovered = _hovered :> IMod<_>
        member x.semanticId = _semanticId :> IMod<_>
        member x.points = _points :> alist<_>
        member x.segments = _segments :> alist<_>
        member x.visible = _visible :> IMod<_>
        member x.text = _text :> IMod<_>
        member x.overrideStyle = _overrideStyle :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Annotation) =
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
                
        
        static member Create(__initial : CorrelationDrawing.Annotation) : MAnnotation = MAnnotation(__initial)
        static member Update(m : MAnnotation, v : CorrelationDrawing.Annotation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Annotation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Annotation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.Annotation, System.String>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let geometry =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.GeometryType>() with
                    override x.Get(r) = r.geometry
                    override x.Set(r,v) = { r with geometry = v }
                    override x.Update(r,f) = { r with geometry = f r.geometry }
                }
            let projection =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.Projection>() with
                    override x.Get(r) = r.projection
                    override x.Set(r,v) = { r with projection = v }
                    override x.Update(r,f) = { r with projection = f r.projection }
                }
            let semanticType =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.SemanticType>() with
                    override x.Get(r) = r.semanticType
                    override x.Set(r,v) = { r with semanticType = v }
                    override x.Update(r,f) = { r with semanticType = f r.semanticType }
                }
            let selected =
                { new Lens<CorrelationDrawing.Annotation, System.Boolean>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let hovered =
                { new Lens<CorrelationDrawing.Annotation, System.Boolean>() with
                    override x.Get(r) = r.hovered
                    override x.Set(r,v) = { r with hovered = v }
                    override x.Update(r,f) = { r with hovered = f r.hovered }
                }
            let semanticId =
                { new Lens<CorrelationDrawing.Annotation, CorrelationDrawing.SemanticId>() with
                    override x.Get(r) = r.semanticId
                    override x.Set(r,v) = { r with semanticId = v }
                    override x.Update(r,f) = { r with semanticId = f r.semanticId }
                }
            let points =
                { new Lens<CorrelationDrawing.Annotation, Aardvark.Base.plist<CorrelationDrawing.AnnotationPoint>>() with
                    override x.Get(r) = r.points
                    override x.Set(r,v) = { r with points = v }
                    override x.Update(r,f) = { r with points = f r.points }
                }
            let segments =
                { new Lens<CorrelationDrawing.Annotation, Aardvark.Base.plist<Aardvark.Base.plist<Aardvark.Base.V3d>>>() with
                    override x.Get(r) = r.segments
                    override x.Set(r,v) = { r with segments = v }
                    override x.Update(r,f) = { r with segments = f r.segments }
                }
            let visible =
                { new Lens<CorrelationDrawing.Annotation, System.Boolean>() with
                    override x.Get(r) = r.visible
                    override x.Set(r,v) = { r with visible = v }
                    override x.Update(r,f) = { r with visible = f r.visible }
                }
            let text =
                { new Lens<CorrelationDrawing.Annotation, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let overrideStyle =
                { new Lens<CorrelationDrawing.Annotation, Microsoft.FSharp.Core.Option<CorrelationDrawing.Style>>() with
                    override x.Get(r) = r.overrideStyle
                    override x.Set(r,v) = { r with overrideStyle = v }
                    override x.Update(r,f) = { r with overrideStyle = f r.overrideStyle }
                }
    
    
    type MAnnotationApp(__initial : CorrelationDrawing.AnnotationApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.AnnotationApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.AnnotationApp>
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _selectedAnnotation = MOption.Create(__initial.selectedAnnotation)
        
        member x.annotations = _annotations :> alist<_>
        member x.selectedAnnotation = _selectedAnnotation :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.AnnotationApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_annotations, v.annotations)
                MOption.Update(_selectedAnnotation, v.selectedAnnotation)
                
        
        static member Create(__initial : CorrelationDrawing.AnnotationApp) : MAnnotationApp = MAnnotationApp(__initial)
        static member Update(m : MAnnotationApp, v : CorrelationDrawing.AnnotationApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.AnnotationApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AnnotationApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let annotations =
                { new Lens<CorrelationDrawing.AnnotationApp, Aardvark.Base.plist<CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let selectedAnnotation =
                { new Lens<CorrelationDrawing.AnnotationApp, Microsoft.FSharp.Core.Option<System.String>>() with
                    override x.Get(r) = r.selectedAnnotation
                    override x.Set(r,v) = { r with selectedAnnotation = v }
                    override x.Update(r,f) = { r with selectedAnnotation = f r.selectedAnnotation }
                }
    
    
    type MBorder(__initial : CorrelationDrawing.Border) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Border> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Border>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Border>
        let _nodeId = ResetMod.Create(__initial.nodeId)
        let _logId = ResetMod.Create(__initial.logId)
        let _isSelected = ResetMod.Create(__initial.isSelected)
        let _correlation = MOption.Create(__initial.correlation)
        let _anno = MAnnotation.Create(__initial.anno)
        let _point = ResetMod.Create(__initial.point)
        let _color = ResetMod.Create(__initial.color)
        let _weight = ResetMod.Create(__initial.weight)
        let _svgPosition = ResetMod.Create(__initial.svgPosition)
        
        member x.id = __current.Value.id
        member x.nodeId = _nodeId :> IMod<_>
        member x.logId = _logId :> IMod<_>
        member x.isSelected = _isSelected :> IMod<_>
        member x.correlation = _correlation :> IMod<_>
        member x.anno = _anno
        member x.point = _point :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.weight = _weight :> IMod<_>
        member x.svgPosition = _svgPosition :> IMod<_>
        member x.borderType = __current.Value.borderType
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Border) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_nodeId,v.nodeId)
                ResetMod.Update(_logId,v.logId)
                ResetMod.Update(_isSelected,v.isSelected)
                MOption.Update(_correlation, v.correlation)
                MAnnotation.Update(_anno, v.anno)
                ResetMod.Update(_point,v.point)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_weight,v.weight)
                ResetMod.Update(_svgPosition,v.svgPosition)
                
        
        static member Create(__initial : CorrelationDrawing.Border) : MBorder = MBorder(__initial)
        static member Update(m : MBorder, v : CorrelationDrawing.Border) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Border> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Border =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.Border, CorrelationDrawing.BorderId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let nodeId =
                { new Lens<CorrelationDrawing.Border, CorrelationDrawing.LogNodeId>() with
                    override x.Get(r) = r.nodeId
                    override x.Set(r,v) = { r with nodeId = v }
                    override x.Update(r,f) = { r with nodeId = f r.nodeId }
                }
            let logId =
                { new Lens<CorrelationDrawing.Border, CorrelationDrawing.LogId>() with
                    override x.Get(r) = r.logId
                    override x.Set(r,v) = { r with logId = v }
                    override x.Update(r,f) = { r with logId = f r.logId }
                }
            let isSelected =
                { new Lens<CorrelationDrawing.Border, System.Boolean>() with
                    override x.Get(r) = r.isSelected
                    override x.Set(r,v) = { r with isSelected = v }
                    override x.Update(r,f) = { r with isSelected = f r.isSelected }
                }
            let correlation =
                { new Lens<CorrelationDrawing.Border, Microsoft.FSharp.Core.Option<CorrelationDrawing.BorderId>>() with
                    override x.Get(r) = r.correlation
                    override x.Set(r,v) = { r with correlation = v }
                    override x.Update(r,f) = { r with correlation = f r.correlation }
                }
            let anno =
                { new Lens<CorrelationDrawing.Border, CorrelationDrawing.Annotation>() with
                    override x.Get(r) = r.anno
                    override x.Set(r,v) = { r with anno = v }
                    override x.Update(r,f) = { r with anno = f r.anno }
                }
            let point =
                { new Lens<CorrelationDrawing.Border, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.point
                    override x.Set(r,v) = { r with point = v }
                    override x.Update(r,f) = { r with point = f r.point }
                }
            let color =
                { new Lens<CorrelationDrawing.Border, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let weight =
                { new Lens<CorrelationDrawing.Border, System.Double>() with
                    override x.Get(r) = r.weight
                    override x.Set(r,v) = { r with weight = v }
                    override x.Update(r,f) = { r with weight = f r.weight }
                }
            let svgPosition =
                { new Lens<CorrelationDrawing.Border, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.svgPosition
                    override x.Set(r,v) = { r with svgPosition = v }
                    override x.Update(r,f) = { r with svgPosition = f r.svgPosition }
                }
            let borderType =
                { new Lens<CorrelationDrawing.Border, CorrelationDrawing.BorderType>() with
                    override x.Get(r) = r.borderType
                    override x.Set(r,v) = { r with borderType = v }
                    override x.Update(r,f) = { r with borderType = f r.borderType }
                }
    
    
    type MLogAxisSection(__initial : CorrelationDrawing.LogAxisSection) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogAxisSection> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogAxisSection>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogAxisSection>
        let _label = ResetMod.Create(__initial.label)
        let _color = ResetMod.Create(__initial.color)
        let _range = ResetMod.Create(__initial.range)
        
        member x.label = _label :> IMod<_>
        member x.color = _color :> IMod<_>
        member x.range = _range :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogAxisSection) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_label,v.label)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_range,v.range)
                
        
        static member Create(__initial : CorrelationDrawing.LogAxisSection) : MLogAxisSection = MLogAxisSection(__initial)
        static member Update(m : MLogAxisSection, v : CorrelationDrawing.LogAxisSection) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogAxisSection> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LogAxisSection =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let label =
                { new Lens<CorrelationDrawing.LogAxisSection, System.String>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let color =
                { new Lens<CorrelationDrawing.LogAxisSection, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let range =
                { new Lens<CorrelationDrawing.LogAxisSection, CorrelationDrawing.Rangef>() with
                    override x.Get(r) = r.range
                    override x.Set(r,v) = { r with range = v }
                    override x.Update(r,f) = { r with range = f r.range }
                }
    
    
    type MLogNode(__initial : CorrelationDrawing.LogNode) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNode> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogNode>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogNode>
        let _label = ResetMod.Create(__initial.label)
        let _isSelected = ResetMod.Create(__initial.isSelected)
        let _hasDefaultX = ResetMod.Create(__initial.hasDefaultX)
        let _nodeType = ResetMod.Create(__initial.nodeType)
        let _level = ResetMod.Create(__initial.level)
        let _lBorder = MBorder.Create(__initial.lBorder)
        let _uBorder = MBorder.Create(__initial.uBorder)
        let _children = MList.Create(__initial.children, (fun v -> MLogNode.Create(v)), (fun (m,v) -> MLogNode.Update(m, v)), (fun v -> v))
        let _svgPos = ResetMod.Create(__initial.svgPos)
        let _nativePos = ResetMod.Create(__initial.nativePos)
        let _svgSize = ResetMod.Create(__initial.svgSize)
        let _nativeSize = ResetMod.Create(__initial.nativeSize)
        
        member x.id = __current.Value.id
        member x.label = _label :> IMod<_>
        member x.isSelected = _isSelected :> IMod<_>
        member x.hasDefaultX = _hasDefaultX :> IMod<_>
        member x.nodeType = _nodeType :> IMod<_>
        member x.level = _level :> IMod<_>
        member x.lBorder = _lBorder
        member x.uBorder = _uBorder
        member x.children = _children :> alist<_>
        member x.svgPos = _svgPos :> IMod<_>
        member x.nativePos = _nativePos :> IMod<_>
        member x.svgSize = _svgSize :> IMod<_>
        member x.nativeSize = _nativeSize :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogNode) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_label,v.label)
                ResetMod.Update(_isSelected,v.isSelected)
                ResetMod.Update(_hasDefaultX,v.hasDefaultX)
                ResetMod.Update(_nodeType,v.nodeType)
                ResetMod.Update(_level,v.level)
                MBorder.Update(_lBorder, v.lBorder)
                MBorder.Update(_uBorder, v.uBorder)
                MList.Update(_children, v.children)
                ResetMod.Update(_svgPos,v.svgPos)
                ResetMod.Update(_nativePos,v.nativePos)
                ResetMod.Update(_svgSize,v.svgSize)
                ResetMod.Update(_nativeSize,v.nativeSize)
                
        
        static member Create(__initial : CorrelationDrawing.LogNode) : MLogNode = MLogNode(__initial)
        static member Update(m : MLogNode, v : CorrelationDrawing.LogNode) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogNode> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LogNode =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.LogNodeId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let label =
                { new Lens<CorrelationDrawing.LogNode, System.String>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let isSelected =
                { new Lens<CorrelationDrawing.LogNode, System.Boolean>() with
                    override x.Get(r) = r.isSelected
                    override x.Set(r,v) = { r with isSelected = v }
                    override x.Update(r,f) = { r with isSelected = f r.isSelected }
                }
            let hasDefaultX =
                { new Lens<CorrelationDrawing.LogNode, System.Boolean>() with
                    override x.Get(r) = r.hasDefaultX
                    override x.Set(r,v) = { r with hasDefaultX = v }
                    override x.Update(r,f) = { r with hasDefaultX = f r.hasDefaultX }
                }
            let nodeType =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.LogNodeType>() with
                    override x.Get(r) = r.nodeType
                    override x.Set(r,v) = { r with nodeType = v }
                    override x.Update(r,f) = { r with nodeType = f r.nodeType }
                }
            let level =
                { new Lens<CorrelationDrawing.LogNode, System.Int32>() with
                    override x.Get(r) = r.level
                    override x.Set(r,v) = { r with level = v }
                    override x.Update(r,f) = { r with level = f r.level }
                }
            let lBorder =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.Border>() with
                    override x.Get(r) = r.lBorder
                    override x.Set(r,v) = { r with lBorder = v }
                    override x.Update(r,f) = { r with lBorder = f r.lBorder }
                }
            let uBorder =
                { new Lens<CorrelationDrawing.LogNode, CorrelationDrawing.Border>() with
                    override x.Get(r) = r.uBorder
                    override x.Set(r,v) = { r with uBorder = v }
                    override x.Update(r,f) = { r with uBorder = f r.uBorder }
                }
            let children =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.plist<CorrelationDrawing.LogNode>>() with
                    override x.Get(r) = r.children
                    override x.Set(r,v) = { r with children = v }
                    override x.Update(r,f) = { r with children = f r.children }
                }
            let svgPos =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.svgPos
                    override x.Set(r,v) = { r with svgPos = v }
                    override x.Update(r,f) = { r with svgPos = f r.svgPos }
                }
            let nativePos =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.nativePos
                    override x.Set(r,v) = { r with nativePos = v }
                    override x.Update(r,f) = { r with nativePos = f r.nativePos }
                }
            let svgSize =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.svgSize
                    override x.Set(r,v) = { r with svgSize = v }
                    override x.Update(r,f) = { r with svgSize = f r.svgSize }
                }
            let nativeSize =
                { new Lens<CorrelationDrawing.LogNode, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.nativeSize
                    override x.Set(r,v) = { r with nativeSize = v }
                    override x.Update(r,f) = { r with nativeSize = f r.nativeSize }
                }
    
    
    type MLogAxisConfig(__initial : CorrelationDrawing.LogAxisConfig) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogAxisConfig> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogAxisConfig>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogAxisConfig>
        
        member x.id = __current.Value.id
        member x.label = __current.Value.label
        member x.defaultRange = __current.Value.defaultRange
        member x.defaultGranularity = __current.Value.defaultGranularity
        member x.styleTemplate = __current.Value.styleTemplate
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogAxisConfig) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                
        
        static member Create(__initial : CorrelationDrawing.LogAxisConfig) : MLogAxisConfig = MLogAxisConfig(__initial)
        static member Update(m : MLogAxisConfig, v : CorrelationDrawing.LogAxisConfig) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogAxisConfig> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LogAxisConfig =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.LogAxisConfig, CorrelationDrawing.LogAxisConfigId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let label =
                { new Lens<CorrelationDrawing.LogAxisConfig, System.String>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let defaultRange =
                { new Lens<CorrelationDrawing.LogAxisConfig, CorrelationDrawing.Rangef>() with
                    override x.Get(r) = r.defaultRange
                    override x.Set(r,v) = { r with defaultRange = v }
                    override x.Update(r,f) = { r with defaultRange = f r.defaultRange }
                }
            let defaultGranularity =
                { new Lens<CorrelationDrawing.LogAxisConfig, System.Double>() with
                    override x.Get(r) = r.defaultGranularity
                    override x.Set(r,v) = { r with defaultGranularity = v }
                    override x.Update(r,f) = { r with defaultGranularity = f r.defaultGranularity }
                }
            let styleTemplate =
                { new Lens<CorrelationDrawing.LogAxisConfig, Microsoft.FSharp.Collections.List<CorrelationDrawing.LogAxisSection>>() with
                    override x.Get(r) = r.styleTemplate
                    override x.Set(r,v) = { r with styleTemplate = v }
                    override x.Update(r,f) = { r with styleTemplate = f r.styleTemplate }
                }
    
    
    type MLogAxisApp(__initial : CorrelationDrawing.LogAxisApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogAxisApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.LogAxisApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.LogAxisApp>
        let _selectedTemplate = ResetMod.Create(__initial.selectedTemplate)
        
        member x.templates = __current.Value.templates
        member x.selectedTemplate = _selectedTemplate :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.LogAxisApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_selectedTemplate,v.selectedTemplate)
                
        
        static member Create(__initial : CorrelationDrawing.LogAxisApp) : MLogAxisApp = MLogAxisApp(__initial)
        static member Update(m : MLogAxisApp, v : CorrelationDrawing.LogAxisApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.LogAxisApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LogAxisApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let templates =
                { new Lens<CorrelationDrawing.LogAxisApp, Microsoft.FSharp.Collections.List<CorrelationDrawing.LogAxisConfig>>() with
                    override x.Get(r) = r.templates
                    override x.Set(r,v) = { r with templates = v }
                    override x.Update(r,f) = { r with templates = f r.templates }
                }
            let selectedTemplate =
                { new Lens<CorrelationDrawing.LogAxisApp, CorrelationDrawing.LogAxisConfigId>() with
                    override x.Get(r) = r.selectedTemplate
                    override x.Set(r,v) = { r with selectedTemplate = v }
                    override x.Update(r,f) = { r with selectedTemplate = f r.selectedTemplate }
                }
    
    
    type MGeologicalLog(__initial : CorrelationDrawing.GeologicalLog) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.GeologicalLog> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.GeologicalLog>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.GeologicalLog>
        let _index = ResetMod.Create(__initial.index)
        let _state = ResetMod.Create(__initial.state)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _isSelected = ResetMod.Create(__initial.isSelected)
        let _label = MTextInput.Create(__initial.label)
        let _annoPoints = ResetMod.Create(__initial.annoPoints)
        let _nodes = MList.Create(__initial.nodes, (fun v -> MLogNode.Create(v)), (fun (m,v) -> MLogNode.Update(m, v)), (fun v -> v))
        let _nativeYRange = ResetMod.Create(__initial.nativeYRange)
        let _svgMaxX = ResetMod.Create(__initial.svgMaxX)
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _semanticApp = MSemanticApp.Create(__initial.semanticApp)
        let _xAxis = ResetMod.Create(__initial.xAxis)
        let _yOffset = ResetMod.Create(__initial.yOffset)
        
        member x.id = __current.Value.id
        member x.index = _index :> IMod<_>
        member x.state = _state :> IMod<_>
        member x.isVisible = _isVisible :> IMod<_>
        member x.isSelected = _isSelected :> IMod<_>
        member x.label = _label
        member x.annoPoints = _annoPoints :> IMod<_>
        member x.nodes = _nodes :> alist<_>
        member x.nativeYRange = _nativeYRange :> IMod<_>
        member x.svgMaxX = _svgMaxX :> IMod<_>
        member x.camera = _camera
        member x.semanticApp = _semanticApp
        member x.xAxis = _xAxis :> IMod<_>
        member x.yOffset = _yOffset :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.GeologicalLog) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_index,v.index)
                ResetMod.Update(_state,v.state)
                ResetMod.Update(_isVisible,v.isVisible)
                ResetMod.Update(_isSelected,v.isSelected)
                MTextInput.Update(_label, v.label)
                ResetMod.Update(_annoPoints,v.annoPoints)
                MList.Update(_nodes, v.nodes)
                ResetMod.Update(_nativeYRange,v.nativeYRange)
                ResetMod.Update(_svgMaxX,v.svgMaxX)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                MSemanticApp.Update(_semanticApp, v.semanticApp)
                ResetMod.Update(_xAxis,v.xAxis)
                ResetMod.Update(_yOffset,v.yOffset)
                
        
        static member Create(__initial : CorrelationDrawing.GeologicalLog) : MGeologicalLog = MGeologicalLog(__initial)
        static member Update(m : MGeologicalLog, v : CorrelationDrawing.GeologicalLog) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.GeologicalLog> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module GeologicalLog =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.LogId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let index =
                { new Lens<CorrelationDrawing.GeologicalLog, System.Int32>() with
                    override x.Get(r) = r.index
                    override x.Set(r,v) = { r with index = v }
                    override x.Update(r,f) = { r with index = f r.index }
                }
            let state =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.State>() with
                    override x.Get(r) = r.state
                    override x.Set(r,v) = { r with state = v }
                    override x.Update(r,f) = { r with state = f r.state }
                }
            let isVisible =
                { new Lens<CorrelationDrawing.GeologicalLog, System.Boolean>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
            let isSelected =
                { new Lens<CorrelationDrawing.GeologicalLog, System.Boolean>() with
                    override x.Get(r) = r.isSelected
                    override x.Set(r,v) = { r with isSelected = v }
                    override x.Update(r,f) = { r with isSelected = f r.isSelected }
                }
            let label =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.TextInput>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let annoPoints =
                { new Lens<CorrelationDrawing.GeologicalLog, Microsoft.FSharp.Collections.List<(Aardvark.Base.V3d * CorrelationDrawing.Annotation)>>() with
                    override x.Get(r) = r.annoPoints
                    override x.Set(r,v) = { r with annoPoints = v }
                    override x.Update(r,f) = { r with annoPoints = f r.annoPoints }
                }
            let nodes =
                { new Lens<CorrelationDrawing.GeologicalLog, Aardvark.Base.plist<CorrelationDrawing.LogNode>>() with
                    override x.Get(r) = r.nodes
                    override x.Set(r,v) = { r with nodes = v }
                    override x.Update(r,f) = { r with nodes = f r.nodes }
                }
            let nativeYRange =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.Rangef>() with
                    override x.Get(r) = r.nativeYRange
                    override x.Set(r,v) = { r with nativeYRange = v }
                    override x.Update(r,f) = { r with nativeYRange = f r.nativeYRange }
                }
            let svgMaxX =
                { new Lens<CorrelationDrawing.GeologicalLog, System.Double>() with
                    override x.Get(r) = r.svgMaxX
                    override x.Set(r,v) = { r with svgMaxX = v }
                    override x.Update(r,f) = { r with svgMaxX = f r.svgMaxX }
                }
            let camera =
                { new Lens<CorrelationDrawing.GeologicalLog, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.camera
                    override x.Set(r,v) = { r with camera = v }
                    override x.Update(r,f) = { r with camera = f r.camera }
                }
            let semanticApp =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.SemanticApp>() with
                    override x.Get(r) = r.semanticApp
                    override x.Set(r,v) = { r with semanticApp = v }
                    override x.Update(r,f) = { r with semanticApp = f r.semanticApp }
                }
            let xAxis =
                { new Lens<CorrelationDrawing.GeologicalLog, CorrelationDrawing.SemanticId>() with
                    override x.Get(r) = r.xAxis
                    override x.Set(r,v) = { r with xAxis = v }
                    override x.Update(r,f) = { r with xAxis = f r.xAxis }
                }
            let yOffset =
                { new Lens<CorrelationDrawing.GeologicalLog, System.Double>() with
                    override x.Get(r) = r.yOffset
                    override x.Set(r,v) = { r with yOffset = v }
                    override x.Update(r,f) = { r with yOffset = f r.yOffset }
                }
    
    
    type MCorrelation(__initial : CorrelationDrawing.Correlation) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Correlation> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.Correlation>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.Correlation>
        let _fromBorder = MBorder.Create(__initial.fromBorder)
        let _toBorder = MBorder.Create(__initial.toBorder)
        
        member x.fromBorder = _fromBorder
        member x.toBorder = _toBorder
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Correlation) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MBorder.Update(_fromBorder, v.fromBorder)
                MBorder.Update(_toBorder, v.toBorder)
                
        
        static member Create(__initial : CorrelationDrawing.Correlation) : MCorrelation = MCorrelation(__initial)
        static member Update(m : MCorrelation, v : CorrelationDrawing.Correlation) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.Correlation> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Correlation =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let fromBorder =
                { new Lens<CorrelationDrawing.Correlation, CorrelationDrawing.Border>() with
                    override x.Get(r) = r.fromBorder
                    override x.Set(r,v) = { r with fromBorder = v }
                    override x.Update(r,f) = { r with fromBorder = f r.fromBorder }
                }
            let toBorder =
                { new Lens<CorrelationDrawing.Correlation, CorrelationDrawing.Border>() with
                    override x.Get(r) = r.toBorder
                    override x.Set(r,v) = { r with toBorder = v }
                    override x.Update(r,f) = { r with toBorder = f r.toBorder }
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
        let _offset = ResetMod.Create(__initial.offset)
        let _zoom = ResetMod.Create(__initial.zoom)
        let _fontSize = ResetMod.Create(__initial.fontSize)
        
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
        member x.offset = _offset :> IMod<_>
        member x.zoom = _zoom :> IMod<_>
        member x.fontSize = _fontSize :> IMod<_>
        
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
                ResetMod.Update(_offset,v.offset)
                ResetMod.Update(_zoom,v.zoom)
                ResetMod.Update(_fontSize,v.fontSize)
                
        
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
            let offset =
                { new Lens<CorrelationDrawing.SvgOptions, Aardvark.Base.V2d>() with
                    override x.Get(r) = r.offset
                    override x.Set(r,v) = { r with offset = v }
                    override x.Update(r,f) = { r with offset = f r.offset }
                }
            let zoom =
                { new Lens<CorrelationDrawing.SvgOptions, CorrelationDrawing.SvgZoom>() with
                    override x.Get(r) = r.zoom
                    override x.Set(r,v) = { r with zoom = v }
                    override x.Update(r,f) = { r with zoom = f r.zoom }
                }
            let fontSize =
                { new Lens<CorrelationDrawing.SvgOptions, CorrelationDrawing.FontSize>() with
                    override x.Get(r) = r.fontSize
                    override x.Set(r,v) = { r with fontSize = v }
                    override x.Update(r,f) = { r with fontSize = f r.fontSize }
                }
    
    
    type MCorrelationPlot(__initial : CorrelationDrawing.CorrelationPlot) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlot> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationPlot>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlot>
        let _logs = MList.Create(__initial.logs, (fun v -> MGeologicalLog.Create(v)), (fun (m,v) -> MGeologicalLog.Update(m, v)), (fun v -> v))
        let _correlations = MList.Create(__initial.correlations, (fun v -> MCorrelation.Create(v)), (fun (m,v) -> MCorrelation.Update(m, v)), (fun v -> v))
        let _selectedBorder = MOption.Create(__initial.selectedBorder, (fun v -> MBorder.Create(v)), (fun (m,v) -> MBorder.Update(m, v)), (fun v -> v))
        let _editCorrelations = ResetMod.Create(__initial.editCorrelations)
        let _selectedPoints = ResetMod.Create(__initial.selectedPoints)
        let _annotations = MList.Create(__initial.annotations, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _selectedLog = MOption.Create(__initial.selectedLog)
        let _secondaryLvl = ResetMod.Create(__initial.secondaryLvl)
        let _viewType = ResetMod.Create(__initial.viewType)
        let _svgFlags = ResetMod.Create(__initial.svgFlags)
        let _svgOptions = MSvgOptions.Create(__initial.svgOptions)
        let _logAxisApp = MLogAxisApp.Create(__initial.logAxisApp)
        let _xAxis = ResetMod.Create(__initial.xAxis)
        let _semanticApp = MSemanticApp.Create(__initial.semanticApp)
        let _currrentYMapping = MOption.Create(__initial.currrentYMapping)
        let _yRange = ResetMod.Create(__initial.yRange)
        
        member x.logs = _logs :> alist<_>
        member x.correlations = _correlations :> alist<_>
        member x.selectedBorder = _selectedBorder :> IMod<_>
        member x.editCorrelations = _editCorrelations :> IMod<_>
        member x.selectedPoints = _selectedPoints :> IMod<_>
        member x.annotations = _annotations :> alist<_>
        member x.selectedLog = _selectedLog :> IMod<_>
        member x.secondaryLvl = _secondaryLvl :> IMod<_>
        member x.viewType = _viewType :> IMod<_>
        member x.svgFlags = _svgFlags :> IMod<_>
        member x.svgOptions = _svgOptions
        member x.logAxisApp = _logAxisApp
        member x.xAxis = _xAxis :> IMod<_>
        member x.semanticApp = _semanticApp
        member x.currrentYMapping = _currrentYMapping :> IMod<_>
        member x.yRange = _yRange :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationPlot) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_logs, v.logs)
                MList.Update(_correlations, v.correlations)
                MOption.Update(_selectedBorder, v.selectedBorder)
                ResetMod.Update(_editCorrelations,v.editCorrelations)
                ResetMod.Update(_selectedPoints,v.selectedPoints)
                MList.Update(_annotations, v.annotations)
                MOption.Update(_selectedLog, v.selectedLog)
                ResetMod.Update(_secondaryLvl,v.secondaryLvl)
                ResetMod.Update(_viewType,v.viewType)
                ResetMod.Update(_svgFlags,v.svgFlags)
                MSvgOptions.Update(_svgOptions, v.svgOptions)
                MLogAxisApp.Update(_logAxisApp, v.logAxisApp)
                ResetMod.Update(_xAxis,v.xAxis)
                MSemanticApp.Update(_semanticApp, v.semanticApp)
                MOption.Update(_currrentYMapping, v.currrentYMapping)
                ResetMod.Update(_yRange,v.yRange)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationPlot) : MCorrelationPlot = MCorrelationPlot(__initial)
        static member Update(m : MCorrelationPlot, v : CorrelationDrawing.CorrelationPlot) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationPlot> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationPlot =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let logs =
                { new Lens<CorrelationDrawing.CorrelationPlot, Aardvark.Base.plist<CorrelationDrawing.GeologicalLog>>() with
                    override x.Get(r) = r.logs
                    override x.Set(r,v) = { r with logs = v }
                    override x.Update(r,f) = { r with logs = f r.logs }
                }
            let correlations =
                { new Lens<CorrelationDrawing.CorrelationPlot, Aardvark.Base.plist<CorrelationDrawing.Correlation>>() with
                    override x.Get(r) = r.correlations
                    override x.Set(r,v) = { r with correlations = v }
                    override x.Update(r,f) = { r with correlations = f r.correlations }
                }
            let selectedBorder =
                { new Lens<CorrelationDrawing.CorrelationPlot, Microsoft.FSharp.Core.Option<CorrelationDrawing.Border>>() with
                    override x.Get(r) = r.selectedBorder
                    override x.Set(r,v) = { r with selectedBorder = v }
                    override x.Update(r,f) = { r with selectedBorder = f r.selectedBorder }
                }
            let editCorrelations =
                { new Lens<CorrelationDrawing.CorrelationPlot, System.Boolean>() with
                    override x.Get(r) = r.editCorrelations
                    override x.Set(r,v) = { r with editCorrelations = v }
                    override x.Update(r,f) = { r with editCorrelations = f r.editCorrelations }
                }
            let selectedPoints =
                { new Lens<CorrelationDrawing.CorrelationPlot, Microsoft.FSharp.Collections.List<(Aardvark.Base.V3d * CorrelationDrawing.Annotation)>>() with
                    override x.Get(r) = r.selectedPoints
                    override x.Set(r,v) = { r with selectedPoints = v }
                    override x.Update(r,f) = { r with selectedPoints = f r.selectedPoints }
                }
            let annotations =
                { new Lens<CorrelationDrawing.CorrelationPlot, Aardvark.Base.plist<CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.annotations
                    override x.Set(r,v) = { r with annotations = v }
                    override x.Update(r,f) = { r with annotations = f r.annotations }
                }
            let selectedLog =
                { new Lens<CorrelationDrawing.CorrelationPlot, Microsoft.FSharp.Core.Option<CorrelationDrawing.LogId>>() with
                    override x.Get(r) = r.selectedLog
                    override x.Set(r,v) = { r with selectedLog = v }
                    override x.Update(r,f) = { r with selectedLog = f r.selectedLog }
                }
            let secondaryLvl =
                { new Lens<CorrelationDrawing.CorrelationPlot, System.Int32>() with
                    override x.Get(r) = r.secondaryLvl
                    override x.Set(r,v) = { r with secondaryLvl = v }
                    override x.Update(r,f) = { r with secondaryLvl = f r.secondaryLvl }
                }
            let viewType =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.CorrelationPlotViewType>() with
                    override x.Get(r) = r.viewType
                    override x.Set(r,v) = { r with viewType = v }
                    override x.Update(r,f) = { r with viewType = f r.viewType }
                }
            let svgFlags =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.SvgFlags>() with
                    override x.Get(r) = r.svgFlags
                    override x.Set(r,v) = { r with svgFlags = v }
                    override x.Update(r,f) = { r with svgFlags = f r.svgFlags }
                }
            let svgOptions =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.SvgOptions>() with
                    override x.Get(r) = r.svgOptions
                    override x.Set(r,v) = { r with svgOptions = v }
                    override x.Update(r,f) = { r with svgOptions = f r.svgOptions }
                }
            let logAxisApp =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.LogAxisApp>() with
                    override x.Get(r) = r.logAxisApp
                    override x.Set(r,v) = { r with logAxisApp = v }
                    override x.Update(r,f) = { r with logAxisApp = f r.logAxisApp }
                }
            let xAxis =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.SemanticId>() with
                    override x.Get(r) = r.xAxis
                    override x.Set(r,v) = { r with xAxis = v }
                    override x.Update(r,f) = { r with xAxis = f r.xAxis }
                }
            let semanticApp =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.SemanticApp>() with
                    override x.Get(r) = r.semanticApp
                    override x.Set(r,v) = { r with semanticApp = v }
                    override x.Update(r,f) = { r with semanticApp = f r.semanticApp }
                }
            let currrentYMapping =
                { new Lens<CorrelationDrawing.CorrelationPlot, Microsoft.FSharp.Core.Option<System.Double>>() with
                    override x.Get(r) = r.currrentYMapping
                    override x.Set(r,v) = { r with currrentYMapping = v }
                    override x.Update(r,f) = { r with currrentYMapping = f r.currrentYMapping }
                }
            let yRange =
                { new Lens<CorrelationDrawing.CorrelationPlot, CorrelationDrawing.Rangef>() with
                    override x.Get(r) = r.yRange
                    override x.Set(r,v) = { r with yRange = v }
                    override x.Update(r,f) = { r with yRange = f r.yRange }
                }
    
    
    type MCorrelationPlotApp(__initial : CorrelationDrawing.CorrelationPlotApp) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlotApp> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationPlotApp>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationPlotApp>
        let _correlationPlot = MCorrelationPlot.Create(__initial.correlationPlot)
        let _semanticApp = MSemanticApp.Create(__initial.semanticApp)
        let _zooming = ResetMod.Create(__initial.zooming)
        let _dragging = ResetMod.Create(__initial.dragging)
        let _lastMousePos = ResetMod.Create(__initial.lastMousePos)
        
        member x.correlationPlot = _correlationPlot
        member x.semanticApp = _semanticApp
        member x.zooming = _zooming :> IMod<_>
        member x.dragging = _dragging :> IMod<_>
        member x.lastMousePos = _lastMousePos :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationPlotApp) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MCorrelationPlot.Update(_correlationPlot, v.correlationPlot)
                MSemanticApp.Update(_semanticApp, v.semanticApp)
                ResetMod.Update(_zooming,v.zooming)
                ResetMod.Update(_dragging,v.dragging)
                ResetMod.Update(_lastMousePos,v.lastMousePos)
                
        
        static member Create(__initial : CorrelationDrawing.CorrelationPlotApp) : MCorrelationPlotApp = MCorrelationPlotApp(__initial)
        static member Update(m : MCorrelationPlotApp, v : CorrelationDrawing.CorrelationPlotApp) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<CorrelationDrawing.CorrelationPlotApp> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CorrelationPlotApp =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let correlationPlot =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, CorrelationDrawing.CorrelationPlot>() with
                    override x.Get(r) = r.correlationPlot
                    override x.Set(r,v) = { r with correlationPlot = v }
                    override x.Update(r,f) = { r with correlationPlot = f r.correlationPlot }
                }
            let semanticApp =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, CorrelationDrawing.SemanticApp>() with
                    override x.Get(r) = r.semanticApp
                    override x.Set(r,v) = { r with semanticApp = v }
                    override x.Update(r,f) = { r with semanticApp = f r.semanticApp }
                }
            let zooming =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, System.Boolean>() with
                    override x.Get(r) = r.zooming
                    override x.Set(r,v) = { r with zooming = v }
                    override x.Update(r,f) = { r with zooming = f r.zooming }
                }
            let dragging =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, System.Boolean>() with
                    override x.Get(r) = r.dragging
                    override x.Set(r,v) = { r with dragging = v }
                    override x.Update(r,f) = { r with dragging = f r.dragging }
                }
            let lastMousePos =
                { new Lens<CorrelationDrawing.CorrelationPlotApp, Aardvark.Base.V2i>() with
                    override x.Get(r) = r.lastMousePos
                    override x.Set(r,v) = { r with lastMousePos = v }
                    override x.Update(r,f) = { r with lastMousePos = f r.lastMousePos }
                }
    
    
    type MCorrelationDrawingModel(__initial : CorrelationDrawing.CorrelationDrawingModel) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationDrawingModel> = Aardvark.Base.Incremental.EqModRef<CorrelationDrawing.CorrelationDrawingModel>(__initial) :> Aardvark.Base.Incremental.IModRef<CorrelationDrawing.CorrelationDrawingModel>
        let _isDrawing = ResetMod.Create(__initial.isDrawing)
        let _hoverPosition = MOption.Create(__initial.hoverPosition)
        let _working = MOption.Create(__initial.working, (fun v -> MAnnotation.Create(v)), (fun (m,v) -> MAnnotation.Update(m, v)), (fun v -> v))
        let _projection = ResetMod.Create(__initial.projection)
        let _exportPath = ResetMod.Create(__initial.exportPath)
        
        member x.isDrawing = _isDrawing :> IMod<_>
        member x.hoverPosition = _hoverPosition :> IMod<_>
        member x.working = _working :> IMod<_>
        member x.projection = _projection :> IMod<_>
        member x.exportPath = _exportPath :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.CorrelationDrawingModel) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_isDrawing,v.isDrawing)
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
            let isDrawing =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, System.Boolean>() with
                    override x.Get(r) = r.isDrawing
                    override x.Set(r,v) = { r with isDrawing = v }
                    override x.Update(r,f) = { r with isDrawing = f r.isDrawing }
                }
            let hoverPosition =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.Option<Aardvark.Base.Trafo3d>>() with
                    override x.Get(r) = r.hoverPosition
                    override x.Set(r,v) = { r with hoverPosition = v }
                    override x.Update(r,f) = { r with hoverPosition = f r.hoverPosition }
                }
            let working =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, Microsoft.FSharp.Core.Option<CorrelationDrawing.Annotation>>() with
                    override x.Get(r) = r.working
                    override x.Set(r,v) = { r with working = v }
                    override x.Update(r,f) = { r with working = f r.working }
                }
            let projection =
                { new Lens<CorrelationDrawing.CorrelationDrawingModel, CorrelationDrawing.Projection>() with
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
        let _appFlags = ResetMod.Create(__initial.appFlags)
        let _sgFlags = ResetMod.Create(__initial.sgFlags)
        let _camera = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.camera)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        let _fill = ResetMod.Create(__initial.fill)
        let _rendering = MRenderingParameters.Create(__initial.rendering)
        let _dockConfig = ResetMod.Create(__initial.dockConfig)
        let _drawingApp = MCorrelationDrawingModel.Create(__initial.drawingApp)
        let _annotationApp = MAnnotationApp.Create(__initial.annotationApp)
        let _semanticApp = MSemanticApp.Create(__initial.semanticApp)
        let _corrPlotApp = MCorrelationPlotApp.Create(__initial.corrPlotApp)
        
        member x.past = __current.Value.past
        member x.saveIndices = _saveIndices :> IMod<_>
        member x.future = __current.Value.future
        member x.appFlags = _appFlags :> IMod<_>
        member x.sgFlags = _sgFlags :> IMod<_>
        member x.camera = _camera
        member x.cullMode = _cullMode :> IMod<_>
        member x.fill = _fill :> IMod<_>
        member x.rendering = _rendering
        member x.dockConfig = _dockConfig :> IMod<_>
        member x.drawingApp = _drawingApp
        member x.annotationApp = _annotationApp
        member x.semanticApp = _semanticApp
        member x.corrPlotApp = _corrPlotApp
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : CorrelationDrawing.Pages) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_saveIndices,v.saveIndices)
                ResetMod.Update(_appFlags,v.appFlags)
                ResetMod.Update(_sgFlags,v.sgFlags)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_camera, v.camera)
                ResetMod.Update(_cullMode,v.cullMode)
                ResetMod.Update(_fill,v.fill)
                MRenderingParameters.Update(_rendering, v.rendering)
                ResetMod.Update(_dockConfig,v.dockConfig)
                MCorrelationDrawingModel.Update(_drawingApp, v.drawingApp)
                MAnnotationApp.Update(_annotationApp, v.annotationApp)
                MSemanticApp.Update(_semanticApp, v.semanticApp)
                MCorrelationPlotApp.Update(_corrPlotApp, v.corrPlotApp)
                
        
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
            let rendering =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.RenderingParameters>() with
                    override x.Get(r) = r.rendering
                    override x.Set(r,v) = { r with rendering = v }
                    override x.Update(r,f) = { r with rendering = f r.rendering }
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
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.AnnotationApp>() with
                    override x.Get(r) = r.annotationApp
                    override x.Set(r,v) = { r with annotationApp = v }
                    override x.Update(r,f) = { r with annotationApp = f r.annotationApp }
                }
            let semanticApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.SemanticApp>() with
                    override x.Get(r) = r.semanticApp
                    override x.Set(r,v) = { r with semanticApp = v }
                    override x.Update(r,f) = { r with semanticApp = f r.semanticApp }
                }
            let corrPlotApp =
                { new Lens<CorrelationDrawing.Pages, CorrelationDrawing.CorrelationPlotApp>() with
                    override x.Get(r) = r.corrPlotApp
                    override x.Set(r,v) = { r with corrPlotApp = v }
                    override x.Update(r,f) = { r with corrPlotApp = f r.corrPlotApp }
                }
