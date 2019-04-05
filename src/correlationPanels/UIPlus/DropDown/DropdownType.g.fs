namespace UIPlus.DropdownType

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open UIPlus.DropdownType

[<AutoOpen>]
module Mutable =

    [<AbstractClass; StructuredFormatDisplay("{AsString}")>]
    type MDropdownList<'va,'na>() = 
        abstract member valueList : Aardvark.Base.Incremental.alist<'na>
        abstract member selected : Aardvark.Base.Incremental.IMod<Microsoft.FSharp.Core.option<'na>>
        abstract member color : Aardvark.Base.Incremental.IMod<Aardvark.Base.C4b>
        abstract member searchable : Aardvark.Base.Incremental.IMod<System.Boolean>
        abstract member AsString : string
    
    
    and private MDropdownListD<'a,'ma,'va>(__initial : UIPlus.DropdownType.DropdownList<'a>, __ainit : 'a -> 'ma, __aupdate : 'ma * 'a -> unit, __aview : 'ma -> 'va) =
        inherit MDropdownList<'va,'va>()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.DropdownType.DropdownList<'a>> = Aardvark.Base.Incremental.EqModRef<UIPlus.DropdownType.DropdownList<'a>>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.DropdownType.DropdownList<'a>>
        let _valueList = MList.Create(__initial.valueList, (fun v -> __ainit(v)), (fun (m,v) -> __aupdate(m, v)), (fun v -> __aview(v)))
        let _selected = MOption.Create(__initial.selected, (fun v -> __ainit(v)), (fun (m,v) -> __aupdate(m, v)), (fun v -> __aview(v)))
        let _color = ResetMod.Create(__initial.color)
        let _searchable = ResetMod.Create(__initial.searchable)
        
        override x.valueList = _valueList :> alist<_>
        override x.selected = _selected :> IMod<_>
        override x.color = _color :> IMod<_>
        override x.searchable = _searchable :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.DropdownType.DropdownList<'a>) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_valueList, v.valueList)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_searchable,v.searchable)
                
        
        static member Update(m : MDropdownListD<'a,'ma,'va>, v : UIPlus.DropdownType.DropdownList<'a>) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        override x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.DropdownType.DropdownList<'a>> with
            member x.Update v = x.Update v
    
    and private MDropdownListV<'a>(__initial : UIPlus.DropdownType.DropdownList<'a>) =
        inherit MDropdownList<IMod<'a>,'a>()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.DropdownType.DropdownList<'a>> = Aardvark.Base.Incremental.EqModRef<UIPlus.DropdownType.DropdownList<'a>>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.DropdownType.DropdownList<'a>>
        let _valueList = MList.Create(__initial.valueList)
        let _selected = MOption.Create(__initial.selected)
        let _color = ResetMod.Create(__initial.color)
        let _searchable = ResetMod.Create(__initial.searchable)
        
        override x.valueList = _valueList :> alist<_>
        override x.selected = _selected :> IMod<_>
        override x.color = _color :> IMod<_>
        override x.searchable = _searchable :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.DropdownType.DropdownList<'a>) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_valueList, v.valueList)
                MOption.Update(_selected, v.selected)
                ResetMod.Update(_color,v.color)
                ResetMod.Update(_searchable,v.searchable)
                
        
        static member Update(m : MDropdownListV<'a>, v : UIPlus.DropdownType.DropdownList<'a>) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        override x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.DropdownType.DropdownList<'a>> with
            member x.Update v = x.Update v
    
    and [<AbstractClass; Sealed>] MDropdownList private() =
        static member Create<'a,'ma,'va>(__initial : UIPlus.DropdownType.DropdownList<'a>, __ainit : 'a -> 'ma, __aupdate : 'ma * 'a -> unit, __aview : 'ma -> 'va) : MDropdownList<'va,'va> = MDropdownListD<'a,'ma,'va>(__initial, __ainit, __aupdate, __aview) :> MDropdownList<'va,'va>
        static member Create<'a>(__initial : UIPlus.DropdownType.DropdownList<'a>) : MDropdownList<IMod<'a>,'a> = MDropdownListV<'a>(__initial) :> MDropdownList<IMod<'a>,'a>
        static member Update<'a,'xva,'xna>(m : MDropdownList<'xva,'xna>, v : UIPlus.DropdownType.DropdownList<'a>) : unit = 
            match m :> obj with
            | :? IUpdatable<UIPlus.DropdownType.DropdownList<'a>> as m -> m.Update(v)
            | _ -> failwith "cannot update"
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DropdownList =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let valueList<'a> =
                { new Lens<UIPlus.DropdownType.DropdownList<'a>, Aardvark.Base.plist<'a>>() with
                    override x.Get(r) = r.valueList
                    override x.Set(r,v) = { r with valueList = v }
                    override x.Update(r,f) = { r with valueList = f r.valueList }
                }
            let selected<'a> =
                { new Lens<UIPlus.DropdownType.DropdownList<'a>, Microsoft.FSharp.Core.Option<'a>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
            let color<'a> =
                { new Lens<UIPlus.DropdownType.DropdownList<'a>, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let searchable<'a> =
                { new Lens<UIPlus.DropdownType.DropdownList<'a>, System.Boolean>() with
                    override x.Get(r) = r.searchable
                    override x.Set(r,v) = { r with searchable = v }
                    override x.Update(r,f) = { r with searchable = f r.searchable }
                }
