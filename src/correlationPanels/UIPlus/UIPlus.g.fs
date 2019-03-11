namespace UIPlus

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open UIPlus

[<AutoOpen>]
module Mutable =

    
    
    type MArrowButton(__initial : UIPlus.ArrowButton) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.ArrowButton> = Aardvark.Base.Incremental.EqModRef<UIPlus.ArrowButton>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.ArrowButton>
        let _direction = ResetMod.Create(__initial.direction)
        let _size = ResetMod.Create(__initial.size)
        
        member x.id = __current.Value.id
        member x.direction = _direction :> IMod<_>
        member x.size = _size :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.ArrowButton) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_direction,v.direction)
                ResetMod.Update(_size,v.size)
                
        
        static member Create(__initial : UIPlus.ArrowButton) : MArrowButton = MArrowButton(__initial)
        static member Update(m : MArrowButton, v : UIPlus.ArrowButton) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.ArrowButton> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ArrowButton =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<UIPlus.ArrowButton, UIPlus.ArrowButtonId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let direction =
                { new Lens<UIPlus.ArrowButton, SimpleTypes.Direction>() with
                    override x.Get(r) = r.direction
                    override x.Set(r,v) = { r with direction = v }
                    override x.Update(r,f) = { r with direction = f r.direction }
                }
            let size =
                { new Lens<UIPlus.ArrowButton, SimpleTypes.Size>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
    
    
    type MTextInput(__initial : UIPlus.TextInput) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.TextInput> = Aardvark.Base.Incremental.EqModRef<UIPlus.TextInput>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.TextInput>
        let _text = ResetMod.Create(__initial.text)
        let _disabled = ResetMod.Create(__initial.disabled)
        let _bgColor = ResetMod.Create(__initial.bgColor)
        let _size = MOption.Create(__initial.size)
        
        member x.text = _text :> IMod<_>
        member x.disabled = _disabled :> IMod<_>
        member x.bgColor = _bgColor :> IMod<_>
        member x.size = _size :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.TextInput) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_text,v.text)
                ResetMod.Update(_disabled,v.disabled)
                ResetMod.Update(_bgColor,v.bgColor)
                MOption.Update(_size, v.size)
                
        
        static member Create(__initial : UIPlus.TextInput) : MTextInput = MTextInput(__initial)
        static member Update(m : MTextInput, v : UIPlus.TextInput) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.TextInput> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TextInput =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let text =
                { new Lens<UIPlus.TextInput, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let disabled =
                { new Lens<UIPlus.TextInput, System.Boolean>() with
                    override x.Get(r) = r.disabled
                    override x.Set(r,v) = { r with disabled = v }
                    override x.Update(r,f) = { r with disabled = f r.disabled }
                }
            let bgColor =
                { new Lens<UIPlus.TextInput, Aardvark.Base.C4b>() with
                    override x.Get(r) = r.bgColor
                    override x.Set(r,v) = { r with bgColor = v }
                    override x.Update(r,f) = { r with bgColor = f r.bgColor }
                }
            let size =
                { new Lens<UIPlus.TextInput, Microsoft.FSharp.Core.Option<System.Int32>>() with
                    override x.Get(r) = r.size
                    override x.Set(r,v) = { r with size = v }
                    override x.Update(r,f) = { r with size = f r.size }
                }
    
    
    type MColourMapItem(__initial : UIPlus.ColourMapItem) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.ColourMapItem> = Aardvark.Base.Incremental.EqModRef<UIPlus.ColourMapItem>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.ColourMapItem>
        let _upper = ResetMod.Create(__initial.upper)
        let _defaultMiddle = ResetMod.Create(__initial.defaultMiddle)
        let _lower = ResetMod.Create(__initial.lower)
        let _upperStr = ResetMod.Create(__initial.upperStr)
        let _colour = Aardvark.UI.Mutable.MColorInput.Create(__initial.colour)
        let _label = ResetMod.Create(__initial.label)
        
        member x.id = __current.Value.id
        member x.upper = _upper :> IMod<_>
        member x.defaultMiddle = _defaultMiddle :> IMod<_>
        member x.lower = _lower :> IMod<_>
        member x.upperStr = _upperStr :> IMod<_>
        member x.colour = _colour
        member x.label = _label :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.ColourMapItem) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_upper,v.upper)
                ResetMod.Update(_defaultMiddle,v.defaultMiddle)
                ResetMod.Update(_lower,v.lower)
                ResetMod.Update(_upperStr,v.upperStr)
                Aardvark.UI.Mutable.MColorInput.Update(_colour, v.colour)
                ResetMod.Update(_label,v.label)
                
        
        static member Create(__initial : UIPlus.ColourMapItem) : MColourMapItem = MColourMapItem(__initial)
        static member Update(m : MColourMapItem, v : UIPlus.ColourMapItem) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.ColourMapItem> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ColourMapItem =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let id =
                { new Lens<UIPlus.ColourMapItem, UIPlus.CMItemId>() with
                    override x.Get(r) = r.id
                    override x.Set(r,v) = { r with id = v }
                    override x.Update(r,f) = { r with id = f r.id }
                }
            let upper =
                { new Lens<UIPlus.ColourMapItem, System.Double>() with
                    override x.Get(r) = r.upper
                    override x.Set(r,v) = { r with upper = v }
                    override x.Update(r,f) = { r with upper = f r.upper }
                }
            let defaultMiddle =
                { new Lens<UIPlus.ColourMapItem, System.Double>() with
                    override x.Get(r) = r.defaultMiddle
                    override x.Set(r,v) = { r with defaultMiddle = v }
                    override x.Update(r,f) = { r with defaultMiddle = f r.defaultMiddle }
                }
            let lower =
                { new Lens<UIPlus.ColourMapItem, System.Double>() with
                    override x.Get(r) = r.lower
                    override x.Set(r,v) = { r with lower = v }
                    override x.Update(r,f) = { r with lower = f r.lower }
                }
            let upperStr =
                { new Lens<UIPlus.ColourMapItem, System.String>() with
                    override x.Get(r) = r.upperStr
                    override x.Set(r,v) = { r with upperStr = v }
                    override x.Update(r,f) = { r with upperStr = f r.upperStr }
                }
            let colour =
                { new Lens<UIPlus.ColourMapItem, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.colour
                    override x.Set(r,v) = { r with colour = v }
                    override x.Update(r,f) = { r with colour = f r.colour }
                }
            let label =
                { new Lens<UIPlus.ColourMapItem, System.String>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
    
    
    type MColourMap(__initial : UIPlus.ColourMap) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<UIPlus.ColourMap> = Aardvark.Base.Incremental.EqModRef<UIPlus.ColourMap>(__initial) :> Aardvark.Base.Incremental.IModRef<UIPlus.ColourMap>
        let _mappings = MList.Create(__initial.mappings, (fun v -> MColourMapItem.Create(v)), (fun (m,v) -> MColourMapItem.Update(m, v)), (fun v -> v))
        let _dataToSvg = ResetMod.Create(__initial.dataToSvg)
        let _svgToData = ResetMod.Create(__initial.svgToData)
        let _defaultValue = ResetMod.Create(__initial.defaultValue)
        let _unit = ResetMod.Create(__initial.unit)
        let _selected = MOption.Create(__initial.selected)
        
        member x.mappings = _mappings :> alist<_>
        member x.dataToSvg = _dataToSvg :> IMod<_>
        member x.svgToData = _svgToData :> IMod<_>
        member x.defaultValue = _defaultValue :> IMod<_>
        member x.unit = _unit :> IMod<_>
        member x.selected = _selected :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : UIPlus.ColourMap) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_mappings, v.mappings)
                ResetMod.Update(_dataToSvg,v.dataToSvg)
                ResetMod.Update(_svgToData,v.svgToData)
                ResetMod.Update(_defaultValue,v.defaultValue)
                ResetMod.Update(_unit,v.unit)
                MOption.Update(_selected, v.selected)
                
        
        static member Create(__initial : UIPlus.ColourMap) : MColourMap = MColourMap(__initial)
        static member Update(m : MColourMap, v : UIPlus.ColourMap) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<UIPlus.ColourMap> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ColourMap =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let mappings =
                { new Lens<UIPlus.ColourMap, Aardvark.Base.plist<UIPlus.ColourMapItem>>() with
                    override x.Get(r) = r.mappings
                    override x.Set(r,v) = { r with mappings = v }
                    override x.Update(r,f) = { r with mappings = f r.mappings }
                }
            let dataToSvg =
                { new Lens<UIPlus.ColourMap, System.Double -> System.Double>() with
                    override x.Get(r) = r.dataToSvg
                    override x.Set(r,v) = { r with dataToSvg = v }
                    override x.Update(r,f) = { r with dataToSvg = f r.dataToSvg }
                }
            let svgToData =
                { new Lens<UIPlus.ColourMap, System.Double -> System.Double>() with
                    override x.Get(r) = r.svgToData
                    override x.Set(r,v) = { r with svgToData = v }
                    override x.Update(r,f) = { r with svgToData = f r.svgToData }
                }
            let defaultValue =
                { new Lens<UIPlus.ColourMap, System.Double>() with
                    override x.Get(r) = r.defaultValue
                    override x.Set(r,v) = { r with defaultValue = v }
                    override x.Update(r,f) = { r with defaultValue = f r.defaultValue }
                }
            let unit =
                { new Lens<UIPlus.ColourMap, UIPlus.Unit>() with
                    override x.Get(r) = r.unit
                    override x.Set(r,v) = { r with unit = v }
                    override x.Update(r,f) = { r with unit = f r.unit }
                }
            let selected =
                { new Lens<UIPlus.ColourMap, Microsoft.FSharp.Core.Option<UIPlus.CMItemId>>() with
                    override x.Get(r) = r.selected
                    override x.Set(r,v) = { r with selected = v }
                    override x.Update(r,f) = { r with selected = f r.selected }
                }
