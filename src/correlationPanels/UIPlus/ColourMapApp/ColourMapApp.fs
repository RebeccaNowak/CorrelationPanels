namespace UIPlus
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open UIPlus
open UIPlus.Table


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColourMapItem =
  type Action =
    | ColourMessage of ColorPicker.Action
    | TextMessage   of TextInput.Action
    | NumericMessage of Numeric.Action

  let empty : ColourMapItem =
    {
      id     = CMItemId.invalid
      upper  = Numeric.init
      colour = {ColorPicker.init with c = C4b.Gray}
      label  = {TextInput.init with text = "--"}
    }

  let init id label colour upper : ColourMapItem =
    {
      id     = id
      upper  = {Numeric.init with min = 0.0
                                  max = System.Double.PositiveInfinity
                                  value = upper
                                  step = 0.1
               }
      colour = {ColorPicker.init with c = colour}
      label  = {TextInput.init with text = label}
    }

  let clay    = init (CMItemId.newId ()) "clay" (new C4b(99,99,99)) 0.1
  let silt    = init (CMItemId.newId ()) "silt" (new C4b(255,247,188)) 1.0
  let sand    = init (CMItemId.newId ()) "sand" (new C4b(254,196,79)) 10.0
  let gravel  = init (CMItemId.newId ()) "gravel" (new C4b(217,95,14)) 1000.0

  let update (model : ColourMapItem) (action : Action) =
    match action with
      | ColourMessage m -> 
        let _c = ColorPicker.update model.colour m
        {model with colour = _c}
      | TextMessage m   ->
        let _t = TextInput.update model.label m
        {model with label = _t}
      | NumericMessage m -> 
        let _n = Numeric.update model.upper m
        {model with upper = _n}

  let view (model : MColourMapItem) = 
      let labelNode = 
        (TextInput.view'' 
          "box-shadow: 0px 0px 0px 1px rgba(0, 0, 0, 0.1) inset"
          model.label)

      let thNode = Numeric.view'' 
                     NumericInputType.InputBox 
                     model.upper
                     (AttributeMap.ofList 
                        [style "margin:auto; color:black; max-width:60px"])

      [

        labelNode
          |> intoTd
          |> UI.map TextMessage
        ColorPicker.view model.colour
          |> intoTd
          |> UI.map ColourMessage
        thNode
          |> intoTd
          |> UI.map NumericMessage
      ]


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColourMap =

  type Action =
    | ItemMessage of (CMItemId * ColourMapItem.Action)


  let initial factor : ColourMap = 
    let _mappings = [ColourMapItem.clay
                     ColourMapItem.silt
                     ColourMapItem.sand
                     ColourMapItem.gravel] |> PList.ofList

    {
      mappings = _mappings
      factor   = factor
    }

  let update (model : ColourMap) (action : Action) =
    match action with
      | ItemMessage (id,m) ->
        let upd x =
          match x.id = id with
            | true -> ColourMapItem.update x m
            | false -> x
        let _mappings = 
          PList.map upd model.mappings
        {model with mappings = _mappings}

  let valueToColourPicker' (model : ColourMap) (value : float) =
    let filtered = 
      model.mappings
        |> PList.filter (fun m -> (value / model.factor < m.upper.value))
    let first =
      filtered
        |> PList.tryAt 0
    match first with
      | Some ma -> Some ma.colour
      | None    -> None

  let valueToColourPicker (model : ColourMap) (value : option<float>) =
    match value with
      | Some v -> 
        valueToColourPicker' model v
      | None    -> None

  let valueToColour (model : ColourMap) (value : float) =
    let cp = valueToColourPicker' model value
    Option.map (fun x -> x.c) cp

  let view (model : MColourMap) =
    let domList =
      alist {                 
        for m in model.mappings do
          let mapper = UI.map (fun a -> Action.ItemMessage (m.id, a) ) 
          let _v = (ColourMapItem.view m) 
                      |> List.map mapper
          yield Table.intoTr _v
                                
      }

    Table.toTableView (div[][]) domList ["Label";"Colour"]