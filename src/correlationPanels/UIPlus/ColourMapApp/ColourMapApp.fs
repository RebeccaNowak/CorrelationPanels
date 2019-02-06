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
    //| TextMessage   of TextInput.Action
    //| NumericMessage of Numeric.Action

  let empty : ColourMapItem =
    {
      id        = CMItemId.invalid
      upper     = 0.0
      upperStr  = ""
      colour    = {ColorPicker.init with c = C4b.Gray}
      label     = ""
    }

  let init id label colour upper upperStr: ColourMapItem =
    {
      id        = id
      upper     = upper
      upperStr  = upperStr
      colour    = {ColorPicker.init with c = colour}
      label     = label
    }

  // default values
  let boulder   = init (CMItemId.newId ()) "boulder"            (new C4b(217,95,14)) 100.0       "< -8    "
  let cobble    = init (CMItemId.newId ()) "cobble"             (new C4b(217,95,14)) 0.256       "-6 to -8"
  let vcGravel  = init (CMItemId.newId ()) "very coarse gravel" (new C4b(217,95,14)) 0.064       "-5 to -6"
  let cGravel   = init (CMItemId.newId ()) "coarse gravel"      (new C4b(217,95,14)) 0.032       "-3 to -4"
  let mGravel   = init (CMItemId.newId ()) "medium gravel"      (new C4b(217,95,14)) 0.016       "-2 to -3"
  let fGravel   = init (CMItemId.newId ()) "fine gravel"        (new C4b(217,95,14)) 0.008       "-1 to 0 "
  let vfGravel  = init (CMItemId.newId ()) "very fine gravel"   (new C4b(217,95,14)) 0.004       " 0 to 1 "
  let vcSand    = init (CMItemId.newId ()) "very coarse sand"   (new C4b(254,196,79)) 0.002      " 1 to 2 "
  let cSand     = init (CMItemId.newId ()) "coarse sand"        (new C4b(254,196,79)) 0.001      " 2 to 3 "
  let mSand     = init (CMItemId.newId ()) "medium sand"        (new C4b(254,196,79)) 0.0005     " 3 to 4 "
  let fSand     = init (CMItemId.newId ()) "fine sand"          (new C4b(254,196,79)) 0.00025    " 4 to 5 "
  let vfSand    = init (CMItemId.newId ()) "very fine sand"     (new C4b(254,196,79)) 0.000125   " 5 to 6 "
  let silt      = init (CMItemId.newId ()) "silt"               (new C4b(255,247,188)) 0.0000625 " 6 to 7 "
  let clay      = init (CMItemId.newId ()) "clay"               (new C4b(99,99,99)) 0.0000039    " 7 to 8 "
  let colloid   = init (CMItemId.newId ()) "colloid"            (new C4b(99,99,99)) 0.00000098   " 8 to 9 "
  

  let update (model : ColourMapItem) (action : Action) =
    match action with
      | ColourMessage m -> 
        let _c = ColorPicker.update model.colour m
        {model with colour = _c}
      //| TextMessage m   ->
      //  let _t = TextInput.update model.label m
      //  {model with label = _t}
      //| NumericMessage m -> 
      //  let _n = Numeric.update model.upper m
      //  {model with upper = _n}

  let view (model : MColourMapItem)  = 
    [
      (div [] [Incremental.text model.label])
        |> intoLeftAlignedTd
      ColorPicker.view model.colour
        |> intoTd
        |> UI.map ColourMessage
      (div [] [Incremental.text model.upperStr])
        |> intoLeftAlignedTd
    ]


      /////////////////////////

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColourMap =

  type Action =
    | ItemMessage of (CMItemId * ColourMapItem.Action)
    | SelectItem  of CMItemId


  let initial factor : ColourMap = 
    let _mappings = [ColourMapItem.boulder 
                     ColourMapItem.cobble  
                     ColourMapItem.vcGravel
                     ColourMapItem.cGravel 
                     ColourMapItem.mGravel 
                     ColourMapItem.fGravel 
                     ColourMapItem.vfGravel
                     ColourMapItem.vcSand  
                     ColourMapItem.cSand   
                     ColourMapItem.mSand   
                     ColourMapItem.fSand   
                     ColourMapItem.vfSand  
                     ColourMapItem.silt    
                     ColourMapItem.clay    
                     ColourMapItem.colloid 
                    ] |> PList.ofList

    {
      mappings = _mappings
      factor   = factor
      unit     = Unit.Micrometre
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
        |> PList.filter (fun m -> (value / model.factor < m.upper))
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
          yield Table.intoTrOnClick (Action.SelectItem m.id) _v
                                
      }

    Table.toTableView (div[][]) domList ["Grain size";"Colour";"φ-scale"]