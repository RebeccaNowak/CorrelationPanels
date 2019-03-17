namespace UIPlus
open Aardvark.UI
open Aardvark.Base
open Aardvark.Base.Incremental
open UIPlus
open UIPlus.Table



      /////////////////////////

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColourMap =

  type Action =
    | ItemMessage       of (CMItemId * ColourMapItem.Action)
    | SelectItem        of CMItemId
    | SelectItemFromSvg of float


  let initial defaultValue dataToSvg svgToData : ColourMap = 
    let _mappings = [
                     ColourMapItem.boulder 
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
      mappings     = _mappings
      dataToSvg    = dataToSvg
      svgToData    = svgToData
      defaultValue = defaultValue
      unit         = Unit.Micrometre
      selected     = None
    }
      
  let svgValueToCMItem (model : ColourMap) (value : float) =
    match value with
      | v when v < 0.0 -> //TODO could be solved better
        Some ColourMapItem.empty
      | _ ->
        let dataValue = model.svgToData value
        let filtered = 
          model.mappings
            |> PList.filter (fun m -> (dataValue < m.upper) && (dataValue >= m.lower))
        let first =
          filtered
            |> PList.tryAt 0
        first

  let svgValueToColourPicker (model : ColourMap) (value : float) =
    let first = svgValueToCMItem model value
    match first with
      | Some ma -> Some ma.colour
      | None    -> None

  //let svgValueToColourPicker_OPT (model : ColourMap) (value : option<float>) =
  //  match value with
  //    | Some v -> 
  //      svgValueToColourPicker model v
  //    | None    -> None

  //let valueToColour (model : ColourMap) (value : float) =
  //  let cp = svgValueToColourPicker model value
  //  Option.map (fun x -> x.c) cp

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
      | SelectItem id ->
        {model with selected = Some id}
      | SelectItemFromSvg value ->
        let optItem = svgValueToCMItem model value
        match optItem with
          | Some item -> {model with selected = Some item.id}
          | None      -> model

  let view (model : MColourMap) =
    let tableview = 
      let domList =
        alist {                 
          for m in model.mappings do
            let mapper = UI.map (fun a -> Action.ItemMessage (m.id, a) ) 
            let _v = (ColourMapItem.view m) 
                        |> List.map mapper
            let! selid = model.selected
            match selid with
              | Some id  -> 
                if id = m.id then
                  yield Table.intoActiveTr (Action.SelectItem m.id) _v           
                else
                  yield Table.intoTrOnClick (Action.SelectItem m.id) _v
              | None ->
                yield Table.intoTrOnClick (Action.SelectItem m.id) _v              
        }

      Table.toTableView (div[][]) domList ["Grain size";"Colour";"φ-scale"]

    //require (GUI.CSS.myCss) (
    //  body [style "overflow-y: scroll"] [
    div [] [
          // menu |> ui.map correlationplotmessage
          Incremental.div (AttributeMap.ofList [clazz "ui inverted segment"])
                          (AList.single tableview)
                             
        ]
      //]
    //)

  let tryfindItem (model : ColourMap) (iid : CMItemId) =
    PList.tryFind (fun ind it -> it.id = iid) model.mappings

  let svgValueFromItemId model cmitemid =
    let dataValue =
      let item = tryfindItem model cmitemid
      match item with
        | Some it when not it.id.isValid -> model.dataToSvg model.defaultValue
        | Some it -> it.defaultMiddle
        | None    -> model.dataToSvg  model.defaultValue
    model.dataToSvg dataValue
    