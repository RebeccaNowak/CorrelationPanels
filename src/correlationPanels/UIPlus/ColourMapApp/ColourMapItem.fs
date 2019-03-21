namespace UIPlus
open Aardvark.UI
open Aardvark.Base
open UIPlus.Tables


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
        defaultMiddle  = -1.0
        lower     = 0.0
        upperStr  = ""
        colour    = {ColorPicker.init with c = C4b.White}
        label     = ""
      }

    let init id label colour upper middle lower upperStr: ColourMapItem =
      {
        id              = id
        upper           = upper
        defaultMiddle   = middle
        lower           = lower
        upperStr        = upperStr
        colour          = {ColorPicker.init with c = colour}
        label           = label
      }

// default values
    let none      = init (CMItemId.invalid)  "none"               (new C4b(255,255,255)) -2.0           0.003          0.0              "   -    "  
    let boulder   = init (CMItemId.newId ()) "boulder"            (new C4b(217,95,14))   1.0            0.628          0.256            "< -8    "  
    let cobble    = init (CMItemId.newId ()) "cobble"             (new C4b(217,95,14))   0.256          0.16           0.064            "-6 to -8" 
    let vcGravel  = init (CMItemId.newId ()) "very coarse gravel" (new C4b(217,95,14))   0.064          0.048          0.032            "-5 to -6"
    let cGravel   = init (CMItemId.newId ()) "coarse gravel"      (new C4b(217,95,14))   0.032          0.024          0.016            "-4 to -5"
    let mGravel   = init (CMItemId.newId ()) "medium gravel"      (new C4b(217,95,14))   0.016          0.012          0.008            "-3 to -4"
    let fGravel   = init (CMItemId.newId ()) "fine gravel"        (new C4b(217,95,14))   0.008          0.006          0.004            "-2 to -3"
    let vfGravel  = init (CMItemId.newId ()) "very fine gravel"   (new C4b(217,95,14))   0.004          0.003          0.002            "-1 to -2"
    let vcSand    = init (CMItemId.newId ()) "very coarse sand"   (new C4b(254,196,79))  0.002          0.0015         0.001            " 0 to -1"
    let cSand     = init (CMItemId.newId ()) "coarse sand"        (new C4b(254,196,79))  0.001          0.00075        0.0005           " 1 to 0 "
    let mSand     = init (CMItemId.newId ()) "medium sand"        (new C4b(254,196,79))  0.0005         0.000375       0.00025          " 2 to 1 "
    let fSand     = init (CMItemId.newId ()) "fine sand"          (new C4b(254,196,79))  0.00025        0.0001875      0.000125         " 3 to 2 "
    let vfSand    = init (CMItemId.newId ()) "very fine sand"     (new C4b(254,196,79))  0.000125       7e-05          0.000015         " 4 to 3 "
    let silt      = init (CMItemId.newId ()) "silt"               (new C4b(255,247,188)) 0.000015       9.4e-06        0.0000038        " 8 to 4 "
    let clay      = init (CMItemId.newId ()) "clay"               (new C4b(99,99,99))    0.0000038      2.39e-06       0.00000098       " 10 to 8 "
    let colloid   = init (CMItemId.newId ()) "colloid"            (new C4b(99,99,99))    0.00000098     1.195e-06      3.8e-08          " 20 to 10 "    
  

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


    let editView (model : MColourMapItem) =
      [
        (div [] [Incremental.text model.label])
          |> intoLeftAlignedTd
        ColorPicker.view model.colour
          |> intoTd
          |> UI.map ColourMessage
        (div [] [Incremental.text model.upperStr])
          |> intoLeftAlignedTd
      ]

    let displayView (model : MColourMapItem) =
      [
        (div [] [Incremental.text model.label])
          |> intoLeftAlignedTd
        ColorPicker.view model.colour
          |> intoTd
          |> UI.map ColourMessage
        (div [] [Incremental.text model.upperStr])
          |> intoLeftAlignedTd
      ]

    let view (model : MColourMapItem)  = 
      displayView model

