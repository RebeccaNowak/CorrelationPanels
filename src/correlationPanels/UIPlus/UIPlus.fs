namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI

  type Direction =
  | Up
  | Down
  | Left
  | Right
  with
    member this.toString =
      match this with
        | Up    -> "up"
        | Down  -> "down"
        | Left  -> "left"
        | Right -> "right"

  type Size =
    | Mini
    | Tiny
    | Small
    | Normal
    | Large
    | Big
    | Huge
    | Massive
    with
      member this.toString =
        match this with
          | Mini      -> "mini"
          | Tiny      -> "tiny"
          | Small     -> "small"
          | Normal    -> ""
          | Large     -> "large"
          | Big       -> "big"
          | Huge      -> "huge"
          | Massive   -> "massive"


  type ArrowButtonId = {
    id        : string 
  } with
    member this.isValid = (this.id <> "")
  module ArrowButtonId = 
    let invalid = {id = ""}
    let newId () : ArrowButtonId  = 
      let id = System.Guid.NewGuid ()
      {id = id.ToString () }

  [<DomainType>]
  type ArrowButton = {
    [<NonIncremental>]
    id            : ArrowButtonId

    direction     : Direction
    size          : Size
  }

  [<DomainType>]
  type TextInput = {
      text      : string
      disabled  : bool
      bgColor   : C4b  
      size      : option<int>
   } 

  type CMItemId = {
      id        : string 
    } with
      member this.isValid = (this.id <> "")
  module CMItemId = 
      let invalid = {id = ""}
      let newId () : CMItemId  = 
        let id = System.Guid.NewGuid ()
        {id = id.ToString () }

  [<DomainType>]
  type ColourMapItem = {
    [<NonIncremental>]
    id     : CMItemId

    upper  : NumericInput
    colour : ColorInput
    label  : TextInput
  }

  type Unit =
    | Metre
    | Centimetre
    | Millimetre
    | Micrometre
  with 
    member this.fromMetre (x : float) =
      match this with
        | Metre -> x
        | Centimetre -> x * 0.01
        | Millimetre -> x * 0.001
        | Micrometre -> x * 0.000001


  [<DomainType>]
  type ColourMap = {
    mappings : plist<ColourMapItem>
    factor   : float
    unit     : Unit
  }