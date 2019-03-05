namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open SimpleTypes



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

  type GrainSize =
    | Boulder  
    | Cobble   
    | VcGravel 
    | CGravel  
    | MGravel  
    | FGravel  
    | VfGravel 
    | VcSand   
    | CSand    
    | MSand    
    | FSand    
    | VfSand   
    | Silt     
    | Clay     
    | Colloid  
  with 
    member this.toString =
      match this with
      | Boulder  -> "boulder"           
      | Cobble   -> "cobble"            
      | VcGravel -> "very coarse gravel"
      | CGravel  -> "coarse gravel"     
      | MGravel  -> "medium gravel"     
      | FGravel  -> "fine gravel"       
      | VfGravel -> "very fine gravel"  
      | VcSand   -> "very coarse sand"  
      | CSand    -> "coarse sand"       
      | MSand    -> "medium sand"       
      | FSand    -> "fine sand"         
      | VfSand   -> "very fine sand"    
      | Silt     -> "silt"              
      | Clay     -> "clay"              
      | Colloid  -> "colloid"                 

  [<DomainType>]
  type ColourMapItem = {
    [<NonIncremental>]
    id                  : CMItemId
                       
    upper               : float
    defaultMiddle       : float
    lower               : float
    upperStr            : string
    colour              : ColorInput
    label               : string
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
    mappings    : plist<ColourMapItem>
    dataToSvg   : float -> float
    svgToData   : float -> float
    unit        : Unit
    selected    : option<CMItemId>
  }