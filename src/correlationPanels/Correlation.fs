namespace CorrelationDrawing
  
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Correlation =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.UI
    open UI

    let initial : Correlation =
      {
        fromBorder    = Border.initialEmpty
        toBorder      = Border.initialEmpty
      }

    let moveDown (by : float) (model : Correlation) =
      let add p = p + new V2d(0.0, by)
      let f = {model.fromBorder with 
                svgPosition = add model.fromBorder.svgPosition}
      let t = {model.toBorder with 
                svgPosition = add model.toBorder.svgPosition}
      {model with fromBorder = f
                  toBorder   = t}

    module Svg =
      let view (model : MCorrelation) = 
        adaptive {
          let! fp = model.toBorder.svgPosition
          let! tp = model.fromBorder.svgPosition
          let! c1 = model.fromBorder.color
          let! c2 = model.toBorder.color

          return Svgplus.Base.drawDottedLine fp tp c1 4.0 2.0 2.0 //TODO hardcoded
        }
        
