namespace CorrelationDrawing
  
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Correlation =
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering
    open Aardvark.UI
    open UtilitiesGUI
    let initial : Correlation =
      {
        fromBorder    = Border.initialEmpty
        toBorder      = Border.initialEmpty
      }

    module Svg =
      let view (model : MCorrelation) = 
        adaptive {
          let! fp = model.toBorder.svgPosition
          let! tp = model.fromBorder.svgPosition
          let! c1 = model.fromBorder.color
          let! c2 = model.toBorder.color

          return Svg.drawDottedLine fp tp c1 4.0 2.0 2.0 //TODO hardcoded
        }
        
