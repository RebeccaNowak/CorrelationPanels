namespace CorrelationDrawing
  
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Correlation =
    let initial : Correlation =
      {
        fromBorder    = Border.initialEmpty
        toBorder      = Border.initialEmpty
      }
