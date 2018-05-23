namespace Aardvark.UI

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Numeric
open Combinators

//extension so attributes can be specified
module Numeric = 
    let numericField'<'msg> ( f : Action -> seq<'msg> ) ( atts : AttributeMap<'msg> ) ( model : MNumericInput ) inputType =         

      let tryParseAndClamp min max fallback s =
          let parsed = 0.0
          match Double.TryParse(s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
              | (true,v) -> clamp min max v
              | _ ->  printfn "validation failed: %s" s
                      fallback

      let attributes = 
          amap {                                

              let! min = model.min
              let! max = model.max
              let! value = model.value
              match inputType with
                  | Slider ->   
                      yield "type" => "range"
                      yield onInput' (tryParseAndClamp min max value >> SetValue >> f)   // continous updates for slider
                  | InputBox -> 
                      yield "type" => "number"
                      yield onChange' (tryParseAndClamp min max value >> SetValue >> f)  // batch updates for input box (to let user type)

              let! step = model.step
              yield UI.onWheel' (fun d -> value + d.Y * step |> clamp min max |> SetValue |> f)

              yield "step" => sprintf "%f" step
              yield "min"  => sprintf "%f" min
              yield "max"  => sprintf "%f" max

              let! format = model.format
              yield "value" => formatNumber format value
          } 

      Incremental.input (AttributeMap.ofAMap attributes |> AttributeMap.union atts)


    let view'' (inputType : NumericInputType)
               (model : MNumericInput)
               (attributes : AttributeMap<Action>) :  DomNode<Action> =
        div [][(numericField' (Seq.singleton) attributes model inputType)] //(numericField (Seq.singleton) attributes model inputTypes )


