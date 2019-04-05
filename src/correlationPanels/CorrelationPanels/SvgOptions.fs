namespace CorrelationDrawing

//  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//  module SvgOptions = 
//    open Aardvark.Base.Incremental
//    open Aardvark.Base
//    let init : SvgOptions = 
//      {
//        logPadding       = 70.0
//        logHeight        = 300.0
//        logMaxWidth      = 250.0
//        cpWidth          = 900.0
//        secLevelWidth    = 20.0
//        xAxisScaleFactor = 30.0 //WIP
//        yAxisScaleFactor = 1.0 //WIP
//        xAxisPadding     = 30.0
//        yAxisPadding     = 5.0 //WIP
//        yAxisStep        = 1.0
//        axisWeight       = 2.0

//        //offset           = V2d.OO
//        //zoom             = SvgZoom.defaultZoom
//        //fontSize         = FontSize.defaultSize
//    }

//    let xAxisYPosition (model : SvgOptions) logHeight =
//            logHeight + model.logPadding + model.xAxisPadding

//    let firstLogOffset (model : SvgOptions) =
//              model.logPadding * 0.5

//    let secLogOffset (model : SvgOptions) offset = //TODO find problem with 0-1 offset
//              offset + model.logPadding * 0.3

//    let xAxisYPosition' (model : MSvgOptions) (logHeight : IMod<float>) =
//      adaptive {
//        let! h = model.logHeight
//        let! p = model.logPadding
//        let! xp = model.xAxisPadding
//        return h + p + xp
//      }

//            //logHeight + model.logPadding + model.xAxisPadding
//    let firstLogOffset' (model : MSvgOptions) =
//      adaptive {
//        let! p = model.logPadding
//        return p * 0.5
//      }

//    let secLogOffset' (model : MSvgOptions) (offset : IMod<float>) = //TODO find problem with 0-1 offset
//      adaptive {
//        let! o = offset
//        let! p = model.logPadding
//        return o + p * 0.3
//      }