namespace CorrelationDrawing
//  open Aardvark.Base
//  open Aardvark.Base.Incremental
//  open Aardvark.UI

//  module LogNodeViews = 

//    module Svg = 
//      let debugOutput (txt : string) = Svg.drawText (new V2d(5.0, 5.0)) txt

//      let angularNode (yPos : float) = Svg.drawText (new V2d(0.0, yPos)) "a"

//      let metricNode  (yPos : float) = Svg.drawText (new V2d(0.0, yPos)) "m"

//      module ViewTypes =




//        let logView (nodeStyle                : IMod<LogNodeStyle>) 
//                    (model                    : MLogNode) 
//                    (semApp                   : MSemanticApp)  
//                    (selectionCallback        : list<string> -> 'msg)
//                    (drawCorrelationCallback  : option<list<string> -> 'msg>)
//                    (offset                   : float)
//                    (editCorrelations         : bool) = 

//          adaptive {
//            let! sel = model.isSelected
//            let! nt = model.nodeType //TODO  performance!
//            let! size = model.size
//            let! logYPos = model.logYPos
//            let! lCol = (Annotation.getConstColor (model.lBoundary.anno) semApp) true
//            let! uCol = (Annotation.getConstColor (model.uBoundary.anno) semApp) true
//            let! level = model.level
//            let! st = nodeStyle
//            let! drawDashedBorder = model.hasDefaultX


//            let borderedR = Svg.drawBorderedRectangle
//                              (new V2d(offset, logYPos)) 
//                              size.X size.Y
//                              st.color lCol uCol
//                              (Default.levelToWeight level)
//                              selectionCallback
//                              sel
//            let domNode = 
//              match nt with
//                | LogNodeType.Angular -> angularNode logYPos
//                | LogNodeType.Metric  -> metricNode logYPos
//                | LogNodeType.Hierarchical -> 
//                  match drawDashedBorder, editCorrelations with
//                    | true, true  ->

//                    | false, false ->
//                      Svg.drawRectangleHVBorder 
//                        (new V2d(offset, logYPos)) 
//                        size.X size.Y
//                        st.color lCol uCol
//                        (Default.levelToWeight level)
//                        selectionCallback
//                        sel


//                | LogNodeType.HierarchicalLeaf ->
//                  Svg.drawHorizontalLine 
//                    (new V2d(offset, logYPos)) 
//                    (size.X - 1.0)
//                    st.color
//                    1.0
//                | LogNodeType.PosInfinity
//                | LogNodeType.NegInfinity ->
//                    Default.debugOutput "LogNode neg or pos infinity"
//                | LogNodeType.Empty | LogNodeType.Infinity ->
//                    Default.debugOutput "LogNode empty or infinity"

//            return [domNode]
//          }  






//        let lineView (model : MLogNode) (semApp : MSemanticApp)  (callback   : V2i -> 'msg) = 
//          adaptive {
//            let! nt = model.nodeType //TODO  performance!
//            let! size = model.size
//            let! logYPos = model.logYPos
//            let! lCol = (Annotation.getConstColor (model.lBoundary.anno) semApp) true
//            let! uCol = (Annotation.getConstColor (model.uBoundary.anno) semApp) true
//            let! level = model.level
          
      
//            return 
//              match nt with
//                | LogNodeType.Angular | LogNodeType.Metric ->
//                    [Svg.drawRectangle 
//                      (new V2d(0.0, logYPos)) 
//                      size.X
//                      size.Y
//                      lCol]
//                | LogNodeType.Hierarchical -> 
//                  match level with
//                    | le when le = 0 -> 
//                      [Svg.drawRectangle 
//                        (new V2d(0.0, logYPos)) 
//                        size.X
//                        size.Y
//                        lCol]
//                    | _ ->
//                      [Svg.drawLinePath [
//                                          (new V2d(5.0, logYPos + 1.0))
//                                          (new V2d(size.X, logYPos + 1.0))
//                                        ]
//                                        lCol;
//                      Svg.drawLinePath [
//                                          (new V2d(5.0, logYPos + size.Y - 1.0))
//                                          (new V2d(size.X, logYPos + size.Y - 1.0))
//                                        ]
//                                        uCol]
                    
//                | LogNodeType.HierarchicalLeaf ->
//                    [Svg.drawRectangle 
//                      (new V2d(0.0, logYPos)) 
//                      size.X
//                      size.Y
//                      lCol]
//                | LogNodeType.PosInfinity
//                | LogNodeType.NegInfinity ->
//                    [Svg.drawRectangle 
//                      (new V2d(0.0, logYPos)) 
//                      size.X
//                      size.Y
//                      lCol]
//                | LogNodeType.Empty | LogNodeType.Infinity -> //TODO draw debug output
//                    [Svg.drawRectangle 
//                      (new V2d(0.0,0.0))
//                      0.0
//                      0.0
//                      C4b.Black]
//          }  





//      let getDomNode (viewType            : CorrelationPlotViewType) 
//                      (nodeStyle          : IMod<LogNodeStyle>) 
//                      (selectionCallback  : list<string> -> 'msg)
//                      (drawCorrelationCallback     : option<list<string> -> 'msg>)
//                      (semApp             : MSemanticApp)  
//                      (offset             : float)
//                      (model              : MLogNode) 
//                        =
//        match viewType with
//          | CorrelationPlotViewType.CorrelationView ->
//              ViewTypes.logView nodeStyle model semApp selectionCallback drawCorrelationCallback offset true
//          | CorrelationPlotViewType.LineView ->
//              ViewTypes.logView' nodeStyle model semApp selectionCallback
//          | CorrelationPlotViewType.LogView ->
//              ViewTypes.logView nodeStyle model semApp selectionCallback drawCorrelationCallback offset false

           
//        //let stackedView (boxCreator : MLogNode -> MSemanticApp -> IMod<DomNode<'a>>) 
//        //                (model : MLogNode) 
//        //                (semApp : MSemanticApp)
//        //                (callback   : V2i -> 'msg) = //(f : MLogNode -> MSemanticApp -> DomNode<'b>)= 
//        //  adaptive {
//        //    let! nt = model.nodeType //TODO  performance!
      
//        //    let! res =  
//        //        match nt with
//        //          | LogNodeType.Angular | LogNodeType.Metric ->
//        //              boxCreator model semApp
//        //          | LogNodeType.Hierarchical -> 
//        //              boxCreator model semApp
//        //          | LogNodeType.HierarchicalLeaf ->
//        //              boxCreator model semApp
//        //          | LogNodeType.NegInfinity
//        //          | LogNodeType.PosInfinity ->
//        //              boxCreator model semApp
//        //          | LogNodeType.Empty | LogNodeType.Infinity -> //TODO draw debug output
//        //              Mod.constant (Svg.drawRectangle 
//        //                                (new V2d(0.0,0.0))
//        //                                0.0
//        //                                0.0
//        //                                C4b.Black)
//        //    return List.singleton <| res 
//        //  }  