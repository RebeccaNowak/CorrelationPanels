namespace CorrelationDrawing

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogNode =

  open Aardvark.Base
  open Aardvark.UI
  open UtilitiesGUI
  open Aardvark.Base.Incremental


  let initialEmpty : LogNode = {
    isSelected   = false
    nodeType     = LogNodeType.Empty
    label        = "log node"
    level        = -1
    lBoundary    = {point = V3d.OOO; anno = (Annotation.initial "-1")}
    uBoundary    = {point = V3d.OOO; anno = (Annotation.initial "-1")}
    children     = plist.Empty
    elevation    = 0.0
    range        = Rangef.init
    logYPos      = 0.0
    logXPos      = 0.0
    pos          = V3d.OOO
    size         = V3d.OOO
  }


  let initialTopLevel 
    ((up, ua) : (V3d * Annotation)) 
    ((lp, la) : (V3d * Annotation)) 
    (children : plist<LogNode>)
    (level    : int) : LogNode = {
    isSelected    = false
    nodeType      = LogNodeType.TopLevel
    label         = "log node"
    level         = level
    lBoundary     = {point = lp; anno = la}
    uBoundary     = {point = up; anno = ua}
    children      = children
    elevation     = (up.Length + lp.Length) * 0.5
    range         = {Rangef.init with min = lp.Length
                                      max = up.Length}
    logYPos       = 0.0
    logXPos       = 0.0
    pos           = V3d.OOO
    size          = V3d.OOO
  }

  // TODO add level
  let initialHierarchical (anno : Annotation) (lower : Border) (upper : Border) =
    {initialEmpty with
      nodeType    = LogNodeType.Hierarchical
      elevation   = Annotation.elevation anno
      lBoundary   = lower
      uBoundary   = upper}

  let intialMetric (anno : Annotation) (lower : Border) (upper : Border) =
    {initialEmpty with
      nodeType     = LogNodeType.Metric
      elevation    = Annotation.elevation anno
      lBoundary    = lower
      uBoundary    = upper
    }


  let intialAngular (anno : Annotation) (lower : Border) (upper : Border)  =
    {initialEmpty with
      nodeType    = LogNodeType.Angular
      elevation   = Annotation.elevation anno
      lBoundary   = lower
      uBoundary   = upper}
  /////////////////////


  ////////////////////
  module View =

/////////////////////////////////////////////////////////////////////////////////
    module Debug =
      let description (model : MLogNode) (semApp : MSemanticApp) = 
        let createDomNode (descString : IMod<string>) =
          div [] [
            Annotation.View.getColourIcon model.uBoundary.anno semApp
            Annotation.View.getColourIcon model.lBoundary.anno semApp
            Incremental.text descString
          ]

        let modStr = 
          model.nodeType 
            |> Mod.bind 
              (fun t -> 
                match t with
                  | LogNodeType.Hierarchical | LogNodeType.Infinity | LogNodeType.TopLevel ->
                    (Mod.map2 (fun (u : V3d) (l : V3d)  -> 
                                    sprintf "%.2f-%.2f" l.Length u.Length)
                              model.uBoundary.point model.lBoundary.point)
                  | LogNodeType.Angular | LogNodeType.Metric ->
                      model.elevation |> Mod.map (sprintf "%.2f" )
                  | LogNodeType.Empty -> Mod.constant "EmptyNode"
              )
      
        createDomNode modStr

      let rec debugView (model : MLogNode) (semApp : MSemanticApp) =
        let childrenView = 
          alist {
            for c in model.children do
              let! (v : alist<DomNode<'a>>) = (debugView c semApp)
              for it in v do
                yield it
          }
    
        let rval =
          adaptive {
            let isEmpty = AList.isEmpty model.children
            let! (b : bool) = isEmpty
            match b with
              | true  -> 
                  return AList.ofList [li [attribute "value" ">"] [(description model semApp)]]
              | false ->                
                  return AList.ofList [li [attribute "value" "-"] [(description model semApp)];
                         ul [] [Incremental.li (AttributeMap.ofList [attribute "value" "-"]) childrenView]]
          }
        rval

/////////////////////////////////////////////////////////////////////////////////
    module Svg =
      let rec createView (model         : MLogNode) 
                         (semApp        : MSemanticApp) 
                         (xFunction     : MLogNode -> IMod<float>)
                         (viewFunction  : MLogNode -> MSemanticApp -> IMod<List<DomNode<'a>>>) =
                         
        let childrenView = 
          alist {
            for c in model.children do
              let v = (createView c semApp xFunction viewFunction)
              for it in (v : alist<DomNode<'a>>) do
                yield it
          }
    
        let rval =
          alist {
            let isEmpty = AList.isEmpty model.children
            let! (b : bool) = isEmpty
            let! sv = (viewFunction model semApp)
            match b with
              | true  -> 
                  for v in sv do
                    yield v                
              | false ->  
                  for v in sv do
                    yield v 
                    let! xAxisPos = (xFunction model)
                    
                    yield Incremental.Svg.svg
                            (AttributeMap.ofList [
                              attribute "x" (sprintf "%.2f" xAxisPos)
                            ])
                            childrenView
                    //for cv in childrenView do
                    //  yield cv
          }
        rval

      

      let stackedView (boxCreator : MLogNode -> MSemanticApp -> IMod<DomNode<'a>>) (model : MLogNode) (semApp : MSemanticApp)   = //(f : MLogNode -> MSemanticApp -> DomNode<'b>)= 
        adaptive {
          let! nt = model.nodeType //TODO  performance!
      
          let! res =  
              match nt with
                | LogNodeType.Angular | LogNodeType.Metric ->
                    boxCreator model semApp
                | LogNodeType.TopLevel -> 
                    boxCreator model semApp
                | LogNodeType.Hierarchical ->
                    boxCreator model semApp
                | LogNodeType.Infinity ->
                    boxCreator model semApp
                | LogNodeType.Empty -> //TODO draw debug output
                    Mod.constant (Svg.drawRectangle 
                                      (new V2d(0.0,0.0))
                                      0.0
                                      0.0
                                      C4b.Black)
          return List.singleton <| res 
        }  


      let lineView (model : MLogNode) (semApp : MSemanticApp) = 
        adaptive {
          let! nt = model.nodeType //TODO  performance!
          let! size = model.size
          let! logYPos = model.logYPos
          let! lCol = (Annotation.getConstColor (model.lBoundary.anno) semApp) true
          let! uCol = (Annotation.getConstColor (model.uBoundary.anno) semApp) true
          let! level = model.level
          
      
          return 
            match nt with
              | LogNodeType.Angular | LogNodeType.Metric ->
                  [Svg.drawRectangle 
                    (new V2d(0.0, logYPos)) 
                    size.X
                    size.Y
                    lCol]
              | LogNodeType.TopLevel -> 
                match level with
                  | le when le = 0 -> 
                    [Svg.drawRectangle 
                      (new V2d(0.0, logYPos)) 
                      size.X
                      size.Y
                      lCol]
                  | _ ->
                    [Svg.drawLinePath [
                                       (new V2d(5.0, logYPos + 1.0))
                                       (new V2d(size.X, logYPos + 1.0))
                                     ]
                                     lCol;
                    Svg.drawLinePath [
                                       (new V2d(5.0, logYPos + size.Y - 1.0))
                                       (new V2d(size.X, logYPos + size.Y - 1.0))
                                     ]
                                     uCol]
                    
              | LogNodeType.Hierarchical ->
                  [Svg.drawRectangle 
                    (new V2d(0.0, logYPos)) 
                    size.X
                    size.Y
                    lCol]
              | LogNodeType.Infinity ->
                  [Svg.drawRectangle 
                    (new V2d(0.0, logYPos)) 
                    size.X
                    size.Y
                    lCol]
              | LogNodeType.Empty -> //TODO draw debug output
                  [Svg.drawRectangle 
                    (new V2d(0.0,0.0))
                    0.0
                    0.0
                    C4b.Black]
        }  


      let createBox  (boxType : LogNodeBoxType) (model : MLogNode) (semApp : MSemanticApp) =
        adaptive {
          let! size = model.size
          let! x = model.logXPos
          let! y = model.logYPos
          let! lCol = (Annotation.getConstColor (model.lBoundary.anno) semApp) true
          let! uCol = (Annotation.getConstColor (model.uBoundary.anno) semApp) true

          return match boxType with
                  | LogNodeBoxType.SimpleBox -> 
                    Svg.drawRectangle 
                          (new V2d(x, y)) 
                          size.X
                          size.Y
                          lCol
                  | LogNodeBoxType.TwoColorBox -> 
                    Svg.drawRectangle2c 
                          (new V2d(x, y)) 
                          size.X
                          size.Y
                          uCol
                          lCol
                  | LogNodeBoxType.FancyBox -> 
                    Svg.drawFancyRectangle 
                          (new V2d(x, y)) 
                          size.X
                          size.Y
                          uCol
                          lCol
        }

      let view (model : MLogNode) (semApp : MSemanticApp) (viewType : LogNodeView) =
        match viewType with
          | LogNodeView.StackedViewSimpleBoxes ->
              let xAxisFunction (node : MLogNode) =
                node.logXPos
              createView model semApp xAxisFunction (stackedView <| (createBox LogNodeBoxType.SimpleBox))
          | LogNodeView.StackedView2ColorBoxes ->
              let xAxisFunction (node : MLogNode) =
                Mod.map2 (fun (a : float) (b : V3d) -> a + b.X) node.logXPos node.size
              createView model semApp xAxisFunction (stackedView <| (createBox LogNodeBoxType.FancyBox))
          | LogNodeView.LineView ->
              let xAxisFunction (node : MLogNode) =
                node.logXPos
              createView model semApp xAxisFunction lineView
         



    
//  let calcElevation (anno : Annotation) =
//    anno.points.Mean (fun x -> x.point.Y)
//    
//  let calcPos (node : LogNode) =
//    {node with pos = (V3d.OOO + V3d.OIO * node.logYPos)}
//
//  //let calcPos' (node : LogNode) (logRange : Rangef) =
//    
//
//  let calcRange (anno : Annotation) =
//    let min = (anno.points.Min (fun x y -> x.point.Y < y.point.Y))
//    let max = (anno.points.Max (fun x y -> x.point.Y > y.point.Y))
//    {Rangef.init with min = min.point.Y
//                      max = max.point.Y}
//                          
//  let recalcRange (node : LogNode) =
//    {node with range =
//                {Rangef.init with min = (calcRange node.lBoundary).min
//                                  max = (calcRange node.uBoundary).max
//                }
//    }

//  let recalcRangeAndSize (node : LogNode) =
//      let range = calcRange node.annotation
//      {node with range = range
//                 size  = (V3d.III + 0.5 * V3d.OIO * (Rangef.calcRange range))}


  //////////////
//  let initial (lBoundary : Annotation)
//             (uBoundary : Annotation) 
//             : LogNode  = {
//    label      = "log node"
//    lBoundary   = lBoundary
//    uBoundary   = uBoundary
//    children    = plist.Empty
//    elevation   = ((calcElevation lBoundary) + (calcElevation uBoundary)) * 0.5
//    range       = {Rangef.init with min = (calcRange lBoundary).min
//                                    max = (calcRange uBoundary).max}  
//
//    logYPos     = 0.0
//    pos         = V3d.OOO
//    size        = V3d.OOO
//  }

//  let view (model : MLogNode) =
//
//  //
////    let intoTd (x) = // TODO move to utils
////      adaptive {
////        let! hov = model.uBoundary.anno.hovered
////        match hov with
////          | true -> return td [clazz "center aligned";  style lrPadding] [x]//return td [clazz "center aligned"; style (sprintf "%s;%s" (bgColorStr C4b.Yellow) lrPadding)] [x]
////          | false -> return td [clazz "center aligned";  style lrPadding] [x]
////      }
//      
//
//    let labelText (p : IMod<V3d> ) = 
//      p |> Mod.map (fun v -> sprintf "(%.1f, %.1f, %.1f)" v.X v.Y v.Z)
//    let pToTxt = Mod.map (fun (x : V3d) -> sprintf "%.2f" (x.Length))     
//
//    let append (x : IMod<string>) (str : string) (y : IMod<string>)  = 
//      Mod.map2 (fun a b -> sprintf "%s%s%s" a str b) x y
//    div [] [
//      Incremental.text (append (pToTxt model.uBoundary.point) ", " (pToTxt model.lBoundary.point));
//      Incremental.text (model.children.Content |> Mod.map (fun lst -> sprintf "children: %i" lst.Count))
//
//      //Incremental.text (Mod.map  (sprintf "%s%s") (model.uBoundary.anno.) )
//      ]
     





    

//  let calcPos' (logHeight : float) (lst : List<LogNode>) =
//      let accHeight = lst |> List.sumBy (fun x -> (abs (Rangef.calcRange x.range)))
//      let factor = logHeight / accHeight
//      lst
//        |> List.map (fun (x : LogNode) -> 
//                        {x with size = V3d.IOI + V3d.OIO * (Rangef.calcRange x.range) * factor}
//                    )
//        |> List.scan (fun (x : LogNode) (y : LogNode) -> 
//                        {y with logYPos = x.logYPos + (x.size.Y * 0.5) + (y.size.Y * 0.5)}
//                      ) (dummyInitial) 
//        |> List.tail
//        |> List.map calcPos
//   
//
//  let onlyHierarchicalNodes (semanticApp : SemanticApp) (nodes : List<LogNode>) =
//    nodes
//      |> List.filter (fun (n : LogNode) -> 
//      match (SemanticApp.getSemantic semanticApp n.lBoundary.semanticId) with //assuming one Node contains two annos of same SemanticType
//        | Some s  -> s.semanticType = SemanticType.Hierarchical
//        | None    -> false)


