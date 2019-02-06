namespace Svgplus

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module RoseDiagram =
    open Aardvark.Base
    open Aardvark.UI
    open Svgplus.Attributes
    open Aardvark.Base.Incremental
    open Svgplus.Base

    let rnd = System.Random()

    type Action =
      | ChangeBin of Bin
      | OnClick   

    let init = 
      

      {
        id            = RoseDiagramId.newId ()
        centre        = V2d (200.0, 200.0)
        outerRadius   = 50.0
        innerRadius   = 10.0
        colour        = [for i in 0 .. 15 -> (gradient_blue_green.Item i)]
        nrCircles     = 5
        weight        = 0.5
        countPerBin   = PList.ofList [for i in 0 .. 15 -> Bin.init i ((rnd.Next ())%5) (gradient_blue_green.Item i)]

        // countPerBin   = PList.ofList [for i in 0 .. 15 -> Bin.init i 0]
      }

    let update (model : RoseDiagram) (action : Action) =
      match action with
        | ChangeBin bin -> 
          let _bins = PList.setAt bin.number bin model.countPerBin
          {model with countPerBin = _bins}
        | OnClick -> 
          let colour = [for i in 0 .. 15 -> (gradient_blue_red.Item i)]
          let newBins = PList.ofList [for i in 0 .. 15 -> Bin.init i ((rnd.Next ())%5) (gradient_blue_red.Item i)]
          {model with countPerBin = newBins
                      colour      = colour
          }


    let view (model : MRoseDiagram) = 
      let lst = 
        alist {
          let! outer = model.outerRadius
          let! inner = model.innerRadius
          let! nrCircles = model.nrCircles

          let circleDist = (outer - inner) / (float nrCircles)
        
          //let! lu = model.leftUpper
          let! centre = model.centre //(lu + (new V2d (outer)))
          let yi = (AList.ofList (drawConcentricCircles' centre outer
                                                         inner C4b.Gray 
                                                         nrCircles circleDist 0.5))
          yield! yi
      
          let! weight = model.weight
          let yi = AList.ofList (draw16StarLines centre outer inner C4b.Gray weight)
          yield! yi 
      
          let circleRadii =
            [1..(nrCircles)]
              |> List.map (fun nr -> (float nr) * circleDist)

          let bins =
            model.countPerBin
              |> AList.map 
                (fun bin -> 
                    let v = bin.value
                    {angularBin = bin.number
                     radius     = inner + (circleRadii.Item v)
                     colour     = bin.colour}
                )
            
        
          // let! color = model.colour
          let yi = (bins
                      |> AList.map (fun bin -> donutSegment centre inner bin)
                   )
          yield! yi
        }

      //let att = Aardvark.UI.Svg.Events.onClickAttributes [(fun _ -> OnClick)]
      //let gr = Incremental.toGroup lst (AMap.ofList (att@[style "pointer-events: bounding-box;"]))
      //gr
      
      let clickable = 
        alist {
          let! outer = model.outerRadius
          let! centre = model.centre
          yield (Svgplus.Base.clickableRectangle centre outer (fun _ -> OnClick))
        }
      AList.append lst clickable
      //toGroup (circles@lines@filledBins) []
    