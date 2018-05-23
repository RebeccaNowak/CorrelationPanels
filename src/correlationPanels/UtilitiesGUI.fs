namespace CorrelationDrawing

open System
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI

module UtilitiesGUI = 
    // SEMUI

    let myCss = [
              { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
              { kind = Stylesheet; name = "semui-overrides"; url = "semui-overrides.css" }
              { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
            ]


    // GENERAL
    let colorToHexStr (color : C4b) = 
        let bytes = [| color.R; color.G; color.B |]
        let str =
            bytes 
                |> (Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x)))
                |> (String.concat System.String.Empty)
        String.concat String.Empty ["#";str] 


    

    // ATTRIBUTES
    let bgColorAttr (color : C4b) =
      style (sprintf "background: %s" (colorToHexStr color))

    let bgColorStr (color : C4b) =
      (sprintf "background: %s" (colorToHexStr color))

    let incrBgColorAMap (colorMod : IMod<C4b>) =      
      amap { 
        let! col =  colorMod
        let str = (sprintf "background: %s" (colorToHexStr col))
        yield style str
      }

    let incrBgColorAttr (colorMod : IMod<C4b>) =
      colorMod 
        |> Mod.map (fun x -> 
                      style (sprintf "background-color: %s" (colorToHexStr x)))      

       

    let modColorToColorAttr (c : IMod<C4b>) =
      let styleStr = Mod.map (fun x -> (sprintf "color:%s" (colorToHexStr x))) c
      Mod.map (fun x -> style x) styleStr  

    let noPadding  = "padding: 0px 0px 0px 0px"
    let tinyPadding  = "padding: 1px 1px 1px 1px"
    let lrPadding = "padding: 1px 4px 1px 4px"


    // TOOLTIPS
    let wrapToolTip (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [attribute "title" text
             attribute "data-position" "top center"
             attribute "data-variation" "mini" ] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipRight (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "right center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )

    let wrapToolTipBottom (text:string) (dom:DomNode<'a>) : DomNode<'a> =

        let attr = 
            [ attribute "title" text
              attribute "data-position" "bottom center"
              attribute "data-variation" "mini"] 
                |> AttributeMap.ofList
                |> AttributeMap.union dom.Attributes                
                
        onBoot "$('#__ID__').popup({inline:true,hoverable:true});" (       
            dom.WithAttributes attr     
        )


