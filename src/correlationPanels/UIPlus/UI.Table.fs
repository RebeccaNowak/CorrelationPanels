﻿namespace UIPlus
    module Table =
      open Aardvark.UI
      open System
      open Aardvark.Base
      open Aardvark.Base.Incremental

      let textColorFromBackground (col  : C4b) =
         match  (int col.R + int col.B + int col.G) with
                  | c when c < 400  ->
                    "color: white"
                  | c when c >= 400 ->
                    "color: black"
                  | _ -> ""

      let intoTd (domNode) = 
        td [clazz "center aligned"] [domNode]

      let intoLeftAlignedTd (domNode) =
        td [clazz "left aligned"] [domNode]

      let intoTd' domNode colSpan = 
        td [clazz "center aligned";
            style GUI.CSS.lrPadding;
            attribute "colspan" (sprintf "%i" colSpan)]
           [domNode]
    
      let intoTr domNodes =
        tr [] domNodes

      let intoTr' (domNode) (atts : list<string * AttributeValue<'a>>) =
        tr atts [domNode]

      let intoTrOnClick fOnClick domNodeList =
        tr [onClick (fun () -> fOnClick)] domNodeList

      let intoActiveTr fOnClick domNodeList =
        tr [clazz "active";onClick (fun () -> fOnClick)] domNodeList

      let toTableView (menu : DomNode<'msg>) 
                      (rows : alist<DomNode<'msg>>) 
                      (columnNames : list<string>) = 

        let header = 
          columnNames
            |> List.map (fun str -> th[] [text str])

        require (GUI.CSS.myCss) (
          body [clazz "ui"; style "background: #1B1C1E;position:fixed;width:100%;overflow: auto;"] [
            div [] [
              menu
              table
                ([clazz "ui celled striped inverted table unstackable";
                                      style "padding: 1px 5px 2px 5px"]) (
                    [
                      thead [][tr[] header]
                      Incremental.tbody  (AttributeMap.ofList []) rows
                    ]
                )
            ]
          ]
        )


