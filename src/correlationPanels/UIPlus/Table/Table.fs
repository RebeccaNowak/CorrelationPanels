namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open UIPlus
  open UIPlus.TableTypes

  module Table =

    let init mapper align colHeadings =
      {
        mapper      = mapper
        colHeadings = colHeadings
      }

    let view  (guiModel  : Table<'dtype, 'mtype, 'action>)
              (data      : alist<'mtype>) =
      let magic d =
        let row = guiModel.mapper d
        let domNode = TableRow.view row d
        domNode
      
      let rows = 
        alist {
          for datum in data do
            yield! (magic datum)
      
        }
      
      let header = 
        guiModel.colHeadings
          |> List.map (fun str -> th[] [text str])

      require (GUI.CSS.myCss) (
        table
          ([clazz "ui celled striped inverted table unstackable";
                                style "padding: 1px 5px 2px 5px"]) (
              [
                thead [][tr[] header]
                Incremental.tbody  (AttributeMap.ofList []) rows
              ]
          )
      )
