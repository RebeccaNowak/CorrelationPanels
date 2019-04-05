namespace UIPlus
  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open UIPlus
  open UIPlus.TableTypes
  open SimpleTypes

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module TableRow =
    
    let init isSelected update displayView editView onSelect alignment : TableRow<'dtype, 'mtype, 'action> = //, 'mtype, 'action> =
      {
        isSelected    = isSelected
        onSelect      = onSelect
        update        = update
        displayView   = displayView
        editView      = editView
        align         = alignment
      }

    let update (guiModel  : TableRow<'dtype, 'mtype, 'action>) 
               (dataModel : 'dtype)
               (action    : 'action) =
      guiModel.update dataModel action

    let intoTd (alignment : Alignment) (domNode : DomNode<'a>) = 
      td [clazz alignment.clazz] [domNode]

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

    let view   (guiModel  : TableRow<'dtype, 'mtype, 'action>)
               (dataModel : 'mtype) =
      
      let itemsContent = guiModel.editView dataModel
      let items =
        List.map (fun c -> intoTd (guiModel.align dataModel) c) itemsContent

      alist {
        let! sel = guiModel.isSelected dataModel
        let row = 
          match sel with
            | true ->
              intoActiveTr guiModel.onSelect items
            | false ->
              intoTrOnClick guiModel.onSelect items
        yield row
      }
      

