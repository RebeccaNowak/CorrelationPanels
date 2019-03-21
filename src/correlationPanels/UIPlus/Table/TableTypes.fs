namespace UIPlus.TableTypes


  open Aardvark.Base
  open Aardvark.Base.Incremental
  open Aardvark.UI
  open SimpleTypes


  type TableItemId = {
    id            : System.Guid 
  }

  module TableItemId = 
    let newId unit : TableItemId  = 
      {
        id = System.Guid.NewGuid()
      }

  type TableRowId = {
    id            : System.Guid 
  }

  module TableRowId = 
    let newId unit : TableRowId  = 
      {
        id = System.Guid.NewGuid()
      }

  type TableRow<'dtype, 'mtype, 'action> =
    {
      isSelected    : 'mtype -> IMod<bool>
      update        : 'dtype -> 'action -> 'dtype
      displayView   : 'mtype -> list<DomNode<'action>>
      editView      : 'mtype -> list<DomNode<'action>>
      onSelect      : 'action
      align         : 'mtype -> Alignment
    }

  type Table<'dtype, 'mtype, 'action>  =
    {
      mapper      : 'mtype -> TableRow<'dtype, 'mtype, 'action>
      colHeadings : list<string>
    }


 

