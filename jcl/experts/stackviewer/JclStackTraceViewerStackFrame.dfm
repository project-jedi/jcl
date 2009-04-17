object frmStack: TfrmStack
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object lv: TListView
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    Columns = <
      item
        Caption = 'ModuleName'
      end
      item
        Caption = 'SourceUnitName'
      end
      item
        Caption = 'ProcedureName'
      end
      item
        Caption = 'SourceName'
      end
      item
        Caption = 'LineNumber'
      end
      item
        Caption = 'LineNumberOffsetFromProcedureStart'
      end
      item
        Caption = 'Revision'
      end
      item
        Caption = 'Project/File'
      end
      item
        Caption = 'TranslatedLineNumber'
      end>
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvChange
    OnDblClick = lvDblClick
  end
end
