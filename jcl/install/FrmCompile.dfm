object FormCompile: TFormCompile
  Left = 348
  Top = 311
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Compiling'
  ClientHeight = 165
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    Left = 8
    Top = 5
    Width = 345
    Height = 124
    TabOrder = 1
    object BevelProject: TBevel
      Left = 10
      Top = 6
      Width = 324
      Height = 19
    end
    object BevelStatus: TBevel
      Left = 10
      Top = 30
      Width = 324
      Height = 19
    end
    object BevelCurrentLine: TBevel
      Left = 10
      Top = 54
      Width = 160
      Height = 19
    end
    object BevelHints: TBevel
      Left = 10
      Top = 78
      Width = 105
      Height = 19
    end
    object LblProject: TLabel
      Left = 64
      Top = 9
      Width = 265
      Height = 13
      AutoSize = False
      Caption = 'Project filename'
      Transparent = True
    end
    object LblStatusCaption: TLabel
      Left = 14
      Top = 33
      Width = 29
      Height = 13
      Caption = 'Done:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object BevelTotalLines: TBevel
      Left = 174
      Top = 54
      Width = 160
      Height = 19
    end
    object LblCurrentLineCaption: TLabel
      Left = 14
      Top = 57
      Width = 56
      Height = 13
      Caption = 'Current line:'
    end
    object LblCurrentLine: TLabel
      Left = 158
      Top = 57
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblTotalLinesCaption: TLabel
      Left = 178
      Top = 57
      Width = 51
      Height = 13
      Caption = 'Total lines:'
    end
    object LblTotalLines: TLabel
      Left = 322
      Top = 57
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object BevelWarnings: TBevel
      Left = 120
      Top = 78
      Width = 105
      Height = 19
    end
    object BevelErrors: TBevel
      Left = 230
      Top = 78
      Width = 104
      Height = 19
    end
    object LblHintsCaption: TLabel
      Left = 14
      Top = 81
      Width = 27
      Height = 13
      Caption = 'Hints:'
    end
    object LblHints: TLabel
      Left = 104
      Top = 81
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblWarningsCaption: TLabel
      Left = 124
      Top = 81
      Width = 48
      Height = 13
      Caption = 'Warnings:'
    end
    object LblWarnings: TLabel
      Left = 213
      Top = 81
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblErrorsCaption: TLabel
      Left = 234
      Top = 81
      Width = 30
      Height = 13
      Caption = 'Errors:'
    end
    object LblErrors: TLabel
      Left = 322
      Top = 81
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblProjectCaption: TLabel
      Left = 14
      Top = 9
      Width = 36
      Height = 13
      Caption = 'Project:'
    end
    object LblStatus: TLabel
      Left = 110
      Top = 33
      Width = 78
      Height = 13
      Caption = 'There are errors.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LblErrorReason: TLabel
      Left = 8
      Top = 104
      Width = 73
      Height = 13
      Caption = 'LblErrorReason'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
  end
  object BtnOk: TButton
    Left = 144
    Top = 134
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = BtnOkClick
  end
end
