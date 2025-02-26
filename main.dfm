object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Extractor'
  ClientHeight = 441
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDragOver = FormDragOver
  TextHeight = 15
  object sg: TStringGrid
    Left = 0
    Top = 0
    Width = 568
    Height = 441
    TabStop = False
    Align = alClient
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 14
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goFixedRowDefAlign]
    TabOrder = 0
    OnKeyUp = sgKeyUp
    OnSelectCell = sgSelectCell
  end
  object pnlButtons: TPanel
    Left = 568
    Top = 0
    Width = 93
    Height = 441
    Align = alRight
    TabOrder = 1
    object lblFeedback: TLabel
      Left = 6
      Top = 162
      Width = 81
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = 'pw: 1 of 1'
    end
    object btnExtract: TButton
      Left = 9
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Extract'
      Default = True
      TabOrder = 0
      OnClick = btnExtractClick
    end
    object btnCancel: TButton
      Left = 9
      Top = 103
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnFind: TButton
      Left = 9
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Find PWs'
      TabOrder = 2
      OnClick = btnFindClick
    end
    object chbReloadPWs: TCheckBox
      Left = 6
      Top = 135
      Width = 84
      Height = 17
      Caption = 'reload PWs'
      TabOrder = 3
    end
    object edtNewPassword: TEdit
      Left = 6
      Top = 192
      Width = 82
      Height = 23
      TabOrder = 4
    end
    object btnAddNewPassword: TButton
      Left = 6
      Top = 221
      Width = 75
      Height = 25
      Caption = 'Add PW'
      TabOrder = 5
      OnClick = btnAddNewPasswordClick
    end
    object btnFindFiles: TButton
      Left = 9
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Find Files'
      TabOrder = 6
      OnClick = btnFindFilesClick
    end
  end
end
