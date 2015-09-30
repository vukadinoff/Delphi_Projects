object KGForm: TKGForm
  Left = 382
  Top = 244
  BorderStyle = bsDialog
  Caption = 'BOS operations (tscrn) key gen'
  ClientHeight = 200
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 64
    Height = 13
    Caption = 'Serial number'
  end
  object Label2: TLabel
    Left = 24
    Top = 55
    Width = 70
    Height = 13
    Caption = 'Expiration date'
  end
  object Label3: TLabel
    Left = 24
    Top = 112
    Width = 22
    Height = 13
    Caption = 'Key'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object editSerial: TEdit
    Left = 24
    Top = 24
    Width = 201
    Height = 21
    TabOrder = 0
  end
  object editKey: TEdit
    Left = 24
    Top = 128
    Width = 201
    Height = 21
    TabOrder = 2
  end
  object dateExp: TDateEdit
    Left = 24
    Top = 71
    Width = 121
    Height = 21
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnGenerate: TButton
    Left = 46
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 3
    OnClick = btnGenerateClick
  end
  object btnClose: TButton
    Left = 134
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 4
    OnClick = btnCloseClick
  end
end
