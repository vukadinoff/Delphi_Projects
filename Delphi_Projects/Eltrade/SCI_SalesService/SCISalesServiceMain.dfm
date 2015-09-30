object MainForm: TMainForm
  Left = 162
  Top = 117
  Width = 308
  Height = 246
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblClient: TLabel
    Left = 16
    Top = 16
    Width = 67
    Height = 13
    Caption = 'Client number:'
  end
  object lblCard: TLabel
    Left = 160
    Top = 16
    Width = 63
    Height = 13
    Caption = 'Card number:'
  end
  object Button1: TButton
    Left = 104
    Top = 179
    Width = 96
    Height = 25
    Caption = 'Verify client/card'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 17
    Top = 67
    Width = 265
    Height = 94
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object edtClient: TEdit
    Left = 16
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'edtClient'
  end
  object edtCard: TEdit
    Left = 160
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'edtClient'
  end
end
