object Form1: TForm1
  Left = 392
  Top = 177
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 407
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 96
    Top = 24
    Width = 25
    Height = 13
    Caption = 'URL:'
  end
  object Label2: TLabel
    Left = 408
    Top = 24
    Width = 44
    Height = 13
    Caption = 'Location:'
  end
  object Label3: TLabel
    Left = 136
    Top = 113
    Width = 45
    Height = 13
    Caption = 'MSISDN:'
  end
  object Label4: TLabel
    Left = 136
    Top = 153
    Width = 47
    Height = 13
    Caption = 'Ammount:'
  end
  object Button1: TButton
    Left = 8
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Destroy'
    TabOrder = 1
    OnClick = Button2Click
  end
  object eURL: TEdit
    Left = 96
    Top = 40
    Width = 305
    Height = 21
    TabOrder = 2
  end
  object eLocatoion: TEdit
    Left = 408
    Top = 40
    Width = 73
    Height = 21
    MaxLength = 4
    TabOrder = 3
  end
  object Button3: TButton
    Left = 8
    Top = 120
    Width = 113
    Height = 25
    Caption = 'Check PrePayed'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 152
    Width = 113
    Height = 25
    Caption = 'Pay PrePayed'
    TabOrder = 5
    OnClick = Button4Click
  end
  object eMsisDn: TEdit
    Left = 136
    Top = 129
    Width = 305
    Height = 21
    MaxLength = 12
    TabOrder = 6
  end
  object Memo1: TMemo
    Left = 8
    Top = 200
    Width = 497
    Height = 209
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object eAmmount: TEdit
    Left = 136
    Top = 169
    Width = 81
    Height = 21
    MaxLength = 12
    TabOrder = 8
    Text = '5.00'
  end
  object FormStorage1: TFormStorage
    StoredProps.Strings = (
      'eURL.Text'
      'eLocatoion.Text'
      'eMsisDn.Text'
      'eAmmount.Text')
    StoredValues = <>
    Left = 256
    Top = 16
  end
end
