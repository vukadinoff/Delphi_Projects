object MtelMainForm: TMtelMainForm
  Left = 392
  Top = 177
  BorderStyle = bsSingle
  Caption = 'Pay Prima'
  ClientHeight = 328
  ClientWidth = 434
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
  object grp1: TGroupBox
    Left = 16
    Top = 8
    Width = 401
    Height = 265
    TabOrder = 8
    object lbl1: TLabel
      Left = 10
      Top = 8
      Width = 53
      Height = 13
      Caption = 'UserName:'
    end
    object lbl2: TLabel
      Left = 234
      Top = 8
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object lbl3: TLabel
      Left = 154
      Top = 57
      Width = 45
      Height = 13
      Caption = 'MSISDN:'
    end
    object lbl4: TLabel
      Left = 10
      Top = 56
      Width = 44
      Height = 13
      Caption = 'Location:'
    end
    object lbl5: TLabel
      Left = 290
      Top = 57
      Width = 68
      Height = 13
      Caption = 'Ammount ('#1083#1074'):'
    end
  end
  object btnCheckPrima: TButton
    Left = 171
    Top = 112
    Width = 117
    Height = 25
    Caption = 'Check Prima'
    TabOrder = 5
    OnClick = btnCheckPrimaClick
  end
  object eLocation: TEdit
    Left = 24
    Top = 80
    Width = 129
    Height = 21
    MaxLength = 4
    TabOrder = 2
  end
  object btnPayPrima: TButton
    Left = 176
    Top = 288
    Width = 121
    Height = 25
    Caption = 'Pay Prima'
    Enabled = False
    TabOrder = 7
    OnClick = btnPayPrimaClick
  end
  object eMSISDN: TEdit
    Left = 168
    Top = 80
    Width = 121
    Height = 21
    MaxLength = 12
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 24
    Top = 144
    Width = 385
    Height = 105
    TabStop = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object eUserName: TEdit
    Left = 24
    Top = 33
    Width = 193
    Height = 21
    MaxLength = 12
    TabOrder = 0
  end
  object ePassword: TEdit
    Left = 248
    Top = 33
    Width = 161
    Height = 21
    MaxLength = 12
    TabOrder = 1
  end
  object cbAmmount: TComboBox
    Left = 304
    Top = 80
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = '10'
    Items.Strings = (
      '10'
      '15'
      '30'
      '60'
      '90'
      '120')
  end
  object FormStorage1: TFormStorage
    StoredProps.Strings = (
      'eLocation.Text'
      'eMSISDN.Text')
    StoredValues = <>
    Top = 65528
  end
end
