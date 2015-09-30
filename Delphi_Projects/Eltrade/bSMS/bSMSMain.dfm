object MainForm: TMainForm
  Left = 393
  Top = 244
  Width = 316
  Height = 352
  Caption = 'Send SMS'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblPhone: TLabel
    Left = 15
    Top = 13
    Width = 124
    Height = 13
    Caption = 'Phone (e.g. 359xxxxxxxxx)'
  end
  object lblMessage: TLabel
    Left = 15
    Top = 61
    Width = 43
    Height = 13
    Caption = 'Message'
  end
  object lblStatus: TLabel
    Left = 15
    Top = 173
    Width = 30
    Height = 13
    Caption = 'Status'
  end
  object edtPhone: TEdit
    Left = 15
    Top = 29
    Width = 281
    Height = 21
    TabOrder = 0
  end
  object btnSendSMS: TButton
    Left = 16
    Top = 281
    Width = 79
    Height = 25
    Caption = 'SendSMS'
    TabOrder = 2
    OnClick = btnSendSMSClick
  end
  object mmoMessage: TMemo
    Left = 15
    Top = 77
    Width = 281
    Height = 89
    TabOrder = 1
  end
  object mmoStatus: TMemo
    Left = 15
    Top = 189
    Width = 281
    Height = 76
    TabStop = False
    Enabled = False
    ReadOnly = True
    TabOrder = 3
  end
  object btnNewSMS: TButton
    Left = 210
    Top = 281
    Width = 85
    Height = 25
    Caption = 'New SMS'
    Enabled = False
    TabOrder = 4
    OnClick = btnNewSMSClick
  end
  object FormStorage1: TFormStorage
    StoredValues = <>
  end
end
