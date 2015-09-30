object frmReadSignature: TfrmReadSignature
  Left = 458
  Top = 237
  BorderStyle = bsDialog
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1077#1083#1077#1082#1090#1088#1086#1085#1077#1085' '#1082#1083#1102#1095'...'
  ClientHeight = 148
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lMsg: TLabel
    Left = 0
    Top = 0
    Width = 419
    Height = 115
    Align = alClient
    Alignment = taCenter
    Caption = #1052#1086#1083#1103', '#1080#1079#1095#1072#1082#1072#1081#1090#1077'...'
    Color = 14215418
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object pBottom: TPanel
    Left = 0
    Top = 115
    Width = 419
    Height = 33
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    object btnCancel: TButton
      Left = 340
      Top = 4
      Width = 75
      Height = 25
      Cancel = True
      Caption = #1054#1090#1082#1072#1079
      Enabled = False
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object ProgressBar: TProgressBar
      Left = 5
      Top = 14
      Width = 255
      Height = 11
      Step = 1
      TabOrder = 1
      Visible = False
    end
  end
  object ESKTimer: TTimer
    Enabled = False
    Interval = 400
    OnTimer = ESKTimerTimer
    Left = 56
    Top = 16
  end
end
