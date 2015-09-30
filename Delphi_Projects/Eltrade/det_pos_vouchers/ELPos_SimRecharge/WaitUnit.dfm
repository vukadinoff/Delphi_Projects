object WaitForm: TWaitForm
  Left = 493
  Top = 246
  Width = 458
  Height = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lHeader: TLabel
    Left = 0
    Top = 0
    Width = 450
    Height = 103
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = #1052#1086#1083#1103', '#1080#1079#1095#1072#1082#1072#1081#1090#1077'...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
    Visible = False
  end
  object lMessage: TLabel
    Left = 0
    Top = 103
    Width = 450
    Height = 103
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
end
