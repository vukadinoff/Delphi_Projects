object frmKitchen: TfrmKitchen
  Left = 799
  Top = 141
  Width = 470
  Height = 480
  Caption = #1045#1051#1058#1056#1045#1049#1044' '#1050#1091#1093#1085#1103
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object scrItems: TScrollBox
    Left = 0
    Top = 0
    Width = 361
    Height = 441
    TabOrder = 0
  end
  object Button1: TButton
    Left = 384
    Top = 224
    Width = 65
    Height = 41
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Tm: TTimer
    Left = 416
    Top = 408
  end
end
