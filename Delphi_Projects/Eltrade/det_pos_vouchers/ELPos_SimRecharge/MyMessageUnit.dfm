object MyMessageForm: TMyMessageForm
  Left = 284
  Top = 300
  BorderStyle = bsToolWindow
  ClientHeight = 188
  ClientWidth = 392
  Color = clBtnFace
  Constraints.MinHeight = 215
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 128
    Height = 128
    Transparent = True
  end
  object MsgLabel: TLabel
    Left = 176
    Top = 24
    Width = 25
    Height = 20
    Caption = '----'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object BtnPanel: TPanel
    Left = 0
    Top = 134
    Width = 392
    Height = 54
    Align = alBottom
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
  end
end
