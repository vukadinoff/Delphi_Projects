object SetSumForm: TSetSumForm
  Left = 761
  Top = 139
  Width = 504
  Height = 606
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  DesignSize = (
    496
    579)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TdxSpeedButton
    Left = 8
    Top = 511
    Width = 100
    Height = 45
    Anchors = [akLeft, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    About = 'Design eXperience. '#169' 2002 M. Hoffmann'
    Version = '1.0.2g'
    OnClick = btnCancelClick
    Caption = #1053#1072#1079#1072#1076
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FFFFFF000000
      97FF000097FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000097FF000097FFFFFFFF0000008CD70000
      8FCF3D3DCBFF000097FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000097FF3434C8FF000080C200008DD40909BCFF0909
      C1FF4D4DE1FF4F4FD4FF000097FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000097FF5B5BD5FF5D5DDFFF0909BCFF0909BCFF0909BCFF2B2B
      E2FF4141E8FF5A5AE7FF5050D4FF000097FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000097FF5E5ED7FF6F6FE8FF5A5AE8FF3F3FE0FF0909BCFF0909BCFF2424
      E7FE3434E8FF4545E8FF5757E8FF4B4BD4FF000097FFFFFFFF00FFFFFF000000
      97FF5B5BD8FF6868E8FF5A5AE8FF4B4BE8FF3B3BE7FB0909BCFFFFFFFF000909
      BCFF2727E7FC3434E8FF4141E8FF4F4FE8FF4141D5FF000097FF000097FF4E4E
      DAFF5A5AE8FF5151E8FF4646E8FF3A3AE7F60909BCFFFFFFFF00FFFFFF00FFFF
      FF000909BCFF2424E7F93333E8FF4646E8FF5151E8FF3A3AD6FF4242DCFF4D4D
      E8FF4343E8FF3C3CE8FF3232E4EE0909BCFFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000909BCFF4B4BE4F46262E8FF6565E8FF6565E8FF6868E8FF6969
      E8FF6060E8FF3232DBE60909BCFFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0006068BD74949E1FF6868E8FF6969E8FF6A6AE8FF6A6A
      E8FF3E3EDAFF03036BB8FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000006BD000009EFF3636DEFF6E6EE8FF6F6FE8FF6F6FE8FF6E6E
      E8FF2525D6FF000096FF000050ACFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000066D4000097FF3B3BDEFF7979E8FF7A7AE8FF7A7AE8FF7A7AE8FF7A7A
      E8FF7777E8FF2626D4FF00008DFF00004AACFFFFFF00FFFFFF00FFFFFF000000
      62D3000091FF4444DEFF7A7AE8FF7A7AE8FF7A7AE8FF0909BCFF0909BCFF7A7A
      E8FF7A7AE8FF7A7AE8FF2929D3FF000086FF000050ACFFFFFF00050585FE0000
      92FF4B4BDEFF8585E8FF8585E8FF8585E8FF0909BCFFFFFFFF00FFFFFF000909
      BCFF8787E8FF8787E8FF8787E8FF2D2DD2FF000084F5000050AC000091FF5151
      DFFF9494E8FF9494E8FF9494E8FF0909BCFFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF000909BCFF9494E8FF9494E8FF9494E8FF3030D0F8000084F53030D0F84E4E
      DEB3A1A1E8FFA1A1E8FF0909BCFFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000909BCFFA1A1E8FFA1A1E8FF5151DFFF3030D0F8FFFFFF000909
      BCFF0909BCFF0909BCFFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000909BCFF0909BCFF0909BCFFFFFFFF00}
    Colors.BackgroundTo = 12500670
    Colors.BorderLine = cl3DDkShadow
    Colors.ClickedFrom = 12500670
    Colors.ClickedTo = clWhite
    Quality = bqHigh
    ShowFocusRect = False
    Spacing = 5
  end
  object pMain: TPanel
    Left = 52
    Top = 10
    Width = 387
    Height = 415
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    Visible = False
    object btnInvoice: TdxSpeedButton
      Tag = 1
      Left = 79
      Top = 354
      Width = 100
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnClick = btnInvoiceClick
      Caption = #1060#1072#1082#1090#1091#1088#1072
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object btnBon: TdxSpeedButton
      Tag = 2
      Left = 201
      Top = 354
      Width = 100
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnClick = btnBonClick
      Caption = #1050#1072#1089#1086#1074#1072' '#1073#1077#1083#1077#1078#1082#1072
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object lSum: TLabel
      Left = 0
      Top = 11
      Width = 187
      Height = 29
      Alignment = taRightJustify
      AutoSize = False
      Caption = #1057#1091#1084#1072' '#1079#1072' '#1079#1072#1088#1077#1078#1076#1072#1085#1077':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object lSumData: TLabel
      Left = 191
      Top = 11
      Width = 162
      Height = 29
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lLimits: TLabel
      Left = 3
      Top = 51
      Width = 683
      Height = 54
      AutoSize = False
      Caption = '('#1083#1080#1084#1080#1090#1080'...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
    object pNumpad: TPanel
      Left = 66
      Top = 113
      Width = 249
      Height = 226
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = True
      ParentFont = False
      TabOrder = 0
      object btnPad7: TdxSpeedButton
        Tag = 7
        Left = 8
        Top = 8
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '7'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad8: TdxSpeedButton
        Tag = 8
        Left = 67
        Top = 8
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '8'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad9: TdxSpeedButton
        Tag = 9
        Left = 126
        Top = 8
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '9'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad4: TdxSpeedButton
        Tag = 4
        Left = 8
        Top = 61
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '4'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad5: TdxSpeedButton
        Tag = 5
        Left = 67
        Top = 61
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '5'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad6: TdxSpeedButton
        Tag = 6
        Left = 126
        Top = 61
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '6'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad1: TdxSpeedButton
        Tag = 1
        Left = 8
        Top = 114
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '1'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad2: TdxSpeedButton
        Tag = 2
        Left = 67
        Top = 114
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '2'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad3: TdxSpeedButton
        Tag = 3
        Left = 126
        Top = 114
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '3'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPad0: TdxSpeedButton
        Left = 8
        Top = 167
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnPad7Click
        Caption = '0'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnDot: TdxSpeedButton
        Left = 67
        Top = 167
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnDotClick
        Caption = '.'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnEsc: TdxSpeedButton
        Left = 126
        Top = 167
        Width = 114
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnEscClick
        Caption = 'CL'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnBackspace: TdxSpeedButton
        Left = 185
        Top = 8
        Width = 55
        Height = 50
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnBackspaceClick
        Caption = 'Back'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnMinus: TdxSpeedButton
        Left = 185
        Top = 61
        Width = 55
        Height = 50
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnDotClick
        Caption = '-'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
      object btnPlus: TdxSpeedButton
        Left = 185
        Top = 114
        Width = 55
        Height = 50
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        About = 'Design eXperience. '#169' 2002 M. Hoffmann'
        Version = '1.0.2g'
        OnClick = btnDotClick
        Caption = '+'
        Colors.BackgroundTo = 12615808
        Colors.BorderLine = cl3DDkShadow
        Colors.ClickedFrom = 12615808
        Colors.ClickedTo = clWhite
        Quality = bqHigh
        ShowFocusRect = False
      end
    end
  end
end
