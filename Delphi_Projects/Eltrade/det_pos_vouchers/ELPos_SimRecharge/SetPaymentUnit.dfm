object SetPaymentForm: TSetPaymentForm
  Left = 593
  Top = 145
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
    Left = 16
    Top = 490
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
    OnMouseUp = btnCancelMouseUp
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
    Left = 32
    Top = 10
    Width = 355
    Height = 447
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    Visible = False
    object lTitle: TLabel
      Left = 0
      Top = 0
      Width = 355
      Height = 73
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = #1052#1086#1083#1103', '#1087#1086#1090#1074#1098#1088#1076#1077#1090#1077' '#13#10#1079#1072#1088#1077#1078#1076#1072#1085#1077#1090#1086' '#1085#1072' '#1057#1048#1052' '#1082#1072#1088#1090#1072#1090#1072'...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
    object btnPay1: TdxSpeedButton
      Left = 49
      Top = 170
      Width = 111
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnMouseUp = btnPay1MouseUp
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object btnPay2: TdxSpeedButton
      Left = 176
      Top = 170
      Width = 111
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnMouseUp = btnPay1MouseUp
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object btnPay3: TdxSpeedButton
      Left = 49
      Top = 234
      Width = 111
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnMouseUp = btnPay1MouseUp
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object btnPay4: TdxSpeedButton
      Left = 176
      Top = 234
      Width = 111
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnMouseUp = btnPay1MouseUp
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object btnPay5: TdxSpeedButton
      Left = 49
      Top = 298
      Width = 111
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnMouseUp = btnPay1MouseUp
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object btnPay6: TdxSpeedButton
      Left = 176
      Top = 298
      Width = 111
      Height = 45
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      About = 'Design eXperience. '#169' 2002 M. Hoffmann'
      Version = '1.0.2g'
      OnMouseUp = btnPay1MouseUp
      Colors.BackgroundTo = 12500670
      Colors.BorderLine = cl3DDkShadow
      Colors.ClickedFrom = 12500670
      Colors.ClickedTo = clWhite
      Quality = bqHigh
      ShowFocusRect = False
    end
    object lSimInfo: TLabel
      Left = 0
      Top = 73
      Width = 355
      Height = 60
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
  end
end
