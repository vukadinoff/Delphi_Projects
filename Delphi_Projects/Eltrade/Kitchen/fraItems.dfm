object frItems: TfrItems
  Left = 0
  Top = 0
  Width = 435
  Height = 130
  Align = alTop
  AutoScroll = False
  DockSite = True
  Color = clBtnFace
  ParentColor = False
  TabOrder = 0
  object pnlDetails: TPanel
    Left = 0
    Top = 0
    Width = 317
    Height = 130
    Align = alClient
    Alignment = taLeftJustify
    BevelInner = bvRaised
    BevelOuter = bvNone
    Color = 12033949
    TabOrder = 0
    OnMouseDown = pnlDetailsMouseDown
    OnMouseMove = pnlDetailsMouseMove
    OnMouseUp = pnlDetailsMouseUp
    object lblTitle: TLabel
      Left = 1
      Top = 1
      Width = 315
      Height = 24
      Align = alTop
      Caption = 'Dish title'
      Constraints.MaxHeight = 43
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      OnMouseDown = lblTitleMouseDown
      OnMouseMove = lblTitleMouseMove
      OnMouseUp = lblTitleMouseUp
    end
    object lblQuantity: TLabel
      Left = 1
      Top = 49
      Width = 315
      Height = 24
      Align = alTop
      Caption = 'Quantity'
      Constraints.MaxHeight = 43
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      OnMouseDown = lblQuantityMouseDown
      OnMouseMove = lblQuantityMouseMove
      OnMouseUp = lblQuantityMouseUp
    end
    object lblMod: TLabel
      Left = 1
      Top = 25
      Width = 315
      Height = 24
      Align = alTop
      Caption = 'Modif'
      Constraints.MaxHeight = 43
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      OnMouseDown = lblModMouseDown
      OnMouseMove = lblModMouseMove
      OnMouseUp = lblModMouseUp
    end
  end
  object pnlTm: TPanel
    Left = 317
    Top = 0
    Width = 118
    Height = 130
    Align = alRight
    BevelInner = bvRaised
    BevelOuter = bvNone
    Color = 12033949
    TabOrder = 1
    object lblTm: TLabel
      Left = 65
      Top = 44
      Width = 53
      Height = 20
      Align = alCustom
      Alignment = taRightJustify
      Anchors = [akLeft, akRight]
      Caption = '0 '#1084#1080#1085'.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnMouseDown = lblTmMouseDown
      OnMouseMove = lblTmMouseMove
      OnMouseUp = lblTmMouseUp
    end
  end
  object tmr: TTimer
    OnTimer = tmrTimer
    Left = 536
    Top = 80
  end
end
