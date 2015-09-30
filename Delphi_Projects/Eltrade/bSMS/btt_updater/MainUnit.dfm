object MainForm: TMainForm
  Left = 254
  Top = 22
  BorderIcons = []
  BorderStyle = bsNone
  Caption = #1045#1051#1058#1056#1045#1049#1044' '#1087#1088#1086#1074#1077#1088#1082#1072' '#1079#1072' '#1085#1086#1074#1072' '#1074#1077#1088#1089#1080#1103' '#1085#1072' '#1089#1086#1092#1090#1091#1077#1088#1072
  ClientHeight = 638
  ClientWidth = 1010
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 17
    Height = 612
    Align = alLeft
    BevelOuter = bvNone
    Color = 12033949
    TabOrder = 1
  end
  object PanelMain: TPanel
    Left = 17
    Top = 0
    Width = 993
    Height = 612
    Align = alClient
    BevelOuter = bvNone
    Color = 12033949
    TabOrder = 0
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 993
      Height = 65
      Align = alTop
      BevelOuter = bvNone
      Color = 12033949
      TabOrder = 2
    end
    object PanelCpanel: TPanel
      Left = 40
      Top = 147
      Width = 953
      Height = 465
      BevelOuter = bvNone
      Color = 12033949
      TabOrder = 0
      object lblMsg: TLabel
        Left = 0
        Top = 0
        Width = 953
        Height = 24
        Align = alTop
        Alignment = taCenter
        Caption = #1052#1086#1083#1103', '#1080#1079#1095#1072#1082#1072#1081#1090#1077' '#1076#1086#1082#1072#1090#1086' '#1084#1080#1085#1077' '#1087#1088#1086#1074#1077#1088#1082#1072#1090#1072' '#1079#1072' '#1085#1086#1074#1072' '#1074#1077#1088#1089#1080#1103'!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object PanelBtn: TPanel
        Left = 0
        Top = 328
        Width = 953
        Height = 137
        Align = alBottom
        BevelOuter = bvNone
        Color = 12033949
        TabOrder = 0
        object btnCheckAgain: TdxSpeedButton
          Left = 16
          Top = 14
          Width = 300
          Height = 115
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
          About = 'Design eXperience. '#169' 2002 M. Hoffmann'
          Version = '1.0.2g'
          OnClick = btnCheckAgainClick
          Caption = #1053#1086#1074' '#1086#1087#1080#1090' '#1079#1072' '#1087#1088#1086#1074#1077#1088#1082#1072' '#1079#1072' '#1085#1086#1074#1072' '#1074#1077#1088#1089#1080#1103
          Colors.BackgroundFrom = 16771022
          Colors.BackgroundTo = 15155229
          Colors.BorderLine = 7295569
          Colors.ClickedFrom = 15155229
          Colors.ClickedTo = 16634072
          Quality = bqHigh
        end
        object btnContinueAdmin: TdxSpeedButton
          Left = 352
          Top = 64
          Width = 225
          Height = 65
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
          About = 'Design eXperience. '#169' 2002 M. Hoffmann'
          Version = '1.0.2g'
          OnClick = btnContinueClick
          Caption = #1087#1088#1086#1076#1098#1083#1078#1080
          Colors.BackgroundFrom = 16771022
          Colors.BackgroundTo = 15155229
          Colors.BorderLine = 7295569
          Colors.ClickedFrom = 15155229
          Colors.ClickedTo = 16634072
          Quality = bqHigh
        end
        object btnContinue: TdxSpeedButton
          Left = 613
          Top = 14
          Width = 300
          Height = 115
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
          About = 'Design eXperience. '#169' 2002 M. Hoffmann'
          Version = '1.0.2g'
          OnClick = btnContinueClick
          Caption = #1055#1088#1086#1076#1098#1083#1078#1072#1074#1072#1085#1077' '#1073#1077#1079' '#1087#1088#1086#1074#1077#1088#1082#1072
          Colors.BackgroundFrom = 16771022
          Colors.BackgroundTo = 15155229
          Colors.BorderLine = 7295569
          Colors.ClickedFrom = 15155229
          Colors.ClickedTo = 16634072
          Quality = bqHigh
        end
      end
      object lstMsg: TMemo
        Left = 0
        Top = 79
        Width = 953
        Height = 249
        Align = alClient
        Color = 12033949
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
        Visible = False
      end
      object PanelProgress: TPanel
        Left = 0
        Top = 48
        Width = 953
        Height = 31
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvNone
        Color = 12033949
        TabOrder = 2
        object prgParts: TProgressBar
          Left = 1
          Top = 1
          Width = 951
          Height = 29
          Align = alClient
          BorderWidth = 1
          Max = 102
          Step = 1
          TabOrder = 0
        end
      end
      object PanelProgressSpace: TPanel
        Left = 0
        Top = 24
        Width = 953
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        Color = 12033949
        TabOrder = 3
      end
    end
    object PanelTitle: TPanel
      Left = 0
      Top = 65
      Width = 993
      Height = 48
      Align = alTop
      BevelOuter = bvNone
      Color = 12033949
      TabOrder = 1
      object lblTitle: TLabel
        Left = 0
        Top = 0
        Width = 993
        Height = 39
        Align = alClient
        Alignment = taCenter
        Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1079#1072' '#1085#1072#1083#1080#1095#1085#1072' '#1085#1086#1074#1072' '#1074#1077#1088#1089#1080#1103' '#1085#1072' '#1089#1086#1092#1090#1091#1077#1088#1072
        Color = 12033949
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object EltradeImage: TImage
        Left = 0
        Top = 0
        Width = 993
        Height = 39
        Align = alClient
        Picture.Data = {
          07544269746D617076090000424D760900000000000076000000280000008000
          000024000000010004000000000000090000C40E0000C40E0000100000000000
          0000000000000000800000800000008080008000000080008000808000008080
          8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
          FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFF99999999999FF9999999999999FFFF9999999FFFFF9999999FF999999
          9F999999FFFFF999999F9999999999999FFFFF9999999999999FFFFFFFFFFFFF
          FFFFFFF99999999999FF9999999999999FFFF9999999FFFFF9999999FF999999
          9F999999FFFFF999999F99999999999999FFFF9999999999999FFFFFFFFFFFFF
          FFFFFFF999999999999F99999999999999FFF9999999FFFFF9999999FF999999
          9FF999999FFFF999999F999999999999999FFF99999999999999FFFFFFFFFFFF
          FFFFFFF999999999999F99999999999999FFF9999999FFFFF9999999FFF99999
          99F999999FFFF999999F9999999999999999FF99999999999999FFFFFFFFFFFF
          FFFFFFFF99999999999FF9999999999999FFFF9999999FFFFF9999999FF99999
          99F999999FFFF999999FF999999999999999FFF9999999999999FFFFFFFFFFFF
          FFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF9999999FF99999
          99FF999999999999999FF999999FFFF999999FF9999999FFFFFFFFFFFFFFFFFF
          FFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF9999999FF99999
          999F999999999999999FF9999999FFF999999FF9999999FFFFFFFFFFFFFFFFFF
          FFFFFFFF9999999FFFFFF9999999FFFFFFFFFF9999999FFFFF9999999FF99999
          999F999999999999999FF9999999FFF999999FF99999999FFFFFFFFFFFFFFFFF
          FFFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF999999FF99999
          999FF99999999999999FFF999999FFFF99999FFF9999999FFFFFFFFFFFFFFFFF
          FFFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF999999FFF9999
          999FF99999999999999FFF999999FFFF999999FF9999999FFFFFFFFFFFFFFFFF
          FFFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF9999999FF9999
          999FF99999FFFF99999FFF9999999FFF999999FF9999999FFFFFFFFFFFFFFFFF
          FFFFFFFFF9999999FFFFFF9999999FFFFFFFFFF9999999FFFFF9999999FF9999
          99FFFF99999FFF99999FFF9999999FFF999999FF99999999FFFFFFFFFFFFFFFF
          FFFFFFFFFF99999999999FF9999999FFFFFFFFFF9999999FFFFF999999999999
          9FFFFF99999FFFF9999FFFF999999FFF999999FFF9999999999999FFFFFFFFFF
          FFFFFFFFFF99999999999FF9999999FFFFFFFFFF9999999FFFFF999999999999
          FFFFFF999999FFF9999FFFF999999FFF999999FFF9999999999999FFFFFFFFFF
          FFFFFFFFFF99999999999FF9999999FFFFFFFFFF9999999FFFFF999999999999
          9FFFFFF99999FFF99999FFF999999FFFF999999FF9999999999999FFFFFFFFFF
          FFFFFFFFFF999999999999F9999999FFFFFFFFFF9999999FFFFF999999999999
          99FFFFF99999FFF99999FFF9999999FFF999999FF99999999999999FFFFFFFFF
          FFFFFFFFFFF99999999999FF9999999FFFFFFFFFF9999999FFFFF999999FFF99
          9999FFF999999FF99999FFFF999999FFF999999FFF9999999999999FFFFFFFFF
          FFFFFFFFFFF99999FFFFFFFF9999999FFFFFFFFFF9999999FFFFF999999FFF99
          9999FFFF99999FF99999FFFF999999FFF999999FFF999999FFFFFFFFFFFFFFFF
          FFFFFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF999999FFF99
          99999FFF99999FF99999FFFF999999FFFF99999FFF9999999FFFFFFFFFFFFFFF
          FFFFFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF9999999FFF9
          99999FFF999999F99999FFFF9999999FFF999999FF9999999FFFFFFFFFFFFFFF
          FFFFFFFFFFFF99999FFFFFFFF9999999FFFFFFFFFF9999999FFFFF999999FFF9
          999999FFF99999F99999FFFFF999999FFF999999FFF999999FFFFFFFFFFFFFFF
          FFFFFFFFFFFF999999FFFFFFF9999999FFFFFFFFFF9999999FFFFF999999FFF9
          999999FFF99999999999FFFFF999999FFF999999FFF9999999FFFFFFFFFFFF99
          FFFFFFFFFFFF999999FFFFFFF9999999FFFFF9999999999999999F9999999FFF
          999999FFF99999999999FFFFF999999FFF999999FFF9999999FFFFFFFFFF99FF
          99FFFFFFFFFF999999999999F9999999FFFFF9999999999999999F9999999999
          999999FFFF9999999999FFFFF999999999999999FFF99999999999999FF9F9F9
          9F9FFFFFFFFFF99999999999FF9999999FFFFF999999999999999FF999999999
          99999FFFFF9999999999FFFFFF99999999999999FFFF9999999999999FF9F9F9
          FF9FFFFFFFFFF99999999999FF9999999FFFFF9999999999999999F999999999
          99999FFFFF9999999999FFFFFF99999999999999FFFF9999999999999F9FF999
          FFF9FFFFFFFFF999999999999F9999999FFFFF9999999999999999F999999999
          9999FFFFFFF999999999FFFFFF9999999999999FFFFF999999999999999FF9FF
          9FF9FFFFFFFFF999999999999F9999999FFFFF9999999999999999F999999999
          99FFFFFFFFF999999999FFFFFF999999999999FFFFFF99999999999999F9F9FF
          9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F999
          FF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF99FF
          99FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF99
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        Transparent = True
      end
      object bvlLine: TBevel
        Left = 0
        Top = 39
        Width = 993
        Height = 9
        Align = alBottom
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 612
    Width = 1010
    Height = 26
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvNone
    Color = 12033949
    TabOrder = 2
    object prgBar: TProgressBar
      Left = 1
      Top = 1
      Width = 1008
      Height = 24
      Align = alClient
      BorderWidth = 2
      Step = 1
      TabOrder = 0
    end
  end
end
