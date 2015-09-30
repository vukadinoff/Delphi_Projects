object MainForm: TMainForm
  Left = 192
  Top = 124
  Width = 1242
  Height = 656
  Caption = #1057#1087#1088#1072#1074#1082#1072' '#1079#1072' '#1087#1086#1088#1098#1095#1082#1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object G1: TcxGrid
    Left = 56
    Top = 80
    Width = 545
    Height = 489
    TabOrder = 0
    object G1V1: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
    end
    object G1L1: TcxGridLevel
      GridView = G1V1
    end
  end
  object G2: TcxGrid
    Left = 632
    Top = 80
    Width = 529
    Height = 489
    TabOrder = 1
    object G2V1: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
    end
    object G2L1: TcxGridLevel
      GridView = G2V1
    end
  end
  object dbMegalanCafeDev: TmySQLDatabase
    ConnectOptions = []
    Params.Strings = (
      'Port=3306'
      'TIMEOUT=30')
    Left = 56
    Top = 24
  end
  object dsOrders: TDataSource
    Left = 144
    Top = 24
  end
  object qryOrders: TmySQLQuery
    SQL.Strings = (
      '')
    Left = 336
    Top = 24
  end
  object dsItems: TDataSource
    Left = 232
    Top = 24
  end
  object qryItems: TmySQLQuery
    Left = 448
    Top = 24
  end
end
