object frmMainForm: TfrmMainForm
  Left = 263
  Top = 236
  Width = 979
  Height = 411
  Caption = #1040#1088#1090#1080#1082#1091#1083#1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cxgrpbx1: TcxGroupBox
    Left = 11
    Top = 8
    TabOrder = 1
    Height = 353
    Width = 937
  end
  object cxGrid: TcxGrid
    Left = 27
    Top = 26
    Width = 905
    Height = 311
    TabOrder = 0
    LookAndFeel.Kind = lfUltraFlat
    object TableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      OnCustomDrawCell = TableViewCustomDrawCell
      DataController.DataSource = dsArticles
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CellAutoHeight = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.FooterAutoHeight = True
      object TableViewDBColumn: TcxGridDBColumn
        DataBinding.FieldName = #1040#1088#1090#1080#1082#1091#1083#1085#1072' '#1075#1088#1091#1087#1072
      end
      object TableViewDBColumn1: TcxGridDBColumn
        DataBinding.FieldName = #1040#1088#1090#1080#1082#1091#1083#1085#1072' '#1087#1086#1076#1075#1088#1091#1087#1072
      end
      object TableViewDBColumn2: TcxGridDBColumn
        DataBinding.FieldName = #1040#1088#1090#1080#1082#1091#1083
      end
      object TableViewDBColumn3: TcxGridDBColumn
        DataBinding.FieldName = #1052#1077#1088#1085#1072' '#1077#1076'.'
      end
    end
    object cxgrdlvlGrid1Level1: TcxGridLevel
      GridView = TableView
    end
  end
  object dbCafeDevDB: TmySQLDatabase
    Connected = True
    DatabaseName = 'megalan_cafe_dev'
    UserName = 'kkroot'
    UserPassword = 'k6415dl'
    Port = 3307
    Host = '192.168.2.23'
    ConnectOptions = []
    Params.Strings = (
      'Port=3307'
      'TIMEOUT=30'
      'DatabaseName=megalan_cafe_dev'
      'UID=kkroot'
      'PWD=k6415dl'
      'Host=192.168.2.23')
    Left = 43
    Top = 42
  end
  object dsArticles: TDataSource
    DataSet = mySQLQuery1
    Left = 107
    Top = 42
  end
  object mySQLQuery1: TmySQLQuery
    Database = dbCafeDevDB
    Active = True
    SQL.Strings = (
      'SELECT cafe_good_groups.name as '#39#1040#1088#1090#1080#1082#1091#1083#1085#1072' '#1075#1088#1091#1087#1072#39', '
      '             cafe_pos_groups.posgr_name as '#39#1040#1088#1090#1080#1082#1091#1083#1085#1072' '#1087#1086#1076#1075#1088#1091#1087#1072#39','
      '             cafe_good.name as '#39#1040#1088#1090#1080#1082#1091#1083#39','
      '             cafe_good.unit as '#39#1052#1077#1088#1085#1072' '#1077#1076'.'#39
      'FROM cafe_good '
      'INNER JOIN cafe_good_groups '
      'ON cafe_good.group_id = cafe_good_groups.id'
      'INNER JOIN cafe_pos_groups'
      'ON cafe_good.posgr_id = cafe_pos_groups.posgr_id;')
    Left = 171
    Top = 42
  end
end
