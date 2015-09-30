object MainF: TMainF
  Left = 210
  Top = 124
  Width = 1124
  Height = 656
  Caption = 'Event times'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object G1: TcxGrid
    Left = 0
    Top = 0
    Width = 1108
    Height = 618
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object G1V1: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataModeController.SyncMode = False
      DataController.DataSource = dsEvents
      DataController.KeyFieldNames = 'tblorditem_id'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsData.Editing = False
      OptionsView.ColumnAutoWidth = True
    end
    object G1L1: TcxGridLevel
      GridView = G1V1
    end
  end
  object dbMegalanCafeProd: TmySQLDatabase
    Connected = True
    DatabaseName = 'megalan_cafe_prod'
    UserName = 'kkroot'
    UserPassword = 'k6415dl'
    Port = 3307
    Host = '192.168.2.23'
    ConnectOptions = []
    Params.Strings = (
      'Port=3307'
      'TIMEOUT=30'
      'DatabaseName=megalan_cafe_prod'
      'Host=192.168.2.23'
      'UID=kkroot'
      'PWD=k6415dl')
    Left = 37
    Top = 72
  end
  object dsEvents: TDataSource
    AutoEdit = False
    DataSet = qryEvents
    Left = 112
    Top = 72
  end
  object qryEvents: TmySQLQuery
    Database = dbMegalanCafeProd
    SQL.Strings = (
      '')
    Left = 170
    Top = 72
  end
  object qryEventNames: TmySQLQuery
    Database = dbMegalanCafeProd
    Active = True
    SQL.Strings = (
      'SELECT * FROM cafe_orditems_event_types ORDER BY event_type_id;')
    Left = 240
    Top = 72
  end
end
