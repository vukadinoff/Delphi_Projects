object DataMod: TDataMod
  OldCreateOrder = False
  Left = 428
  Top = 195
  Height = 385
  Width = 573
  object dsTown: TDataSource
    DataSet = cdsTown
    Left = 80
    Top = 56
  end
  object dsDistrict: TDataSource
    DataSet = cdsDistrict
    Left = 80
    Top = 8
  end
  object dsArea: TDataSource
    DataSet = cdsArea
    Left = 80
    Top = 104
  end
  object dsStreet: TDataSource
    DataSet = cdsStreet
    Left = 80
    Top = 152
  end
  object cdsDistrict: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 24
    Top = 8
  end
  object cdsTown: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 24
    Top = 56
  end
  object cdsArea: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 24
    Top = 104
  end
  object cdsStreet: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 24
    Top = 152
  end
  object cdsSiteTypes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 176
    Top = 8
  end
  object cdsEIKTypes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 176
    Top = 56
  end
  object cdsDRegReasons: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 176
    Top = 104
  end
  object cdsFDTypes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 176
    Top = 152
  end
  object cdsPayTypes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 176
    Top = 200
  end
  object cdsReturnStat: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 176
    Top = 248
  end
  object cdsVATValues: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 252
    Top = 8
  end
  object IBDatabase: TIBDatabase
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey'
      'lc_ctype=WIN1251')
    LoginPrompt = False
    DefaultTransaction = IBTransaction
    Left = 24
    Top = 216
  end
  object IBTransaction: TIBTransaction
    DefaultDatabase = IBDatabase
    Left = 64
    Top = 216
  end
  object cdsSIMPeriods: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 252
    Top = 56
  end
  object IBQuery: TIBQuery
    Database = IBDatabase
    Transaction = IBTransaction
    AutoCalcFields = False
    ParamCheck = False
    Left = 24
    Top = 264
  end
  object frReport: TfrReport
    DefaultCollate = False
    InitialZoom = pzPageWidth
    ModifyPrepared = False
    PreviewButtons = [pbZoom, pbLoad, pbSave, pbPrint, pbFind, pbExit, pbPageSetup]
    ShowProgress = False
    RebuildPrinter = False
    UseDefaultDataSetName = True
    OnGetValue = frReportGetValue
    Left = 336
    Top = 8
    ReportForm = {19000000}
  end
  object frDesigner: TfrDesigner
    Left = 336
    Top = 56
  end
  object frOLEObject: TfrOLEObject
    Left = 380
    Top = 8
  end
  object frRichObject: TfrRichObject
    Left = 380
    Top = 56
  end
  object frCheckBoxObject: TfrCheckBoxObject
    Left = 380
    Top = 104
  end
  object frShapeObject: TfrShapeObject
    Left = 423
    Top = 8
  end
  object frBarCodeObject: TfrBarCodeObject
    Left = 423
    Top = 56
  end
  object frChartObject: TfrChartObject
    Left = 423
    Top = 104
  end
  object frRoundRectObject: TfrRoundRectObject
    Left = 463
    Top = 56
  end
  object frTextExport: TfrTextExport
    ScaleX = 1.000000000000000000
    ScaleY = 1.000000000000000000
    Left = 336
    Top = 224
  end
  object frRTFExport: TfrRTFExport
    ScaleX = 1.300000000000000000
    ScaleY = 1.000000000000000000
    Left = 376
    Top = 176
  end
  object frCSVExport: TfrCSVExport
    ScaleX = 1.000000000000000000
    ScaleY = 1.000000000000000000
    Delimiter = ';'
    Left = 336
    Top = 176
  end
  object frIBXComponents: TfrIBXComponents
    Left = 463
    Top = 104
  end
  object frDialogControls: TfrDialogControls
    Left = 463
    Top = 8
  end
  object frPrintGrid: TfrPrintGrid
    AutoWidth = False
    FitWidth.Enabled = True
    FitWidth.ShrinkOptions = [frsoProportional, frsoShrinkOnly]
    FitWidth.ResizePercent = 30
    FitWidth.ApplyBeforeOnCustomize = False
    PageSize = 0
    PageWidth = 0
    PageHeight = 0
    PageMargins.Left = 10
    PageMargins.Top = 10
    PageMargins.Right = 5
    PageMargins.Bottom = 10
    Orientation = poLandscape
    Title.Font.Charset = DEFAULT_CHARSET
    Title.Font.Color = clNavy
    Title.Font.Height = -16
    Title.Font.Name = 'Arial'
    Title.Font.Style = [fsBold]
    Title.Color = clSilver
    Title.Frame = []
    Title.FrameWidth = 1
    PageHeader.Font.Charset = DEFAULT_CHARSET
    PageHeader.Font.Color = clNavy
    PageHeader.Font.Height = -16
    PageHeader.Font.Name = 'Arial'
    PageHeader.Font.Style = [fsBold]
    PageHeader.Color = clWhite
    PageHeader.Frame = []
    PageHeader.FrameWidth = 1
    PageFooter.Font.Charset = DEFAULT_CHARSET
    PageFooter.Font.Color = clWindowText
    PageFooter.Font.Height = -13
    PageFooter.Font.Name = 'Arial'
    PageFooter.Font.Style = []
    PageFooter.Color = clWhite
    PageFooter.Frame = []
    PageFooter.FrameWidth = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clBlack
    Header.Font.Height = -13
    Header.Font.Name = 'Arial'
    Header.Font.Style = []
    Header.Color = clSilver
    Header.Frame = [frLeft, frTop, frRight, frBottom]
    Header.FrameWidth = 1
    Body.Font.Charset = DEFAULT_CHARSET
    Body.Font.Color = clWindowText
    Body.Font.Height = -13
    Body.Font.Name = 'Arial'
    Body.Font.Style = []
    Body.Color = clWhite
    Body.Frame = [frLeft, frTop, frRight, frBottom]
    Body.FrameWidth = 1
    Left = 336
    Top = 104
  end
  object frHTMExport: TfrHTMExport
    ScaleX = 1.000000000000000000
    ScaleY = 1.000000000000000000
    Left = 415
    Top = 176
  end
  object frHTML2Export: TfrHTML2Export
    Scale = 1.000000000000000000
    Navigator.Position = []
    Navigator.Font.Charset = DEFAULT_CHARSET
    Navigator.Font.Color = clWindowText
    Navigator.Font.Height = -11
    Navigator.Font.Name = 'MS Sans Serif'
    Navigator.Font.Style = []
    Navigator.InFrame = False
    Navigator.WideInFrame = False
    Left = 415
    Top = 224
  end
  object frBMPExport: TfrBMPExport
    Left = 493
    Top = 176
  end
  object frJPEGExport: TfrJPEGExport
    Left = 455
    Top = 176
  end
  object frTIFFExport: TfrTIFFExport
    Left = 455
    Top = 224
  end
  object frRtfAdvExport: TfrRtfAdvExport
    Left = 376
    Top = 224
  end
  object frOLEExcelExport: TfrOLEExcelExport
    Left = 493
    Top = 224
  end
  object cdsOperators: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 253
    Top = 104
  end
  object cdsCCErrors: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 254
    Top = 152
  end
end
