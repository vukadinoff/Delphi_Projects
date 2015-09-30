unit FrameRptOrderArticlesUnit;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Grids, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, dxSkinsCore, dxSkinsDefaultPainters,
  dxSkinscxPCPainter, cxCustomData, cxClasses, cxFilter, cxData, cxDataStorage,
  cxEdit, cxNavigator, DB, cxDBData, dxSkinsdxBarPainter, Menus, ImgList,
  mySQLDbTables, cxGridLevel, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGridCustomView, cxGrid, StdCtrls, cxGridStrs, cxGridCustomPopupMenu, cxGridPopupMenu,
  LocalizeDevExpressUnit, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev,
  dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore, dxPSPDFExport,
  cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv, dxPSPrVwRibbon, dxPScxPageControlProducer,
  dxPScxGridLnk, dxPScxGridLayoutViewLnk, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxSkinsdxRibbonPainter, dxPSCore, dxPScxCommon, cxContainer, ComCtrls,
  dxCore, cxDateUtils, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCalendar,
  cxObjectDateEdit, cxLabel, cxGroupBox, dxScreenTip, dxCustomHint, cxHint;

type
  TFrameRptOrderArticlesF = class(TFrame)
    dbMegalanCafeDev      : TmySQLDatabase;
    dsItems               : TDataSource;
    dsOrders              : TDataSource;
    qryItems              : TmySQLQuery;
    qryOrders             : TmySQLQuery;

    G1                    : TcxGrid;
    G1L1                  : TcxGridLevel;
    G1V1                  : TcxGridDBTableView;
    G1V1order_id          : TcxGridDBColumn;
    G1V1cafe_name         : TcxGridDBColumn;
    G1V1table_name        : TcxGridDBColumn;
    G1V1creator_name      : TcxGridDBColumn;
    G1V1creation_timestamp: TcxGridDBColumn;
    G1V1order_status      : TcxGridDBColumn;
    G1V1closed_timestamp  : TcxGridDBColumn;
    G1V1order_total       : TcxGridDBColumn;
    PopupMenu             : TPopupMenu;
    N1                    : TMenuItem;
    N2                    : TMenuItem;
    N3                    : TMenuItem;
    IL1small              : TImageList;
    GroupBoxGridOrders    : TGroupBox;
    G1PopupMenu           : TcxGridPopupMenu;
    Printer               : TdxComponentPrinter;
    PrinterG1             : TdxGridReportLink;

    G2                    : TcxGrid;
    G2L1                  : TcxGridLevel;
    G2V1                  : TcxGridDBTableView;
    G1V1good_name         : TcxGridDBColumn;
    G2V1good_price        : TcxGridDBColumn;
    G2V1good_quantity     : TcxGridDBColumn;
    G2V1good_unit         : TcxGridDBColumn;
    G2V1good_room         : TcxGridDBColumn;
    G2V1create_timestamp  : TcxGridDBColumn;
    G2V1good_billed       : TcxGridDBColumn;
    G2V1total             : TcxGridDBColumn;
    GroupBoxGridItems     : TGroupBox;
    G2PopupMenu           : TcxGridPopupMenu;

    GB1                   : TcxGroupBox;
    cxLabel1              : TcxLabel;
    cxLabel2              : TcxLabel;
    //edStartDate           : TcxObjectDateEdit;
    //edEndDate             : TcxObjectDateEdit;
    cxHintStyleController1: TcxHintStyleController;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure G1V1FocusedRecordChanged(Sender: TcxCustomGridTableView;
                                       APrevFocusedRecord,
                                       AFocusedRecord: TcxCustomGridRecord;
                                       ANewItemRecordFocusingChanged: Boolean);
    procedure G1V1OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure G1V1order_idGetCellHint1(Sender: TcxCustomGridTableItem;
                                      ARecord: TcxCustomGridRecord;
                                      ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint;
                                      var AHintText: TCaption; var AIsHintMultiLine: Boolean;
                                      var AHintTextRect: TRect);
  private
    procedure InitializeDataBase;
    procedure InitializeDataSourceOrders;
    procedure InitializeDataSourceItems;
    procedure InitializeG1V1(gridView: TcxGridDBTableView);
    procedure InitializeG2V1(gridView: TcxGridDBTableView);
  private
    function OpenDatabase(const dbName: string; const dbHost: string; const dbPort: Integer;
                          const dbUserName: string; const dbUserPassword: string): Boolean;
    function GetSelectedOrderId: Integer;
  private
    procedure SaveGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
    procedure LoadGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
    procedure SetFooterSummary(const gridView: TcxGridDBTableView; const fieldName: string;
                               const action: TcxSummaryKind; const summaryFieldFormat: string);
    function QueryExecute(var qryQuery: TmySQLQuery; const sSQLstring: string): Boolean;
    procedure ExecuteQueryOrders;
    procedure ExecuteQueryItems(const orderId: Integer);
    procedure RefreshGrid(const gridView: TcxGridDBTableView; const qryQuery: TmySQLQuery);
  public
    procedure Notifier_RefreshAll;
    procedure Notifier_PrintReport;
		procedure Notifier_ExportReport(const aExportFmt:Integer);

  end;

const
  gcDB_Name         = 'megalan_cafe_dev';
  gcDB_Host         = '192.168.2.23';
  gcDB_Port         = 3307;
  gcDB_UserName     = 'kkroot';
  gcDB_UserPassword = 'k6415dl';

const
  gcEXE_FILE_EXT    = '.exe';
  gcG1V1_VIEW       = 'G1V1_ver0.ini';
  gcG2V1_VIEW       = 'G2V1_ver0.ini';

implementation

uses
  MLDMS_CommonExportsUnit;

{$R *.dfm}

constructor TFrameRptOrderArticlesF.Create(AOwner: TComponent);
var
  lviOrderId: Integer;
begin
  inherited Create(AOwner);

  InitializeDataBase;
  if not (OpenDatabase(gcDB_Name, gcDB_Host, gcDB_Port, gcDB_UserName, gcDB_UserPassword)) then
    Exit;// If Open Database process fail then application terminate

  InitializeDataSourceOrders;
  InitializeDataSourceItems;
  ExecuteQueryOrders;

  lviOrderId := GetSelectedOrderId;
  if (lviOrderId > -1) then
    ExecuteQueryItems(lviOrderId);

  InitializeG1V1(G1V1);
  InitializeG2V1(G2V1);

  G1V1.OnMouseMove := G1V1OnMouseMove;
end;

destructor TFrameRptOrderArticlesF.Destroy;
begin
  SaveGridView(G1V1, gcG1V1_VIEW);
  SaveGridView(G2V1, gcG2V1_VIEW);

  if Assigned(G1)then
    G1.Free;
  if Assigned(G2)then
    G2.Free;
  if Assigned(qryItems)then
    qryItems.Free;
  if Assigned(qryOrders)then
    qryOrders.Free;
  if Assigned(dsItems)then
    dsItems.Free;
  if Assigned(dsOrders)then
    dsOrders.Free;
  if Assigned(dbMegalanCafeDev)then
    dbMegalanCafeDev.Free;

  inherited Destroy;
end;

procedure TFrameRptOrderArticlesF.InitializeDataBase;
begin
  dbMegalanCafeDev.DatabaseName := gcDB_Name;
  dbMegalanCafeDev.Host         := gcDB_Host;
  dbMegalanCafeDev.Port         := gcDB_Port;
  dbMegalanCafeDev.UserName     := gcDB_UserName;
  dbMegalanCafeDev.UserPassword := gcDB_UserPassword;
end;

function TFrameRptOrderArticlesF.OpenDatabase(const dbName: string; const dbHost: string; const dbPort: Integer;
                                              const dbUserName: string; const dbUserPassword: string): Boolean;
begin
  try
    dbMegalanCafeDev.Open;
    Result := dbMegalanCafeDev.Connected;
  finally end;
end;

function TFrameRptOrderArticlesF.QueryExecute(var qryQuery: TmySQLQuery; const sSQLstring: string): Boolean;
begin
  if (qryQuery.Active) then qryQuery.Close;
  qryQuery.SQL.Text := sSQLstring;

  Result := False;
  if not (qryQuery.Active) then
  begin
    Screen.Cursor := crSQLWait;
    qryQuery.DisableControls;
    try
      qryQuery.Open;
    finally
      qryQuery.EnableControls;
      Screen.Cursor := crDefault;
      Result := qryQuery.Active;
    end;
  end;
end;

procedure TFrameRptOrderArticlesF.ExecuteQueryOrders;
const
  lcsSqlString = 'SELECT ord.tblord_id AS order_id, c.name AS cafe_name,          ' +
                 '       u.name AS creator_name, ob.tblobj_name AS table_name,    ' +
                 '       ord.createdon AS creation_timestamp,                     ' +
                 '       CASE                                                     ' +
                 '          WHEN ord.status = 0                                   ' +
                 '             THEN ''отворена''                                  ' +
                 '          ELSE IF(ord.status = 1, ''приключена'', ''анулирана'')' +
                 '       END AS order_status,                                     ' +
                 '       ord.processedon AS closed_timestamp,                     ' +
                 '       (ord.total + ord.discount) AS order_total                ' +
                 'FROM cafe_tblorders AS ord                                      ' +
                 'INNER JOIN megalan_dev.firm_cafe AS c                           ' +
                 '   ON (ord.cafe_id = c.cafe_id)                                 ' +
                 'LEFT OUTER JOIN cafe_tblobjects AS ob                           ' +
                 '   ON (ord.tblobj_id = ob.tblobj_id)                            ' +
                 'LEFT OUTER JOIN megalan_dev.users AS u                          ' +
                 '   ON (ord.createdby = u.user_id)                               ' +
                 'LEFT OUTER JOIN megalan_dev.user_types AS ut                    ' +
                 '   ON (u.type = ut.id)                                          ' +
                 'ORDER BY order_id;                                              ';
begin
  QueryExecute(qryOrders, lcsSqlString);
end;

procedure TFrameRptOrderArticlesF.ExecuteQueryItems(const orderId: Integer);
const
  lcsSqlString = 'SELECT cg.name AS good_name, ci.price AS good_price,         ' +
                 '       ci.quantity AS good_quantity, cg.unit AS good_unit,   ' +
                 '       CASE                                                  ' +
                 '          WHEN (cg.room_group = 1)                           ' +
                 '             THEN ''кухня''                                  ' +
                 '          ELSE IF(cg.room_group = 2, ''бар'',                ' +
                 '                  IF(cg.room_group = 3, ''суши'', ''други''))' +
                 '       END AS good_room,                                     ' +
                 '       ci.dt_create AS create_timestamp,                     ' +
                 '       IF(ci.cashed = 1, ''да'', ''не'') AS good_billed,     ' +
                 '       ci.price*ci.quantity AS total                         ' +
                 'FROM cafe_tblorder_items AS ci                               ' +
                 'INNER JOIN cafe_good AS cg                                   ' +
                 '    ON (ci.good_id = cg.id)                                  ' +
                 'WHERE (ci.tblord_id = %d);                                   ';
begin
  QueryExecute(qryItems, Format(lcsSqlString, [orderId]));
end;

procedure TFrameRptOrderArticlesF.InitializeDataSourceOrders;
begin
  dsOrders.DataSet := qryOrders;
  qryOrders.Database := dbMegalanCafeDev;
end;

procedure TFrameRptOrderArticlesF.InitializeDataSourceItems;
begin
  dsItems.DataSet := qryOrders;
  qryItems.Database := dbMegalanCafeDev;
end;

procedure TFrameRptOrderArticlesF.InitializeG1V1(gridView: TcxGridDBTableView);
begin
  LoadGridView(gridView, gcG1V1_VIEW);

  SetFooterSummary(gridView, 'order_id', skCount, 'Брой ордери: ######');
  SetFooterSummary(gridView, 'cafe_name', skNone, '');
  SetFooterSummary(gridView, 'order_total', skSum, 'Сума ордери: ####.##');

  gridView.DataController.DataSource := dsOrders;
  gridView.DataController.DataSource.DataSet := qryOrders;
  gridView.OptionsView.ColumnAutoWidth := True;

  TcxGridViewControlsMenuGroup.CreateMenuGroup(PopupMenu, N1, G1V1);
  TcxGridExportMenuGroup.CreateMenuGroup(PopupMenu, N3);
end;

procedure TFrameRptOrderArticlesF.InitializeG2V1(gridView: TcxGridDBTableView);
begin
  LoadGridView(gridView, gcG2V1_VIEW);

  SetFooterSummary(gridView, 'good_name', skCount, 'Брой артикули: ######');
  SetFooterSummary(gridView, 'total', skSum, 'Сума поръчки: ####.##');

  gridView.DataController.DataSource := dsItems;
  gridView.DataController.DataSource.DataSet := qryItems;
  gridView.DataController.FocusedRowIndex := 0;
  gridView.OptionsView.ColumnAutoWidth := True;
end;

procedure TFrameRptOrderArticlesF.SaveGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
var
  lvsIniFileName: string;
begin
  lvsIniFileName := StringReplace(Application.ExeName, gcEXE_FILE_EXT, '',
                                  [rfReplaceAll, rfIgnoreCase]) + iniFileExt;
  try
    gridView.StoreToIniFile(lvsIniFileName);
  finally end;
end;

procedure TFrameRptOrderArticlesF.LoadGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
var
  lvsIniFileName: string;
begin
  lvsIniFileName := StringReplace(Application.ExeName, gcEXE_FILE_EXT, '',
                                  [rfReplaceAll, rfIgnoreCase]) + lvsIniFileName;
  if FileExists(lvsIniFileName) then
  begin
    try
      gridView.RestoreFromIniFile(lvsIniFileName);
    finally end;
  end;
end;

procedure TFrameRptOrderArticlesF.SetFooterSummary(const gridView: TcxGridDBTableView; const fieldName: string;
                                                   const action: TcxSummaryKind; const summaryFieldFormat: string);
var
  lviColumnIndex: Integer;
begin
  gridView.OptionsView.Footer := True;

  lviColumnIndex := gridView.GetColumnByFieldName(fieldName).Index;
  gridView.Columns[lviColumnIndex].Summary.Footerkind := action;
  gridView.Columns[lviColumnIndex].Summary.FooterFormat := summaryFieldFormat;
end;

function TFrameRptOrderArticlesF.GetSelectedOrderId: Integer;
begin
  Result := -1;
  If ((qryOrders.Active) and (G1V1.Controller.FocusedRecord <> nil) and
      (G1V1.Controller.FocusedRecord is TcxGridDataRow)) then
    Result := Integer(G1V1.Controller.FocusedRecord.Values[0]);
end;

procedure TFrameRptOrderArticlesF.RefreshGrid(const gridView: TcxGridDBTableView; const qryQuery: TmySQLQuery);
var
  lviTopRecordIndex : Integer;
  lviSelectedOrderId: Integer;
begin
  Screen.Cursor := crSQLWait;
  try
    lviSelectedOrderId := GetSelectedOrderId;
    lviTopRecordIndex := G1V1.DataController.Controller.TopRecordIndex;

    qryQuery.DisableControls;
    try
      if(qryQuery.Active)then qryQuery.Close;
      qryQuery.Open;
    finally
      qryQuery.EnableControls;
    end;

    gridView.DataController.Controller.TopRecordIndex := lviTopRecordIndex;
    if(lviSelectedOrderId > 0)then
      gridView.DataController.LocateByKey(lviSelectedOrderId)
    else
      gridView.DataController.FocusedRowIndex := 0;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrameRptOrderArticlesF.G1V1FocusedRecordChanged(Sender: TcxCustomGridTableView;
                                                           APrevFocusedRecord,
                                                           AFocusedRecord: TcxCustomGridRecord;
                                                           ANewItemRecordFocusingChanged: Boolean);
var
  lviOrderId: Integer;
begin
  lviOrderId := GetSelectedOrderId;
  if (lviOrderId > -1) then
  begin
    ExecuteQueryItems(lviOrderId);
    GroupBoxGridItems.Caption := 'Артикули в поръчка №' + IntToStr(lviOrderId);
  end;
end;

procedure TFrameRptOrderArticlesF.Notifier_PrintReport;
// var lvLeftTitle:string;
begin
  inherited;
  PrinterG1.ReportTitle.Text:=Format('Справка "%s"', [Self.Caption]);
  {if(FPeriodFrame1.Active)then
      lvLeftTitle:=Format('Период от %s до %s',  [FPeriodFrame1.StartPeriod_AsText, FPeriodFrame1.EndPeriod_AsText]);
  Printer1G1.PrinterPage.PageHeader.LeftTitle.Text:=lvLeftTitle;
  Printer1G1.PrinterPage.PageHeader.RightTitle.Text:='В обекти: ' + FCoffeePanel1.CheckedItemsAsText;}
  PrinterG1.Preview();
end;

procedure TFrameRptOrderArticlesF.Notifier_RefreshAll;
begin
  RefreshGrid(G1V1, qryOrders);
  RefreshGrid(G2V1, qryItems);
end;

procedure TFrameRptOrderArticlesF.Notifier_ExportReport(const aExportFmt: Integer);
begin
  inherited;
  CommonExports.ExportGridTo(G1);
end;

procedure TFrameRptOrderArticlesF.G1V1OnMouseMove(Sender: TObject;
                                                  Shift: TShiftState; X, Y: Integer);
var 
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := TcxGridSite(Sender).ViewInfo.GetHitTest(X, Y);
  case AHitTest.HitTestCode of
    htColumnHeader:
      caption := TcxGridColumnHeaderHitTest(AHitTest).Column.Name;
    htCell:
      caption := TcxGridRecordCellHitTest(AHitTest).GridRecord.Values [TcxGridRecordCellHitTest(AHitTest).Item.Index];
      //and so for everyone  an element (the full list in help)
  end;
  cxHintStyleController1.ShowHint(X, Y, '', 'hint');
end;

procedure TFrameRptOrderArticlesF.G1V1order_idGetCellHint1(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint;
  var AHintText: TCaption; var AIsHintMultiLine: Boolean;
  var AHintTextRect: TRect);
begin
  AHintText := 'tooltip';
end;

initialization
  Setup_QuantumGridsResources;
end.
