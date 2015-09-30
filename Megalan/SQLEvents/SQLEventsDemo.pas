unit SQLEventsDemo;

interface

uses
  SysUtils,Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxStyles, dxSkinsCore, dxSkinsDefaultPainters, dxSkinscxPCPainter, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, DB, cxDBData, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxClasses, cxGridCustomView,
  Controls, cxGrid, mySQLDbTables, Classes;

type
  TMainF = class(TForm)
    dbMegalanCafeProd: TmySQLDatabase;
    dsEvents         : TDataSource;
    qryEvents        : TmySQLQuery;
    qryEventNames    : TmySQLQuery;

    G1               : TcxGrid;
    G1L1             : TcxGridLevel;
    G1V1             : TcxGridDBTableView;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitializeDataBase;
    procedure InitializeG1V1(gridView: TcxGridDBTableView);
  private
    function OpenDatabase(const dbName: string; const dbHost: string; const dbPort: Integer;
                          const dbUserName: string; const dbUserPassword: string): Boolean;
  private
    function BuildSQLString: string;
    procedure ExecuteQuery;
    procedure SaveGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
    procedure LoadGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
  end;

const
  gcDB_Name         = 'megalan_cafe_prod';
  gcDB_Host         = '192.168.2.23';
  gcDB_Port         = 3307;
  gcDB_UserName     = 'kkroot';
  gcDB_UserPassword = 'k6415dl';

const
  gcEXE_FILE_EXT    = '.exe';
  gcG1V1_VIEW       = 'G1V1_ver0.ini';

var
  MainF: TMainF;

implementation

{$R *.dfm}

procedure TMainF.FormCreate(Sender: TObject);
begin
  inherited;
  InitializeDataBase;
  if not (OpenDatabase(gcDB_Name, gcDB_Host, gcDB_Port, gcDB_UserName, gcDB_UserPassword)) then Exit;
  ExecuteQuery;
  LoadGridView(G1V1, gcG1V1_VIEW);
  InitializeG1V1(G1V1);
end;

procedure TMainF.FormDestroy(Sender: TObject);
var
  iter: Integer;
begin
  SaveGridView(G1V1, gcG1V1_VIEW);

  for iter := Pred(G1V1.ColumnCount) downto 0 do
    G1V1.Columns[iter].Destroy;

  if Assigned(G1)then G1.Free;
  if Assigned(qryEvents)then qryEvents.Free;
  if Assigned(qryEventNames)then qryEventNames.Free;
  if Assigned(dsEvents)then dsEvents.Free;
  if Assigned(dbMegalanCafeProd)then dbMegalanCafeProd.Free;
  inherited;
end;

procedure TMainF.InitializeDataBase;
begin
  dbMegalanCafeProd.DatabaseName := gcDB_Name;
  dbMegalanCafeProd.Host         := gcDB_Host;
  dbMegalanCafeProd.Port         := gcDB_Port;
  dbMegalanCafeProd.UserName     := gcDB_UserName;
  dbMegalanCafeProd.UserPassword := gcDB_UserPassword;
end;

procedure TMainF.InitializeG1V1(gridView: TcxGridDBTableView);
begin
  gridView.DataController.DataSource := dsEvents;
  gridView.DataController.DataSource.DataSet := qryEvents;
  gridView.DataController.FocusedRowIndex := 0;
  gridView.OptionsView.ColumnAutoWidth := False;
end;

function TMainF.OpenDatabase(const dbName: string; const dbHost: string; const dbPort: Integer;
                             const dbUserName: string; const dbUserPassword: string): Boolean;
begin
  try
    dbMegalanCafeProd.Open;
    Result := dbMegalanCafeProd.Connected;
  finally end;
end;

function TMainF.BuildSQLString: string;
const
  lcSQLString = 'MAX(CASE WHEN event_type_id = %d THEN event_dt ELSE "" END) AS "%s"';
var
  lsSqlString: string;
begin
  if (qryEventNames.Active) then qryEventNames.Close;

  if not (qryEventNames.Active) then
  begin
    Screen.Cursor := crSQLWait;
    qryEventNames.DisableControls;
      try
        qryEventNames.SQL.Text := 'SELECT * FROM cafe_orditems_event_types ORDER BY event_type_id;';
        qryEventNames.Open;
      finally
        qryEventNames.EnableControls;
        Screen.Cursor := crDefault;
      end;
  end;

  qryEventNames.First;
  lsSqlString := 'SELECT tblorditem_id AS "Артикулен номер в поръчка", ';
  while not (qryEventNames.Eof) do
  begin
    lsSqlString := lsSqlString + Format(lcSQLString, [Integer(qryEventNames.FieldValues['event_type_id']),
                                                      qryEventNames.FieldValues['event_desc']]);
    qryEventNames.Next;
    if not (qryEventNames.Eof) then lsSqlString := lsSqlString + ', ';
  end;

  Result := lsSqlString;
end;

procedure TMainF.ExecuteQuery;
const
  lcSQLString = ' FROM cafe_orditems_events GROUP BY tblorditem_id;';
var
  lsSqlString: string;
begin
  qryEvents.Database := dbMegalanCafeProd;
  if (qryEvents.Active) then qryEvents.Active:=False;

  lsSqlString := BuildSQLString + lcSQLString;
  if not (qryEvents.Active) then
  begin
    Screen.Cursor := crSQLWait;
    qryEvents.DisableControls;
    try
      qryEvents.SQL.Text:=lsSqlString;
      qryEvents.Open;
      qryEvents.Active:=True;

      G1V1.DataController.CreateAllItems;
      LoadGridView(G1V1, gcG1V1_VIEW);
    finally
      qryEvents.EnableControls;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainF.SaveGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
var
  lvsIniFileName: string;
begin
  lvsIniFileName := StringReplace(Application.ExeName, gcEXE_FILE_EXT, '',
                                  [rfReplaceAll, rfIgnoreCase]) + iniFileExt;
  gridView.StoreToIniFile(lvsIniFileName);
end;

procedure TMainF.LoadGridView(const gridView: TcxGridDBTableView; const iniFileExt: string);
var
  lvsIniFileName: string;
begin
  lvsIniFileName := StringReplace(Application.ExeName, gcEXE_FILE_EXT, '',
                                  [rfReplaceAll, rfIgnoreCase]) + lvsIniFileName;
  if FileExists(lvsIniFileName) then
    try
      gridView.RestoreFromIniFile(lvsIniFileName);
    finally end;
end;

end.
