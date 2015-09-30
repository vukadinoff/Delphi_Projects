unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, mySQLDbTables, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, dxSkinsCore, dxSkinsDefaultPainters,
  dxSkinscxPCPainter, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxEdit, cxNavigator, cxDBData, cxGridLevel, cxClasses, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxGridStrs;

type
  TMainForm = class(TForm)
    dbMegalanCafeDev: TmySQLDatabase;
    dsOrders        : TDataSource;
    dsItems         : TDataSource;
    qryOrders       : TmySQLQuery;
    qryItems        : TmySQLQuery;
    G1              : TcxGrid;
    G1L1            : TcxGridLevel;
    G1V1            : TcxGridDBTableView;
    G2V1: TcxGridDBTableView;
    G2L1: TcxGridLevel;
    G2: TcxGrid;

    procedure FormCreate(Sender: TObject);

  private
    function OpenDatabase(dbName: string; dbHost: string; dbPort: Integer;
                          dbUserName: string; dbUserPassword: string): Boolean;

    procedure ExecuteQueryOrders();
    procedure ExecuteQueryItems();
    procedure InitializeDataSourceOrders();
    procedure InitializeDataSourceItems();
    procedure InitializeG1();
    procedure InitializeG2();
  end;

const
  dbName         = 'megalan_cafe_dev';
  dbHost         = '192.168.2.23';
  dbPort         = 3307;
  dbUserName     = 'kkroot';
  dbUserPassword = 'k6415dl';

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function TMainForm.OpenDatabase(dbName: string; dbHost: string; dbPort: Integer;
                                dbUserName: string; dbUserPassword: string): Boolean;
begin
  dbMegalanCafeDev.DatabaseName := dbName;
  dbMegalanCafeDev.Host         := dbHost;
  dbMegalanCafeDev.Port         := dbPort;
  dbMegalanCafeDev.UserName     := dbUserName;
  dbMegalanCafeDev.UserPassword := dbUserPassword;

  try
    dbMegalanCafeDev.Open;
    Result := dbMegalanCafeDev.Connected;
  except
    on E : Exception do
      begin
        MessageDlg('Неуспешно отваряне на базата данни. Проверете настройките!' + #13 + #10 +
                    E.Message, mtWarning, [mbOK], 0);
        Result := false;
      end;
  end;
end;

procedure TMainForm.ExecuteQueryOrders();
var
  sqlString: string;
begin
  qryOrders.Database := dbMegalanCafeDev;

  sqlString := 'SELECT * FROM cafe_tblorders;';

  qryOrders.SQL.Text := sqlString;
  qryOrders.Active := True;
end;

procedure TMainForm.ExecuteQueryItems();
var
  sqlString: string;
begin
  qryItems.Database := dbMegalanCafeDev;

  sqlString := 'SELECT * FROM cafe_tblorders;';

  qryItems.SQL.Text := sqlString;
  qryItems.Active := True;
end;

procedure TMainForm.InitializeDataSourceOrders();
begin
  dsOrders.DataSet := qryOrders;
end;

procedure TMainForm.InitializeDataSourceItems();
begin
  dsItems.DataSet := qryOrders;
end;

procedure TMainForm.InitializeG1();
begin
  G1V1.DataController.DataSource := dsOrders;
  G1V1.DataController.DataSource.DataSet := qryOrders;
  G1V1.OptionsView.ColumnAutoWidth := True;
  G1V1.OptionsView.Footer := True;
end;

procedure TMainForm.InitializeG2();
begin
  G2V1.DataController.DataSource := dsItems;
  G2V1.DataController.DataSource.DataSet := qryItems;
  G2V1.OptionsView.ColumnAutoWidth := True;
  G2V1.OptionsView.Footer := True;
end;

{ Processing Form Events                                                        }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if not (OpenDatabase(dbName, dbHost, dbPort, dbUserName, dbUserPassword)) then
  begin
    Halt;
  end;
  InitializeDataSourceOrders();
  InitializeDataSourceItems();
  InitializeG1();
  InitializeG2();
  ExecuteQueryOrders();
  ExecuteQueryItems();
end;

initialization
  cxSetResourceString(@scxGridGroupByBoxCaption, '<Лента за групиране>');

end.
