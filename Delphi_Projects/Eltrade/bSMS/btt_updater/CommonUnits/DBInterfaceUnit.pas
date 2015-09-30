unit DBInterfaceUnit;

interface

uses Classes, SysUtils, Forms, DB, ADODB, IniFiles, Variants;

type
  TDBInterfaceError = procedure(ErrMessage_, SQL_: String) of object;

  TDBInterfaceADO = class (TObject)
  private
   FLastError    : String;
   FDBCnString   : String;
   FDbConnection : TADOConnection;
   FDBQuery      : TADOQuery;
   FDBCommand    : TADOCommand;
   FOnSQLError   : TDBInterfaceError;
   function FGetDbConnected: Boolean;
   function FGetDataSet: TADOQuery;
   procedure PostError(Error_, SQL_: String);
  public
   constructor Create(DBConnectionString: String; AutoCreateConnection: Boolean = true);
   destructor Destroy; override;

   procedure CreateConnectionObject;
   procedure DestroyConnectionObject;
   function  OpenDatabase: Boolean;
   function  CloseDatabase: Boolean;
   procedure StartTransaction;
   procedure CommitTransaction;
   procedure RollbackTransaction;
   function  ExecuteSQLStatement(SQL_: String; var RecAffected: Integer): Boolean;
   function  FillDataSet(SQL_: String): Boolean;
   procedure CloseDataSet;
   function  SaveDataSetToFile(Fname: String): Boolean;
   function  DateToSQL(Value : TDateTime): String;
   function  DateTimeToSQL(Value : TDateTime): String;
   function  StrToSQL(Value: String; MaxLength: Integer = 0; NullForEmpty: Boolean=false): String;
   function  FloatToSql(Value: Real): String;
   function  IntToSql(Value: Integer; NullForEmpty: Boolean=false): String;
   function  BoolToSql(Value: Boolean): String;

   property DbConnection: TADOConnection read FDbConnection;
   property DBConnected: Boolean read FGetDbConnected;
   property DataSet: TADOQuery read FGetDataSet;
   property LastError: String read FLastError write FLastError;
   property OnSQLError: TDBInterfaceError read FOnSQLError write FOnSQLError;
  end;

implementation

//*******************************************************************************
//                        TDBInterfaceADO
//*******************************************************************************
constructor TDBInterfaceADO.Create(DBConnectionString: String; AutoCreateConnection: Boolean = true);
begin
 inherited Create;
 FDBCnString   := DBConnectionString;
 FDbConnection := nil;
 FDBQuery      := nil;
 FDBCommand    := nil;

 if AutoCreateConnection then CreateConnectionObject;
end;

destructor TDBInterfaceADO.Destroy;
begin
 DestroyConnectionObject;
 inherited Destroy;
end;

procedure TDBInterfaceADO.CreateConnectionObject;
begin
 try
  FDbConnection := TADOConnection.Create(nil);
  FDbConnection.LoginPrompt       := false;
  FDbConnection.ConnectionString  := FDBCnString;
  FDbConnection.ConnectOptions    := coConnectUnspecified;
  FDbConnection.CursorLocation    := clUseClient;

  FDBQuery := TADOQuery.Create(nil);
  FDBQuery.ParamCheck := false;
  FDBQuery.Connection := FDbConnection;

  FDBCommand := TADOCommand.Create(nil);
  FDBCommand.ParamCheck := false;
  FDBCommand.CommandType:= cmdText;
  FDBCommand.Connection := FDbConnection;
 except
  on E: exception do
   begin
    FLastError := 'Fail to create DB connection object: ' + E.Message;
    FDbConnection := nil;
   end;
 end;
end;

procedure TDBInterfaceADO.DestroyConnectionObject;
begin
 CloseDatabase;
 if FDBQuery <> nil then FreeAndNil(FDBQuery);
 if FDBCommand <> nil then FreeAndNil(FDBCommand);
 if FDbConnection <> nil then FreeAndNil(FDbConnection);
end;

function TDBInterfaceADO.FGetDbConnected: Boolean;
begin
 if FDbConnection <> nil then
   Result := FDbConnection.Connected
 else
   Result := false;
end;

procedure TDBInterfaceADO.PostError(Error_, SQL_: String);
begin
 if Assigned(FOnSQLError) then FOnSQLError(Error_, SQL_);
end;

function  TDBInterfaceADO.OpenDatabase: Boolean;
begin
 try
   if FDbConnection <> nil then
    begin
     if not FDbConnection.Connected then FDbConnection.Open;
     Result := FDbConnection.Connected;
    end
   else
    raise EAbort.Create('DB Connection object is not initialized.');
 except
  on E: exception do
   begin
    Result     := false;
    FLastError := 'Fail to connect to Database: '+E.Message + ' / CnnStr: '+ FDbConnection.ConnectionString;
    PostError(FLastError, '');
   end;
 end;
end;

function TDBInterfaceADO.CloseDatabase: Boolean;
begin
 Result := true;
 try
   if FDbConnection <> nil then
    begin
     if FDbConnection.Connected then
      begin
       if FDbConnection.InTransaction then FDbConnection.CommitTrans;
       if FDBQuery <> nil then
        begin
         if FDBQuery.Active then FDBQuery.Close;
        end;
       FDbConnection.Close;
      end;
    end;
 except
  on E: exception do
   begin
    Result     := false;
    FLastError := 'Fail to disconnect from Database: '+E.Message;
    PostError(FLastError, '');
   end;
 end;
end;

procedure TDBInterfaceADO.StartTransaction;
begin
 if FDbConnection = nil then Exit;
// if not FDbConnection.Connected then Exit;
 if FDbConnection.InTransaction then FDbConnection.RollbackTrans;
 FDbConnection.BeginTrans;
end;

procedure TDBInterfaceADO.CommitTransaction;
begin
 if FDbConnection = nil then Exit;
// if not FDbConnection.Connected then Exit;
 if FDbConnection.InTransaction then FDbConnection.CommitTrans;
end;

procedure TDBInterfaceADO.RollbackTransaction;
begin
 if FDbConnection = nil then Exit;
// if not FDbConnection.Connected then Exit;
 if FDbConnection.InTransaction then FDbConnection.RollbackTrans;
end;

function TDBInterfaceADO.ExecuteSQLStatement(SQL_: String; var RecAffected: Integer): Boolean;
begin
 try
   {
  if FDBCommand = nil then raise Exception.Create('Query is not assigned.');
  with FDBCommand do
   begin
    CommandText := SQL_;
    Execute(RecAffected, EmptyParam);
   end;

  if FDBQuery = nil then raise Exception.Create('Query is not assigned.');
  with FDBQuery do
   begin
    if Active then Close;
    SQL.Text := SQL_;
    Prepared := true;
    RecAffected := ExecSQL;
   end;          }
  Result := true;
 except
  on E: exception do
   begin
    Result     := false;
    FLastError := 'Fail Execute SQL: '+E.Message;
    PostError(FLastError, SQL_);
   end;
 end;
end;

procedure TDBInterfaceADO.CloseDataSet;
begin
 if FDBQuery = nil then Exit;
 if FDBQuery.Active then FDBQuery.Close;
 CommitTransaction;
end;

function  TDBInterfaceADO.FillDataSet(SQL_: String): Boolean;
begin
 try
  if FDBQuery = nil then raise Exception.Create('Query is not assigned.');
  with FDBQuery do
   begin
    if Active then Close;
    SQL.Text := SQL_;
    Open;
   end;
  Result := true;
 except
  on E: exception do
   begin
    Result     := false;
    FLastError := 'Fail get data from DB: '+E.Message;
    PostError(FLastError, SQL_);
   end;
 end;
end;

function TDBInterfaceADO.FGetDataSet: TADOQuery;
begin
 Result := FDBQuery;
end;

function TDBInterfaceADO.SaveDataSetToFile(Fname: String): Boolean;
begin
  Result := false;
  try
   if FileExists(Fname) then DeleteFile(Fname);
   if (FDBQuery.Active)and(FDBQuery.RecordCount > 0) then
    begin
     FDBQuery.SaveToFile(Fname);
     Result := true;
    end;
  except
   on E: exception do
    begin
     FLastError := 'Fail to save dataset to file: '+E.Message;
     PostError(FLastError, '');     
    end;
  end;
end;

function  TDBInterfaceADO.DateToSQL(Value : TDateTime): String;
begin
 Result := QuotedStr(FormatDateTime('YYYY-MM-DD',Value));
end;

function  TDBInterfaceADO.DateTimeToSQL(Value : TDateTime): String;
begin
 if Value = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz',Value));
end;

function  TDBInterfaceADO.StrToSQL(Value: String; MaxLength: Integer = 0; NullForEmpty: Boolean=false): String;
begin
 if (NullForEmpty)and(Value = '') then
  Result := 'NULL'
 else
 if (MaxLength > 0)and(Length(Value) > MaxLength) then
  Result := QuotedStr(Copy(Value, 1, MaxLength))
 else
  Result := QuotedStr(Value);
end;

function  TDBInterfaceADO.FloatToSql(Value: Real): String;
begin
 DecimalSeparator := '.';
 Result := FormatFloat('0.####', Value);
end;

function  TDBInterfaceADO.IntToSql(Value: Integer; NullForEmpty: Boolean=false): String;
begin
 if (NullForEmpty)and(Value = 0) then
  Result := 'NULL'
 else
  Result := IntToStr(Value);
end;

function  TDBInterfaceADO.BoolToSql(Value: Boolean): String;
begin
 if Value then Result := '1'
  else Result := '0';
end;


end.
