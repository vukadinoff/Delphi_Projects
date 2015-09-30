//**************************************************************************************
// d.lukarski to stiliyan vukadinov, user db updater, 26.06.2014, deadline 01.09.2014
//**************************************************************************************


unit DBHandlerUnit;

interface

uses
  SysUtils, Classes, Windows, Forms, WinSvc, IBDatabase, IBServices, IBSQL, IBHeader, DB;

type
  TDBHandlerIB_Base = class(TObject)
  private
   FLastError : String;
   FOnError   : TGetStrProc;
   FOnExecSQL : TGetStrProc;
   procedure FSetLastError(Value: String);
  public
   constructor Create;

   function StrToSQL(Src: String; MaxLen: Integer=0; NullIfEmpty: Boolean=false): String;
   function DateTimeToSQL(Src: TDateTime): String;
   function DateToSQL(Src: TDateTime): String;
   function TimeToSQL(Src: TDateTime): String;
   function BoolToSQL(Src: Boolean): String;
   function IntToSQL(Src: Integer; NullForZero: Boolean=false): String; overload;
   function IntToSQL(Src: Int64; NullForZero: Boolean=false): String; overload;
   function FloatToSQL(Src: Real; NullForZero: Boolean=false): String;
   function FloatToSQLEx(Src: Real; NullForZero: Boolean=false): String;

   property OnError: TGetStrProc read FOnError write FOnError;
   property OnExecSQL: TGetStrProc read FOnExecSQL write FOnExecSQL;
   property LastError: String read FLastError write FSetLastError;
  end;

  TDBHandlerIB_Dset = class(TDBHandlerIB_Base)
  private
   FIBSql     : TIBSQL;
   FIBTrn     : TIBTransaction;
  public
   constructor Create(IBDatabase: TIBDatabase);
   destructor Destroy; override;

   function ExecuteSQL(SQL: String; Comment: String=''; Commit: Boolean=true): Boolean; overload;
   function ExecuteSQL(SQL: String; var RecAffected: Integer; Comment: String=''; Commit: Boolean=true): Boolean; overload;
   function FillDataSet(SQL: String; Comment: String=''): Boolean;
   procedure CloseDataSet;
   procedure StartTransaction;
   procedure CommitTransaction;
   procedure RollbackTransaction;

   function GetTableRecordCount(TblName, WhereCnd: String; var RecCount: Integer): Boolean;
   function ExtractGenerator(GenName: String; Increment: Boolean; var GenValue: Int64): Boolean;
   function ExtractGeneratorA(GenName: String; Increment: Boolean; var GenValue: Integer): Boolean;

   function CheckFieldExist(TableName, FieldName: String; var Exist: Boolean): Boolean;
   function CheckTableExist(TableName: String; var Exist: Boolean): Boolean;
   function CheckGeneratorExist(GenName: String; var Exist: Boolean): Boolean;
   function CheckProcedureExist(ProcName: String; var Exist: Boolean): Boolean;
   function CheckTriggerExist(TriggerName: String; var Exist: Boolean): Boolean;
   function CheckDomainExist(DomainName: String; var Exist: Boolean): Boolean;
   function CheckPrimaryKeyExist(TableName: String; var Exist: Boolean): Boolean;
   function CheckForeignKeyExist(TableName, ConstraintName: String; var Exist: Boolean): Boolean;

   property Dataset: TIBSQL read FIBSql write FIBSql;
  end;

  TDBHandlerIB = class(TDBHandlerIB_Dset)
  private
   FIBDb      : TIBDatabase;
   FIBCnf     : TIBConfigService;

   function FGetUserName: String;
   function FGetPassword: String;
   function FGetCharSet: String;
   function FGetConnectionString: String;
   function FGetIsLocalDb: Boolean;
   function FGetConnected: Boolean;
   function FCheckInterbaseServer: Boolean;
   function FServiceIsRunning(ServiceName_: String): Boolean;
   function FGetDBFName: String;
   function FGetDBFileSize: Int64;
   function FGetFileSize(const Fname: String): Int64;
   procedure FSetUserName(Value: String);
   procedure FSetPassword(Value: String);
   procedure FSetCharSet(Value: String);
   procedure FSetConnectionString(Value: String);
  protected
   function GetHostName: String;
  public
   constructor Create(DbConnString: String='');
   destructor Destroy; override;

   function CloseDatabase: Boolean;
   function OpenDatabase(ReconnectIfActive: Boolean=true): Boolean;

   function BackupDatabase(BackupFile: String; PostLog: Boolean=true; ForceShutDown: Boolean=true): Boolean;
   function RestoreDatabase(BackupFile: String; PostLog: Boolean=true): Boolean;
   function CheckDriveSpaceForBackup(BackupFile: String): Boolean;
   function CheckDriveSpaceForRestore(BackupFile: String): Boolean;

   property ConnectionString: String read FGetConnectionString write FSetConnectionString;
   property UserName: String read FGetUserName write FSetUserName;
   property Password: String read FGetPassword write FSetPassword;
   property CharSet: String read FGetCharSet write FSetCharSet;
   property Connected: Boolean read FGetConnected;
   property IsLocalDb: Boolean read FGetIsLocalDb;
   property IbDatabase: TIBDatabase read FIBDb write FIBDb;
   property DBFileName: String read FGetDBFName;
   property DBFileSize: Int64 read FGetDBFileSize; // -1 ��� ������
  end;


  TDBHandlerIB = class(TDBHandlerIB_DB)
  private
   FDset : TDBHandlerIB_Dset;

   function FGetDataset: TIBSQL;
   procedure FSetDataset(Value: TIBSQL);
  public
   constructor Create(DbConnString: String='');
   destructor Destroy; override;

   function ExecuteSQL(SQL: String; Comment: String=''; Commit: Boolean=true): Boolean; overload;
   function ExecuteSQL(SQL: String; var RecAffected: Integer; Comment: String=''; Commit: Boolean=true): Boolean; overload;
   function FillDataSet(SQL: String; Comment: String=''): Boolean;
   procedure CloseDataSet;

   function GetTableRecordCount(TblName, WhereCnd: String; var RecCount: Integer): Boolean;

   property Dataset: TIBSQL read FGetDataset write FSetDataset;

  end;

implementation
uses DBUtilsUnit, DateUtils, StrUtils, Math;

//**************************************************************************************************
//      TDBHandlerIB_Base
//**************************************************************************************************
constructor TDBHandlerIB_Base.Create;
begin
 inherited Create;
 FLastError := '';
 FOnError   := nil;
 FOnExecSQL := nil;
end;

//**************************************************************************************************
//     TDBHandlerIB
//**************************************************************************************************
constructor TDBHandlerIB.Create(DbConnString: String='');
begin
 FIBDb   := TIBDatabase.Create(nil);
 FIBCnf  := TIBConfigService.Create(nil);

 inherited Create(FIBDb);

 FIBDb.LoginPrompt := false;
 FIBDb.Params.Clear;
 FIBDb.Params.Values['user_name'] := 'sysdba';
 FIBDb.Params.Values['password']  := 'masterkey';
 FIBDb.Params.Values['lc_ctype']  := 'WIN1251';
 FIBDb.DefaultTransaction := FIBTrn;

 FIBCnf.LoginPrompt := false;
 FIBCnf.Params.Clear;
 FIBCnf.Params.Values['user_name'] := 'sysdba';
 FIBCnf.Params.Values['password']  := 'masterkey';

 if DbConnString <> '' then FSetConnectionString(DbConnString);
end;

destructor TDBHandlerIB.Destroy;
begin
 inherited Destroy;
 CloseDatabase;
 FIBDb.Free;
 FIBCnf.Free;
end;

function TDBHandlerIB.FGetUserName: String;
begin
 Result := FIBDb.Params.Values['user_name'];
end;

procedure TDBHandlerIB.FSetUserName(Value: String);
begin
 if FGetConnected then CloseDatabase;
 FIBDb.Params.Values['user_name']   := Value;
 FIBCnf.Params.Values['user_name']  := Value;
end;

function TDBHandlerIB.FGetPassword: String;
begin
 Result := FIBDb.Params.Values['password'];
end;

procedure TDBHandlerIB.FSetPassword(Value: String);
begin
 if FGetConnected then CloseDatabase;
 FIBDb.Params.Values['password']   := Value;
 FIBCnf.Params.Values['password']  := Value;
end;

function TDBHandlerIB.FGetCharSet: String;
begin
 Result := FIBDb.Params.Values['lc_ctype'];
end;

procedure TDBHandlerIB.FSetCharSet(Value: String);
begin
 if FGetConnected then CloseDatabase;
 FIBDb.Params.Values['lc_ctype'] := Value;
end;

function TDBHandlerIB.FGetConnectionString: String;
begin
 Result := FIBDb.DatabaseName;
end;

procedure TDBHandlerIB.FSetConnectionString(Value: String);
var DbHost: String;
    DbFile: String;

    function SubStr(Src_: String; Sep_: Char): String;
    var J : Integer;
    begin
     J := Pos(Sep_, Src_);
     if J > 0 then Result := Copy(Src_, 1, J-1);
    end;
begin
 if FGetConnected then CloseDatabase;
 FIBDb.DatabaseName   := Value;
 FIBDb.Tag            := 0; // ������ � ���������� ����

 // �������� ���� ������ � �������
 // F:\Work\InterBASE_software\Replication\SAP_Soap_BTT\Bin\EBOSAPINTERFACELOG.gdb
 // LOCALHOST:F:\Work\InterBASE_software\Replication\SAP_Soap_BTT\Bin\EBOSAPINTERFACELOG.gdb
 // \\LOCALHOST\F:\Work\InterBASE_software\Replication\SAP_Soap_BTT\Bin\EBOSAPINTERFACELOG.gdb
 // LOCALHOST@F:\Work\InterBASE_software\Replication\SAP_Soap_BTT\Bin\EBOSAPINTERFACELOG.gdb

 DbFile := Value;
 if Pos('@', Value) > 0 then
  begin
   DbHost := SubStr(Value, '@');

   FIBCnf.Protocol := SPX;
  end
 else
 if Pos('\\', Value) = 1 then
  begin
   Delete(DbFile, 1, 2);
   DbHost := SubStr(Value, '\');
   Delete(DbFile, 1, Length(DbHost)+1);

   FIBCnf.Protocol := NamedPipe;
  end
 else
  begin
   DbHost := SubStr(Value, ':');
   if (Length(DbHost) = 1)and(DbHost[1] in ['C'..'Z']) then
    begin
     DbHost := '';

     FIBCnf.Protocol := Local;
    end
   else
    begin
     FIBCnf.Protocol  := TCP;
    end;
  end;
 if DbHost <> '' then Delete(DbFile, 1, Length(DbHost)+1);

 FIBCnf.ServerName  := DbHost;

 if (SameText(DbHost, 'localhost'))or(SameText(DbHost, '127.0.0.0'))or(SameText(DbHost, GetHostName)) then
  begin
   if FileExists(DbFile) then FIBDb.Tag := 1; // ������ � ������� �� ���������
  end;

 FIBCnf.DatabaseName  := DbFile;
end;

function TDBHandlerIB.FGetIsLocalDb: Boolean;
begin
 Result := (FIBDb.Tag > 0);
end;

function TDBHandlerIB.FGetConnected: Boolean;
begin
 Result := FIBDb.Connected;
end;

function TDBHandlerIB.FGetDBFName: String;
begin
 Result := FIBCnf.DatabaseName;
end;

function TDBHandlerIB.FGetDBFileSize: Int64;
begin
 Result := -1;
 if (FGetIsLocalDb)and(FileExists(FGetDBFName)) then  Result := FGetFileSize(FGetDBFName);
end;

function TDBHandlerIB.FGetFileSize(const Fname: String): Int64;
var FD: TWin32FindData;
    FH: THandle;
begin
  Result := -1;
  FH := Windows.FindFirstFile(PChar(FName), FD);
  if FH <> INVALID_HANDLE_VALUE then
   try
    Result := FD.nFileSizeHigh;
    Result := Result shl 32;
    Result := Result + FD.nFileSizeLow;
   finally
    Windows.FindClose(FH);
   end;
end;

function TDBHandlerIB.CloseDatabase: Boolean;
var I : Integer;
begin
 Result := false;
 try
  if FIBDb.Connected then
   begin
    for I := 0 to FIBDb.TransactionCount - 1 do
     if FIBDb.Transactions[I].InTransaction then FIBDb.Transactions[I].Commit;
    FIBDb.CloseDataSets;
    FIBDb.Close;
   end;
  Result := true;
 except
  on E: Exception do LastError := '[Disconnect]'+E.Message;
 end;
end;

function TDBHandlerIB.OpenDatabase(ReconnectIfActive: Boolean=true): Boolean;
var ST : TdateTime;
begin
 Result := false;
 try
  if (FIBDb.Connected)and(ReconnectIfActive) then CloseDatabase;

  if FIBDb.DatabaseName = '' then raise EAbort.Create('Database connection string is not specified!');

  if FGetIsLocalDb then // Database is local. Perform interbase server check
   begin
    ST := Now;
    while not FCheckInterbaseServer do
     begin
      Sleep(3000);
      if SecondsBetween(ST, Now) > 40 then raise EAbort.Create('Database is local! Interbase server is not running!');
     end;
   end;

  FIBDb.Open;
  Result := FIBDb.Connected;
 except
  on E: Exception do LastError := '[OpenDatabase] Fail connect to: ['+FIBDb.DatabaseName+']:'+ E.Message;
 end;
end;

function TDBHandlerIB.FCheckInterbaseServer: Boolean;
begin
 if Win32Platform <> VER_PLATFORM_WIN32_NT then
   Result := true
 else
   Result := (FServiceIsRunning('InterBaseServer'))or(FServiceIsRunning('FirebirdServerDefaultInstance'));
end;

function TDBHandlerIB.FServiceIsRunning(ServiceName_: String): Boolean;
var SCOpen    : SC_HANDLE;
    SrvStat   : TServiceStatus;
    SCManager : SC_HANDLE;
begin
  Result := false;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
  if SCManager <> 0 then
   begin
    try
      SCOpen := OpenService(SCManager, PChar(ServiceName_), SERVICE_QUERY_STATUS);
      if SCOpen > 0 then
       begin
        if QueryServiceStatus(SCOpen, SrvStat) then
         begin
          Result := SrvStat.dwCurrentState = SERVICE_RUNNING;
         end;
        CloseServiceHandle(SCOpen);
       end;
    finally
      CloseServiceHandle(SCManager);
    end;
   end;
end;

function TDBHandlerIB.GetHostName: String;
var host_buff : PChar;
    host_len  : DWORD;
begin
  Result    := '';
  host_len  := MAX_COMPUTERNAME_LENGTH + 1;
  GetMem(host_buff, host_len);
  try
    if GetComputerName(host_buff, host_len) then  Result := StrPas(host_buff);
  finally
    FreeMem(host_buff);
  end;
end;

function TDBHandlerIB.BackupDatabase(BackupFile: String; PostLog: Boolean=true; ForceShutDown: Boolean=true): Boolean;
var IBBack : TIBBackupService;
    StrLog : TStrings;
    S      : String;
begin
 Result := false;
 StrLog := nil;
 try
  if BackupFile = '' then raise EAbort.Create('Backup file name is not specified!');
  if not FGetConnected then raise EAbort.Create('Database is not connected!');
  if (ForceShutDown)and(not CloseDatabase) then raise EAbort.Create(LastError);

  try
    if PostLog then StrLog := TStringList.Create;
    try
     if ForceShutDown then
      try
       FIBCnf.Active := true;
       FIBCnf.ShutdownDatabase(Forced, 0);
      finally
       FIBCnf.Active := false;
      end;

     IBBack := TIBBackupService.Create(nil);
     try
        IBBack.LoginPrompt  := false;
        IBBack.ServerName   := FIBCnf.ServerName;
        IBBack.Protocol     := FIBCnf.Protocol;
        IBBack.DatabaseName := FIBCnf.DatabaseName;
        IBBack.Params.Values['user_name'] := FIBCnf.Params.Values['user_name'];
        IBBack.Params.Values['password']  := FIBCnf.Params.Values['password'];

        IBBack.Options := [NoGarbageCollection];  //IgnoreChecksums, IgnoreLimbo, MetadataOnly, NoGarbageCollection, OldMetadataDesc, NonTransportable, ConvertExtTables
        IBBack.Verbose := true;
        IBBack.BackupFile.Clear;
        IBBack.BackupFile.Add(BackupFile);

        IBBack.Active := True;
        try
          IBBack.ServiceStart;
          while not IBBack.Eof do
           begin
            S := IBBack.GetNextLine;
            if StrLog <> nil then StrLog.Add(S)
           end;
          Result := true;
        finally
         IBBack.Active := false;
        end;
     finally
       IBBack.Free;

       if ForceShutDown then
        try
         FIBCnf.Active := true;
         FIBCnf.BringDatabaseOnline;
        finally
         FIBCnf.Active := false;
        end;
     end;

     if (StrLog <> nil) then StrLog.SaveToFile(ChangeFileExt(BackupFile, '.log'));

    finally
     if StrLog <> nil then FreeAndNil(StrLog);
    end;
  finally
   if not OpenDatabase then raise EAbort.Create(LastError);
  end;
 except
  on E: Exception do LastError := '[BackupDatabase] Fail:'+ E.Message;
 end;
end;

function TDBHandlerIB.RestoreDatabase(BackupFile: String; PostLog: Boolean=true): Boolean;
var IBRest : TIBRestoreService;
    IBVld  : TIBValidationService;
    StrLog : TStrings;
    S      : String;
begin
 Result := false;
 StrLog := nil;
 try
  if not FileExists(BackupFile) then raise EAbort.Create('Backup file not found! ('+BackupFile+')');
  if not FGetConnected then raise EAbort.Create('Database is not connected!');
  if not CloseDatabase then raise EAbort.Create(LastError);

  // ���������� �� ������� ����� �� ������ �����
  // ���� ������������� ����� �� �����
  // �� ������ ����� �� �������� ���� �����
  S := ChangeFileExt(FIBCnf.DatabaseName, '_restore.gdb');  // ���������� �� ������� ����� �� ������
  if FileExists(S) then SysUtils.DeleteFile(S);

  try
    if PostLog then StrLog := TStringList.Create;
    try
      try
       FIBCnf.Active := true;
       FIBCnf.ShutdownDatabase(Forced, 0);
      finally
       FIBCnf.Active := false;
      end;

     // restore DB into .tmp file
     IBRest := TIBRestoreService.Create(nil);
     try
        IBRest.LoginPrompt  := false;
        IBRest.ServerName   := FIBCnf.ServerName;
        IBRest.Protocol     := FIBCnf.Protocol;
        IBRest.Params.Values['user_name'] := FIBCnf.Params.Values['user_name'];
        IBRest.Params.Values['password']  := FIBCnf.Params.Values['password'];

        IBRest.PageSize:= 16384;
        IBRest.Options := [OneRelationAtATime, Replace];  //DeactivateIndexes, NoShadow, NoValidityCheck, OneRelationAtATime, Replace, CreateNewDB, UseAllSpace, ValidationCheck
        IBRest.Verbose := true;
        IBRest.BackupFile.Clear;
        IBRest.BackupFile.Add(BackupFile);
        IBRest.DatabaseName.Clear;
        IBRest.DatabaseName.Add(ChangeFileExt(FIBCnf.DatabaseName, '.tmp'));

        IBRest.Active := True;
        try
          IBRest.ServiceStart;
          while not IBRest.Eof do
           begin
            S := IBRest.GetNextLine;
            if StrLog <> nil then StrLog.Add(S)
           end;
          Result := true;
        finally
         IBRest.Detach;
         IBRest.Active := false;
        end;
     finally
      IBRest.Free;
     end;

     if StrLog <> nil then
      begin
       StrLog.Add('');
       StrLog.Add('Start database validation ...');
      end;

     // ��������� �� ������ -> .tmp file
     IBVld := TIBValidationService.Create(nil);
     try
      IBVld.LoginPrompt  := false;
      IBVld.ServerName   := FIBCnf.ServerName;
      IBVld.Protocol     := FIBCnf.Protocol;
      IBVld.Params.Values['user_name'] := FIBCnf.Params.Values['user_name'];
      IBVld.Params.Values['password']  := FIBCnf.Params.Values['password'];
      IBVld.DatabaseName := ChangeFileExt(FIBCnf.DatabaseName, '.tmp');

      IBVld.Options := [CheckDB, ValidateDB, ValidateFull]; // LimboTransactions, CheckDB, IgnoreChecksum, KillShadows, MendDB, SweepDB, ValidateDB, ValidateFull

      IBVld.Active := True;
      try
        IBVld.ServiceStart;
        while not IBVld.Eof do
         begin
          S := IBVld.GetNextLine;
          if StrLog <> nil then StrLog.Add(S)
         end;
        Result := true;
      finally
       IBVld.Detach;
       IBVld.Active := false;
      end;
     finally
      IBVld.Free;
     end;

     if (StrLog <> nil) then
      begin
       StrLog.Add('Database validation completed...');
       StrLog.SaveToFile(ChangeFileExt(FIBCnf.DatabaseName, '_restore.log'));
      end;

      try
       FIBCnf.Active := true;
       FIBCnf.ShutdownDatabase(Forced, 0);
      finally
       FIBCnf.Active := false;
      end;

     // ������� �� ������
     // ������� ����� �� ���������� ���� (�� ����� ��� ���� �� �����)
     S := ChangeFileExt(FIBCnf.DatabaseName, '_restore.gdb');  // ���������� �� ������� ����� �� ������ (��� �� ���)
     if FileExists(S) then SysUtils.DeleteFile(S);
     if not RenameFile(FIBCnf.DatabaseName, S) then raise EAbort.Create('Fail rename old DB: '+FIBCnf.DatabaseName+' -> '+S);

     S := ChangeFileExt(FIBCnf.DatabaseName, '.tmp');
     if not RenameFile(S, FIBCnf.DatabaseName) then raise EAbort.Create('Fail rename new DB: '+S+' -> '+FIBCnf.DatabaseName);

    finally
     if StrLog <> nil then FreeAndNil(StrLog);
    end;
  finally
   if not OpenDatabase then raise EAbort.Create(LastError);
  end;
 except
  on E: Exception do LastError := '[RestoreDatabase] Fail:'+ E.Message;
 end;
end;

function TDBHandlerIB.CheckDriveSpaceForBackup(BackupFile: String): Boolean;
var S     : String;
    SpFree: Int64;
    SpDb  : Int64;
begin
 Result := true;
 if not FGetIsLocalDb then Exit;
 S := ExtractFileDrive(BackupFile);
 if S = '' then Exit;
 SpFree := DiskFree(Ord(S[1]) - 64);
 if SpFree < 0 then Exit;
 SpDb := FGetDBFileSize;
 if SpDb < 0 then Exit;
 if SpDb * 2 > SpFree then
  begin
   Result    := false;
   LastError := 'DbFile size:'+IntToStr(SpDb)+' FreeSpace:'+IntToStr(SpFree);
  end;
end;

function TDBHandlerIB.CheckDriveSpaceForRestore(BackupFile: String): Boolean;
var S     : String;
    SpFree: Int64;
    SpBkp : Int64;
    SpDb  : Int64;
begin
 Result := true;
 if not FGetIsLocalDb then Exit;
 SpDb := FGetDBFileSize;
 if SpDb < 0 then Exit;
 SpBkp := FGetFileSize(BackupFile);
 if SpBkp < 0 then Exit;
 S := ExtractFileDrive(FGetDBFName);
 if S = '' then Exit;
 SpFree := DiskFree(Ord(S[1]) - 64);
 if SpFree < 0 then Exit;
 if Max(SpBkp * 2, SpDb * 2) > SpFree then
  begin
   Result    := false;
   LastError := 'DbFile size:'+IntToStr(SpDb)+' DbBkpFile size:'+IntToStr(SpBkp)+' FreeSpace:'+IntToStr(SpFree);
  end;
end;

//**************************************************************************************************
//    TDBHandlerIB_Dset
//**************************************************************************************************
constructor TDBHandlerIB_Dset.Create(IBDatabase: TIBDatabase);
begin
 inherited Create;
 DB_CreateIBSQL(FIBSql, FIBTrn, IBDatabase);
end;


destructor TDBHandlerIB_Dset.Destroy;
begin
 DB_DestroyIBSQL(FIBSql, FIBTrn);
 inherited Destroy;
end;

function TDBHandlerIB_Dset.ExecuteSQL(SQL: String; Comment: String=''; Commit: Boolean=true): Boolean;
var Err : String;
begin
 Result := DB_ExecuteSQL(SQL, FIBSql, Commit, Comment, Err);
 if (not Result)and(Pos('deadlock', LowerCase(Err)) > 0) then
  begin
   DB_RollbackIBSQL(FIBSql);
   Result := DB_ExecuteSQL(SQL, FIBSql, Commit, Comment, Err);
 end;
 if Result then
  begin
   if Assigned(FOnExecSQL) then FOnExecSQL(SQL);
  end
 else
  LastError := Err;
end;

function TDBHandlerIB_Dset.ExecuteSQL(SQL: String; var RecAffected: Integer; Comment: String=''; Commit: Boolean=true): Boolean;
var Err : String;
begin
 Result := DB_ExecuteSQL(SQL, FIBSql, false, Comment, Err);
 if Result then
  begin
   RecAffected := FIBSql.RowsAffected;
   if Assigned(FOnExecSQL) then FOnExecSQL(SQL);
  end
 else
  LastError := Err;
 if Commit then DB_CommitIBSQL(FIBSql);
end;

function TDBHandlerIB_Dset.FillDataSet(SQL: String; Comment: String=''): Boolean;
var Err : String;
begin
 Result := DB_ExecuteSQL(SQL, FIBSql, false, Comment, Err);
 if not Result then LastError := Err;
end;

procedure TDBHandlerIB_Dset.CloseDataSet;
begin
 if FIBSql.Open then FIBSql.Close;
 DB_CommitIBSQL(FIBSql);
end;

procedure TDBHandlerIB_Dset.StartTransaction;
begin
 if not FIBSql.Transaction.InTransaction then FIBSql.Transaction.StartTransaction;
end;

procedure TDBHandlerIB_Dset.CommitTransaction;
begin
 if FIBSql.Transaction.InTransaction then FIBSql.Transaction.Commit;
end;

procedure TDBHandlerIB_Dset.RollbackTransaction;
begin
 if FIBSql.Transaction.InTransaction then FIBSql.Transaction.Rollback;
end;

function TDBHandlerIB_Dset.GetTableRecordCount(TblName, WhereCnd: String; var RecCount: Integer): Boolean;
var SQL : String;
    Err : String;
begin
 RecCount := 0;
 try
  SQL := 'SELECT COUNT(*) "RCNT" FROM '+TblName;
  if WhereCnd <> '' then SQL := SQL + ' WHERE '+WhereCnd;

  Result := DB_ExecuteSQL(SQL, FIBSql, false, 'GetTableRecordCount', Err);

  if Result then  RecCount := FIBSql.FieldByName('RCNT').AsInteger
   else LastError := Err;
 finally
  DB_CommitIBSQL(FIBSql);
 end;
end;

function TDBHandlerIB_Dset.ExtractGenerator(GenName: String; Increment: Boolean; var GenValue: Int64): Boolean;
var Err : String;
begin
 GenValue := 0;
 Result := DB_ExtractGenerator(GenName, Increment, FIBSql, GenValue, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.ExtractGeneratorA(GenName: String; Increment: Boolean; var GenValue: Integer): Boolean;
var Gen: Int64;
begin
 Result := ExtractGenerator(GenName, Increment, Gen);
 GenValue := Gen;
end;

function TDBHandlerIB_Dset.CheckFieldExist(TableName, FieldName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_FieldExist(TableName, FieldName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckTableExist(TableName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_TableExist(TableName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckGeneratorExist(GenName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_GeneratorExist(GenName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckProcedureExist(ProcName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_ProcedureExist(ProcName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckTriggerExist(TriggerName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_TriggerExist(TriggerName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckDomainExist(DomainName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_DomainExist(DomainName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckPrimaryKeyExist(TableName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_PrimaryKeyExist(TableName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

function TDBHandlerIB_Dset.CheckForeignKeyExist(TableName, ConstraintName: String; var Exist: Boolean): Boolean;
var Err : String;
begin
 Result := DB_ForeignKeyExist(TableName, ConstraintName, FIBSql, Exist, Err);
 if not Result then LastError := Err;
end;

//**************************************************************************************************
//   TDBHandlerIB_Base
//**************************************************************************************************
function TDBHandlerIB_Base.StrToSQL(Src: String; MaxLen: Integer=0; NullIfEmpty: Boolean=false): String;
begin
 if (MaxLen > 0)and(Length(Src) > MaxLen) then Src := Copy(Src, 1, MaxLen);
 if (NullIfEmpty)and(Src = '') then Result := 'NULL'
  else Result := QuotedStr(Src);
end;

function TDBHandlerIB_Base.DateTimeToSQL(Src: TDateTime): String;
begin
 if Src = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('DD.MM.YYYY HH:NN:SS', Src));
end;

function TDBHandlerIB_Base.DateToSQL(Src: TDateTime): String;
begin
 if Src = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('DD.MM.YYYY', Src));
end;

function TDBHandlerIB_Base.TimeToSQL(Src: TDateTime): String;
begin
 if Src = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('HH:NN:SS', Src));
end;

function TDBHandlerIB_Base.BoolToSQL(Src: Boolean): String;
begin
 if Src then Result := '1'
  else Result := '0';
end;

function TDBHandlerIB_Base.IntToSQL(Src: Integer; NullForZero: Boolean=false): String;
begin
 if (Src = 0)and(NullForZero) then Result := 'NULL'
  else Result := IntToStr(Src)
end;

function TDBHandlerIB_Base.IntToSQL(Src: Int64; NullForZero: Boolean=false): String;
begin
 if (Src = 0)and(NullForZero) then Result := 'NULL'
  else Result := IntToStr(Src)
end;

function TDBHandlerIB_Base.FloatToSQL(Src: Real; NullForZero: Boolean=false): String;
begin
 DecimalSeparator := '.';
 if (Src = 0)and(NullForZero) then Result := 'NULL'
  else Result := FormatFloat('0.###', Src);
end;

function TDBHandlerIB_Base.FloatToSQLEx(Src: Real; NullForZero: Boolean=false): String;
begin
 DecimalSeparator := '.';
 if (Src = 0)and(NullForZero) then Result := 'NULL'
  else Result := FormatFloat('0.########', Src);
end;

procedure TDBHandlerIB_Base.FSetLastError(Value: String);
begin
 FLastError := Value;
 if Assigned(FOnError) then FOnError(FLastError);
end;

//    Todo

//**************************************************************************************************
//    TDBHandlerIB
//**************************************************************************************************
{constructor TDBHandlerIB.Create(DbConnString: String='');
begin
 inherited Create(DbConnString);

 FDset := TDBHandlerIB_Dset.Create(FIBDb);
end;

destructor TDBHandlerIB.Destroy;
begin
 FDset.Free;

 inherited Destroy;
end;

function TDBHandlerIB.ExecuteSQL(SQL: String; Comment: String=''; Commit: Boolean=true): Boolean;
begin

end;

function TDBHandlerIB.ExecuteSQL(SQL: String; var RecAffected: Integer; Comment: String=''; Commit: Boolean=true): Boolean;
begin

end;

function TDBHandlerIB.FillDataSet(SQL: String; Comment: String=''): Boolean;
begin

end;

procedure TDBHandlerIB.CloseDataSet;
begin

end;

function TDBHandlerIB.GetTableRecordCount(TblName, WhereCnd: String; var RecCount: Integer): Boolean;
begin

end;

function TDBHandlerIB.FGetDataset: TIBSQL;
begin

end;

procedure TDBHandlerIB.FSetDataset(Value: TIBSQL);
begin

end;    }


end.
