unit DataUnit;

interface

uses
  SysUtils, Classes, Controls, DB, Forms, DBClient, IBDatabase, WinSvc,
  IBCustomDataSet, StdCtrls, RxLookup, ToolEdit, Windows,
  IBQuery, frOLEExl, frRtfExp, frexpimg, FR_E_HTML2, FR_E_HTM, FR_PTabl,
  FR_DCtrl, FR_IBXDB, FR_E_CSV, FR_E_RTF, FR_Class, FR_E_TXT, FR_RRect,
  FR_Chart, FR_BarC, FR_Shape, FR_ChBox, FR_Rich, FR_OLE, FR_Desgn, DBGrids,FiscalizeHandler;

const
 // Event codes
 EV_START     = 'START';
 EV_STOP      = 'STOP';
 EV_SYSTEM    = 'SYS';
 EV_ERROR     = 'ERR';
 EV_VALIDATE  = 'VALIDATE';
 EV_FISCREQ   = 'FISCREQ';
 EV_FISCDEV   = 'FISCDEV';
 EV_FISCCOMIT = 'FISCCMT';
 EV_DREGREQ   = 'DREGREQ';
 EV_DREGDEV   = 'DREGDEV';
 EV_DREGCOMIT = 'DREGCMT';
 EV_PRINT     = 'PRINT';
 EV_UPD       = 'UPDATE';
 EV_EDITDATA  = 'EDITDATA';
 EV_PAYMENT   = 'PAYMENT';

 C_EVENTS : array [1..15, 1..2] of String = (
          (EV_START,     'Старт на системата'),
          (EV_STOP,      'Стоп на системата'),
          (EV_SYSTEM,    'Системни събития'),
          (EV_ERROR,     'Грешки'),
          (EV_VALIDATE,  'Валидиране данни'),
          (EV_FISCREQ,   'Заявка фискализация'),
          (EV_FISCDEV,   'Фисклизация устройство'),
          (EV_FISCCOMIT, 'Потв. фискализация'),
          (EV_DREGREQ,   'Заявка де-регистрация'),
          (EV_DREGDEV,   'Де-регистрация устройство'),
          (EV_DREGCOMIT, 'Потв. де-регистрация'),
          (EV_PRINT,     'Печат документ'),
          (EV_UPD,       'Обновяване фирмуер'),
          (EV_EDITDATA,  'Редакция на данни'),
          (EV_PAYMENT,   'Подновяване абонамент СИМ')
          );

 // Operation codes (MAX 8 chars)
 OP_FiscValidate_OK  = 'FVAL';
 OP_FiscValidate_Err = 'FVALerr';
 OP_FiscRequest_OK   = 'FREQ';
 OP_FiscRequest_Err  = 'FREQerr';
 OP_FiscDevice_OK    = 'FDEV';
 OP_FiscDevice_Err   = 'FDEVerr';
 OP_FiscCommit_OK    = 'FCMT';
 OP_FiscCommit_Err   = 'FCMTerr';
 OP_DregRequest_OK   = 'DREQ';
 OP_DregRequest_Err  = 'DREQerr';
 OP_DregDevice_OK    = 'DDEV';
 OP_DregDevice_Err   = 'DDEVerr';
 OP_DregCommit_OK    = 'DCMT';
 OP_DregCommit_Err   = 'DCMTerr';

type
  TDataMod = class(TDataModule)
    dsTown: TDataSource;
    dsDistrict: TDataSource;
    dsArea: TDataSource;
    dsStreet: TDataSource;
    cdsDistrict: TClientDataSet;
    cdsTown: TClientDataSet;
    cdsArea: TClientDataSet;
    cdsStreet: TClientDataSet;
    cdsSiteTypes: TClientDataSet;
    cdsEIKTypes: TClientDataSet;
    cdsDRegReasons: TClientDataSet;
    cdsFDTypes: TClientDataSet;
    cdsPayTypes: TClientDataSet;
    cdsReturnStat: TClientDataSet;
    cdsVATValues: TClientDataSet;
    IBDatabase: TIBDatabase;
    IBTransaction: TIBTransaction;
    cdsSIMPeriods: TClientDataSet;
    IBQuery: TIBQuery;
    frReport: TfrReport;
    frDesigner: TfrDesigner;
    frOLEObject: TfrOLEObject;
    frRichObject: TfrRichObject;
    frCheckBoxObject: TfrCheckBoxObject;
    frShapeObject: TfrShapeObject;
    frBarCodeObject: TfrBarCodeObject;
    frChartObject: TfrChartObject;
    frRoundRectObject: TfrRoundRectObject;
    frTextExport: TfrTextExport;
    frRTFExport: TfrRTFExport;
    frCSVExport: TfrCSVExport;
    frIBXComponents: TfrIBXComponents;
    frDialogControls: TfrDialogControls;
    frPrintGrid: TfrPrintGrid;
    frHTMExport: TfrHTMExport;
    frHTML2Export: TfrHTML2Export;
    frBMPExport: TfrBMPExport;
    frJPEGExport: TfrJPEGExport;
    frTIFFExport: TfrTIFFExport;
    frRtfAdvExport: TfrRtfAdvExport;
    frOLEExcelExport: TfrOLEExcelExport;
    cdsOperators: TClientDataSet;
    cdsCCErrors: TClientDataSet;
    procedure frReportGetValue(const ParName: String; var ParValue: Variant);
  private
    DBConnectionID: Integer;

    procedure SendCommitOnError(Sender: TObject);
    procedure SendCommitOnDone(Sender: TObject);
//    function ServiceIsRunning(ServiceName_: String): Boolean;
//    procedure SendErrorOnFinishWork(Sender: TObject);

  public
    procedure PostException(Message_: String; ErrorCode_: Integer=0);
    procedure PostEvent(evType, evDevice, evComment, evData: String);

    function CheckDatabase: Boolean;
    function OpenDatabase: Boolean;
    function RegisterDatabase: Boolean;
    function ExtractGenerator(GenName: String; var GenValue: Integer; Increment: Boolean=true): Boolean;
    function FillDataset(SQL_: String): Boolean;
    function ExecuteSQL(SQL_: String): Boolean;
    function RecordExist(TableName: String; FieldName, FieldValue: array of String): Boolean;
    function UpdateRecord(TableName, WhereCond: String; FieldName: array of String; FieldValue: array of Variant): Boolean;
    function StrToSQL(Src: String; MaxLen: Integer=0; NullIfEmpty: Boolean=false): String;
    function DateTimeToSQL(Src: TDateTime): String;
    function DateToSQL(Src: TDateTime): String;
    procedure CloseDatabase;
    procedure CloseDataset;

    function OpenResources: Boolean;
    function GetReturnStstusMessage(Code: Integer): String;
    function ReadGlobalParam(Section, Name: String; var Value: String): Boolean;
    function WriteGlobalParam(Section, Name, Value: String): Boolean;

    procedure PrintGrid(DBGrid_: TDBGrid; Title_: String);
    function PrindDeviceSvidetelstvo(DeviceSerial: String; CreateNew: Boolean): Boolean;

    function SendErrorToServer(ErrorCode: Integer; ErrorMessage: String; DeviceType: String=''; DeviceSerial: String=''): Boolean;
    function SendCommitRequest(FDSerial: String; FReqId, NraRegId, TimeElps, ErrCode: Integer; ErrMessage: String; TestMode: Boolean): Boolean;
    procedure LoadSavedRequests;
//    procedure SendErrorToServer(ErrorCode: Integer; ErrorMessage: String; DeviceType: String=''; DeviceSerial: String='');
  end;

var
  DataMod: TDataMod;

implementation
uses Dialogs, ZLIBArchive, WinUtilsUnit, IniFiles, ConstUnit, Variants, Math,
     ServiceAppDataUnit, BillingConstUnit, ServerCommitUnit, 
     SendMessageHandler, DateUtils;

{$R *.dfm}

procedure TDataMod.PostException(Message_: String; ErrorCode_: Integer=0);
var F : TextFile;
    I : Integer;
    FN: String;
begin
 if ErrorCode_ > 0 then
  begin
   SendErrorToServer(ErrorCode_, Message_);
  end;

{$I-}
  FN := IncludeTrailingPathDelimiter(LocalPath + 'Log');
  if not DirectoryExists(FN) then CreateDir(FN);
  FN := FN + 'ErrorLog.txt';
  AssignFile(f, FN);
  Append(f);
  if IOResult <> 0 then Rewrite(f);
  Writeln(f, FormatDateTime('DD.MM.YY HH:NN:SS.zzz', Now)+' '+Message_);
  I := FileSize(F);
  CloseFile(f);
{$I+}
  if I > 600 then RenameFile(FN, ChangeFileExt(FN, FormatDateTime('DDMMYY_HHNNSS_',Now)+'.txt'));
end;

function TDataMod.WriteGlobalParam(Section, Name, Value: String): Boolean;
begin
 Result := false;
 try
   with TIniFile.Create(AppPath + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini')) do
   try
    WriteString(Section, Name, Value);
    Result := true;
   finally
    Free;
   end;
 except
 end;
end;

function TDataMod.ReadGlobalParam(Section, Name: String; var Value: String): Boolean;
var Path : String;
begin
 Result := false;
 Value  := '';
 Path  := AppPath + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
 if not FileExists(Path) then Exit;
 try
   with TIniFile.Create(Path) do
   try
    if ValueExists(Section, Name) then
     begin
      Value := ReadString(Section, Name, '');
      Result := true;
     end;
   finally
    Free;
   end;
 except
 end;
end;

//******************************************************************************
//   LOCAL DATA - NOMENCLATURES
//******************************************************************************

function TDataMod.OpenResources: Boolean;
var DbFile : TZLBArchive;
    Fname  : String;

  procedure LoadDSFromFile(ZLB_: TZLBArchive; DS_: TClientDataSet; Fname_: String);
  var Strm_ : TMemoryStream;
  begin
   if DS_.Active then DS_.Close;
   Strm_ := TMemoryStream.Create;
   try
    ZLB_.ExtractStreamByName(Strm_, Fname_);
    Strm_.Position := 0;
    DS_.LoadFromStream(Strm_);
   finally
    Strm_.Free;
   end;
  end;
begin
 try
  DbFile := TZLBArchive.create(nil);
  try
   DbFile.CheckCRC := true;
   try
    // address database
    Fname := LocalPath + 'eft_address.db';
    {
    if not FileExists(Fname) then raise Exception.Create('File does not exist: '+Fname);
    DbFile.OpenArchive(Fname);
    LoadDSFromFile(DbFile, cdsDistrict, 'eft_district');
    LoadDSFromFile(DbFile, cdsTown,     'eft_town');
    LoadDSFromFile(DbFile, cdsArea,     'eft_area');
    LoadDSFromFile(DbFile, cdsStreet,   'eft_street');
    DbFile.CloseArchive;
     }
    // system database
    Fname := LocalPath + 'eft_system.db';
    {
    if not FileExists(Fname) then raise Exception.Create('File does not exist: '+Fname);
    DbFile.OpenArchive(Fname);

    LoadDSFromFile(DbFile, cdsEIKTypes,    'eft_eiktypes');
    LoadDSFromFile(DbFile, cdsDRegReasons, 'eft_dregreasons');
    LoadDSFromFile(DbFile, cdsFDTypes,     'eft_fdtypes');
    LoadDSFromFile(DbFile, cdsPayTypes,    'eft_paytypes');
    LoadDSFromFile(DbFile, cdsReturnStat,  'eft_returnstatus');
    LoadDSFromFile(DbFile, cdsVATValues,   'eft_vatvalues');
    LoadDSFromFile(DbFile, cdsSIMPeriods,  'eft_simperiods');
    LoadDSFromFile(DbFile, cdsOperators,   'eft_operators');
    LoadDSFromFile(DbFile, cdsCCErrors,    'eft_ccerrors');
     }
    // зареждане на обектите с номер пред името им
    {
    if (Set_UseSiteTypeNumb)and(DbFile.FileInArchive('eft_sitetypes_n') >= 0) then
     LoadDSFromFile(DbFile, cdsSiteTypes,   'eft_sitetypes_n')
    else
     LoadDSFromFile(DbFile, cdsSiteTypes,   'eft_sitetypes');

    DbFile.CloseArchive;
    }
   except
    on E: Exception do
     begin
      PostException('Error loading data file ['+Fname+']:'+E.Message, errcode_OpenDatabaseFail);
      raise Exception.Create('Can not open file: '+Fname);
     end;
   end;
  finally
   DbFile.CloseArchive;
   DbFile.Free;
  end;

{  cdsTown.MasterSource    := dsDistrict;
  cdsTown.IndexFieldNames := 'T_DISTCODES';
  cdsTown.MasterFields    := 'D_CODES';

  cdsArea.MasterSource    := dsTown;
  cdsArea.IndexFieldNames := 'A_TOWNCODE';
  cdsArea.MasterFields    := 'T_CODE';

  cdsStreet.MasterSource    := dsTown;
  cdsStreet.IndexFieldNames := 'S_TOWNCODE';
  cdsStreet.MasterFields    := 'T_CODE';}

  Result := true;
 except
  on E: Exception do
   begin
    Result := false;
    PostException('Fail open data file: '+E.Message);
    MessageDlg('Неуспешно отваряне на файловете с данни.'+#13+#10+
               'Моля преинсталирайте програмата.'+#13+#10+
               ''+#13+#10+
               E.Message, mtError, [mbOK], 0);
   end;
 end;
end;

//******************************************************************************
//   DATABASE
//******************************************************************************
{function TDataMod.ServiceIsRunning(ServiceName_: String): Boolean;
var SCOpen    : SC_HANDLE;
    SrvStat   : TServiceStatus;
    SCManager : SC_HANDLE;
begin
  Result := false;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
  if SCManager <> 0 then
   try
      SCOpen := OpenService(SCManager, PChar(ServiceName_), SERVICE_QUERY_STATUS);
      if SCOpen > 0 then
       begin
        if QueryServiceStatus(SCOpen, SrvStat) then
          Result := SrvStat.dwCurrentState = SERVICE_RUNNING;

        CloseServiceHandle(SCOpen);
       end;
   finally
    CloseServiceHandle(SCManager);
   end;
end;}

function TDataMod.OpenDatabase: Boolean;
var DBStr   : String;
    UsrStr  : String;
    PassStr : String;
begin
 try
  if IBDatabase.Connected then IBDatabase.Close;

  with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'))do
  try
   if not ValueExists(C_DBIniSectionMain, C_DBIniValConn) then WriteString(C_DBIniSectionMain, C_DBIniValConn, '');
   if not ValueExists(C_DBIniSectionMain, C_DBIniValUser) then WriteString(C_DBIniSectionMain, C_DBIniValUser, '');
   if not ValueExists(C_DBIniSectionMain, C_DBIniValPass) then WriteString(C_DBIniSectionMain, C_DBIniValPass, '');

   DBStr   := ReadString(C_DBIniSectionMain, C_DBIniValConn, '');
   UsrStr  := ReadString(C_DBIniSectionMain, C_DBIniValUser, '');
   PassStr := ReadString(C_DBIniSectionMain, C_DBIniValPass, '');
  finally
   Free;
  end;

  if DBStr   = '' then
   begin
    DBStr  := LocalPath + 'EltradeFiscalizationDB.GDB';
//    if (ServiceIsRunning('FirebirdServerDefaultInstance')) then DBStr := 'localhost:'+DBStr;
   end;
  if UsrStr  = '' then UsrStr := 'SYSDBA';
  if PassStr = '' then PassStr:= 'masterkey';

  IBDatabase.DatabaseName := DBStr;
  IBDatabase.Params.Values['user_name'] := UsrStr;
  IBDatabase.Params.Values['password']  := PassStr;
  IBDatabase.Params.Values['lc_ctype']  := 'win1251';
  IBDatabase.LoginPrompt := false;

  IBDatabase.Open;

  Result := IBDatabase.Connected;
 except
  on E: Exception do
   begin
    Result := false;
    PostException('Fail open IB database ['+DBStr+']:'+ E.Message, errcode_OpenDatabaseFail);
    MessageDlg('Неуспешно отваряне на базата с данни.'+#13+#10+
               ''+#13+#10+
               DBStr+#13+#10+
               ''+#13+#10+
               E.Message, mtError, [mbOK], 0);
   end;
 end;
end;

procedure TDataMod.CloseDatabase;
begin
 try
  if IBDatabase.Connected then
   begin
    IBDatabase.CloseDataSets;
    IBDatabase.Close;
   end;
 except
  on E: Exception do
   begin
    PostException('Fail close database: '+E.Message);
   end;
 end;
end;

function TDataMod.StrToSQL(Src: String; MaxLen: Integer=0; NullIfEmpty: Boolean=false): String;
begin
 if (MaxLen > 0)and(Length(Src) > MaxLen) then Src := Copy(Src, 1, MaxLen);
 if (NullIfEmpty)and(Src = '') then Result := 'NULL'
  else Result := QuotedStr(Src);
end;

function TDataMod.DateTimeToSQL(Src: TDateTime): String;
begin
 if Src = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('DD.MM.YYYY HH:NN:SS', Src));
end;

function TDataMod.DateToSQL(Src: TDateTime): String;
begin
 if Src = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('DD.MM.YYYY', Src));
end;

procedure TDataMod.CloseDataset;
begin
  with IBQuery do
  begin
   if Active then Close;
   if Transaction.InTransaction then Transaction.Commit;
   SQL.Clear;
  end;
end;

function TDataMod.FillDataset(SQL_: String): Boolean;
begin
 try
   CloseDataset;
   IBQuery.SQL.Add(SQL_);
   IBQuery.Open;
   Result := true;
 except
  on E: Exception do
   begin
    Result := false;
    PostException('Execute SELECT SQL fail: '+E.Message+sLineBreak+SQL_, errcode_DatabaseError);
    CloseDataset;
   end;
 end;
end;

function TDataMod.ExecuteSQL(SQL_: String): Boolean;
begin
 try
  CloseDataset;
  IBQuery.SQL.Add(SQL_);
  IBQuery.Prepare;
  IBQuery.ExecSQL;
  if IBQuery.Transaction.InTransaction then IBQuery.Transaction.Commit;
  Result := true;
 except
  on E: Exception do
   begin
    Result := false;
    PostException('Execute SQL fail: '+E.Message+sLineBreak+SQL_, errcode_DatabaseError);
    if IBQuery.Active then IBQuery.Close;
    if IBQuery.Transaction.InTransaction then IBQuery.Transaction.Rollback;
   end;
 end;
end;

function TDataMod.CheckDatabase: Boolean;
begin
 try
  Result := true;
  // изтриване на всички празни обекти (без каса)
  ExecuteSQL('DELETE FROM DEVADDRESS WHERE DEVADDRESS.CUSTS_ID IN '+
             '(SELECT A.CUSTS_ID FROM DEVADDRESS A '+
             'LEFT JOIN FISCALDEVICE F ON A.CUSTS_ID = F.FD_ADDRESS '+
             'WHERE F.FD_SERIAL IS NULL)');


 except
  MessageDlg('Неуспешна проверка на базата данни!'+sLineBreak+
             'За подробности погледнете лога на грешките.', mtWarning, [mbOK], 0);
  Result := false;
 end;
end;

function TDataMod.RegisterDatabase: Boolean;
begin
 DBConnectionID := 0;
 try
   if not FillDataset('SELECT CN_ID FROM CONNECTIONS WHERE '+
                      '(CN_DB_CNNSTR = '+StrToSQL(IBDatabase.DatabaseName, 200)+')AND'+
                      '(CN_APP_PATH = '+StrToSQL(LocalPath, 200)+')AND'+
                      '(CN_APP_HOST = '+StrToSQL(CurrentHostName, 200)+')AND'+
                      '(CN_VERSION = '+StrToSQL(CurrentAppVers, 15)+')') then Abort;
   if IBQuery.IsEmpty then
    begin
     if not ExtractGenerator('GEN_CONNECTIONS_ID', DBConnectionID) then Abort;
     if not ExecuteSQL('INSERT INTO CONNECTIONS (CN_ID, CN_DB_CNNSTR, CN_APP_PATH, CN_APP_HOST, CN_VERSION)'+
                       'VALUES ('+IntToStr(DBConnectionID)+', '+
                       StrToSQL(IBDatabase.DatabaseName, 200)+', '+
                       StrToSQL(LocalPath, 200)+', '+
                       StrToSQL(CurrentHostName, 200)+', '+
                       StrToSQL(CurrentAppVers, 15)+')') then Abort;
    end
   else
    DBConnectionID := IBQuery.FieldByName('CN_ID').AsInteger;

   if DBConnectionID  <= 0 then Abort;
   Result := true;
 except
  MessageDlg('Неуспешна регистрация на базата данни!'+sLineBreak+
             'За подробности погледнете лога на грешките.', mtWarning, [mbOK], 0);
  Result := false;
 end;
end;

function TDataMod.ExtractGenerator(GenName: String; var GenValue: Integer; Increment: Boolean=true): Boolean;
begin
 try
   if Increment then
    Result := FillDataset('SELECT GEN_ID('+GenName+', 1) "GENVALUE" FROM RDB$DATABASE')
   else
    Result := FillDataset('SELECT GEN_ID('+GenName+', 0) "GENVALUE" FROM RDB$DATABASE');

   if Result then GenValue := IBQuery.fieldByname('GENVALUE').AsInteger
    else GenValue := 0;
 finally
  CloseDataset;
 end;
end;

procedure TDataMod.PostEvent(evType, evDevice, evComment, evData: String);
begin
{
 ExecuteSQL('INSERT INTO EVENTS (EV_DBCONNECTION, EV_TYPE, EV_ESK, EV_PC, EV_DEVICE, '+
            'EV_COMMENT, EV_DATA) VALUES ('+
            IntToStr(DBConnectionID)           +', '+ //:EV_DBCONNECTION,
            StrToSQL(evType, 8)                +', '+ //:EV_TYPE,
            StrToSQL(CurrentESK.ESKSerial, 10) +', '+ //:EV_ESK,
            StrToSQL(CurrentHostName, 50)      +', '+ //:EV_PC,
            StrToSQL(evDevice, 50, true)       +', '+ //:EV_DEVICE,
            StrToSQL(evComment, 500, true)     +', '+ //:EV_COMMENT,
            StrToSQL(evData, 0, true)          +')'); //:EV_DATA
            }
end;

function TDataMod.GetReturnStstusMessage(Code: Integer): String;
begin
 Result := '';
 if not cdsReturnStat.Active then Exit;
 if cdsReturnStat.Locate('ST_ID', Code, []) then Result := cdsReturnStat.FieldByName('ST_NAME').AsString;
end;

function TDataMod.RecordExist(TableName: String; FieldName, FieldValue: array of String): Boolean;
var SQL : String;
    I   : Integer;
begin
 Assert(Length(FieldName) = Length(FieldValue));
 Result := false;
 SQL := 'SELECT COUNT(*) "RCNT" FROM '+TableName+' ';
 for I := 0 to Length(FieldName) - 1 do
  begin
   if I = 0 then SQL := SQL + 'WHERE '
    else SQL := SQL + 'AND';

   if FieldValue[I] = '' then
    SQL := SQL + '('+FieldName[I]+' IS NULL)'
   else
    SQL := SQL + '('+FieldName[I]+' = '+StrToSQL(FieldValue[I])+')';
  end;

 if FillDataset(SQL) then Result := (IBQuery.FieldByName('RCNT').AsInteger > 0)
  else PostException('Check record existance fail!');
 CloseDataset;
end;

function TDataMod.UpdateRecord(TableName, WhereCond: String; FieldName: array of String; FieldValue: array of Variant): Boolean;
var SQL : String;
    I   : Integer;
begin
 Assert(Length(FieldName) = Length(FieldValue));
 SQL := 'UPDATE '+TableName+' SET ';
 for I := 0 to Length(FieldName) - 1 do
  begin
   if I > 0 then SQL := SQL + ', ';
   SQL := SQL + ' '+FieldName[I]+' = ';
   if VarIsOrdinal(FieldValue[I]) then SQL := SQL + IntToStr(FieldValue[I])
   else
   if VarIsFloat(FieldValue[I]) then SQL := SQL + FormatFloat('0.####', FieldValue[I])
   else
    SQL := SQL + StrToSQL(FieldValue[I]);
  end;
 if WhereCond <> '' then SQL := SQL + ' WHERE '+WhereCond;

 Result := ExecuteSQL(SQL);
 if not Result then PostException('Fail update table: '+TableName);
end;

//******************************************************************************************
//                             GRID functions
//******************************************************************************************

procedure TDataMod.PrintGrid(DBGrid_: TDBGrid; Title_: String);
var BM   : TBookmark;
begin
  if not DBGrid_.DataSource.DataSet.Active then Exit;
  try
   DBGrid_.DataSource.DataSet.DisableControls;
   BM := DBGrid_.DataSource.DataSet.GetBookmark;
   with frPrintGrid do
    begin
     DBGrid                  := DBGrid_;
     PageHeader.Text         := Title_;
     PageHeader.Font.Name    := Set_FontName;
     PageHeader.Font.Charset := Set_Charset;
     PageHeader.Font.Size    := 12;
     PageFooter.Text         := 'Отпечатано с : "'+Application.Title+'"  регистриран на: '+CurrentESK.CompanyName;
     PageFooter.Align        := taLeftJustify;
     PageFooter.Font.Name    := Set_FontName;
     PageFooter.Font.Charset := Set_Charset;
     PageFooter.Font.Size    := 8;
     Body.Font.Name          := Set_FontName;
     Body.Font.Charset       := Set_Charset;
     Body.Font.Size          := 8;
     Footer.Font.Name        := Set_FontName;
     Footer.Font.Charset     := Set_Charset;
     Footer.Font.Size        := 8;
     Header.Font.Name        := Set_FontName;
     Header.Font.Charset     := Set_Charset;
     Header.Font.Size        := 8;
     Summary.Font.Name       := Set_FontName;
     Summary.Font.Charset    := Set_Charset;
     Summary.Font.Size       := 8;
     ShowReport;
    end;
   DBGrid_.DataSource.DataSet.GotoBookmark(BM);
   DBGrid_.DataSource.DataSet.FreeBookmark(BM);
 except
  on E: Exception do
   begin
    MessageDlg('Грешка при печат:'+#13+#10+
               E.Message, mtError, [mbOK], 0);
    PostException('Error Print DBGrid: '+E.Message, errcode_PrintingError);
   end;
 end;
 DBGrid_.DataSource.DataSet.EnableControls;
end;

procedure TDataMod.frReportGetValue(const ParName: String; var ParValue: Variant);
begin
 if ParName = 'UserName'         then ParValue := CurrentESK.ESKSerial;
 if ParName = 'UserFullName'     then ParValue := CurrentESK.UserFullName;
 if ParName = 'UserLevel'        then ParValue := CurrentESK.UserLevel;
 if ParName = 'SvceEIK'          then ParValue := CurrentESK.CompanyEIK;
 if ParName = 'SvceName'         then ParValue := CurrentESK.CompanyName;
 if ParName = 'SvceTown'         then ParValue := CurrentESK.CompanyTown;
 if ParName = 'SvceAddress'      then ParValue := CurrentESK.CompanyAddres;
 if ParName = 'SvcePhone'        then ParValue := CurrentESK.CompanyPhone;
 if ParName = 'SvceContrNumb'    then ParValue := CurrentESK.CompanyContrN;
 if ParName = 'SvceContrDate'    then ParValue := CurrentESK.CompanyContrD;
 if ParName = 'SvceBranchName'   then ParValue := CurrentESK.BranchName;
 if ParName = 'SvceBranchTown'   then ParValue := CurrentESK.BranchTown;
 if ParName = 'SvceBranchAddress' then ParValue := CurrentESK.BranchAddres;
 if ParName = 'SvceBranchPhone'  then ParValue := CurrentESK.BranchPhone;

 if TVarData(ParValue).VType <> varEmpty then Exit;
   {
 if (Application.MainForm.ActiveMDIChild is TfrmNewFiscalization)and(frReport.Tag = 0) then
  TfrmNewFiscalization(Application.MainForm.ActiveMDIChild).ReadReportParam(ParName, ParValue)
 else
 if (Application.MainForm.ActiveMDIChild is TfrmEditFiscalization)and(frReport.Tag = 0) then
  TfrmEditFiscalization(Application.MainForm.ActiveMDIChild).ReadReportParam(ParName, ParValue)
 else
 if (Application.MainForm.ActiveMDIChild is TfrmDropFiscalization)and(frReport.Tag = 0) then
  TfrmDropFiscalization(Application.MainForm.ActiveMDIChild).ReadReportParam(ParName, ParValue)
  }
end;

function TDataMod.PrindDeviceSvidetelstvo(DeviceSerial: String; CreateNew: Boolean): Boolean;
var SQL: String;
    SvN: Integer;
begin
 // връща true при генериране на ново свидетелство
 Result := false;
 try
  if not FileExists(LocalPath+C_Template_Svidetelstvo) then
   raise EAbort.Create('Не е намерен файла шаблон:'+sLineBreak+LocalPath+C_Template_Svidetelstvo);

  SQL := 'SELECT FD_SERIAL, FD_STATUS, FD_NRAREGID, FD_REQUESTID, FD_FISCDATE, FD_SVIDNUMBER '+
         'FROM FISCALDEVICE WHERE UPPER(FD_SERIAL) = UPPER('+StrToSQL(DeviceSerial)+')';
  if not FillDataset(SQL) then raise EAbort.Create('Fail load device data [FISCALDEVICE]');
  if IBQuery.IsEmpty then raise EAbort.Create('Устройството не съществува в базата данни!');

  DeviceSerial := IBQuery.FieldByName('FD_SERIAL').AsString;

  if not CreateNew then
   begin
    SvN := IBQuery.FieldByName('FD_SVIDNUMBER').AsInteger;
    if SvN = 0 then
     begin
      if IBQuery.FieldByName('FD_NRAREGID').AsInteger > 0 then
       begin
        if MessageDlg('Избраното устройство няма'+sLineBreak+
                      'генерирани свидетелства.'+sLineBreak+
                      'Желаете ли да отпечатате'+sLineBreak+
                      'ново свидетелство?', mtConfirmation, [mbYes,mbNo], 0) = mrYes then
         begin
          CreateNew := true;
         end;
       end
      else
       raise EAbort.Create('Не е намерено издадени свидетелство'+sLineBreak+
                           'за това фискално устройство.'+sLineBreak+
                           'Устройството не е фискализирано!');
     end;
   end;

  if CreateNew then
   begin
    if IBQuery.FieldByName('FD_NRAREGID').IsNull then
     begin
      if MessageDlg('Устройството не е фискализирано!'+sLineBreak+
                    'Желаете ли да генерирате ново свидетелство?', mtConfirmation, [mbYes,mbNo], 0) = mrNo then
       raise EAbort.Create('');
     end;

    if not DataMod.ExtractGenerator('GEN_DEVSVIDLOG_ID', SvN) then raise EAbort.Create('Fail extract GEN_DEVSVIDLOG_ID');

    SQL := 'INSERT INTO DEVSVIDLOG (FDS_ID, FDS_SERIAL, FDS_MODEL_ID, FDS_MODEL_M, FDS_MODEL_SVN, '+
           'FDS_MODEL_SVD, FDS_MODEL_SVM, FDS_MFM, FDS_GPRS, FDS_SIMIMSI, FDS_CUST_EIKT, FDS_CUST_EIK, '+
           'FDS_CUST_NAME, FDS_CUST_TOWN, FDS_CUST_ADDR, FDS_CUST_MOL, FDS_ADDR_ID, FDS_ADDR_TOWNP, FDS_ADDR_TOWN, '+
           'FDS_ADDR_STREETP, FDS_ADDR_STREET, FDS_ADDR_ADDR, FDS_SITE_NAME, FDS_SITE_TYPE, '+
           'FDS_INSERTESK, FDS_CONTRACTNUMB, FDS_CONTRACTFROM, FDS_CONTRACTTO, FDS_COMMENT, FDS_NRAREGID, '+
           'FDS_REQUESTID, FDS_FISCDATE) '+
           'SELECT '+IntToStr(SvN)+',F.FD_SERIAL, F.FD_MODEL, M.FDM_NAME, M.FDM_SVIDNUMBER, '+
           'M.FDM_SVIDDATE, M.FDM_SWIDMODEL, F.FD_MFM, F.FD_GPRS, F.FD_SIMIMSI, C.CUST_EIKTYPEN, F.FD_CUSTOMER, '+
           'C.CUST_NAME, C.CUST_TOWN, C.CUST_SHORTADDRESS, C.CUST_MOL, F.FD_ADDRESS, A.CUSTS_TOWNPREF, A.CUSTS_TOWNNAME, '+
           'A.CUSTS_STREETPREF, A.CUSTS_STREETNAME, '+
           'COALESCE('' No.''||A.CUSTS_STREETNO, '''')||COALESCE('' бл.''||A.CUSTS_BLOCK, '''')||COALESCE('' вх.''||A.CUSTS_ENTRANCE, '''')||COALESCE('' ет.''||A.CUSTS_FLOOR, '''')||COALESCE('' ап.''||A.CUSTS_APARTMENT, '''') AS "SHADDR" ,'+
           'A.CUSTS_NAME, A.CUSTS_TYPENAME, '+
           'F.FD_INSERTESK, F.FD_CONTRACTNUMB, F.FD_CONTRACTFROM, F.FD_CONTRACTTO, F.FD_COMMENT, F.FD_NRAREGID, '+
           'F.FD_REQUESTID, F.FD_FISCDATE '+
           'FROM FISCALDEVICE F '+
           'LEFT JOIN DEVMODELS M ON F.FD_MODEL = M.FDM_ID '+
           'LEFT JOIN CUSTOMERS C ON F.FD_CUSTOMER = C.CUST_EIK '+
           'LEFT JOIN DEVADDRESS A ON F.FD_ADDRESS = A.CUSTS_ID '+
           'WHERE F.FD_SERIAL = '+StrToSQL(DeviceSerial);
    if not ExecuteSQL(SQL) then raise EAbort.Create('Fail generate sviddocument');

    if not UpdateRecord('FISCALDEVICE', 'FD_SERIAL = '+StrToSQL(DeviceSerial),
                        ['FD_SVIDNUMBER', 'FD_SVIDDATE'], [SvN, FormatDateTime('DD.MM.YYYY', Now)]) then
     raise EAbort.Create('Update FISCALDEVICE fail');

    Result := true;
   end;

  // запис на данните в сервизната програма
  with ServiceAppDataMod do
  try
   if SAppDB_OpenDatabase then
    begin
     if not SAppDB_SaveEcrData(DeviceSerial) then Abort;
     if not SAppDB_SaveSvidData(DeviceSerial, 'FS'+FormatFloat('00000000', SvN)) then Abort;
     SAppDB_CloseDatabase;
    end;
  except
   SAppDB_CloseDatabase;
   MessageDlg('Неуспешен запис в Сервизната програма!'+sLineBreak+
              'За подробности погледнете лог файла.', mtWarning, [mbOK], 0);
  end;


  SQL := 'SELECT * FROM DEVSVIDLOG WHERE FDS_ID = '+IntToStr(SvN);
  if not FillDataset(SQL) then raise EAbort.Create('Fail load DEVSVIDLOG');

  if IBQuery.RecordCount = 0 then
   raise EAbort.Create('Не е намерено издадени свидетелство'+sLineBreak+
                       'за това фискално устройство.');

  frReport.Tag := 1; // печат от датасет; не обхожда формите
  frReport.LoadFromFile(LocalPath+C_Template_Svidetelstvo);
  if GetKeyState(VK_LCONTROL) < 0 then DataMod.frReport.DesignReport
   else DataMod.frReport.ShowReport;

  PostEvent(EV_PRINT, DeviceSerial, 'Отпечатано свидетелство', 'Номер: '+IntToStr(SvN));
 except
  on E: Exception do
   begin
    PostException('Fail print svidetelstvo: '+E.Message, errcode_PrintingError);
    PostEvent(EV_ERROR, DeviceSerial, 'Грешка при печат на свидетелство', E.Message);
    MessageDlg('Грешка при печат на свидетелство!'+sLineBreak+
               ''+sLineBreak+
               E.Message, mtWarning, [mbOK], 0);
   end;
 end;
 CloseDataset;
end;

//******************************************************************************
// SERVER COMMUNICATOPN
//******************************************************************************

procedure TDataMod.SendCommitOnError(Sender: TObject);
var IniF : TIniFile;
    Cnt  : Integer;
begin
{
 if (Sender is TSrvCommitThread) then
  with TSrvCommitThread(Sender) do
   begin
    if Command is TCmd_FiscCommitClient then
     begin
      DataMod.PostException('Commit fiscalization FAIL: '+LastError);
      DataMod.PostEvent(EV_ERROR, TCmd_FiscCommitClient(Command).DeviceSerial, 'Неуспешно изпращане на потвърждаване!', LastError);
     end;

    if SectionId <> '' then
     begin
      IniF := TIniFile.Create(AppPath + C_FilenameFailedRequest);
      try
       Cnt := IniF.ReadInteger('RetryCount', SectionId, 0);
       Inc(Cnt);
       IniF.WriteInteger('RetryCount', SectionId, Cnt);
       if Cnt > 50 then
        begin
         IniF.EraseSection(SectionId);
         IniF.DeleteKey('RetryCount', SectionId);
        end;
      finally
       IniF.Free;
      end;
     end;
   end;
   }
end;

procedure TDataMod.SendCommitOnDone(Sender: TObject);
var IniF : TIniFile;
begin
 if (Sender is TSrvCommitThread) then
  with TSrvCommitThread(Sender) do
   begin
    {
    if Command is TCmd_FiscCommitClient then
     begin
      DataMod.PostEvent(EV_FISCCOMIT, TCmd_FiscCommitClient(Command).DeviceSerial, 'Изпратено потвърждение!',
                        'Заявка: '+IntToStr(TCmd_FiscCommitClient(Command).FiscRequestID)+sLineBreak+
                        ''+sLineBreak+
                        '['+IntToStr(TCmd_FiscCommitClient(Command).ErrorCode)+']'+TCmd_FiscCommitClient(Command).ErrorMessage);
     end
    else
    }
    if Command is TCmd_PCErrorLogClient then
     begin

     end;

    // remove request from file
    if SectionId <> '' then
     begin
      IniF := TIniFile.Create(AppPath + C_FilenameFailedRequest);
      try
       IniF.EraseSection(SectionId);
       IniF.DeleteKey('RetryCount', SectionId);
      finally
       IniF.Free;
      end;
     end;
   end;
end;

function TDataMod.SendErrorToServer(ErrorCode: Integer; ErrorMessage: String; DeviceType: String=''; DeviceSerial: String=''): Boolean;
var CmdError : TCmd_PCErrorLogClient;
    IniFile  : TIniFile;
    Section  : String;
begin
 Result := false;
 if CurrentESK.ESKSerial = '' then Exit;
 try
   if (DeviceType = '')and(DeviceSerial = '') then
    begin
     DeviceType   := C_DeviceType_PC;
     DeviceSerial := Win_GetHostName;
    end;

   // post request to file
   Section := 'ErrMsg'+IntToStr(GetTickCount);
   IniFile := TIniFile.Create(AppPath + C_FilenameFailedRequest);
   try
    IniFile.WriteInteger(Section, 'ErrorCode',    ErrorCode);
    IniFile.WriteString (Section, 'ErrorMessage', RemoveLFCR(ErrorMessage));
    IniFile.WriteString (Section, 'DeviceType',   RemoveLFCR(DeviceType));
    IniFile.WriteString (Section, 'DeviceSerial', RemoveLFCR(DeviceSerial));
    IniFile.WriteFloat  (Section, 'PostTime',     Now);
   finally
    IniFile.Free;
   end;

   // start send thread
   with TSrvCommitThread.Create(false, CurrentESK.ESKSerial, CurrentAppVers, Section) do
    begin
     CmdError := TCmd_PCErrorLogClient(AddCommand(TCmd_PCErrorLogClient));
     CmdError.EskSerial  := CurrentESK.ESKSerial;
     CmdError.DevType    := DeviceType;
     CmdError.DevSerial  := DeviceSerial;
     CmdError.ErrorCode  := ErrorCode;
     CmdError.ErrorData.Text := ErrorMessage;

     OnCommitError := SendCommitOnError;
     OnCommitDone  := SendCommitOnDone;
     Start;
    end;
   Result := true;
 except
  on E: Exception do
   begin
    Result := false;
    DataMod.PostException('Start commit thread FAIL: '+E.Message);
   end;
 end;
end;

function TDataMod.SendCommitRequest(FDSerial: String; FReqId, NraRegId, TimeElps, ErrCode: Integer;
                                        ErrMessage: String; TestMode: Boolean): Boolean;
var CmdFiscCmt : TCmd_FiscCommitClient;
    IniFile    : TIniFile;
    Section    : String;
begin
 Result := true;
 try
   // post request to file
   Section := 'FReqID'+IntToStr(GetTickCount);
   IniFile := TIniFile.Create(AppPath + C_FilenameFailedRequest);
   try
    IniFile.WriteString (Section, 'FDSerial',   RemoveLFCR(FDSerial));
    IniFile.WriteInteger(Section, 'FiscReqId',  FReqId);
    IniFile.WriteString (Section, 'ESKSerial',  RemoveLFCR(CurrentESK.ESKSerial));
    IniFile.WriteInteger(Section, 'NRARegId',   NraRegId);
    IniFile.WriteInteger(Section, 'ErrCode',    ErrCode);
    IniFile.WriteString (Section, 'ErrMessage', RemoveLFCR(ErrMessage));
    IniFile.WriteInteger(Section, 'TimeElps',   TimeElps);
    IniFile.WriteBool   (Section, 'TestMode',   TestMode);
    IniFile.WriteFloat  (Section, 'PostTime',   Now);
   finally
    IniFile.Free;
   end;

   // start send thread
   with TSrvCommitThread.Create(TestMode, CurrentESK.ESKSerial, CurrentAppVers, Section) do
    begin
     CmdFiscCmt := TCmd_FiscCommitClient(AddCommand(TCmd_FiscCommitClient));
     CmdFiscCmt.DeviceSerial   := FDSerial;
     CmdFiscCmt.FiscRequestID  := FReqId;
     CmdFiscCmt.EskSerial      := CurrentESK.ESKSerial;
     CmdFiscCmt.RegistrationID := NraRegId;
     CmdFiscCmt.ErrorCode      := ErrCode;
     CmdFiscCmt.ErrorMessage   := ErrMessage;
     CmdFiscCmt.TimeElapsedMsec:= TimeElps;
     CmdFiscCmt.TestMode       := TestMode;

     OnCommitError := SendCommitOnError;
     OnCommitDone  := SendCommitOnDone;
     Start;
    end;
 except
  on E: Exception do
   begin
    Result := false;
    DataMod.PostException('Start commit thread FAIL: '+E.Message);
    SendErrorToServer(errcode_CommitOperationFail, 'Start commit thread FAIL: '+E.Message, C_DeviceType_FD, FDSerial);
   end;
 end;
end;

procedure TDataMod.LoadSavedRequests;
var IniFile   : TIniFile;
    Secs      : TStrings;
    S         : String;
    CmdFiscCmt: TCmd_FiscCommitClient;
    CmdError  : TCmd_PCErrorLogClient;
begin
 if not FileExists(AppPath + C_FilenameFailedRequest) then Exit;

 IniFile := TIniFile.Create(AppPath + C_FilenameFailedRequest);
 Secs    := TStringList.Create;
 try
  IniFile.ReadSections(Secs);
  while Secs.Count > 0 do
   begin
    S := Secs.Strings[0];
    Secs.Delete(0);
    if Copy(S, 1, 6) = 'FReqID' then
     begin
       if DaysBetween(IniFile.ReadFloat(S, 'PostTime', 0), Now) > 5 then Continue;

       with TSrvCommitThread.Create(IniFile.ReadBool(S, 'TestMode', false), CurrentESK.ESKSerial, CurrentAppVers, S) do
       try
         CmdFiscCmt := TCmd_FiscCommitClient(AddCommand(TCmd_FiscCommitClient));
         CmdFiscCmt.DeviceSerial   := AddLFCR(IniFile.ReadString (S, 'FDSerial',   ''));
         CmdFiscCmt.FiscRequestID  :=         IniFile.ReadInteger(S, 'FiscReqId',  0);
         CmdFiscCmt.EskSerial      := AddLFCR(IniFile.ReadString (S, 'ESKSerial',  ''));
         CmdFiscCmt.RegistrationID :=         IniFile.ReadInteger(S, 'NRARegId',   0);
         CmdFiscCmt.ErrorCode      :=         IniFile.ReadInteger(S, 'ErrCode',    0);
         CmdFiscCmt.ErrorMessage   := AddLFCR(IniFile.ReadString (S, 'ErrMessage', ''));
         CmdFiscCmt.TimeElapsedMsec:=         IniFile.ReadInteger(S, 'TimeElps',   0);
         CmdFiscCmt.TestMode       :=         IniFile.ReadBool   (S, 'TestMode',   true);

         if CmdFiscCmt.DeviceSerial   = '' then Abort;
         if CmdFiscCmt.FiscRequestID  = 0  then Abort;
         if CmdFiscCmt.EskSerial      = '' then Abort;

         OnCommitError := SendCommitOnError;
         OnCommitDone  := SendCommitOnDone;
         Start;
       except
        FreeOnTerminate := false;       // иначе гърми при free
        Free;
       end;
     end
    else
    if Copy(S, 1, 6) = 'ErrMsg' then
     begin
       with TSrvCommitThread.Create(false, CurrentESK.ESKSerial, CurrentAppVers, S) do
       try
         CmdError := TCmd_PCErrorLogClient(AddCommand(TCmd_PCErrorLogClient));
         CmdError.EskSerial  := CurrentESK.ESKSerial;
         CmdError.DevType    := IniFile.ReadString (S, 'DeviceType',   '');
         CmdError.DevSerial  := IniFile.ReadString (S, 'DeviceSerial', '');
         CmdError.ErrorCode  := IniFile.ReadInteger(S, 'ErrorCode',    0);
         CmdError.ErrorData.Text := AddLFCR(IniFile.ReadString(S, 'ErrorMessage', ''));

         OnCommitError := SendCommitOnError;
         OnCommitDone  := SendCommitOnDone;
         Start;
       except
        FreeOnTerminate := false;       // иначе гърми при free
        Free;
       end;
     end;
   end;
 finally
  IniFile.Free;
  Secs.Free;
 end;
end;


end.
