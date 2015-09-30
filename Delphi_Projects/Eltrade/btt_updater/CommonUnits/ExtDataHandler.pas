unit ExtDataHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit;

type
 THndr_ExtDataServer = class(TObject)
 private
  FDevice    : TRemoteDevice;
  FEskSerial : String;
  FData      : TStrings;

  function FGetDealerDBDATA_ID(DataType, LocalHost, DBConnString, CompanyData: String): Integer;
  procedure FPutDealerDBDATA_DETAIL(DbDataID, DataType: Integer; Data: String);
  procedure FPostDealerEvent(Text: String);
 public
  constructor Create(RemoteDevice: TRemoteDevice; ESKSerial: String; DataAsText: String='');
  destructor Destroy; override;

  function Execute(var ErrorMessage: String): Boolean;

  property Device: TRemoteDevice read FDevice write FDevice;
  property EskSerial: String read FEskSerial write FEskSerial;
  property Data: TStrings read FData write FData;
 end;


implementation
uses BillingConstUnit, DBInterfaceUnit, MD5;


constructor THndr_ExtDataServer.Create(RemoteDevice: TRemoteDevice; ESKSerial: String; DataAsText: String='');
begin
 inherited Create;
 FDevice    := RemoteDevice;
 FEskSerial := ESKSerial;
 FData      := TStringList.Create;
 FData.Text := DataAsText;
end;

destructor THndr_ExtDataServer.Destroy;
begin
 FData.Free;
 inherited Destroy;
end;

function THndr_ExtDataServer.Execute(var ErrorMessage: String): Boolean;
var DataType  : String;
    I, Ra     : Integer;
    S         : String;
begin
 Result    := true;
 try
  if FEskSerial = '' then raise EAbort.Create('ESK serial is missung!!!');
  DataType := FData.Values['TYPE'];

  if DataType = 'SVCAPP' then  // сервизна програма ЕЛТРЕЙД - свързана със фискализатора
    begin
      // вдига exception прии грешка
      I := FGetDealerDBDATA_ID('DE0',
                               FData.Values['LHOST'],
                               FData.Values['SVCAPPDB'],
                               FData.Values['SVCAPPOWNER']);
      S := FData.Values['SVCAPPYEAR'];
      if S <> '' then FPutDealerDBDATA_DETAIL(I, 1, S);

      S := FData.Values['SVCAPPMONTH'];
      if S <> '' then FPutDealerDBDATA_DETAIL(I, 2, S);

      S := 'UPDATE DEALERS_DBDATA SET DDB_FCONNECT = 1 WHERE DDB_ID = '+Device.DbInterface.IntToSql(I);
      if not Device.DbInterface.ExecuteSQLStatement(S, Ra) then
       Device.PostEventSystem(C_EvType_Error, 'Fail update incomming data from client: '+Device.DbInterface.LastError);
   end
  else
  if DataType = 'SVCAPP_3' then // сервизна програма ЕЛТРЕЙД - НЕ свързана със фискализатора
   begin
      // вдига exception прии грешка
      I := FGetDealerDBDATA_ID('DE1',
                               FData.Values['LHOST'],
                               FData.Values['SVCAPPDB'],
                               FData.Values['SVCAPPOWNER']);
      S := FData.Values['SVCAPPYEAR'];
      if S <> '' then FPutDealerDBDATA_DETAIL(I, 1, S);

      S := FData.Values['SVCAPPMONTH'];
      if S <> '' then FPutDealerDBDATA_DETAIL(I, 2, S);

      S := 'UPDATE DEALERS_DBDATA SET DDB_DBVERSION = '+Device.DbInterface.StrToSQL(FData.Values['FBVERSION'], 100) +
           ' WHERE DDB_ID = '+Device.DbInterface.IntToSql(I);
      if not Device.DbInterface.ExecuteSQLStatement(S, Ra) then
       Device.PostEventSystem(C_EvType_Error, 'Fail update incomming data from client: '+Device.DbInterface.LastError);
   end
  else
  if DataType = 'SVCAPP_1' then // сервизна програма Датекс
   begin
      // вдига exception прии грешка
      I := FGetDealerDBDATA_ID('DD0',
                               FData.Values['LHOST'],
                               FData.Values['SVCAPPDB'],
                               '');
      S := FData.Values['SVCAPPYEAR'];
      if S <> '' then FPutDealerDBDATA_DETAIL(I, 1, S);

      S := FData.Values['SVCAPPMONTH'];
      if S <> '' then FPutDealerDBDATA_DETAIL(I, 2, S);

      S := 'UPDATE DEALERS_DBDATA SET DDB_DBVERSION = '+Device.DbInterface.StrToSQL(FData.Values['FBVERSION'], 100) +
           ' WHERE DDB_ID = '+Device.DbInterface.IntToSql(I);
      if not Device.DbInterface.ExecuteSQLStatement(S, Ra) then
       Device.PostEventSystem(C_EvType_Error, 'Fail update incomming data from client: '+Device.DbInterface.LastError);
   end
  else
  if DataType = 'ERROR' then
   begin
    FPostDealerEvent('Error message received: '+sLineBreak+sLineBreak+FData.Text);
   end
  else
   raise EAbort.Create('Client data of unknown type has been received');

 except
  on E: Exception do
   begin
    FPostDealerEvent('Error processing client data:'+sLineBreak+
                     'ERROR: '+E.Message+sLineBreak+sLineBreak+
                     FData.Text);

    ErrorMessage := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

procedure THndr_ExtDataServer.FPostDealerEvent(Text: String);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
   SQL := 'INSERT INTO EVENTS_DEALER(ED_DATETIME, ED_MODULE, ED_SUBMODULE, ED_MESSAGETYPE, '+
          'ED_DEALEREIK, ED_ESKSERIAL, ED_REMOTEIP, ED_DATA) VALUES ('+
          DateTimeToSQL(Now)                                + ', '+ // [ED_DATETIME] datetime NOT NULL,
          StrToSQL(C_ModuleNameDev, 20)                     + ', '+ // [ED_MODULE] varchar(20)
          StrToSQL(Device.ServerName, 50)                   + ', '+ // [ED_SUBMODULE] varchar(50)
          StrToSQL('EXTDATA', 20)                           + ', '+ // [ED_MESSAGETYPE] varchar(20)
          '(SELECT DU_DEALEREIK FROM DEALERS_USERS WHERE DU_USERNAME = ' + StrToSQL(FEskSerial) + '), '+ // [ED_DEALEREIK] varchar(13)
          StrToSQL(EskSerial, 10)                           + ', '+ // [ED_ESKSERIAL] varchar(10)
          StrToSQL(Device.ConnectionInfo.RemoteIP, 20)      + ', '+ // [ED_REMOTEIP] varchar(20)
          StrToSQL(Text, 0)                                 + ')';  // [ED_DATA] varchar(max)

   if not ExecuteSQLStatement(SQL, Ra) then
    Device.PostEventSystem(C_EvType_Error, 'Fail post incomming data from client: '+Device.DbInterface.LastError);
  end;
end;

function THndr_ExtDataServer.FGetDealerDBDATA_ID(DataType, LocalHost, DBConnString, CompanyData: String): Integer;
var SQL : String;
    RA  : Integer;
begin
 Result := 0;

 DBConnString := DataType + MD5Print(MD5String(UpperCase(Trim(DBConnString))));

 with Device.DbInterface do
 try
   SQL := 'SELECT D.DDB_ID, D.DDB_DEALEREIK, DU.DU_DEALEREIK, D.DDB_COMPANYDATA '+
          'FROM DEALERS_DBDATA D '+
          'LEFT JOIN DEALERS_USERS DU ON DU.DU_USERNAME = D.DDB_ESKSERIAL '+
          'WHERE '+
          '(DDB_ESKSERIAL = '+StrToSQL(FEskSerial)+')AND'+
          '(DDB_LOCALHOST = '+StrToSQL(LocalHost)+')AND'+
          '(DDB_DBCNNSTR = '+StrToSQL(DBConnString)+')';
   if not FillDataSet(SQL) then raise EAbort.Create('Fail GetDealerDBDATA_ID '+LastError);
   if DataSet.IsEmpty then
    begin
     SQL := 'INSERT INTO DEALERS_DBDATA(DDB_DEALEREIK, DDB_ESKSERIAL, DDB_LOCALHOST, '+
            'DDB_DBCNNSTR, DDB_COMPANYDATA) VALUES ('+
            '(SELECT DU_DEALEREIK FROM DEALERS_USERS WHERE DU_USERNAME = ' + StrToSQL(FEskSerial) + '), '+  //DDB_DEALEREIK VARCHAR(13),
            StrToSQL(FEskSerial, 10)          + ', '+ //DDB_ESKSERIAL VARCHAR(10),
            StrToSQL(LocalHost, 50)           + ', '+ //DDB_LOCALHOST VARCHAR(50),
            StrToSQL(DBConnString, 250 )      + ', '+ //DDB_DBCNNSTR VARCHAR(250),
            StrToSQL(CompanyData, 1000, true) + ');'+ //DDB_COMPANYDATA VARCHAR(1000),
            'select SCOPE_IDENTITY() "DDB_ID";';

     if not FillDataSet(SQL) then raise EAbort.Create('Fail insert new DealerDBDATA_ID '+LastError);
     Result := DataSet.FieldByName('DDB_ID').AsInteger;
    end
   else
    begin
     Result := DataSet.FieldByName('DDB_ID').AsInteger;
     SQL    := 'UPDATE DEALERS_DBDATA SET DDB_LASTDATAIN = getdate()';

     if DataSet.FieldByName('DDB_DEALEREIK').AsString <> DataSet.FieldByName('DU_DEALEREIK').AsString then
      SQL := SQL + ', DDB_MODIFYDATE = getdate(), DDB_DEALEREIK = '+StrToSQL(DataSet.FieldByName('DU_DEALEREIK').AsString);

     if DataSet.FieldByName('DDB_COMPANYDATA').AsString <> CompanyData then
       SQL := SQL + ', DDB_MODIFYDATE = getdate(), DDB_COMPANYDATA = '+StrToSQL(CompanyData);

     SQL := SQL +  ' WHERE DDB_ID = '+IntToSql(Result);

     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Fail update DealerDBDATA last action '+LastError);
    end;
 finally
  CloseDataSet;
 end;
end;

procedure THndr_ExtDataServer.FPutDealerDBDATA_DETAIL(DbDataID, DataType: Integer; Data: String);
var ColCnt : Integer;
    RowCnt : Integer;
    I, J   : Integer;
    RA     : Integer;
    SQL    : String;
    Model  : Integer;
    Period : Integer;
    Count  : Integer;

    function SubStrr(var Src_: String; Sep_: Char): String;
    var kk : Integer;
    begin
     kk := Pos(Sep_, Src_);
     if kk > 0 then
      begin
       Result := Copy(Src_, 1, kk - 1);
       Delete(Src_, 1, kk);
      end
     else
      begin
       Result := Src_;
       Src_   := '';
      end;
    end;
begin
 if DbDataID = 0 then raise EAbort.Create('Internal errror! Invalid DbDataID');
 if UpperCase(Copy(Data, 1, 5)) = 'ERROR' then raise EAbort.Create('Detail data contain error message.');
 SubStrr(Data, '['); // remove leading characters if any
 ColCnt := StrToIntDef(SubStrr(Data, ','), -1);
 RowCnt := StrToIntDef(SubStrr(Data, ']'), -1);
 if (ColCnt < 0)or(RowCnt < 0) then raise EAbort.Create('Invalide data format. Col and Row count error. Data type: '+IntToStr(DataType));
 for I := 1 to RowCnt do
  begin
   Model := -1; Period := -1; Count := -1;
   for J := 1 to ColCnt do
    begin
      case J of
      1: Model  := StrToIntDef(SubStrr(Data, ','), -1);
      2: Period := StrToIntDef(SubStrr(Data, ','), -1);
      3: Count  := StrToIntDef(SubStrr(Data, ','), -1);
      else SubStrr(Data, ',');  // ако имаме в повече данни ...
      end;
    end;

   if (Model < 0)or(Period < 0)or(Count < 0) then raise EAbort.Create('Invalide data format. Row: '+IntToStr(I));

  //DDD_ID
  //DDD_DBID
  //DDD_TYPE  int      - тип 1 - годишни; 2 - месечни
  //DDD_P1    int      - година - годината - 2000
  //DDD_P2    int      - месец
  //DDD_P3    int      - модел 0-друг;  1-DT;  2-DY;  3-ED;   4-ZK;   5-OT;  6-EE;
  //DDD_P4    int      - количество

   with Device.DbInterface do
   try
    case DataType of
    1: begin
         SQL := 'UPDATE DEALERS_DBDETAIL SET '+
                'DDD_P4 = '+IntToSql(Count)+' '+
                'WHERE '+
                '(DDD_DBID = '+ IntToSql(DbDataID)+')AND'+
                '(DDD_TYPE = '+ IntToSql(DataType)+')AND'+
                '(DDD_P1 = '+   IntToSql(Period)+')AND'+
                '(DDD_P2 IS NULL)AND'+
                '(DDD_P3 = '+   IntToSQL(Model)+');'+
                'IF @@ROWCOUNT = 0 '+
                'INSERT INTO DEALERS_DBDETAIL(DDD_DBID, DDD_TYPE, '+
                'DDD_P1, DDD_P3, DDD_P4) VALUES ('+
                IntToSql(DbDataID) + ', '+ //:DDD_DBID,
                IntToSql(DataType) + ', '+ //:DDD_TYPE,
                IntToSql(Period)   + ', '+ //:DDD_P1,
                IntToSQL(Model)    + ', '+ //:DDD_P3,
                IntToSql(Count)    + ');'; //:DDD_P4
       end;
    2: begin
         SQL := 'UPDATE DEALERS_DBDETAIL SET '+
                'DDD_P4 = '+IntToSql(Count)+' '+
                'WHERE '+
                '(DDD_DBID = '+ IntToSql(DbDataID)+')AND'+
                '(DDD_TYPE = '+ IntToSQL(DataType)+')AND'+
                '(DDD_P1 = '+   IntToSql(CurrentYear-2000)+')AND'+
                '(DDD_P2 = '+   IntToSql(Period)+')AND'+
                '(DDD_P3 = '+   IntToSQL(Model)+');'+
                'IF @@ROWCOUNT = 0 '+
                'INSERT INTO DEALERS_DBDETAIL(DDD_DBID, DDD_TYPE, '+
                'DDD_P1, DDD_P2, DDD_P3, DDD_P4) VALUES ('+
                IntToSql(DbDataID)    + ', '+ //:DDD_DBID,
                IntToSql(DataType)    + ', '+ //:DDD_TYPE,
                IntToSql(CurrentYear-2000) + ', '+ //:DDD_P1,
                IntToSql(Period)      + ', '+ //:DDD_P2,
                IntToSQL(Model)       + ', '+ //:DDD_P3,
                IntToSql(Count)       + ');'; //:DDD_P4
       end;
    else raise EAbort.Create('Invalid data type: '+IntToStr(DataType));
    end;

    if not ExecuteSQLStatement(SQL, RA) then
     raise EAbort.Create('Fail update/insert DEALERS_DBDETAIL '+Device.DbInterface.LastError);
   finally
    CloseDataSet;
   end;
  end;
end;

end.
