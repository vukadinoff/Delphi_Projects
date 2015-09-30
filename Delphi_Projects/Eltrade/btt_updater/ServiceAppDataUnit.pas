unit ServiceAppDataUnit;

interface

uses
  SysUtils, Classes, DB, IBDatabase, IniFiles, Forms, ConstUnit, Dialogs,
  IBCustomDataSet, IBQuery, Variants;

type
  TServiceAppDataMod = class(TDataModule)
    IBDatabase: TIBDatabase;
    IBTr: TIBTransaction;
    IBQry: TIBQuery;
  private
    procedure CloseDataset;
    function ExecuteSQL(SQL_: String; Commit_: Boolean = True): Boolean;
    function RecordExist(TableName: String; FieldName, FieldValue: array of String): Boolean;
    function FillDataset(SQL_: String): Boolean;
    function ExtractGenerator(GenName: String; var GenValue: Integer; Increment: Boolean = True): Boolean;
    function StrToSQL(Src: String; MaxLen: Integer = 0; NullIfEmpty: Boolean = False): String;
    function DateToSQL(Src: TDateTime): String;

    procedure UpdateServiceTechData(var ServID, TechID: Integer);
    procedure UpdateCustomerData(CustEIK: String; var CustID_SApp: Integer);
    procedure UpdateObjectData(SiteID, CustID_SApp: Integer; var ObjID_SApp: Integer);
    procedure UpdateModelData(ModelID: Integer; var ModelID_SApp: Integer);

    function UpdateRecord(TableName, WhereCond: String; FieldName: array of String; FieldValue: array of Variant): Boolean;
    function GetDatasetAsText(SQL_: String; var Data: String): Boolean;
    { Private declarations }
  public
    function SAppDB_OpenDatabase: Boolean;
    procedure SAppDB_CloseDatabase;
    function SAppDB_SaveEcrData(FDSerial: String): Boolean;
    function SAppDB_SaveSvidData(FDSerial, SvN: String): Boolean;
    function SAppDB_GenerateContractNumb(FDSerial: String): String;
    function SAppDB_CommitContractNumb(FDSerial: String): Boolean;
    function SAppDB_SaveContractData(FDSerial, CNumber, CComment: String; StartDate, EndDate: TDateTime): Boolean;
    function SAppDB_DeregisterECR(FDSerial: String): Boolean;
    function SAppDB_GetCompanyData(var Data: String): Boolean;
    function SAppDB_GetDevCntMonths(Year: Word; var Data: String): Boolean;
    function SAppDB_GetDevCntYears(var Data: String): Boolean;
    function SAppDB_FindCustomer(CustEIK: String): Boolean;
    { Public declarations }
  end;

var
  ServiceAppDataMod: TServiceAppDataMod;

implementation
uses DataUnit, DateUtils, BillingConstUnit;
{$R *.dfm}

function TServiceAppDataMod.SAppDB_OpenDatabase: Boolean;
var DBStr   : String;
    UsrStr  : String;
    PassStr : String;
begin
 Result := false;
 try
  if IBDatabase.Connected then IBDatabase.Close;

  with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'))do
  try
   if not ValueExists(C_DBIniSectionSvceApp, C_DBIniValConn) then WriteString(C_DBIniSectionSvceApp, C_DBIniValConn, '');
   if not ValueExists(C_DBIniSectionSvceApp, C_DBIniValUser) then WriteString(C_DBIniSectionSvceApp, C_DBIniValUser, '');
   if not ValueExists(C_DBIniSectionSvceApp, C_DBIniValPass) then WriteString(C_DBIniSectionSvceApp, C_DBIniValPass, '');

   DBStr   := ReadString(C_DBIniSectionSvceApp, C_DBIniValConn, '');
   UsrStr  := ReadString(C_DBIniSectionSvceApp, C_DBIniValUser, '');
   PassStr := ReadString(C_DBIniSectionSvceApp, C_DBIniValPass, '');
  finally
   Free;
  end;

  if DBStr <> '' then
   begin
    if UsrStr  = '' then UsrStr := 'eltrade';
    if PassStr = '' then PassStr:= 'gdelchev';

    IBDatabase.DatabaseName := DBStr;
    IBDatabase.Params.Values['user_name'] := UsrStr;
    IBDatabase.Params.Values['password']  := PassStr;
    IBDatabase.Params.Values['lc_ctype']  := 'win1251';
    IBDatabase.LoginPrompt := false;

    IBDatabase.Open;

    Result := IBDatabase.Connected;
   end;
 except
  on E: Exception do
   begin
    Result := false;
    DataMod.PostException('Fail open service database ['+DBStr+']:'+ E.Message);
    MessageDlg('Неуспешно отваряне на сервизната базата с данни.'+#13+#10+
               ''+#13+#10+
               DBStr+#13+#10+
               ''+#13+#10+
               E.Message, mtError, [mbOK], 0);
   end;
 end;
end;

procedure TServiceAppDataMod.SAppDB_CloseDatabase;
begin
 try
  if IBDatabase.Connected then
   begin
    IBDatabase.CloseDataSets;
    IBDatabase.Close;
   end;
 except
  on E: Exception do
   DataMod.PostException('Fail to close database: '+E.Message);
 end;
end;

procedure TServiceAppDataMod.CloseDataset;
begin
 if IBQry.Active then IBQry.Close;
 if IBQry.Transaction.InTransaction then IBQry.Transaction.Commit;
 IBQry.SQL.Clear;
end;

function TServiceAppDataMod.ExecuteSQL(SQL_: String; Commit_: Boolean = True): Boolean;
begin
 try
  if Commit_ then CloseDataset;
  if not IBQry.Transaction.InTransaction then IBQry.Transaction.StartTransaction;
  IBQry.SQL.Text := SQL_;
  IBQry.Prepare;
  IBQry.ExecSQL;
  if (Commit_)and(IBQry.Transaction.InTransaction ) then IBQry.Transaction.Commit;
  Result := True;
 except
  on E: Exception do
   begin
    Result := False;
    if IBQry.Transaction.InTransaction then IBQry.Transaction.Rollback;
    DataMod.PostException('Execute SP SQL fail: '+E.Message+sLineBreak+SQL_, errcode_DatabaseError);
   end;
 end;
end;

function TServiceAppDataMod.RecordExist(TableName: String; FieldName, FieldValue: array of String): Boolean;
var SQL_: String;
    I: Integer;
begin
 Assert(Length(FieldName) = Length(FieldValue));
 Result := False;
 SQL_ := 'SELECT COUNT(*) "RCNT" FROM '+TableName;
 for I := 0 to Length(FieldName)-1 do
  begin
   if I = 0 then SQL_ := SQL_ + ' WHERE '
    else SQL_ := SQL_ + ' AND ';
   SQL_ := SQL_ + ' ('+FieldName[I]+' = '+DataMod.StrToSQL(FieldValue[I])+')';
  end;

 if FillDataset(SQL_) then Result := (IBQry.FieldByName('RCNT').AsInteger > 0)
  else DataMod.PostException('Check record existence failed!');
 CloseDataset;
end;

function TServiceAppDataMod.UpdateRecord(TableName, WhereCond: String;
                                         FieldName: array of String; FieldValue: array of Variant): Boolean;
var SQL: String;
    I: Integer;
begin
 Assert(Length(FieldName) = Length(FieldValue));
 SQL := 'UPDATE '+TableName+' SET ';
 for I := 0 to Length(FieldName)-1 do
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
 if not Result then DataMod.PostException('Fail update table: '+TableName);
end;

function TServiceAppDataMod.FillDataset(SQL_: String): Boolean;
begin
 try
  CloseDataset;
  IBQry.Transaction.StartTransaction;
  IBQry.SQL.Add(SQL_);
  IBQry.Open;
  Result := True;
 except
  on E: Exception do
   begin
    Result := False;
    DataMod.PostException('Open SP query fail: '+E.Message+sLineBreak+SQL_, errcode_DatabaseError);
    CloseDataset;
   end;
 end;
end;

function TServiceAppDataMod.GetDatasetAsText(SQL_: String; var Data: String): Boolean;
var I : Integer;
    function CheckStr_(Src_: String): String;
    var Cp: Integer;
    begin
     Result := Src_;
     Cp := Pos(',', Result);
     while Cp > 0 do
      begin
       Delete(Result, Cp, 1);
       Cp := Pos(',', Result);
      end;
    end;
begin
 Data := '';
 try
  CloseDataset;
  IBQry.Transaction.StartTransaction;
  IBQry.SQL.Add(SQL_);
  IBQry.Open;
  IBQry.Last;
  IBQry.First;
  Data := '['+IntToStr(IBQry.FieldCount)+','+IntToStr(IBQry.RecordCount)+']';
  while not IBQry.Eof do
   begin
    for I := 0 to IBQry.FieldCount - 1 do Data := Data + CheckStr_(IBQry.Fields[I].AsString)+',';
    IBQry.Next;
   end;
  CloseDataset;
  Result := True;
 except
  on E: Exception do
   begin
    Data   := E.Message;
    Result := False;
    CloseDataset;
   end;
 end;
end;

function TServiceAppDataMod.ExtractGenerator(GenName: String; var GenValue: Integer;
 Increment: Boolean = True): Boolean;
begin
 try
  if Increment then
   Result := FillDataset('SELECT GEN_ID('+GenName+', 1) "GENVALUE" FROM RDB$DATABASE')
  else
   Result := FillDataset('SELECT GEN_ID('+GenName+', 0) "GENVALUE" FROM RDB$DATABASE');

  if Result then GenValue := IBQry.FieldByName('GENVALUE').AsInteger
   else GenValue := 0;
 finally
  CloseDataset;
 end;
end;

function TServiceAppDataMod.StrToSQL(Src: String; MaxLen: Integer = 0; NullIfEmpty: Boolean = False): String;
begin
 if (MaxLen > 0) and (Length(Src) > MaxLen) then Src := Copy(Src, 1, MaxLen);
 if (NullIfEmpty) and (Src = '') then Result := 'NULL'
  else Result := QuotedStr(Src);
end;

function TServiceAppDataMod.DateToSQL(Src: TDateTime): String;
begin
 if Src = 0 then Result := 'NULL'
  else Result := QuotedStr(FormatDateTime('DD.MM.YYYY', Src));
end;

procedure TServiceAppDataMod.UpdateCustomerData(CustEIK: string; var CustID_SApp: Integer);
var SQL : String;

    procedure AddToUpdateSQL(var SQL_: String; FldNameS, FldNameF: String; FldSizeS: Integer);
    begin
     if (IBQry.FieldByName(FldNameS).AsString = '')and
        (DataMod.IBQuery.FieldByName(FldNameF).AsString <> '') then
      SQL_ := SQL_ + ','+FldNameS+' = ' + StrToSQL(DataMod.IBQuery.FieldByName(FldNameF).AsString, FldSizeS);
    end;
begin
 CustID_SApp := 0;
 try
   if CustEIK = '' then raise EAbort.Create('Не е зададен ЕИК на клиент');

   if not DataMod.FillDataset('SELECT * FROM CUSTOMERS WHERE CUST_EIK = '+QuotedStr(CustEIK)) then
    raise EAbort.Create('Грешка при четене данните за клиента [БДФ]');

   if not DataMod.IBQuery.IsEmpty then
    begin
     case (DataMod.IBQuery.FieldByName('CUST_EIKTYPE').AsInteger) of
     0:
      begin // bulstat (company)
       // search customer in ServiceApplication database
       if not FillDataset('SELECT * FROM CUSTOMERS WHERE CUST_BULSTAT = '+QuotedStr(CustEIK)) then
        raise EAbort.Create('Грешка при търсене на клиент [БДС]');

       if not IBQry.IsEmpty then CustID_SApp := IBQry.FieldByName('CUST_ID').AsInteger;

       if CustID_SApp > 0 then
        begin
         SQL := 'UPDATE CUSTOMERS SET'+
                ' CUST_BULSTAT = ' + StrToSQL(DataMod.IBQuery.FieldByName('CUST_EIK').AsString, 13);

         AddToUpdateSQL(SQL, 'CUST_NAME',        'CUST_NAME',         50);
         AddToUpdateSQL(SQL, 'CUST_TOWN',        'CUST_TOWN',         50);
         AddToUpdateSQL(SQL, 'CUST_STREET',      'CUST_SHORTADDRESS', 50);
         AddToUpdateSQL(SQL, 'CUST_MANAGERNAME', 'CUST_MOL',          50);
         AddToUpdateSQL(SQL, 'CUST_PHONE',       'CUST_PHONE_G1',     50);
         SQL := SQL + ' WHERE CUST_ID = '+   IntToStr(CustID_SApp);

         if not ExecuteSQL(SQL) then raise EAbort.Create('Грешка при редактиране данни за клиент [БДС]');
        end
       else
        begin
         if not ExtractGenerator('GEN_CUSTOMERS_ID', CustID_SApp) then
          raise EAbort.Create('Грешка при генериране ID клиент [БДС]');

         if not ExecuteSQL('INSERT INTO CUSTOMERS (CUST_ID, CUST_TAXNUMB, CUST_BULSTAT, CUST_ISFIRM_,'+
                           ' CUST_NAME, CUST_TOWN, CUST_STREET, CUST_MANAGERNAME, CUST_TDD_NAME,'+
                           ' CUST_PHONE, CUST_COMMENT) VALUES ('+
                           IntToStr(CustID_SApp)                                                  +','+//CUST_ID
                           StrToSQL('', 10)                                                       +','+//CUST_TAXNUMB
                           StrToSQL(Copy(DataMod.IBQuery.FieldByName('CUST_EIK').AsString,1,13))  +','+//CUST_BULSTAT
                           StrToSQL('T', 1)                                                       +','+//CUST_ISFIRM_
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_NAME').AsString, 50)        +','+//CUST_NAME
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_TOWN').AsString, 50)        +','+//CUST_TOWN
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_SHORTADDRESS').AsString, 50)+','+//CUST_STREET
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_MOL').AsString, 50)         +','+//CUST_MANAGERNAME
                           StrToSQL('ТДД '+DataMod.IBQuery.FieldByName('CUST_TOWN').AsString, 50) +','+//CUST_TDD_NAME
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_PHONE_G1').AsString, 50)    +','+//CUST_PHONE
                           StrToSQL('Авт. въведени данни [ФС]', 100)                              +')' //CUST_COMMENT
                           ) then
          raise EAbort.Create('Грешка при въвеждане на нов клиент [БДС]');
        end;
      end;
     1,2:
      begin // EGN (person)
       // search customer in ServiceApplication database
       if not FillDataset('SELECT * FROM CUSTOMERS WHERE CUST_TAXNUMB = '+StrToSQL(CustEIK)) then
        raise EAbort.Create('Грешка при търсене на клиент [БДС]');

       if not IBQry.IsEmpty then CustID_SApp := IBQry.FieldByName('CUST_ID').AsInteger;

       if CustID_SApp > 0 then
        begin
          SQL := 'UPDATE CUSTOMERS SET'+
                 ' CUST_TAXNUMB = '+     StrToSQL(DataMod.IBQuery.FieldByName('CUST_EIK').AsString, 10);

          AddToUpdateSQL(SQL, 'CUST_NAME',        'CUST_NAME',         50);
          AddToUpdateSQL(SQL, 'CUST_TOWN',        'CUST_TOWN',         50);
          AddToUpdateSQL(SQL, 'CUST_STREET',      'CUST_SHORTADDRESS', 50);
          AddToUpdateSQL(SQL, 'CUST_MANAGERNAME', 'CUST_MOL',          50);
          AddToUpdateSQL(SQL, 'CUST_PHONE',       'CUST_PHONE_G1',     50);
          SQL := SQL + ' WHERE CUST_ID = '+   IntToStr(CustID_SApp);

          if not ExecuteSQL(SQL) then raise EAbort.Create('Грешка при редактиране данни за клиент [БДС]');
        end
       else
        begin
         if not ExtractGenerator('GEN_CUSTOMERS_ID', CustID_SApp) then
          raise EAbort.Create('Грешка при генериране ID клиент [БДС]');

         if not ExecuteSQL('INSERT INTO CUSTOMERS (CUST_ID, CUST_TAXNUMB,'+
                           ' CUST_ISFIRM_,CUST_NAME, CUST_TOWN, CUST_STREET, CUST_MANAGERNAME,'+
                           ' CUST_TDD_NAME, CUST_PHONE, CUST_COMMENT) VALUES ('+
                           IntToStr(CustID_SApp)                                                  +','+ //CUST_ID
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_EIK').AsString, 10)         +','+ //CUST_TAXNUMB
                           StrToSQL('F', 1)                                                       +','+ //CUST_ISFIRM_
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_NAME').AsString, 50)        +','+ //CUST_NAME
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_TOWN').AsString, 50)        +','+ //CUST_TOWN
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_SHORTADDRESS').AsString, 50)+','+ //CUST_STREET
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_MOL').AsString, 50)         +','+ //CUST_MANAGERNAME
                           StrToSQL('ТДД '+DataMod.IBQuery.FieldByName('CUST_TOWN').AsString, 50) +','+ //CUST_TDD_NAME
                           StrToSQL(DataMod.IBQuery.FieldByName('CUST_PHONE_G1').AsString)        +','+ //CUST_PHONE
                           StrToSQL('Авт. въведени данни [ФС]', 100)                              +')' //CUST_COMMENT
                     ) then
          raise EAbort.Create('Грешка при въвеждане на нов клиент [БДС]');
        end;
      end;
     end;
    end;

 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;

procedure TServiceAppDataMod.UpdateObjectData(SiteID, CustID_SApp: Integer; var ObjID_SApp: Integer);
var CustAddr: string;
begin
 ObjID_SApp := 0;
 try
   // get device address' data
   if not DataMod.FillDataset('SELECT * FROM DEVADDRESS WHERE CUSTS_ID = '+IntToStr(SiteID)) then
    raise EAbort.Create('Грешка при четене данните за обект [БДФ]');

   if not DataMod.IBQuery.IsEmpty then
    begin
     CustAddr := '';
     with DataMod.IBQuery do
      begin
       if FieldByName('CUSTS_AREANAME').AsString <> '' then   CustAddr := CustAddr+','+FieldByName('CUSTS_AREANAME').AsString;
       if FieldByName('CUSTS_STREETNAME').AsString <> '' then CustAddr := CustAddr+','+FieldByName('CUSTS_STREETNAME').AsString;
       if FieldByName('CUSTS_STREETNO').AsString <> '' then   CustAddr := CustAddr+',No'+FieldByName('CUSTS_STREETNO').AsString;
       if FieldByName('CUSTS_BLOCK').AsString <> '' then      CustAddr := CustAddr+',Бл.'+FieldByName('CUSTS_BLOCK').AsString;
       if FieldByName('CUSTS_ENTRANCE').AsString <> '' then   CustAddr := CustAddr+',Вх.'+FieldByName('CUSTS_ENTRANCE').AsString;
       if FieldByName('CUSTS_FLOOR').AsString <> '' then      CustAddr := CustAddr+',Ет.'+FieldByName('CUSTS_FLOOR').AsString;
       if FieldByName('CUSTS_APARTMENT').AsString <> '' then  CustAddr := CustAddr+',Ап.'+FieldByName('CUSTS_APARTMENT').AsString;
      end;
     Delete(CustAddr, 1, 1);

     if not FillDataset('SELECT OBJ_ID FROM OBJECTS WHERE OBJ_CUSTOMER_ID = '+IntToStr(CustID_SApp)+
                        ' AND OBJ_NAME = '+StrToSQL(DataMod.IBQuery.FieldByName('CUSTS_NAME').AsString)+
                        ' AND OBJ_TOWN = '+StrToSQL(DataMod.IBQuery.FieldByName('CUSTS_TOWNNAME').AsString)+
                        ' AND OBJ_STREET = '+StrToSQL(CustAddr)) then
      raise EAbort.Create('Грешка при търсене на обект [БДС]');

     if not IBQry.IsEmpty then ObjID_SApp := IBQry.FieldByName('OBJ_ID').AsInteger;

     if ObjID_SApp <= 0 then
      begin
       if not ExtractGenerator('GEN_OBJECTS_ID', ObjID_SApp) then
        raise EAbort.Create('Грешка при генериране ID обект [БДС]');

       if not ExecuteSQL('INSERT INTO OBJECTS (OBJ_ID, OBJ_CUSTOMER_ID,'+
                         ' OBJ_NAME, OBJ_TOWN, OBJ_STREET, OBJ_COMMENT) VALUES ('+
                         IntToStr(ObjID_SApp)                                                +','+ //OBJ_ID
                         IntToStr(CustID_SApp)                                               +','+ //OBJ_CUSTOMER_ID
                         StrToSQL(DataMod.IBQuery.FieldByName('CUSTS_NAME').AsString, 50)    +','+ //OBJ_NAME
                         StrToSQL(DataMod.IBQuery.FieldByName('CUSTS_TOWNNAME').AsString, 50)+','+ //OBJ_TOWN
                         StrToSQL(CustAddr, 50)                                              +','+ //OBJ_STREET
                         StrToSQL('Авт. въведени данни [ФС]', 100)                           +')'  //OBJ_COMMENT
                         ) then
        raise EAbort.Create('Грешка при въвеждане на нов обект [БДС]');
      end;
    end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;

procedure TServiceAppDataMod.UpdateModelData(ModelID: Integer; var ModelID_SApp: Integer);
begin
 ModelID_SApp := 0;
 try
   // get model's data
   if not DataMod.FillDataset('SELECT * FROM DEVMODELS WHERE FDM_ID = '+IntToStr(ModelID)) then
    raise EAbort.Create('Грешка при четене данните за модел ФУ [БДФ]');

   if not DataMod.IBQuery.IsEmpty then
    begin
     if not FillDataset('SELECT MODEL_ID FROM ECR_MODELS'+
                        ' WHERE MODEL_NAME = '+StrToSQL(DataMod.IBQuery.FieldByName('FDM_SWIDMODEL').AsString)+
                        ' AND MODEL_SVIDNUMB = '+StrToSQL(DataMod.IBQuery.FieldByName('FDM_SVIDNUMBER').AsString)) then
      raise EAbort.Create('Грешка при търсене на модел ФУ [БДС]');

     if not IBQry.IsEmpty then ModelID_SApp := IBQry.FieldByName('MODEL_ID').AsInteger;

     if ModelID_SApp > 0 then
      begin
        if not ExecuteSQL('UPDATE ECR_MODELS SET'+
                          ' MODEL_SVIDDATE = '+DateToSQL(StrToDateDef(DataMod.IBQuery.FieldByName('FDM_SVIDDATE').AsString,0))+
                          ',MODEL_COMMENT = '+StrToSQL('ЕЛТРЕЙД',100,True)+
                          ' WHERE MODEL_ID = '+IntToStr(ModelID_SApp)) then
         raise EAbort.Create('Грешка при редактиране данни за модел ФУ [БДС]');;
      end
     else
      begin
       if not ExtractGenerator('GEN_ECR_MODELS_ID', ModelID_SApp) then
        raise EAbort.Create('Грешка при генериране ID модел ФУ [БДС]');

       if not ExecuteSQL('INSERT INTO ECR_MODELS (MODEL_ID, MODEL_NAME,'+
                         ' MODEL_SVIDNUMB, MODEL_SVIDDATE, MODEL_COMMENT) VALUES ('+
                         IntToStr(ModelID_SApp)+','+ //MODEL_ID
                         StrToSQL(DataMod.IBQuery.FieldByName('FDM_SWIDMODEL').AsString, 50)            +','+ //MODEL_NAME
                         StrToSQL(DataMod.IBQuery.FieldByName('FDM_SVIDNUMBER').AsString,50,True)       +','+ //MODEL_SVIDNUMB
                         DateToSQL(StrToDateDef(DataMod.IBQuery.FieldByName('FDM_SVIDDATE').AsString,0))+','+ //MODEL_SVIDDATE
                         StrToSQL('ЕЛТРЕЙД', 100, True)                                                 +')'  //MODEL_COMMENT
                         ) then
        raise EAbort.Create('Грешка при въвеждане на нов модел ФУ [БДС]');
      end;
    end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;

procedure TServiceAppDataMod.UpdateServiceTechData(var ServID, TechID: Integer);
begin
 ServID := 999999;  // ИД на собствения сервиз !!!
 TechID := 0;
 try
  // get service's data
  if not FillDataset('SELECT SERV_ID FROM SERVICES WHERE SERV_ID = '+IntToStr(ServID)) then
   raise EAbort.Create('Грешка при четене данните за сервиз [БДФ]');

  if IBQry.IsEmpty then
   begin
    if not ExecuteSQL('INSERT INTO SERVICES (SERV_ID, SERV_NAME,'+
                      ' SERV_STREET, SERV_TOWN, SERV_PHONE) VALUES ('+
                      IntToStr(ServID)+','+ //SERV_ID
                      QuotedStr(CurrentESK.CompanyName)+','+ //SERV_NAME
                      QuotedStr(CurrentESK.CompanyAddres)+','+ //SERV_STREET
                      QuotedStr(CurrentESK.CompanyTown)+','+ //SERV_TOWN
                      QuotedStr(CurrentESK.CompanyPhone)+')'  //SERV_PHONE
                      ) then
     raise EAbort.Create('Грешка при въвеждане на нов сервиз [БДС]');
   end;

  // get technician's data
  if not FillDataset('SELECT TECH_ID FROM TECHNICS '+
                     'WHERE (UPPER(TECH_NAME) = UPPER('+QuotedStr(CurrentESK.UserFullName)+'))'+
                     'AND (TECH_SERVICE_ID = '+IntToStr(ServID)+')') then
   raise EAbort.Create('Грешка при търсене на техник [БДС]');

  if not IBQry.IsEmpty then TechID := IBQry.FieldByName('TECH_ID').AsInteger;

  if TechID <= 0 then
   begin
    if not ExtractGenerator('GEN_TECHNICS_ID', TechID) then
     raise EAbort.Create('Грешка при генериране ID техник [БДС]');

    if not ExecuteSQL('INSERT INTO TECHNICS (TECH_ID, TECH_SERVICE_ID, TECH_NAME) VALUES ('+
                      IntToStr(TechID)+','+ //TECH_ID
                      IntToStr(ServID)+','+ //TECH_SERVICE_ID
                      QuotedStr(CurrentESK.UserFullName)+')' //TECH_NAME
                      ) then
     raise EAbort.Create('Грешка при въвеждане на нов техник [БДС]');
   end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;


function TServiceAppDataMod.SAppDB_SaveEcrData(FDSerial: String): Boolean;
var Dev_Cust : String;
    Dev_Site : Integer;
    Dev_Model: Integer;
    CustID   : Integer;
    ObjID    : Integer;
    ModelID  : Integer;
    EcrID    : Integer;
    ServID   : Integer;
    TechID   : Integer;
begin
 Result := true;
 EcrID  := 0;
 try
  try
   if (FDSerial = '') then raise EAbort.Create('Не е зададен идент. номер на ФУ.');

   if not DataMod.FillDataset('SELECT FD_CUSTOMER, FD_ADDRESS, FD_MODEL FROM FISCALDEVICE WHERE FD_SERIAL = '+QuotedStr(FDSerial)) then
    raise EAbort.Create('Грешка при четене данните за ФУ [БДФ]');

   if DataMod.IBQuery.IsEmpty then raise EAbort.Create('ФУ с номер '+FDSerial+' не е намерено! [БДФ]');

   Dev_Cust  := DataMod.IBQuery.FieldByName('FD_CUSTOMER').AsString;
   Dev_Site  := DataMod.IBQuery.FieldByName('FD_ADDRESS').AsInteger;
   Dev_Model := DataMod.IBQuery.FieldByName('FD_MODEL').AsInteger;
   DataMod.CloseDataset;

   // insert or update parent data
   UpdateCustomerData(Dev_Cust, CustID);
   UpdateObjectData(Dev_Site, CustID, ObjID);
   UpdateModelData(Dev_Model, ModelID);
   UpdateServiceTechData(ServID, TechID);

   // get ecr's data
   if not DataMod.FillDataset('SELECT * FROM FISCALDEVICE WHERE FD_SERIAL = '+QuotedStr(FDSerial)) then
    raise EAbort.Create('Грешка при четене данните за ФУ [БДФ]');

   if not DataMod.IBQuery.IsEmpty then
    begin
     if not FillDataset('SELECT ECR_ID, ECR_CUSTOMER_ID FROM ECRS WHERE (ECR_NUMB = '+QuotedStr(FDSerial)+')AND(ECR_FLAGS = 0)') then
      raise EAbort.Create('Грешка при търсене на ФУ [БДС]');

     if not IBQry.IsEmpty then EcrID := IBQry.FieldByName('ECR_ID').AsInteger;

     if EcrID > 0 then
      begin
         if CustID <> IBQry.FieldByName('ECR_CUSTOMER_ID').AsInteger then
          begin // simulation of change ownership
           if not ExecuteSQL('INSERT INTO ECRS (ECR_CUSTOMER_ID, ECR_OBJECT_ID, '+
                             ' ECR_MODEL_ID, ECR_SERVICE_ID, ECR_TECHNIC_ID, ECR_NUMB, ECR_FMNUMB,'+
                             ' ECR_COMMENT, ECR_BUY_DATE, ECR_BUY_INVOICENUMB, ECR_FISC_DATE,'+
                             ' ECR_CRU_NAME, ECR_CRU_TYPE, ECR_FLAGS, ECR_FDRID)'+
                             ' SELECT '+IntToStr(CustID)+', E.ECR_OBJECT_ID,'+
                             ' E.ECR_MODEL_ID, E.ECR_SERVICE_ID, E.ECR_TECHNIC_ID, E.ECR_NUMB, E.ECR_FMNUMB,'+
                             ' E.ECR_COMMENT, E.ECR_BUY_DATE, E.ECR_BUY_INVOICENUMB, E.ECR_FISC_DATE,'+
                             ' E.ECR_CRU_NAME, E.ECR_CRU_TYPE, 0, E.ECR_FDRID'+
                             ' FROM ECRS E WHERE E.ECR_ID = '+IntToStr(EcrID), False) then
            raise EAbort.Create('Грешка при смяна собственика на ФУ [БДС]');

           if not ExecuteSQL('UPDATE ECRS SET ECR_FLAGS = 1 WHERE ECR_ID = '+IntToStr(EcrID), False) then
            raise EAbort.Create('Грешка при смяна собственика на ФУ [БДС] 1');

           if IBQry.Transaction.InTransaction then IBQry.Transaction.Commit;
          end
         else
          begin
           if not ExecuteSQL('UPDATE ECRS SET ECR_OBJECT_ID = '+IntToStr(ObjID)+
                             ',ECR_MODEL_ID = '+  IntToStr(ModelID)+
                             ',ECR_FMNUMB = '+    DataMod.IBQuery.FieldByName('FD_MFM').AsString+
                             ',ECR_COMMENT = '+   StrToSQL(DataMod.IBQuery.FieldByName('FD_COMMENT').AsString,100,True)+
                             ',ECR_FISC_DATE = '+ DateToSQL(StrToDateDef(DataMod.IBQuery.FieldByName('FD_FISCDATE').AsString,0))+
                             ',ECR_FDRID = '+     StrToSQL(DataMod.IBQuery.FieldByName('FD_NRAREGID').AsString,0,True)+
                             ',ECR_SIMIMSI = '+   StrToSQL(DataMod.IBQuery.FieldByName('FD_SIMIMSI').AsString, 50, true)+
                             ',ECR_SIMMSISDN = '+ StrToSQL(DataMod.IBQuery.FieldByName('FD_SIMMSISDN').AsString, 50, true)+
                             ' WHERE ECR_ID = '+IntToStr(EcrID)) then Abort;
          end;
      end
     else
      begin
       if not ExtractGenerator('GEN_ECRS_ID', EcrID) then raise EAbort.Create('Грешка при генериране ID ФУ [БДС]');

       if not ExecuteSQL('INSERT INTO ECRS (ECR_ID, ECR_CUSTOMER_ID, ECR_OBJECT_ID,'+
                         ' ECR_MODEL_ID, ECR_SERVICE_ID, ECR_TECHNIC_ID, ECR_NUMB,'+
                         ' ECR_FMNUMB, ECR_COMMENT, ECR_FISC_DATE, ECR_FDRID, ECR_SIMIMSI,'+
                         ' ECR_SIMMSISDN) VALUES ('+
                         IntToStr(EcrID)                                                          +','+ //ECR_ID
                         IntToStr(CustID)                                                         +','+ //ECR_CUSTOMER_ID
                         IntToStr(ObjID)                                                          +','+ //ECR_OBJECT_ID
                         IntToStr(ModelID)                                                        +','+ //ECR_MODEL_ID
                         IntToStr(ServID)                                                         +','+ //ECR_SERVICE_ID
                         IntToStr(TechID)                                                         +','+ //ECR_TECHNIC_ID
                         QuotedStr(FDSerial)                                                      +','+ //ECR_NUMB
                         DataMod.IBQuery.FieldByName('FD_MFM').AsString                           +','+ //ECR_FMNUMB
                         StrToSQL(DataMod.IBQuery.FieldByName('FD_COMMENT').AsString,100,True)    +','+ //ECR_COMMENT
                         DateToSQL(DataMod.IBQuery.FieldByName('FD_FISCDATE').AsDateTime)         +','+ //ECR_FISC_DATE
                         StrToSQL(DataMod.IBQuery.FieldByName('FD_NRAREGID').AsString, 50, true)  +','+ //ECR_FDRID
                         StrToSQL(DataMod.IBQuery.FieldByName('FD_SIMIMSI').AsString, 50, true)   +','+ //ECR_SIMIMSI
                         StrToSQL(DataMod.IBQuery.FieldByName('FD_SIMMSISDN').AsString, 50, true) +')'  //ECR_SIMMSISDN
                         //+','+ //ECR_SIMTODATE
                         //+')'  //ECR_SIMSUM
                         ) then
       raise EAbort.Create('Грешка при въвеждане на ново ФУ [БДС]');
      end;
    end;
  except
  on E: Exception do
   begin
    Result := false;
    E.Message := 'Грешка при запис данни за ФУ в Сервизната база.'+sLineBreak+E.Message;
    DataMod.PostEvent(EV_ERROR, '', 'Грешка при запис сервизна информация', E.Message);
    DataMod.PostException(E.Message, errcode_DatabaseError);
   end;
  end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;

function TServiceAppDataMod.SAppDB_SaveSvidData(FDSerial, SvN: string): Boolean;
var EcrID: Integer;
begin
 Result := true;
 EcrID  := 0;
 try
  try
   if (FDSerial = '') then raise EAbort.Create('Не е зададен идент. номер на ФУ.');

   // looking for certificate in ServiceAppication
   if not RecordExist('ECRS LEFT JOIN CONTRACTS ON ECRS.ECR_ID = CONTRACTS.C_ECR_ID',
                      ['ECR_NUMB','C_TYPE', 'C_NUMBER'], [FDSerial,'1',SvN]) then
    begin
     if DataMod.RecordExist('DEVSVIDLOG LEFT JOIN FISCALDEVICE ON DEVSVIDLOG.FDS_SERIAL = FISCALDEVICE.FD_SERIAL',
                            ['FDS_SERIAL','FD_SERIAL'], [FDSerial,FDSerial]) then
      begin // found some certificates in Fiscalizator
       if not FillDataset('SELECT ECR_ID FROM ECRS WHERE ECR_NUMB = '+QuotedStr(FDSerial)+' AND ECR_FLAGS = 0') then
        raise EAbort.Create('Грешка при търсене на ФУ [БДС]');

       if not IBQry.IsEmpty then EcrID := IBQry.FieldByName('ECR_ID').AsInteger;

       // get last certificate from Fiscalizator
       if not DataMod.FillDataset('SELECT * FROM DEVSVIDLOG'+
                           ' LEFT JOIN FISCALDEVICE ON DEVSVIDLOG.FDS_SERIAL = FISCALDEVICE.FD_SERIAL'+
                           ' WHERE FDS_SERIAL = '+QuotedStr(FDSerial)+' AND FD_SERIAL = '+QuotedStr(FDSerial)+
                           ' ORDER BY FDS_ID DESC') then Abort;
       if not DataMod.IBQuery.IsEmpty then
        begin // add certificate in ServiceAppication
         if not ExecuteSQL('INSERT INTO CONTRACTS (C_CUSTOMER_ID, C_ECR_ID, C_TYPE,'+
                ' C_FROMDATE, C_TODATE, C_COMMENT, C_TECHNIC_NAME, C_NUMBER, C_CUST_ISFIRM_,'+
                ' C_CUST_TAXNUMB, C_CUST_BULSTAT, C_CUST_NAME, C_CUST_TOWN, C_CUST_STREET,'+
                ' C_CUST_MANAGERNAME, C_CUST_TDD_NAME, C_OBJ_NAME, C_OBJ_TOWN, C_OBJ_STREET,'+
                ' C_OBJ_PHONE, C_ECR_NUMB, C_ECR_FMNUMB, C_SERV_NAME, C_SERV_TOWN,'+
                ' C_SERV_STREET, C_SERV_PHONE, C_TECH_NAME, C_CONTRACT_ID)'+sLineBreak+
                ' SELECT C.CUST_ID, E.ECR_ID, 1, '+QuotedStr(FormatDateTime('dd.mm.yyyy',Date))+','+
                QuotedStr(FormatDateTime('dd.mm.yyyy',Date))+','+StrToSQL(DataMod.IBQuery.FieldByName('FDS_COMMENT').AsString,100,True)+
                ',T.TECH_NAME, '+StrToSQL(SvN, 50)+','+
                ' C.CUST_ISFIRM_, C.CUST_TAXNUMB, C.CUST_BULSTAT, C.CUST_NAME, C.CUST_TOWN, C.CUST_STREET,'+
                ' C.CUST_MANAGERNAME, C.CUST_TDD_NAME, O.OBJ_NAME, O.OBJ_TOWN, O.OBJ_STREET, O.OBJ_PHONE,'+
                ' E.ECR_NUMB, E.ECR_FMNUMB, S.SERV_NAME, S.SERV_TOWN, S.SERV_STREET, S.SERV_PHONE,'+
                ' T.TECH_NAME, CNTR.RC_ID'+sLineBreak+
                ' FROM ECRS E'+
                ' LEFT JOIN CUSTOMERS C ON E.ECR_CUSTOMER_ID = C.CUST_ID'+
                ' LEFT JOIN OBJECTS O ON E.ECR_OBJECT_ID = O.OBJ_ID'+
                ' LEFT JOIN SERVICES S ON E.ECR_SERVICE_ID = S.SERV_ID'+
                ' LEFT JOIN TECHNICS T ON E.ECR_TECHNIC_ID = T.TECH_ID'+
                ' LEFT JOIN GET_LAST_CONTRACT(E.ECR_ID) CNTR ON ((CNTR.RC_ID > 0) AND (CNTR.RC_TYPE = 2))'+
                ' WHERE E.ECR_ID = '+IntToStr(EcrID)
                ) then
          raise EAbort.Create('Грешка при въвеждане на ново свидетелство [БДС]');
        end;
      end;
    end;

  except
   on E: Exception do
    begin
     Result := false;
     E.Message := 'Грешка при запис на свидетелство в Сервизната база данни'+sLineBreak+E.Message;
     DataMod.PostEvent(EV_ERROR, '', 'Грешка при запис сервизна информация', E.Message);
     DataMod.PostException(E.Message, errcode_DatabaseError);
    end;
  end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;


function TServiceAppDataMod.SAppDB_CommitContractNumb(FDSerial: String): Boolean;
begin
 Result := ExecuteSQL('DELETE FROM CONTRACT_RESERVATION '+
                      'WHERE (CR_ECRNUMB = '+StrToSQL(FDSerial)+')'+
                      'OR(CR_DATETIME < '+QuotedStr(FormatDateTime('dd.mm.yyyy', Date - 30))+')');
 if not Result then
  begin
     ExecuteSQL('CREATE TABLE CONTRACT_RESERVATION ('+
                'CR_ECRNUMB ECR_NUMBS NOT NULL, '+
                'CR_CONTRACTNUMB  TEXT_NOTNULL_50, '+
                'CR_DATETIME DATETIME_NOTNULL DEFAULT CURRENT_TIMESTAMP);');
     ExecuteSQL('ALTER TABLE CONTRACT_RESERVATION ADD CONSTRAINT PK_CONTRACT_RESERVATION PRIMARY KEY (CR_ECRNUMB);');
  end;
end;

function TServiceAppDataMod.SAppDB_GenerateContractNumb(FDSerial: String): String;
var N   : Integer;
    SQL : String;
    OK  : Boolean;
begin
 Result := '';

 // Проверка дали има резервиран номер за този апарат
 // Ако имаме резервация ползваме нея. Прави се за да няма много дупки в номерацията на договорите
 if FDSerial <> '' then
  try
   SQL := 'SELECT CR_CONTRACTNUMB FROM CONTRACT_RESERVATION WHERE CR_ECRNUMB = '+StrToSQL(FDSerial);
   OK  := FillDataset(SQL);
   if not OK then
    begin
     ExecuteSQL('CREATE TABLE CONTRACT_RESERVATION ('+
                'CR_ECRNUMB ECR_NUMBS NOT NULL, '+
                'CR_CONTRACTNUMB  TEXT_NOTNULL_50, '+
                'CR_DATETIME DATETIME_NOTNULL DEFAULT CURRENT_TIMESTAMP);');
     ExecuteSQL('ALTER TABLE CONTRACT_RESERVATION ADD CONSTRAINT PK_CONTRACT_RESERVATION PRIMARY KEY (CR_ECRNUMB);');

     OK := FillDataset(SQL);
    end;
   if (OK)and(not IBQry.IsEmpty) then Result := IBQry.FieldByName('CR_CONTRACTNUMB').AsString;
  finally
   CloseDataset;
  end;

 // Няма резервация - генерираме нов номер
 if Result = '' then
  begin
   if ExtractGenerator('CONTRACT_NUMB', N, true) then
    begin
     Result := FormatFloat('000000000', N);
     ExecuteSQL('INSERT INTO CONTRACT_RESERVATION(CR_ECRNUMB, CR_CONTRACTNUMB)VALUES('+StrToSQL(FDSerial, 8)+', '+StrToSQL(Result, 50)+');');
    end
   else
    begin
      DataMod.PostEvent(EV_ERROR, '', 'Грешка при запис сервизна информация', 'Неуспешно генериране номер на договор');
      DataMod.PostException('Неуспешно генериране номер на договор', errcode_DatabaseError);
    end;
  end;
end;

function TServiceAppDataMod.SAppDB_SaveContractData(FDSerial, CNumber, CComment: String; StartDate, EndDate: TDateTime): Boolean;
var EcrID: Integer;
begin
 EcrID  := 0;
 Result := true;
 try
  try
   if (FDSerial = '') then raise EAbort.Create('Не е зададен идент. номер на ФУ.');

   // looking for certificate in ServiceAppication
   if not RecordExist('ECRS LEFT JOIN CONTRACTS ON ECRS.ECR_ID = CONTRACTS.C_ECR_ID',
                      ['ECR_NUMB','C_TYPE', 'C_NUMBER'], [FDSerial, '2', CNumber]) then
    begin
     if not FillDataset('SELECT ECR_ID FROM ECRS WHERE ECR_NUMB = '+QuotedStr(FDSerial)+' AND ECR_FLAGS = 0') then
      raise EAbort.Create('Грешка при търсене на ФУ [БДС]');

     if not IBQry.IsEmpty then EcrID := IBQry.FieldByName('ECR_ID').AsInteger;

     if EcrID > 0 then
      begin // add certificate in ServiceAppication
       if not ExecuteSQL('INSERT INTO CONTRACTS (C_CUSTOMER_ID, C_ECR_ID, C_TYPE,'+
              ' C_FROMDATE, C_TODATE, C_COMMENT, C_TECHNIC_NAME, C_NUMBER, C_CUST_ISFIRM_,'+
              ' C_CUST_TAXNUMB, C_CUST_BULSTAT, C_CUST_NAME, C_CUST_TOWN, C_CUST_STREET,'+
              ' C_CUST_MANAGERNAME, C_CUST_TDD_NAME, C_OBJ_NAME, C_OBJ_TOWN, C_OBJ_STREET,'+
              ' C_OBJ_PHONE, C_ECR_NUMB, C_ECR_FMNUMB, C_SERV_NAME, C_SERV_TOWN,'+
              ' C_SERV_STREET, C_SERV_PHONE, C_TECH_NAME, C_CONTRACT_ID)'+sLineBreak+
              ' SELECT C.CUST_ID, E.ECR_ID, 2, '+QuotedStr(FormatDateTime('dd.mm.yyyy',StartDate))+','+
              QuotedStr(FormatDateTime('dd.mm.yyyy',EndDate))+','+StrToSQL(CComment,100,True)+
              ',T.TECH_NAME, '+StrToSQL(CNumber, 50)+','+
              ' C.CUST_ISFIRM_, C.CUST_TAXNUMB, C.CUST_BULSTAT, C.CUST_NAME, C.CUST_TOWN, C.CUST_STREET,'+
              ' C.CUST_MANAGERNAME, C.CUST_TDD_NAME, O.OBJ_NAME, O.OBJ_TOWN, O.OBJ_STREET, O.OBJ_PHONE,'+
              ' E.ECR_NUMB, E.ECR_FMNUMB, S.SERV_NAME, S.SERV_TOWN, S.SERV_STREET, S.SERV_PHONE,'+
              ' T.TECH_NAME, NULL '+sLineBreak+
              ' FROM ECRS E'+
              ' LEFT JOIN CUSTOMERS C ON E.ECR_CUSTOMER_ID = C.CUST_ID'+
              ' LEFT JOIN OBJECTS O ON E.ECR_OBJECT_ID = O.OBJ_ID'+
              ' LEFT JOIN SERVICES S ON E.ECR_SERVICE_ID = S.SERV_ID'+
              ' LEFT JOIN TECHNICS T ON E.ECR_TECHNIC_ID = T.TECH_ID'+
              ' WHERE E.ECR_ID = '+IntToStr(EcrID)
              ) then
        raise EAbort.Create('Грешка при въвеждане на нов договор [БДС]');
      end;
    end;
  except
   on E: Exception do
    begin
     Result := false;
     E.Message := 'Грешка при запис на договор в Сервизната база данни'+sLineBreak+E.Message;
     DataMod.PostEvent(EV_ERROR, '', 'Грешка при запис сервизна информация', E.Message);
     DataMod.PostException(E.Message, errcode_DatabaseError);
    end;
  end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;

function TServiceAppDataMod.SAppDB_DeregisterECR(FDSerial: String): Boolean;
begin
 Result := true;
 try
  try
   if (FDSerial = '') then raise EAbort.Create('Не е зададен идент. номер на ФУ.');

   if not FillDataset('SELECT ECR_ID, ECR_CUSTOMER_ID FROM ECRS WHERE (ECR_NUMB = '+QuotedStr(FDSerial)+')') then
    raise EAbort.Create('Грешка при търсене на ФУ [БДС]');

   if IBQry.IsEmpty then raise EAbort.Create('Не са намерени данни за ФУ [БДС]');

   if not UpdateRecord('ECRS', 'ECR_NUMB = '+StrToSQL(FDSerial), ['ECR_FLAGS'], [1]) then
    raise EAbort.Create('Грешка при смяна стстуса на ФУ [БДС]');

  except
  on E: Exception do
   begin
    Result := false;
    E.Message := 'Грешка при бракуване на ФУ в Сервизната база.'+sLineBreak+E.Message;
    DataMod.PostEvent(EV_ERROR, '', 'Грешка при запис сервизна информация', E.Message);
    DataMod.PostException(E.Message, errcode_DatabaseError);
   end;
  end;
 finally
  DataMod.CloseDataset;
  CloseDataset;
 end;
end;

function TServiceAppDataMod.SAppDB_FindCustomer(CustEIK: String): Boolean;
begin
 Result := FillDataset('SELECT * FROM CUSTOMERS WHERE '+
                       '(CUST_TAXNUMB = '+StrToSQL(CustEIK)+')OR'+
                       '(CUST_BULSTAT = '+StrToSQL(CustEIK)+')');
 if Result then Result := (IBQry.RecordCount > 0);
 if not Result then CloseDataset;                      
end;

function TServiceAppDataMod.SAppDB_GetCompanyData(var Data: String): Boolean;
var SQL : String;
begin
 SQL := 'SELECT MD_TAXNUMB, MD_BULSTAT, MD_NAME, MD_ADDRESS, MD_PHONE, MD_EMAIL FROM MY_DATA';
 Result := GetDatasetAsText(SQL, Data);
end;

function TServiceAppDataMod.SAppDB_GetDevCntMonths(Year: Word; var Data: String): Boolean;
var SQL : String;
begin
 SQL := 'SELECT CASE UPPER(SUBSTRING(ECRS.ECR_NUMB FROM 1 FOR 2)) '+
        'WHEN ''DT'' THEN ''1'' '+
        'WHEN ''ДТ'' THEN ''1'' '+
        'WHEN ''DY'' THEN ''2'' '+
        'WHEN ''ДУ'' THEN ''2'' '+
        'WHEN ''ED'' THEN ''3'' '+
        'WHEN ''ЕД'' THEN ''3'' '+
        'WHEN ''ZK'' THEN ''4'' '+
        'WHEN ''ЗК'' THEN ''4'' '+
        'WHEN ''OT'' THEN ''5'' '+
        'WHEN ''ОТ'' THEN ''5'' '+
        'WHEN ''EE'' THEN ''6'' '+
        'WHEN ''ЕЕ'' THEN ''6'' '+
        'ELSE ''0'' END, '+
        'EXTRACT(MONTH FROM ECRS.ECR_INSERT_TIME), '+
        'COUNT(ECRS.ECR_ID) '+
        'FROM ECRS '+
        'WHERE ECRS.ECR_INSERT_TIME BETWEEN '+DateToSQL(StartOfAYear(Year))+' AND '+DateToSQL(EndOfAYear(Year))+' '+
        'GROUP BY 1, 2';
 Result := GetDatasetAsText(SQL, Data);
end;

function TServiceAppDataMod.SAppDB_GetDevCntYears(var Data: String): Boolean;
var SQL : String;
begin
 SQL := 'SELECT CASE UPPER(SUBSTRING(ECRS.ECR_NUMB FROM 1 FOR 2)) '+
        'WHEN ''DT'' THEN ''1'' '+
        'WHEN ''ДТ'' THEN ''1'' '+
        'WHEN ''DY'' THEN ''2'' '+
        'WHEN ''ДУ'' THEN ''2'' '+
        'WHEN ''ED'' THEN ''3'' '+
        'WHEN ''ЕД'' THEN ''3'' '+
        'WHEN ''ZK'' THEN ''4'' '+
        'WHEN ''ЗК'' THEN ''4'' '+
        'WHEN ''OT'' THEN ''5'' '+
        'WHEN ''ОТ'' THEN ''5'' '+
        'WHEN ''EE'' THEN ''6'' '+
        'WHEN ''ЕЕ'' THEN ''6'' '+
        'ELSE ''0'' END, '+
        'EXTRACT(YEAR FROM ECRS.ECR_INSERT_TIME)-2000, '+
        'COUNT(ECRS.ECR_ID) '+
        'FROM ECRS '+
        'GROUP BY 1, 2';
 Result := GetDatasetAsText(SQL, Data);
end;

end.
