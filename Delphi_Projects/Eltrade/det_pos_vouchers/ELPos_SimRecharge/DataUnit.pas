unit DataUnit;

interface

uses
  Windows, SysUtils, Forms, DB, IBServices, IB, dxSpeedButton, Dialogs, DBUtilsUnit,
  IBSQL, IBDatabase, IBQuery, StdCtrls, IniFiles, DateUtils, WinSvc,
  Classes, ConstUnit, CreateFilesUnit, SimRchVivacomUnit;

type
  TDataMod = class(TDataModule)
    aIBDatabase: TIBDatabase;
    IBSecurityService: TIBSecurityService;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure aIBDatabaseAfterConnect(Sender: TObject);
  private
    IBQ : TIBSQL;
    IBT : TIBTransaction;
    function AddSystemUser(ServerHost_: String): Boolean;
    function CheckInterbaseServer: Boolean;
    function ServiceIsRunning(ServiceName_: String): Boolean;
    function ConfirmMarkedPlues(Qry: TIBSQL): boolean;
    function InsSalesBon(Qry: TIBSQL; SellID: integer): boolean;
    function InsSalesPay(Qry: TIBSQL; SellID: integer): boolean;
    function InsSalesPlues(Qry: TIBSQL; SellID: integer): boolean;
    function InsTmpQRest(Qry: TIBSQL): boolean;
    function RollbackMarkedPlues(Qry: TIBSQL): boolean;
    function GetCustData(CBulst: string; var CObj: TCustomerObject): boolean;
    function CreateReport(Qry: TIBSQL): boolean;
    function LoadFactNumb: Boolean;
    procedure SaveFactNumb;
  public
    ServerAddress : String;

    function LoadDatabaseConfig: Boolean;
    function ConnectToDatabase: Boolean;

//    procedure PushBtnDown(Btn: TdxSpeedButton);
//    procedure PushBtnUp(Btn: TdxSpeedButton);
    procedure ClearList(sList: TListBox);
    function LogModule(LogIn: Boolean): boolean;
    function GetNomeclatures: Boolean;
    function DBConnected: Boolean;
    procedure PostEvent(EvType: integer; EvDescr: string);
    procedure LogOut;
    function AddDataToList(AList: TListBox; ANumb: Integer; AName: String): Integer;
    procedure MovePageUp(AList: TListBox);
    procedure MovePageDown(AList: TListBox);
    function SaveSale: boolean;
    function GetOperName(OperID: integer): string;
    function GetFirstStorageNumb : Integer;
    function GetStorageName(StrgNumb: Integer): String;
    procedure SetDestObject(DestObj: TObject; Name_, Bulstat_: String);
    procedure PostException(S: ShortString);overload;
    procedure PostException(S: String);overload;
    function TurnoverForToday(var SellSum: double): boolean;
    function LimitsInfo: string;
    function CheckPrePaid(MSISDN: string): boolean;
    function PayPrePaid(MSISDN: string; Ammount: double): boolean;
    function GetPayPrnNumbAndName(PayID: Integer; var PayPrnNumb: Integer; var PayPrnName: String):Boolean;
  end;

var
  DataMod: TDataMod;

implementation

uses MyMessageUnit, KBDUnit, MainUnit, ProtectUnit, ResStrUnit, SetPaymentUnit;

{$R *.dfm}

procedure TDataMod.DataModuleCreate(Sender: TObject);
begin
 SimRChObj := TSimRChObj.Create;
 SimRChObj.FPhoneNumb  := '';
 SimRChObj.FMobOper    := 0;
 SimRChObj.FSum        := 0;
 SimRChObj.FPluNumb    := -1;
 SimRChObj.FPluName    := '';
 SimRChObj.FVat        := 20;
 SimRChObj.FTaxGroup   := 2;
// SimRChObj.FBuyPr      := 0;
// SimRChObj.FBuyCurrID  := 1;
// SimRChObj.FBuyCCource := 0;
// SimRChObj.FSellPr     := 0;
// SimRChObj.FSellCurrID := 1;
// SimRChObj.FSellCCource:= 0;
 SimRChObj.FMaxSumTr   := 0;
 SimRChObj.FMaxSumDay  := 0;
 SimRChObj.FDocType    := 0;
 SimRChObj.FDocName    := '';
 SimRChObj.FPayType    := 0;
 SimRChObj.FPayName    := '';
 SimRChObj.FPayPrnNumb := 0;
 SimRChObj.FPayPrnName := '';
 SimRChObj.FPayCource  := 1;
 SimRChObj.FCustBulst  := '';
 SimRChObj.FCustName   := '';


 DB_CreateIBSQL(IBQ, IBT, aIBDatabase);

 if not LoadFactNumb then
  begin
   CurrentFactNumb := 0;
   SaveFactNumb;
  end;
end;

procedure TDataMod.DataModuleDestroy(Sender: TObject);
var I: Integer;
begin
 if aIBDatabase.Connected then LogModule(False);
 aIBDatabase.CloseDataSets;
 for I := 0 to aIBDatabase.TransactionCount - 1 do
  begin
   if aIBDatabase.Transactions[I].InTransaction then aIBDatabase.Transactions[I].Commit;
  end;
 DB_DestroyIBSQL(IBQ, IBT);
 aIBDatabase.Close;
 FreeAndNil(SimRChObj);
end;

function TDataMod.LoadDatabaseConfig: Boolean;
var DBConnection_, DBProtocol_: Integer;
    DBHost_, DBFileName_: String;
begin
 Result := FileExists(LocalPath + C_DatabaseIniFName);
 if not Result then Exit;

 with TIniFile.Create(LocalPath + C_DatabaseIniFName) do
  try
   DBConnection_ := ReadInteger('DATABASEOPTIONS', 'Connection', 0);
   DBProtocol_   := ReadInteger('DATABASEOPTIONS', 'Protocol', 0);
   DBHost_       := ReadString ('DATABASEOPTIONS', 'ServerName', '');
   DBFileName_   := ReadString ('DATABASEOPTIONS', 'DBPath', '');
  finally
   Free;
  end;

 if DBFileName_ <> '' then
  begin
   ServerAddress := '';
   if DBConnection_ = 0 then // local
     aIBDatabase.DatabaseName := DBFileName_
   else
    begin
     DBHost_ := Trim(DBHost_);
     if ((AnsiUpperCase(DBHost_) <> 'LOCALHOST') and (DBHost_ <> '127.0.0.1')) then // type of connection is TCP
      begin
       ServerAddress := DBHost_;
      end;

     case DBProtocol_ of
      0: aIBDatabase.DatabaseName := DBHost_+':'+DBFileName_;
      1: aIBDatabase.DatabaseName := '\\'+DBHost_+'\'+DBFileName_;
      2: aIBDatabase.DatabaseName := DBHost_+'@'+DBFileName_;
     end;
    end;
  end
 else Result := False;
end;

function TDataMod.DBConnected: Boolean;
begin
 Result := aIBDatabase.Connected;
end;

function TDataMod.AddSystemUser(ServerHost_: String): Boolean;
begin
 try
  Result := True;
  with TIBSecurityService.Create(nil) do
  try
    ServerName := ServerHost_;
    if ServerName = '' then ServerName := 'localhost';
    Protocol   := TCP;
    LoginPrompt:= False;
    Params.Add('user_name=sysdba');
    Params.Add('password=masterkey');
    Active := True;
    try
     UserName := 'eltrade';
     FirstName:= 'System Account';
     Password := 'gdelchev';
     AddUser;
    finally
     Active := False;
    end;
  finally
   Free;
  end;
 except
  Result := False;
 end;
end;

function TDataMod.ConnectToDatabase: Boolean;
var ST     : TDateTime;
    ErrCnt : Integer;
begin
 // DB is connected ->> reconnect
 if aIBDatabase.Connected then aIBDatabase.Close;

 // server is local: Check Interbase
 if (ServerAddress = '') or (SameText(ServerAddress, GetHostName)) then
  begin
   ST := Now;
   while not CheckInterbaseServer do
    begin
     Sleep(3000);
     if SecondsBetween(ST, Now) > 40 then Break;
    end;
  end;

 ErrCnt := 0;
 repeat
  try
   aIBDatabase.Open;
  except
   on E : Exception do
    begin
     E.Message := UpperCase(E.Message);
     if (Pos('USER', E.Message) > 0)and(Pos('PASSWORD', E.Message) > 0) then
      begin
       AddSystemUser(ServerAddress);
      end
     else
      begin
       PostException('Error opening Database: ' + E.Message);
       Inc(ErrCnt);
      end;
    end;
  end;

  if ErrCnt >= 2 then Break;
 until aIBDatabase.Connected;
 Result := aIBDatabase.Connected;
end;

function TDataMod.CheckInterbaseServer: Boolean;
begin
 if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
   Result := True
  end
 else
  begin
   Result := (ServiceIsRunning('InterBaseServer')) or (ServiceIsRunning('FirebirdServerDefaultInstance'));
  end;
end;

function TDataMod.ServiceIsRunning(ServiceName_: String): Boolean;
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

function TDataMod.GetNomeclatures: boolean;
begin
 Result := False;
 BCurID     := 1;
 BCurName   := '';
 BCurAbr    := '';
 BCurSign   := '';
 BCurCource := 1;
 try
  // get base currency
  if not DB_ExecuteSQL('SELECT * FROM N_CURRENCY WHERE CURR_ISBASE_ = 1', IBQ, False, 'GetBaseCurr') then exit;

  if (IBQ.RecordCount = 0) then exit;
  BCurID   := IBQ.FieldByName('CURR_ID').AsInteger;
  BCurName := IBQ.FieldByName('CURR_NAME').AsString;
  BCurAbr  := IBQ.FieldByName('CURR_ABR').AsString;
  BCurSign := IBQ.FieldByName('CURR_SIGN').AsString;
  if (IBQ.FieldByName('CURR_COURCEFOR').AsFloat = 0) then
   BCurCource := IBQ.FieldByName('CURR_COURCE').AsFloat
  else
   BCurCource := IBQ.FieldByName('CURR_COURCE').AsFloat/IBQ.FieldByName('CURR_COURCEFOR').AsFloat;

  // get current office numb
  if not DB_ExecuteSQL('SELECT GEN_ID(NN_CURRENT_OFFICE_NUMB,0) "GENVALUE"'+
                       ' FROM RDB$DATABASE', IBQ, False, 'GetOfficeNumb') then exit;
  CurrentOfficeNumb := IBQ.FieldByName('GENVALUE').AsInteger;
  Result := True;
 finally
  DB_RollbackIBSQL(IBQ);
 end;
end;

function TDataMod.LogModule(LogIn: Boolean): boolean;
var sn: string;
begin
 Result := False;
 try
  Protection.ModuleCode := C_CurrentModuleCode;
  sn := Protection.GenerateSerialNumber;
  if not DB_ExecuteSQL('UPDATE NN_MODULES SET MODULE_ISLOGGEDON_ = '+BoolToStr_(LogIn)+
                       ' WHERE MODULE_SERIAL = '+QuotedStr(sn)+sLineBreak+
                       ' AND MODULE_PATH = '+QuotedStr(Application.ExeName)+
                       ' AND (MODULE_DESCR = '+QuotedStr(GetHostName)+')', IBQ, True, 'LogModule') then exit;
  if LogIn then PostEvent(1, S_ModuleStart)
   else PostEvent(2, S_ModuleStop);
  Result := True;
 except
  on E: Exception do
   begin
    PostException('Fail login/logout module: '+Trim(E.Message));
    PostEvent(13, S_ModuleLoginFail+' '+Trim(E.Message));
   end;
 end;
end;

procedure TDataMod.ClearList(sList: TListBox);
var I: integer;
begin
 for I := 0 to sList.Items.Count-1 do
  begin
   if sList.Items.Objects[I] <> nil then sList.Items.Objects[I].Free;
  end;
 sList.Clear;
end;

procedure TDataMod.PostEvent(EvType: Integer; EvDescr: string);
begin
 try
  if not aIBDatabase.Connected then raise Exception.Create('The Database is not connected.');
  DB_ExecuteSQL('INSERT INTO EVENTS (EV_TYPE, EV_MODULE, EV_OPERATOR, EV_DATETIME, EV_DESCRIPTION) '+#13+#10+
                'VALUES ('+IntToStr(EvType)+','+IntToStr(CurrentModuleID)+
                ','+IntToStr(CurrentOperID)+','+QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn:ss',Now))+
                ','+QuotedStr('('+VerApp+') '+EvDescr)+')', IBQ, True, 'AddEvent');
 except
  on E: Exception do PostException('Post DB event fail: '+E.Message);
 end;
end;

procedure TDataMod.LogOut;
var Qry: TIBSQL;
    Tr: TIBTransaction;
begin
 Qry := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(Qry, Tr, aIBDatabase);
   DB_ExecuteSQL('UPDATE N_OPERATORS SET OPERATOR_LOGON_ = 0'+
                 ' WHERE OPERATOR_ID = '+IntToStr(CurrentOperID), Qry, True, 'LogOut');

   DataMod.PostEvent(4, 'Затворена сесия ('+CurrentOperName+').');
  except
  on E: Exception do
   begin
    PostException('Logout failed for user: "'+CurrentOperName+'" '+Trim(E.Message));
    DataMod.PostEvent(13, S_LogoutFailed+' ('+CurrentOperName+'): '+Trim(E.Message));
   end;
  end;
 finally
  DB_DestroyIBSQL(Qry, Tr);
 end;
end;

function TDataMod.AddDataToList(AList: TListBox; ANumb: Integer; AName: String): Integer;
var Obj: TOperObj;
begin
 Obj := TOperObj.Create;
 Obj.FNumb := ANumb;
 Obj.FName := AName;

 Result := AList.Items.AddObject('', Obj);
end;

procedure TDataMod.MovePageUp(AList: TListBox);
begin
 if AList.ItemIndex > 0 then AList.ItemIndex := AList.ItemIndex-1;
 InvalidateRect(AList.Handle, nil, True);
end;

procedure TDataMod.MovePageDown(AList: TListBox);
begin
 if AList.ItemIndex < AList.Count-1 then AList.ItemIndex := AList.ItemIndex+1;
 InvalidateRect(AList.Handle, nil, True);
end;

function TDataMod.SaveSale: boolean;
var Qry: TIBSQL;
    Tr: TIBTransaction;
    SellID, BonType: integer;
    CObj: TCustomerObject;
    ErrMsg: string;
begin
 Result := False;
 Qry := nil;   Tr := nil;   CObj := nil;
 try
  try
   ErrMsg := S_UnsuccSaveSale;
   CObj := TCustomerObject.Create;
   DB_CreateIBSQL(Qry, Tr, DataMod.aIBDatabase);
   // insert into TMPQUANT_REST
   if not InsTmpQRest(Qry) then raise Exception.Create('');

   SellID := -1;
   if DB_ExecuteSQL('SELECT GEN_ID(GEN_SALES_BON_ID,1) "GENVALUE"'+
                    ' FROM RDB$DATABASE', Qry, False, 'ExtractGen') then
     SellID := Qry.FieldByName('GENVALUE').Value;
   if SellID < 0 then raise Exception.Create('');

   // insert into SALES_BON
   if not InsSalesBon(Qry, SellID) then raise Exception.Create('');
   // insert into SALES_PLUES
   if not InsSalesPlues(Qry, SellID) then raise Exception.Create('');
   // insert into SALES_PAYMENT
   if not InsSalesPay(Qry, SellID) then raise Exception.Create('');
   // comfirm && delete TMPQUANT_REST
   if not ConfirmMarkedPlues(Qry) then raise Exception.Create('');

   if SimRChObj.FDocType = 1 then
    begin             // invoice
     BonType := 4;
     if not GetCustData(SimRChObj.FCustBulst, CObj) then raise Exception.Create(S_ErrGetCustData);
    end
   else
    BonType := 2; // fiscal receipt

   if Set_PrnPath <> '' then
    begin
     if not Create_EndBon_File(BonType, 0, SellID, CObj, Set_PrnPath, DataMod.PostException) then
       raise Exception.Create(S_ErrCreateFiscPrnFile);
    end;   

   DB_CommitIBSQL(Qry);

   if SimRChObj.FDocType = 1 then
    begin             // invoice
     Inc(CurrentFactNumb);
     SaveFactNumb;
    end;

   Result := True;
   MyMessageDlg(S_SuccSaveSale+sLineBreak+
                sLineBreak+
                S_Ammount+' '+FormatFloat('0.00', SimRChObj.FSum)+' '+BCurSign, mtInformation, [mbOK], 0);

   if not CreateReport(Qry) then exit;
  except
  on E: Exception do
   begin
    if E.Message <> '' then PostException('Error saving sale: '+E.Message);
    if Qry.SQL.Text <> '' then PostException('   SQL: '+Qry.SQL.Text);
    RollbackMarkedPlues(Qry);
    MyMessageDlg(S_UnsuccSaveSale, mtError, [mbOK], 0);
    DisplayErrMsg(ErrMsg);
   end;
  end;
 finally
  if Assigned(CObj) then FreeAndNil(CObj);
  DB_DestroyIBSQL(Qry, Tr);
 end;
end;

function TDataMod.InsTmpQRest(Qry: TIBSQL): boolean;
begin
 Result := False;
 try
  Result := DB_ExecuteSQL('INSERT INTO TMPQUANT_REST (TQR_MODULE_ID, TQR_BILLNUMB, '+
                          'TQR_SELL_BON_ID, TQR_PLU_NUMB, TQR_QUANTITY, TQR_STORAGE_ID) VALUES ('+
                          IntToStr(CurrentModuleID)       +','+ // TQR_MODULE_ID
                          '0'                             +','+ // TQR_BILLNUMB
                          '1'                             +','+ // TQR_SELL_BON_ID
                          IntToStr(SimRChObj.FPluNumb)    +','+ // TQR_PLU_NUMB
                          '-1'                            +','+ // TQR_QUANTITY
                          IntToStr(CurrentWorkStorage)    +')', // TQR_STORAGE_ID
                          Qry, False, 'InsTmpQRest');
  if not Result then raise Exception.Create('');
 except
 on E: Exception do
  PostException('Error insert into TMPQUANT_REST: '+E.Message);
 end;
end;

function TDataMod.InsSalesBon(Qry: TIBSQL; SellID: integer): boolean;

 function GetInvNumber: String;
 begin
  if SimRChObj.FDocType = 1 then Result := IntToStr(CurrentFactNumb+1)
   else Result := 'NULL';
 end;
 function GetInvDate: String;
 begin
  if SimRChObj.FDocType = 1 then Result := QuotedStr(FormatDateTime('dd.mm.yyyy', Today))
   else Result := 'NULL';
 end;
 function GetInvCustomer: String;
 begin
  if SimRChObj.FDocType = 1 then Result := QuotedStr(SimRChObj.FCustBulst)
   else Result := 'NULL';
 end;
begin
 Result := False;
 try
  Result := DB_ExecuteSQL('INSERT INTO SALES_BON (SELL_ID, SELL_TERMINAL,'+
              ' SELL_OPERATOR, SELL_OPERNAME, SELL_BONNUMB, SELL_TBLNUMB, SELL_SUM,'+
              ' SELL_CURRTYPE, SELL_CURRCOURCE, SELL_DISCSUM, SELL_FACTNUMB,'+
              ' SELL_FACTDATE, SELL_CLIENTBULST, SELL_FISCAL_) VALUES ('+
              IntToStr(SellID)                              +','+ // SELL_ID
              IntToStr(Set_CurrentTermN)                    +','+ // SELL_TERMINAL
              IntToStr(CurrentOperID)                       +','+ // SELL_OPERATOR
              QuotedStr(CurrentOperFullName)                +','+ // SELL_OPERNAME
              IntToStr(SellID)                              +','+ // SELL_BONNUMB
              '0'                                           +','+ // SELL_TBLNUMB
              FormatFloat('0.00', SimRChObj.FSum)           +','+ // SELL_SUM
              IntToStr(BCurID {SimRChObj.FSellCurrID})      +','+ // SELL_CURRTYPE
              '1'{FloatToStr(SimRChObj.FSellCCource)}       +','+ // SELL_CURRCOURCE
              '0'                                           +','+ // SELL_DISCSUM
              GetInvNumber                                  +','+ // SELL_FACTNUMB
              GetInvDate                                    +','+ // SELL_FACTDATE
              GetInvCustomer                                +','+ // SELL_CLIENTBULST
              '1'                                           +')', // SELL_FISCAL_
              Qry, False, 'InsSalesBon');
  if not Result then raise Exception.Create('');
 except
 on E: Exception do
  PostException('Error insert into SALES_BON: '+E.Message);
 end;
end;

function TDataMod.InsSalesPlues(Qry: TIBSQL; SellID: integer): boolean;
var LQty: double;
begin
 Result := False;
 try      
  Result := DB_ExecuteSQL('SELECT TQR_LEFTQUANT FROM TMPQUANT_REST'+
              ' WHERE TQR_MODULE_ID = '+IntToStr(CurrentModuleID)+
              ' AND TQR_BILLNUMB = 0 AND TQR_PLU_NUMB = '+IntToStr(SimRChObj.FPluNumb)+
              ' AND TQR_STORAGE_ID = '+IntToStr(CurrentWorkStorage), Qry, False, 'GetLeftQty');
  if not Result then raise Exception.Create('');
  if Qry.FieldByName('TQR_LEFTQUANT').IsNull then LQty := 0
   else LQty := Qry.FieldByName('TQR_LEFTQUANT').AsFloat;
  
  Result := DB_ExecuteSQL('INSERT INTO SALES_PLUES (SPLU_SELL_ID, SPLU_BONNUMB,'+
              ' SPLU_WAT, SPLU_PLUNUMB, SPLU_NAME, SPLU_SOLDQUANT, SPLU_FROMSTORAGE,'+
              ' SPLU_LEFTQUANT, SPLU_BUYPRICE, SPLU_BUYCURRENCY, SPLU_BUYCURRCOURCE,'+
              ' SPLU_SELLPRICE, SPLU_SELLCURRENCY, SPLU_CURRCOURCE, '+
              'SPLU_SELLDISCOUNT, SPLU_SERIALNO) VALUES ('+
              IntToStr(SellID)                             +','+ // SPLU_SELL_ID
              IntToStr(SellID)                             +','+ // SPLU_BONNUMB
              FloatToStr(SimRChObj.FVat)                   +','+ // SPLU_WAT
              IntToStr(SimRChObj.FPluNumb)                 +','+ // SPLU_PLUNUMB
              QuotedStr(Copy(SimRChObj.FPluName,1,255))    +','+ // SPLU_NAME
              '1'                                          +','+ // SPLU_SOLDQUANT
              IntToStr(CurrentWorkStorage)                 +','+ // SPLU_FROMSTORAGE
              FormatFloat('0.000',LQty)                    +','+ // SPLU_LEFTQUANT
              '0'{FormatFloat('0.0000',SimRChObj.FBuyPr)}  +','+ // SPLU_BUYPRICE
              IntToStr(BCurID {SimRChObj.FBuyCurrID})           +','+ // SPLU_BUYCURRENCY
              '1'{FormatFloat('0.0000',SimRChObj.FBuyCCource)}  +','+ // SPLU_BUYCURRCOURCE
              FormatFloat('0.0000', SimRChObj.FSum)             +','+ // SPLU_SELLPRICE
              IntToStr(BCurID {SimRChObj.FSellCurrID})          +','+ // SPLU_SELLCURRENCY
              '1'{FormatFloat('0.0000',SimRChObj.FsellCCource)} +','+ // SPLU_CURRCOURCE
              '0'                                               +','+ // SPLU_SELLDISCOUNT
              QuotedStr(NormalizeMSISDN(SimRChObj.FPhoneNumb))  +')', // SPLU_SERIALNO
              Qry, False, 'InsSalesPlues');
  if not Result then raise Exception.Create('');
 except
 on E: Exception do
  PostException('Error insert into SALES_PLUES: '+E.Message);
 end;
end;

function TDataMod.InsSalesPay(Qry: TIBSQL; SellID: integer): boolean;
begin
 Result := False;
 try      
  Result := DB_ExecuteSQL('INSERT INTO SALES_PAYMENT (PAY_SELL_ID,'+
              ' PAY_BONNUMB, PAY_TYPE, PAY_SUM) VALUES ('+
              IntToStr(SellID)+','+
              IntToStr(SellID)+','+
              IntToStr(SimRChObj.FPayType)+','+
              FormatFloat('0.00 ', SimRChObj.FSum)+')', Qry, False, 'InsSalesPay');
  if not Result then raise Exception.Create('');
 except
 on E: Exception do
  PostException('Error insert into SALES_PAYMENT: '+E.Message);
 end;
end;

function TDataMod.ConfirmMarkedPlues(Qry: TIBSQL): boolean;
begin
 Result := False;
 try     
  Result := DB_ExecuteSQL('UPDATE TMPQUANT_REST SET TQR_CONFIRM_QUANT_= 1'+
              ' WHERE TQR_MODULE_ID = '+IntToStr(CurrentModuleID)+
              ' AND TQR_BILLNUMB = 0', Qry, False, 'ConfirmPlues');
  if not Result then raise Exception.Create('');
  
  Result := DB_ExecuteSQL('DELETE FROM TMPQUANT_REST'+
              ' WHERE TQR_MODULE_ID = '+IntToStr(CurrentModuleID)+
              ' AND TQR_BILLNUMB = 0', Qry, False, 'ConfirmPlues');
  if not Result then raise Exception.Create('');
 except
 on E: Exception do
  PostException('Error confirm in TMPQUANT_REST: '+E.Message);
 end;
end;

function TDataMod.RollbackMarkedPlues(Qry: TIBSQL): boolean;
begin
 Result := False;
 try
  DB_RollbackIBSQL(Qry);
  Result := DB_ExecuteSQL('DELETE FROM TMPQUANT_REST'+
              ' WHERE TQR_MODULE_ID = '+IntToStr(CurrentModuleID)+
              ' AND TQR_BILLNUMB = 0', Qry, True, 'RollbackPlues');
  if not Result then raise Exception.Create('');
 except
 on E: Exception do
  PostException('Error delete from TMPQUANT_REST: '+E.Message);
 end;
end;

function TDataMod.GetOperName(OperID: integer): string;
var ibSQL: TIBSQL;
    Tr: TIBTransaction;
begin
 Result := '';
 ibSQL := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(ibSQl, Tr, aIBDatabase);

   if not DB_ExecuteSQL('SELECT OPERATOR_USERNAME FROM N_OPERATORS'+
            ' WHERE OPERATOR_ID = '+IntToStr(OperID), ibSQL, False, 'GetUserName') then raise EAbort.Create('');

   Result := ibSQL.FieldByName('OPERATOR_USERNAME').AsString;
  except
  on E: Exception do
   begin
    if E.Message <> '' then PostException(S_ErrGetOperData+': '+E.Message);
    MyMessageDlg(S_ErrGetOperData+'.'+sLineBreak+S_SeeErrForDetails, mtError, [mbOK], 0);
    DisplayErrMsg(S_ErrGetOperData+'.');
   end;
  end;
 finally
  DB_DestroyIBSQL(ibSQL, Tr);
 end;
end;

function TDataMod.GetFirstStorageNumb : Integer;
var ibSQL : TIBSQL;
    Tr    : TIBTransaction;
begin
 Result := -1;
 ibSQL := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(ibSQl, Tr, aIBDatabase);

   if not DB_ExecuteSQL('SELECT STORAGE_NUMB from N_STORAGES ORDER BY STORAGE_NUMB ASC', ibSQL, False, 'GetStorage') then
    raise EAbort.Create('');

   if ibSQL.RecordCount > 0  then Result := ibSQL.FieldByName('STORAGE_NUMB').AsInteger;

  except
  on E: Exception do
   begin
    if E.Message <> '' then PostException(S_ErrGetOperData+': '+E.Message);
    MyMessageDlg(S_ErrGetOperData+'.'+sLineBreak+S_SeeErrForDetails, mtError, [mbOK], 0);
    DisplayErrMsg(S_ErrGetOperData+'.');
   end;
  end;
 finally
  DB_DestroyIBSQL(ibSQL, Tr);
 end;
end;

function TDataMod.GetStorageName(StrgNumb: Integer): String;
var ibSQL : TIBSQL;
    Tr    : TIBTransaction;
begin
 Result:= '';
 ibSQL := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(ibSQl, Tr, aIBDatabase);

   if not DB_ExecuteSQL('SELECT STORAGE_NAME from N_STORAGES WHERE STORAGE_NUMB = '+IntToStr(StrgNumb), ibSQL, False, 'GetStorage') then
    raise EAbort.Create('');

   if ibSQL.RecordCount > 0  then Result := ibSQL.FieldByName('STORAGE_NAME').AsString;

  except
  on E: Exception do
   begin
    if E.Message <> '' then PostException(S_ErrGetOperData+': '+E.Message);
    MyMessageDlg(S_ErrGetOperData+'.'+sLineBreak+S_SeeErrForDetails, mtError, [mbOK], 0);
    DisplayErrMsg(S_ErrGetOperData+'.');
   end;
  end;
 finally
  DB_DestroyIBSQL(ibSQL, Tr);
 end;
end;

procedure TDataMod.SetDestObject(DestObj: TObject; Name_, Bulstat_: String);
var CObj: TCustomerObject;
begin
 if DestObj is TLabel then
  begin
   SimRChObj.FCustBulst:= Bulstat_;
   SimRChObj.FCustName := Name_;

   TLabel(DestObj).Caption := SimRChObj.FCustName+' ('+SimRChObj.FCustBulst+')';
   CObj := nil;
   try 
    CObj := TCustomerObject.Create;
    if not GetCustData(Bulstat_, CObj) then
      DisplayErrMsg(S_ErrGetCustData)
    else
     begin
       if Set_PrnPath <> '' then
        begin
          if not Create_Confirm_File(CObj, Set_PrnPath, DataMod.PostException) then
           DisplayErrMsg(S_ErrCreatePrnFile);
        end;
     end;  
   finally
    FreeAndNil(CObj);
   end;

   with TSetPaymentForm.Create(Self.Owner) do
    begin
     lSimInfo.Caption := S_Phone   +' '+SimRChObj.FPhoneNumb+sLineBreak+
                         S_Ammount +' '+FormatFloat('0.00', SimRChObj.FSum)+' '+BCurSign;
     FitComponents;
     if not LoadData then Close;
    end;
  end;
end;

procedure TDataMod.aIBDatabaseAfterConnect(Sender: TObject);
begin
 RollbackMarkedPlues(IBQ);
end;

procedure TDataMod.PostException(S: ShortString);
var F: TextFile;
    I: Integer;
begin
{$I-}
 AssignFile(F, ExceptPath+C_ErrFName);
 Append(F);
 if IOResult <> 0 then Rewrite(F);
 Writeln(F, FormatDateTime('DD.MM.YY HH:NN:SS', Now)+' ('+VerApp+'): '+ S);
 I := FileSize(F);
 CloseFile(F);
{$I+}
 if I > 200 then
   RenameFile(ExceptPath+C_ErrFName, ExceptPath+ChangeFileExt(C_ErrFName,'')+
              FormatDateTime('_DDMMYY_HHNNSS',Now)+'.txt');
end;

procedure TDataMod.PostException(S: String);
var F: TextFile;
    I: Integer;
begin
{$I-}
 AssignFile(F, ExceptPath+C_ErrFName);
 Append(F);
 if IOResult <> 0 then Rewrite(F);
 Writeln(F, FormatDateTime('DD.MM.YY HH:NN:SS', Now)+' ('+VerApp+'): '+ S);
 I := FileSize(F);
 CloseFile(F);
{$I+}
 if I > 200 then
   RenameFile(ExceptPath+C_ErrFName, ExceptPath+ChangeFileExt(C_ErrFName,'')+
              FormatDateTime('_DDMMYY_HHNNSS',Now)+'.txt');
end;

function TDataMod.GetCustData(CBulst: string; var CObj: TCustomerObject): boolean;
var Qry: TIBSQL;
    Tr: TIBTransaction;
begin
 Result := False;
 Qry := nil;   Tr := nil;
 try
  try
   if CBulst = '' then EAbort.Create('The bulstat of the customer is not defined.');
   DB_CreateIBSQL(Qry, Tr, DataMod.aIBDatabase);
   if not DB_ExecuteSQL('SELECT * FROM CUSTOMERS_DATA'+
            ' WHERE CUST_BULSTAT = '+QuotedStr(CBulst),
            Qry, False, 'GetCustData') then raise Exception.Create('');
   if not Qry.FieldByName('CUST_BULSTAT').IsNull then
    begin
     CObj.FBulstat  := Qry.FieldByName('CUST_BULSTAT').AsString;
     CObj.FBulstatL := Qry.FieldByName('CUST_BULST_LETTER').AsString;
     CObj.FTaxNumb  := Qry.FieldByName('CUST_TAXNUMB').AsString;
     CObj.FFirmName := Qry.FieldByName('CUST_FIRMNAME').AsString;
     CObj.FTown     := Qry.FieldByName('CUST_TOWN').AsString;
     CObj.FAddress  := Qry.FieldByName('CUST_FIRMADDRESS').AsString;
     CObj.FMOL      := Qry.FieldByName('CUST_MOL').AsString;
     CObj.FReceiver := Qry.FieldByName('CUST_RECEIVER').AsString;
     CObj.FDiscount := Qry.FieldByName('CUST_DISCOUNT').AsFloat;
    end;
   Result := True; 
  except
  on E: Exception do
   PostException('Error getting customer''s data: '+E.Message);
  end;
 finally
  DB_DestroyIBSQL(Qry, Tr);
 end; 
end;

function TDataMod.CreateReport(Qry: TIBSQL): boolean;
var O_RhID, T_RhID, RdID: integer;
begin
 Result := False;
 try
  O_RhID := 0;   T_RhID := 0;
  if not DB_ExecuteSQL('SELECT MAX(RH_ID) "MAX_RH_ID" FROM POSREPORT_HDR'+
           ' WHERE RH_TYPE = 0 AND RH_TNUMB = '+IntToStr(CurrentOperID)+
           ' AND RH_ENDDATETIME IS NULL AND RH_FISCAL_ = 1',
           Qry, False, 'GetMaxRhID') then raise Exception.Create('');
  if not Qry.FieldByName('MAX_RH_ID').IsNull then
   O_RhID := Qry.FieldByName('MAX_RH_ID').AsInteger;

  if not DB_ExecuteSQL('SELECT MAX(RH_ID) "MAX_RH_ID" FROM POSREPORT_HDR'+
           ' WHERE RH_TYPE = 1 AND RH_TNUMB = '+IntToStr(Set_CurrentTermN)+
           ' AND RH_ENDDATETIME IS NULL AND RH_FISCAL_ = 1',
           Qry, False, 'GetMaxRhID') then raise Exception.Create('');
  if not Qry.FieldByName('MAX_RH_ID').IsNull then
   T_RhID := Qry.FieldByName('MAX_RH_ID').AsInteger;
   
  if O_RhID > 0 then
   begin  
    if not DB_ExecuteSQL('UPDATE POSREPORT_HDR SET RH_BONCOUNT  = RH_BONCOUNT+1,'+
             ' RH_TOTALSUM = RH_TOTALSUM+'+FormatFloat('0.00',SimRChObj.FSum)+
             ' WHERE RH_ID = '+IntToStr(O_RhID), Qry, False, 'UpdPOSRepHdr') then raise Exception.Create('');
   end
  else
   begin
    O_RhID := 0;
    if DB_ExecuteSQL('SELECT GEN_ID(GEN_POSREPORT_HDR,1) "GENVALUE"'+
                     ' FROM RDB$DATABASE', Qry, False, 'ExtractGen') then
      O_RhID := Qry.FieldByName('GENVALUE').Value;
    if O_RhID < 0 then raise Exception.Create('');

    if not DB_ExecuteSQL('INSERT INTO POSREPORT_HDR (RH_ID, RH_TYPE, RH_TNUMB,'+
             ' RH_TNAME, RH_STARTDATETIME, RH_BONCOUNT, RH_TABLECOUNT, RH_TOTALSUM,'+
             ' RH_REVOKEDSUM, RH_REVOKEDCOUNT, RH_RETURNEDSUM, RH_RETURNEDCOUNT,'+
             ' RH_DISCSUM, RH_DISCCOUNT, RH_ADDONSUM, RH_ADDONCOUNT, RH_FIRSTBONTIME, RH_VOIDCOUNT,'+
             ' RH_VOIDSUM, RH_DEEPVOIDCOUNT, RH_DEEPVOIDSUM, RH_FISCAL_) VALUES ('+
             IntToStr(O_RhID)+',0,'+IntToStr(CurrentOperID)+','+QuotedStr(CurrentOperFullName)+','+
             QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn:ss',Now))+',1,0,'+FormatFloat('0.00',SimRChObj.FSum)+
             ',0,0,0,0,0,0,0,0,'+QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn:ss',Now))+',0,0,0,0,1)', 
             Qry, False, 'InsPOSRepHdr') then raise Exception.Create('');
   end;

  if T_RhID > 0 then
   begin  
    if not DB_ExecuteSQL('UPDATE POSREPORT_HDR SET RH_BONCOUNT  = RH_BONCOUNT+1,'+
             ' RH_TOTALSUM = RH_TOTALSUM+'+FormatFloat('0.00',SimRChObj.FSum)+
             ' WHERE RH_ID = '+IntToStr(T_RhID), Qry, False, 'UpdPOSRepHdr') then raise Exception.Create('');
   end
  else
   begin
    T_RhID := 0;
    if DB_ExecuteSQL('SELECT GEN_ID(GEN_POSREPORT_HDR,1) "GENVALUE"'+
                     ' FROM RDB$DATABASE', Qry, False, 'ExtractGen') then
      T_RhID := Qry.FieldByName('GENVALUE').Value;
    if T_RhID < 0 then raise Exception.Create('');

    if not DB_ExecuteSQL('INSERT INTO POSREPORT_HDR (RH_ID, RH_TYPE, RH_TNUMB,'+
             ' RH_TNAME, RH_STARTDATETIME, RH_BONCOUNT, RH_TABLECOUNT, RH_TOTALSUM,'+
             ' RH_REVOKEDSUM, RH_REVOKEDCOUNT, RH_RETURNEDSUM, RH_RETURNEDCOUNT,'+
             ' RH_DISCSUM, RH_DISCCOUNT, RH_ADDONSUM, RH_ADDONCOUNT, RH_FIRSTBONTIME, RH_VOIDCOUNT,'+
             ' RH_VOIDSUM, RH_DEEPVOIDCOUNT, RH_DEEPVOIDSUM, RH_FISCAL_) VALUES ('+
             IntToStr(T_RhID)+',1,'+IntToStr(Set_CurrentTermN)+','+QuotedStr(Set_CurrentTermName)+','+       
             QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn:ss',Now))+',1,0,'+FormatFloat('0.00',SimRChObj.FSum)+
             ',0,0,0,0,0,0,0,0,'+QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn:ss',Now))+',0,0,0,0,1)', 
             Qry, False, 'InsPOSRepHdr') then raise Exception.Create('');
   end;
  // POSREPORT_DATA
  RdID := 0;
  if not DB_ExecuteSQL('SELECT MAX(RD_ID) "MAX_RD_ID" FROM POSREPORT_DATA'+
           ' WHERE RD_HDRID  = '+IntToStr(O_RhID)+' AND RD_TYPE = ''P'''+
           ' AND RD_PAYTYPEID = '+IntToStr(SimRChObj.FPayType), Qry, False, 'GetMaxRhID') then raise Exception.Create('');
  if not Qry.FieldByName('MAX_RD_ID').IsNull then
   RdID := Qry.FieldByName('MAX_RD_ID').AsInteger;
  if RdID > 0 then
   begin
    if not DB_ExecuteSQL('UPDATE POSREPORT_DATA SET RD_COUNT  = RD_COUNT+1,'+
             ' RD_SUM = RD_SUM+'+FormatFloat('0.00',SimRChObj.FSum)+
             ' WHERE RD_ID = '+IntToStr(RdID), Qry, False, 'UpdPOSRepData') then raise Exception.Create('');
   end
  else
   begin
    if not DB_ExecuteSQL('INSERT INTO POSREPORT_DATA (RD_HDRID,'+
             ' RD_TYPE, RD_COUNT, RD_SUM, RD_PAYTYPEID, RD_PAYTYPENAME) VALUES ('+
             IntToStr(O_RhID)+',''P'',1,'+FormatFloat('0.00',SimRChObj.FSum)+','+IntToStr(SimRChObj.FPayType)+
             ','+QuotedStr(SimRChObj.FPayName)+')', Qry, False, 'UpdPOSRepData') then raise Exception.Create('');
   end;


  RdID := 0;
  if not DB_ExecuteSQL('SELECT MAX(RD_ID) "MAX_RD_ID" FROM POSREPORT_DATA'+
           ' WHERE RD_HDRID = '+IntToStr(T_RhID)+' AND RD_TYPE = ''P'''+
           ' AND RD_PAYTYPEID = '+IntToStr(SimRChObj.FPayType), Qry, False, 'GetMaxRhID') then raise Exception.Create('');
  if not Qry.FieldByName('MAX_RD_ID').IsNull then
   RdID := Qry.FieldByName('MAX_RD_ID').AsInteger;
  if RdID > 0 then
   begin
    if not DB_ExecuteSQL('UPDATE POSREPORT_DATA SET RD_COUNT  = RD_COUNT+1,'+
             ' RD_SUM = RD_SUM+'+FormatFloat('0.00',SimRChObj.FSum)+
             ' WHERE RD_ID = '+IntToStr(RdID), Qry, False, 'UpdPOSRepData') then raise Exception.Create('');
   end
  else
   begin
    if not DB_ExecuteSQL('INSERT INTO POSREPORT_DATA (RD_HDRID,'+
             ' RD_TYPE, RD_COUNT, RD_SUM, RD_PAYTYPEID, RD_PAYTYPENAME) VALUES ('+
             IntToStr(T_RhID)+',''P'',1,'+FormatFloat('0.00',SimRChObj.FSum)+','+IntToStr(SimRChObj.FPayType)+
             ','+QuotedStr(SimRChObj.FPayName)+')', Qry, False, 'UpdPOSRepData') then raise Exception.Create('');
   end;

  DB_CommitIBSQL(Qry);
  Result := True;
 except
 on E: Exception do
  begin
   PostException('Error insert into POSREPORT_HDR and POSREPORT_DATA: '+E.Message);
   MyMessageDlg(S_ErrCreateReport, mtError, [mbOK], 0);   
  end; 
 end;
end;

function TDataMod.TurnoverForToday(var SellSum: double): boolean;
var ibSQL: TIBSQL;
    Tr: TIBTransaction;
begin
 Result := False;
 ibSQL := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(ibSQl, Tr, DataMod.aIBDatabase);

   if not DB_ExecuteSQL('SELECT SUM(cast((SPLU_SELLPRICE*SPLU_CURRCOURCE*SPLU_SOLDQUANT)as numeric(10,2))+'+
           ' cast((SPLU_SOLDQUANT*SPLU_SELLPRICE*SPLU_CURRCOURCE*SPLU_SELLDISCOUNT/100) as numeric(10,2))) "TOTAL_SELL"'+
            ' FROM SALES_BON'+sLineBreak+
            ' LEFT JOIN SALES_PLUES ON SALES_BON.SELL_ID = SALES_PLUES.SPLU_SELL_ID'+
            ' WHERE SELL_OPERATOR = '+IntToStr(CurrentOperID)+' AND SELL_REVOKED_ = 0 AND SPLU_REVOKED_ = 0'+
            ' AND SPLU_DATETIME BETWEEN '+QuotedStr(FormatDateTime('dd.mm.yyyy', Today)+' 00:00:00')+
            ' AND '+QuotedStr(FormatDateTime('dd.mm.yyyy', Today)+' 23:59:59')+
            ' AND SPLU_PLUNUMB = '+IntToStr(SimRChObj.FPluNumb), ibSQL, False, 'GetSells') then raise EAbort.Create('');
   if not ibSQL.FieldByName('TOTAL_SELL').IsNull then
     SellSum := SellSum+ibSQL.FieldByName('TOTAL_SELL').AsFloat;
   Result := True;
  except
  on E: Exception do
   begin
    if E.Message <> '' then DataMod.PostException(S_ErrCalcTrunover+': '+E.Message);
    MyMessageDlg(S_ErrCalcTrunover+'.'+sLineBreak+S_SeeErrForDetails, mtError, [mbOK], 0);
    DisplayErrMsg(S_ErrCalcTrunover+'.');
   end;
  end;
 finally
  DB_DestroyIBSQL(ibSQL, Tr);
 end;
end;

function TDataMod.LimitsInfo: string;
var Sum_: double;
begin
 Result := '';
 Sum_ := 0;
 if DataMod.TurnoverForToday(Sum_) then
  begin
   if (SimRChObj.FMaxSumDay > 0) then
    begin
     if (Sum_ >= SimRChObj.FMaxSumDay) then
       Result := '('+S_PassedDayLimit+')'
     else
      begin
       Sum_ := SimRChObj.FMaxSumDay-Sum_;
       if (Sum_ > SimRChObj.FMaxSumTr) and (SimRChObj.FMaxSumTr > 0) then Sum_ := SimRChObj.FMaxSumTr;
       if Sum_ < Set_Vivacom_MinAmnt then
         Result := '('+S_LimitIsSmallerThanMinMO+'.)'
       else
        begin
         if (Sum_ > Set_Vivacom_MaxAmnt) and (Set_Vivacom_MaxAmnt > 0) then Sum_ := Set_Vivacom_MaxAmnt;
         Result := '('+S_SumFrom+' '+FormatFloat('0.00 ',Set_Vivacom_MinAmnt)+BCurSign+
                   ' '+S_To+' '+FormatFloat('0.00 ',Sum_)+BCurSign+'.)';
        end;
      end;
    end
   else
    begin
     if SimRChObj.FMaxSumTr <= 0 then Sum_ := -1// no limit
      else Sum_ := SimRChObj.FMaxSumTr;
     if Sum_ <= 0 then
      begin
       if Set_Vivacom_MinAmnt > 0 then
        begin
         if Set_Vivacom_MaxAmnt > 0 then
          Result := '('+S_SumFrom+' '+FormatFloat('0.00 ',Set_Vivacom_MinAmnt)+BCurSign+
                    ' '+S_To+' '+FormatFloat('0.00 ', Set_Vivacom_MaxAmnt)+BCurSign+'.)'
         else
          Result := '('+S_SumAbove+' '+FormatFloat('0.00 ',Set_Vivacom_MinAmnt)+BCurSign+'.)';
        end
       else
        begin
         if Set_Vivacom_MaxAmnt > 0 then
          Result := '('+S_SumFrom+' 0.00 '+BCurSign+' '+S_To+' '+FormatFloat('0.00 ', Set_Vivacom_MaxAmnt)+BCurSign+'.)'
         else
          Result := '('+S_NoLimit+'.)';
        end;
      end
     else
      begin
       if Sum_ < Set_Vivacom_MinAmnt then
         Result := '('+S_LimitIsSmallerThanMinMO+'.)'
       else
        begin
         if Sum_ > Set_Vivacom_MaxAmnt then Sum_ := Set_Vivacom_MaxAmnt;
         Result := '('+S_SumTo+' '+FormatFloat('0.00 ',Sum_)+BCurSign+'.)';
        end;
      end;
    end;
  end;
end;

function TDataMod.CheckPrePaid(MSISDN: String): boolean;
var Res: Integer;
begin
 // за дебъг цели
 if SameText(Set_Vivacom_URL, 'test') then
  begin
   DisplayErrMsg(S_MobOprTestMode);
   Result := true; Sleep(1000); Exit;
  end;

 Result := false;
 with TSimRechargeVivacom.Create(Set_Vivacom_URL, IntToStr(CurrentOfficeNumb)) do
 try
  case CheckPrePaid(MSISDN, Res) of
  resOK:      begin
                Result := (Res = 0);
                if not Result then
                 begin
                  DisplayErrMsg(S_ErrCheckPrePaid+sLineBreak+TranslateResultCodeCheck(Res));
                  MyMessageDlg(S_ErrCheckPrePaid+sLineBreak+sLineBreak+
                               TranslateResultCodeCheck(Res), mtError, [mbOK], 0);
                 end;
              end;
  resErrSys:  begin
                DisplayErrMsg(S_ErrConnectSystem+sLineBreak+sLineBreak+LastError);
                PostException('[VIVACOM]'+LastError);
                MyMessageDlg(S_ErrConnectSystem, mtError, [mbOK], 0);
              end;
  resErrComm: begin
                DisplayErrMsg(S_ErrConnectServer+sLineBreak+sLineBreak+LastError);
                PostException('[VIVACOM]'+LastError);
                MyMessageDlg(S_ErrConnectServer, mtError, [mbOK], 0);
              end;
  end;
 finally
  Free;
 end;
end;

function TDataMod.PayPrePaid(MSISDN: string; Ammount: double): boolean;
var Res: integer;
begin
 // за дебъг цели
 if SameText(Set_Vivacom_URL, 'test') then
  begin
   DisplayErrMsg(S_MobOprTestMode);
   Result := true; Sleep(1000); Exit;
  end;

 Result := false;
 with TSimRechargeVivacom.Create(Set_Vivacom_URL, IntToStr(CurrentOfficeNumb)) do
 try
  case PayPrePaid(MSISDN, Ammount, Res) of
  resOK:      begin
                Result := (Res = 0);
                if not Result then
                 begin
                  DisplayErrMsg(S_ErrPayPrePaid+sLineBreak+TranslateResultCodeCheck(Res));
                  MyMessageDlg (S_ErrPayPrePaid+sLineBreak+sLineBreak+
                               TranslateResultCodePay(Res), mtError, [mbOK], 0);
                 end;
              end;
  resErrSys:  begin
                DisplayErrMsg(S_ErrConnectSystem+sLineBreak+sLineBreak+LastError);
                PostException('[VIVACOM]'+LastError);
                MyMessageDlg(S_ErrConnectSystem, mtError, [mbOK], 0);
              end;
  resErrComm: begin
                DisplayErrMsg(S_ErrConnectServer+sLineBreak+sLineBreak+LastError);
                PostException('[VIVACOM]'+LastError);
                MyMessageDlg(S_ErrConnectServer, mtError, [mbOK], 0);
              end;
  end;
 finally
  Free;
 end;

end;

function TDataMod.LoadFactNumb: Boolean;
var F: TextFile;
    S: String;
begin
 Result := FileExists(LocalIniPath + C_InvoiceNumbIFile);
 if not Result then Exit;

 AssignFile(F, LocalIniPath + C_InvoiceNumbIFile);
{$I-}
 Reset(F);
 if IOResult = 0 then
  begin
   Readln(F,S);
   try
    CurrentFactNumb := StrToInt64(S);
   except
    Result := False;
   end;
  end
 else Result := False;
 CloseFile(F);
{$I+}
end;

procedure TDataMod.SaveFactNumb;
var F: TextFile;
begin
 AssignFile(F, LocalIniPath + C_InvoiceNumbIFile);
{$I-}
 Rewrite(F);
 if IOResult = 0 then
  Writeln(F, IntToStr(CurrentFactNumb))
 else
  DataMod.PostEvent(13,S_ErrOnSavingCurrInvN);
 CloseFile(F);
{$I+}
end;


function TDataMod.GetPayPrnNumbAndName(PayID: Integer; var PayPrnNumb: Integer; var PayPrnName: String): Boolean;
begin
 Result := True;

 try
  if not DB_ExecuteSQL('SELECT * FROM N_PAYMENT_TYPES WHERE PAYTYPE_NUMB='+IntToStr(PayID),
                        IBQ, False, 'GetPaymentPrnName') then
     raise EAbort.Create('Fail load payment prn numb and name');
  if (IBQ.RecordCount = 0) then
     raise EAbort.Create('Fail load payment prn numb and name. Not found payment with id '+IntToStr(PayID));
  PayPrnNumb := IBQ.FieldByName('PAYTYPE_PRNNUMB').AsInteger;
  PayPrnName := IBQ.FieldByName('PAYTYPE_PRNNAME').AsString;
 except
  on E:Exception do
    begin
     Result := False;
    end
 end;
end;

end.
