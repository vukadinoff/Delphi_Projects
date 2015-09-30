unit CCDeviceHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit, XMLHandler, Contnrs, DB;

type
 TCCApn = record
  LimitBytes : Cardinal; // Максимален лимит за ползване - задава се от сървъра
  LimitLeft  : Cardinal; // Останало от лимита - изчислява се
  LimitPerc  : Real;
  SentBytes  : Cardinal;
  ReceBytes  : Cardinal;
  ErrorData  : Boolean;
 end;

 TCCIdentData = record
  ModSerial : String;
  FDModel   : String;
  FDSerial  : String;
  FMSerial  : String;
  SimImsi   : String;
  Version   : String;
  NraType   : Integer;
  NraRegId  : Integer;
 end;

 TCCStatusData = record
  ZRepSend  : Word;
  ZRepSaved : Word;
  Apn1      : TCCApn;
  Apn2      : TCCApn;
  PayedTo   : TDateTime;
 end;

 THndr_CCServer = class(THandlerServer)
 private
  FCCIdent  : TCCIdentData;
  FCCStatus : TCCStatusData;
 protected
  procedure LoadIdentFromXML(iNode: IXMLNode);
  procedure LoadStatusFromXML(iNode: IXMLNode);
 public
  property CCIdentData: TCCIdentData read FCCIdent write FCCIdent;
  property CCStatus: TCCStatusData read FCCStatus write FCCStatus;
 end;

 // Ъпдейт на данни за устройството в базата
 // Грижи се за абонамента и лимитите - добавя задачи за това
 THndr_CCDevice = class(THndr_CCServer)
 private
  procedure FUpdateDeviceStatistics(var LastDeviceSession: TDateTime);
  procedure FUpdateDeviceGprs;
  procedure FUpdateDeviceFU;
  procedure FUpdateSim;
  procedure FUpdateApnTraffic(ApnId: Byte; CCApn: TCCApn; NewLimit: Word; CustEik, DealerEik: String; TaskId: Integer);
  procedure FInsertDeviceAction(ActType: Integer; ActComment, ActData: String);
  function  FAddTaskHeader(TaskType, Period: Integer; Comment: String; Start: TDateTime): Integer;
  procedure FUpdateTaskHeader(TaskID, Period: Integer; Start: TDateTime);
  function  FGetInstantTaskID(Comment: String): Integer;
  procedure FUpdateTaskOperation(TaskID: Integer; OprType, OprName: String; OprValue: String='');
  procedure FUpdateTaskComment(TaskID: Integer; Comment: String);
  function  FApnNeedTrafficUpdate(CCApn: TCCApn; NewLimit:Cardinal): Boolean;
  function  FCalcModemPingTime(PingPeriodDays: Integer): TDateTime;
  function  FCalcModemPingPeriod(PingPeriodDays: Integer): Integer;
  function  FCalcModemPingComment(PingPeriodDays: Integer): String;
  function  FGenFd_RChangeReq(NewDate: TDateTime; var FileRows, FileRChange: String): Boolean;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function AddAnswerToDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
 end;

 // Обработва отговорите на получените задачи
 // Изпраща новите чакащи задачи
 // таг <tsk>
 TCCTaskOperation = class(TObject)
  OprType  : String;        // par/file/rep
  OprName  : String;
  OprValue : String;
  OprFrom  : TDateTime;     // използват се при отчет
  OprTo    : TDateTime;     // използват се при отчет
  OprErr   : Boolean;       // флаг за грешка
 end;

 TCCTaskHeader = class(THandlerXML)
 private
  FTaskId     : Integer;
  FTaskType   : Integer;
  FTaskStart  : TDateTime;
  FTaskPeriod : Integer;
  FTaskOpers  : TObjectList; // използва се един списък за входящи и изходящи
 published
  procedure LoadFromXML(iNode: IXMLNode);
  procedure SaveToXML(iNode: IXMLNode);
  procedure SaveToTextXml(var TextXml: String);
 public
  constructor Create(XmlDoc: IXMLDocument);
  destructor Destroy; override;
 end;

 THndr_CCTask = class(THndr_CCDevice)
 private
  FTaskList : TObjectList;
  function FGetSentTasksIdList: String;
  function FExtractTaskAsXmlText(TaskType: Integer; var TaskId: Integer; var TaskXml: String): Boolean;
  procedure FHandleOperationResult(Tsk: TCCTaskHeader; Opr: TCCTaskOperation);
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function AddAnswerToDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;

  property SentTasksIdList: String read FGetSentTasksIdList;
 end;

 // Обработва събития
 // таг <event>
 TCCEvent = record
  EvType: String;
  EvData: String;
 end;

 THndr_CCEvent = class(THndr_CCServer)
 private
  FTaskSnd: String;
  FEvents : array of TCCEvent;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function AddAnswerToDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;

  property TaskSentList: String read FTaskSnd write FTaskSnd;
 end;

 THndr_CCReport = class(THndr_CCServer)
 private
  FTaskId  : Integer;
  FRepText : String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function AddAnswerToDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
 end;


implementation
uses BillingConstUnit, Math, DateUtils, XMLHandlerMS, DBInterfaceUnit, RegistrationXmlUnit,
     FiscalizationTypes;

//**************************************************************************************************
//   THndr_CCServer
//**************************************************************************************************
procedure THndr_CCServer.LoadIdentFromXML(iNode: IXMLNode);
begin
  FCCIdent.ModSerial := XML_GetNodeText(iNode, xml_NodeCC_DevSerMod);
  FCCIdent.FDModel   := XML_GetNodeText(iNode, xml_NodeCC_DevModel);
  FCCIdent.FDSerial  := XML_GetNodeText(iNode, xml_NodeCC_DevSerFu);
  FCCIdent.FMSerial  := XML_GetNodeText(iNode, xml_NodeCC_DevMFM);
  FCCIdent.SimImsi   := XML_GetNodeText(iNode, xml_NodeCC_DevImsi);
  FCCIdent.Version   := XML_GetNodeText(iNode, xml_NodeCC_DevVer);
  FCCIdent.NraType   := XML_GetNodeInt (iNode, xml_NodeCC_DevNraType, false);
  FCCIdent.NraRegId  := XML_GetNodeInt (iNode, xml_NodeCC_DevRegId, false);
end;

procedure THndr_CCServer.LoadStatusFromXML(iNode: IXMLNode);
var S : String;

    function SubStr(var Src_: String): String;
    var JJ : Integer;
    begin
     JJ := Pos(',', Src_);
     if JJ > 0 then
      begin
       Result := Copy(Src_, 1, JJ-1);
       Delete(Src_, 1, JJ);
      end
     else
      begin
       Result := Src_;
       Src_   := ''
      end;
    end;
begin
  FCCStatus.ZRepSend  := XML_GetNodeInt(iNode, xml_NodeCC_DevZSend);
  FCCStatus.ZRepSaved := XML_GetNodeInt(iNode, xml_NodeCC_DevZSaved);

  try
   S := XML_GetNodeText(iNode, xml_NodeCC_DevPayedTo);
   if S = '' then Abort;
   FCCStatus.PayedTo := XSDateTimeToDateTime(S);
  except
   FCCStatus.PayedTo := 0;
  end;

  S := XML_GetNodeText(iNode, xml_NodeCC_DevApn1ctr);
  with FCCStatus.Apn1 do
  try
   LimitBytes := StrToInt64(SubStr(S));
   SentBytes  := StrToInt64Def(SubStr(S), 0);
   ReceBytes  := StrToInt64Def(SubStr(S), 0);
   LimitLeft  := 0;
   LimitPerc  := 100;
   ErrorData  := false;

   if LimitBytes > 0 then LimitPerc := RoundTo((SentBytes + ReceBytes) / LimitBytes*100, -2);
   if LimitBytes > (SentBytes + ReceBytes) then LimitLeft := LimitBytes - SentBytes - ReceBytes;
  except
   ErrorData := true;
  end;

  S := XML_GetNodeText(iNode, xml_NodeCC_DevApn2ctr);
  with FCCStatus.Apn2 do
  try
   LimitBytes := StrToInt64(SubStr(S));
   SentBytes  := StrToInt64Def(SubStr(S), 0);
   ReceBytes  := StrToInt64Def(SubStr(S), 0);
   LimitLeft  := 0;
   LimitPerc  := 100;
   ErrorData  := false;

   if LimitBytes > 0 then LimitPerc := RoundTo((SentBytes + ReceBytes) / LimitBytes*100, -2);
   if LimitBytes > (SentBytes + ReceBytes) then LimitLeft := LimitBytes - SentBytes - ReceBytes;
  except
   ErrorData := true;
  end;
end;

//**************************************************************************************************
//   THndr_CCDevice
//**************************************************************************************************
constructor THndr_CCDevice.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
end;

destructor THndr_CCDevice.Destroy;
begin
 inherited Destroy;
end;

function THndr_CCDevice.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
begin
 try
  iNode   := XML_GetRootNode;

  LoadIdentFromXML(iNode); // raise exception on fail
  LoadStatusFromXML(iNode); // raise exception on fail

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

function THndr_CCDevice.AddAnswerToDocument: Boolean;
begin
 try

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

procedure THndr_CCDevice.FUpdateDeviceStatistics(var LastDeviceSession: TDateTime);
var SQL : String;        // Запис на статистическа информация за връзката
    Ra  : Integer;       // последно обаждане, брой обаждания ...
begin
 with Device.DbInterface do
 try
    // запис на основни параметри - статтистика
    SQL := 'SELECT CS_SIMIMSI, CS_MFM, CS_ZSEND, CS_ZSAVED, CS_SVCEND, CS_MODEL, CS_LOCALPORT, CS_LASTSESSION '+
           'FROM CC_STATISTICS '+
           'WHERE (CS_MDSERIAL = '+StrToSQL(CCIdentData.ModSerial, 13)+')'+
           'AND(CS_FDSERIAL = '+StrToSQL(CCIdentData.FDSerial, 13)+')';
    if not FillDataSet(SQL) then raise EAbort.Create('Select CC_STATISTICS fail:'+Device.DbInterface.LastError);
    if DataSet.IsEmpty then
     begin
      LastDeviceSession := 0;
      SQL := 'INSERT INTO CC_STATISTICS(CS_MDSERIAL, CS_FDSERIAL, CS_LASTSESSION, CS_SESSIONCNT, '+
             'CS_SIMIMSI, CS_MFM, CS_ZSEND, CS_ZSAVED, CS_SVCEND, CS_MODEL, CS_LOCALPORT, '+
             'CS_APN1USAGE, CS_APN2USAGE, CS_APN1LMT, CS_APN2LMT)VALUES('+
             StrToSQL(CCIdentData.ModSerial, 10)       + ','+ //  [CS_MDSERIAL] varchar(10) NOT NULL,
             StrToSQL(CCIdentData.FDSerial, 10)        + ','+ //  [CS_FDSERIAL] varchar(10) NOT NULL,
             DateTimeToSQL(Now)                        + ','+ //  [CS_LASTSESSION] datetime DEFAULT getdate() NOT NULL,
             IntToSql(1)                               + ','+ //  [CS_SESSIONCNT] int DEFAULT 1 NOT NULL,
             StrToSQL(CCIdentData.SimImsi, 32)         + ','+ //  [CS_SIMIMSI] varchar(32) NOT NULL,
             StrToSQL(CCIdentData.FMSerial, 10)        + ','+ //  [CS_MFM] varchar(10) NULL,
             IntToSQL(CCStatus.ZRepSend)               + ','+ //  [CS_ZSEND] int NULL,
             IntToSQL(CCStatus.ZRepSaved)              + ','+ //  [CS_ZSAVED] int NULL,
             DateTimeToSQL(CCStatus.PayedTo )          + ','+ //  [CS_SVCEND] datetime NULL,
             StrToSQL(CCIdentData.FDModel, 50)         + ','+ //  [CS_MODEL] varchar(50) COLLATE Cyrillic_General_CI_AS NULL,
             IntToSql(Device.ConnectionInfo.LocalPort) + ','+ //  [CS_LOCALPORT] int NULL,
             FloatToSql(CCStatus.Apn1.LimitPerc)       + ','+ //  [CS_APN1USAGE] float NULL,
             FloatToSql(CCStatus.Apn2.LimitPerc)       + ','+ //  [CS_APN2USAGE] float NULL,
             FloatToSql(CCStatus.Apn1.LimitBytes/1024) + ','+ //  [CS_APN1LMT] float NULL,
             FloatToSql(CCStatus.Apn2.LimitBytes/1024) + ')'; //  [CS_APN2LMT] float NULL,
     end
    else
     begin
      LastDeviceSession := DataSet.FieldByName('CS_LASTSESSION').AsDateTime;

      SQL := 'UPDATE CC_STATISTICS SET '+
             'CS_SESSIONCNT = CS_SESSIONCNT + 1, '+
             'CS_LASTSESSION = '+ DateTimeToSQL(Now)                        +','+
             'CS_APN1USAGE = '  + FloatToSql(CCStatus.Apn1.LimitPerc)       +','+
             'CS_APN2USAGE = '  + FloatToSql(CCStatus.Apn2.LimitPerc)       +','+
             'CS_APN1LMT = '    + FloatToSql(CCStatus.Apn1.LimitBytes/1024) +','+
             'CS_APN2LMT = '    + FloatToSql(CCStatus.Apn2.LimitBytes/1024) +' ';
      if DataSet.FieldByName('CS_SIMIMSI').AsString <> CCIdentData.SimImsi then SQL := SQL + ',CS_SIMIMSI = '+StrToSQL(CCIdentData.SimImsi, 32);
      if DataSet.FieldByName('CS_MFM').AsString <> CCIdentData.FMSerial then    SQL := SQL + ',CS_MFM = '    +StrToSQL(CCIdentData.FMSerial, 10);
      if DataSet.FieldByName('CS_ZSEND').AsInteger <> CCStatus.ZRepSend then    SQL := SQL + ',CS_ZSEND = '  +IntToSql(CCStatus.ZRepSend);
      if DataSet.FieldByName('CS_ZSAVED').AsInteger <> CCStatus.ZRepSaved then  SQL := SQL + ',CS_ZSAVED = ' +IntToSql(CCStatus.ZRepSaved);
      if DataSet.FieldByName('CS_SVCEND').AsDateTime <> CCStatus.PayedTo then   SQL := SQL + ',CS_SVCEND = ' +DateTimeToSQL(CCStatus.PayedTo);
      if DataSet.FieldByName('CS_MODEL').AsString <> CCIdentData.FDModel then   SQL := SQL + ',CS_MODEL = '+StrToSQL(CCIdentData.FDModel, 50);
      if DataSet.FieldByName('CS_LOCALPORT').AsInteger <> Device.ConnectionInfo.LocalPort then SQL := SQL + ',CS_LOCALPORT = '+IntToSql(Device.ConnectionInfo.LocalPort);
      SQL := SQL + ' WHERE (CS_MDSERIAL = '+StrToSQL(CCIdentData.ModSerial, 13)+')'+
                   'AND(CS_FDSERIAL = '+StrToSQL(CCIdentData.FDSerial, 13)+')'
     end;
    if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Insert CC_STATISTICS fail: '+Device.DbInterface.LastError);
 finally
  CloseDataSet;
 end;
end;

procedure THndr_CCDevice.FUpdateDeviceGprs;
var SQL : String;        // Актуализира или въвежда наново данни за ФУ в базата;
    Ra  : Integer;       // Добре е всяко обадило се устройство да фигурира като такова в БД
begin
 Ra := 0;
 with Device.DbInterface do
 try
  // провекка на данните за модем в базата
  SQL := 'SELECT DC_FUDEVICE, DC_SIMIMSI, DC_VERSION '+
         'FROM DEVICES_GPRS '+
         'WHERE DC_SERIAL = '+ StrToSQL(CCIdentData.ModSerial, 10);
  if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_GPRS fail:'+Device.DbInterface.LastError);
  if DataSet.IsEmpty then
   begin
    SQL := 'INSERT INTO DEVICES_GPRS(DC_SERIAL, DC_FUDEVICE, DC_SIMIMSI, DC_VERSION, DC_TESTMODE, DC_DEALEREIK, DC_DEALERBRANCH) '+
           'SELECT TOP 1 '+
           StrToSQL(CCIdentData.ModSerial, 10)     + ',' + // [DC_SERIAL] varchar(10) NOT NULL,
           StrToSQL(CCIdentData.FDSerial, 10)      + ',' + // [DC_FUDEVICE] varchar(10) NULL,
           StrToSQL(CCIdentData.SimImsi, 32)       + ',' + // [DC_SIMIMSI] varchar(32) NULL,
           StrToSQL(CCIdentData.Version, 20)       + ',' + // [DC_VERSION] varchar(20) NULL,
           BoolToSql(CCIdentData.NraRegId<3000000) + ',' + // [DC_TESTMODE] int DEFAULT 0 NOT NULL,
           'DB.DB_DEALEREIK'                       + ',' + // [DC_DEALEREIK] varchar(13) NOT NULL,
           'DB.DB_ID '+                                    // [DC_DEALERBRANCH] int NOT NULL,
           'FROM DEALERS_BRANCHES DB ORDER BY DB.DB_DEALEREIK ASC';

    if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Insert DEVICES_GPRS fail: '+Device.DbInterface.LastError);
   end
  else
   begin
    // проверка дали трябва да се промени нещо
    if (not SameText(DataSet.FieldByName('DC_FUDEVICE').AsString, CCIdentData.FDSerial))or
       (not SameText(DataSet.FieldByName('DC_SIMIMSI').AsString,  CCIdentData.SimImsi))or
       (not SameText(DataSet.FieldByName('DC_VERSION').AsString,  CCIdentData.Version)) then
     begin
      SQL := 'UPDATE DEVICES_GPRS SET '+
             'DC_FUDEVICE = '+StrToSQL(CCIdentData.FDSerial, 10) +','+  // [DC_FUDEVICE] varchar(10) NULL,
             'DC_SIMIMSI = '+StrToSQL(CCIdentData.SimImsi, 32)   +','+  // [DC_SIMIMSI] varchar(32) NULL,
             'DC_VERSION = '+StrToSQL(CCIdentData.Version, 20)   +' '+  // [DC_VERSION] varchar(20) NULL,
             'WHERE DC_SERIAL = '+ StrToSQL(CCIdentData.ModSerial, 10);
      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update DEVICES_GPRS fail: '+Device.DbInterface.LastError);
     end;
   end;

  if Ra > 0 then
   begin
     // remove duplicate SIM
     SQL := 'UPDATE DEVICES_GPRS SET '+
            'DC_SIMIMSI = '+ StrToSQL('000000000000000', 32) +' '+ // [DC_SIMIMSI] varchar(32) NULL,
            'WHERE (DC_SIMIMSI = '+StrToSQL(CCIdentData.SimImsi, 32)+')'+
            'AND(DC_SERIAL <> '+StrToSQL(CCIdentData.ModSerial, 10)+')';
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_GPRS.DC_SIMIMSI Fail: '+Device.DbInterface.LastError);

     // remove duplicate FD
     SQL := 'UPDATE DEVICES_GPRS SET '+
            'DC_FUDEVICE = '+ StrToSQL('0000000000', 10) +' '+ // [DC_FUDEVICE] varchar(10) NULL,
            'WHERE (DC_FUDEVICE = '+StrToSQL(CCIdentData.FDSerial, 10)+')'+
            'AND(DC_SERIAL <> '+StrToSQL(CCIdentData.ModSerial, 10)+')';
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_GPRS.DC_FUDEVICE Fail: '+Device.DbInterface.LastError);
   end;
 finally
  CloseDataSet;
 end;
end;

procedure THndr_CCDevice.FUpdateDeviceFU;
var SQL : String;        // Актуализира или въвежда наново данни за ФУ в базата;
    Ra  : Integer;       // Добре е всяко обадило се устройство да фигурира като такова в БД
    DevModel : Integer;
    DevStatus: Integer;
    DlrEik   : String;
    DlrBrnch : Integer;
    CustEik  : String;
    CustSite : Integer;
begin
 Ra := 0;
 with Device.DbInterface do
 try
  // провекка на данните за ФУ в базата
  SQL := 'SELECT DF_DEVICETYPE, DF_MFM, DF_MODEL, DF_GPRSDEVICE, DF_NRAREGID, DF_SIMIMSI, DF_FISCALIZATIONDATE, '+
         'DF_FIRSTFISCALIZATION, DF_FIRSTNRAREGID '+
         'FROM DEVICES_FU '+
         'WHERE DF_SERIAL = '+ StrToSQL(CCIdentData.FDSerial, 10);
  if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU fail:'+Device.DbInterface.LastError);
  if DataSet.IsEmpty then
   begin
    SQL := 'SELECT DFM_ID FROM DEVICES_FU_MODELS WHERE DFM_MODELNAME like '+QuotedStr('%'+CCIdentData.FDModel+'%');
    if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU_MODELS fail:'+Device.DbInterface.LastError);
    if DataSet.IsEmpty then raise EHandledException.Create(errcode_CC_UnknownFDModel, 'Unknown device model!', 'Unknown device model reported to the system (APN2)');
    DevModel := DataSet.FieldByName('DFM_ID').AsInteger;

    SQL := 'SELECT TOP 1 C.CUST_EIK, S.CUSTS_ID, D.D_EIK, B.DB_ID '+
           'FROM CUSTOMERS C '+
           'LEFT JOIN CUSTOMER_SITES S ON C.CUST_EIK = S.CUSTS_CUSTEIK '+
           'LEFT JOIN DEALERS D ON D.D_EIK = C.CUST_RESPDEALER '+
           'LEFT JOIN DEALERS_BRANCHES B ON B.DB_DEALEREIK = D.D_EIK '+
           'WHERE ISNUMERIC(C.CUST_EIK) = 1 '+
           'ORDER BY C.CUST_EIK ASC';
    if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU_MODELS fail:'+Device.DbInterface.LastError);
    if DataSet.IsEmpty then raise EHandledException.Create(errcode_CC_UnknownFDModel, 'Unknown device dealer!', 'Fail to obtain default dealer and customer!');

    DlrEik   := DataSet.FieldByName('D_EIK').AsString;
    DlrBrnch := DataSet.FieldByName('DB_ID').AsInteger;
    CustEik  := DataSet.FieldByName('CUST_EIK').AsString;
    CustSite := DataSet.FieldByName('CUSTS_ID').AsInteger;
    CloseDataSet;

    if CCIdentData.NraRegId > 0 then DevStatus := 2
     else DevStatus := 0;

    SQL := 'INSERT INTO DEVICES_FU(DF_SERIAL, DF_DEVICETYPE, DF_MFM, DF_MODEL, DF_GPRSDEVICE, DF_SIMIMSI, '+
           'DF_DEALEREIK, DF_DEALERBRANCH, DF_CUSTEIK, DF_CUSTSITE, DF_COMMENT, ';
    if CCIdentData.NraRegId > 0 then
     SQL := SQL + 'DF_FISCALIZATIONDATE, DF_FIRSTFISCALIZATION, DF_NRAREGID, DF_FIRSTNRAREGID, DF_TESTMODE, ';

    SQL := SQL + 'DF_STATUS)VALUES('+
    StrToSQL(CCIdentData.FDSerial, 10)  +','+ // [DF_SERIAL] varchar(10) NOT NULL,
    IntToSql(CCIdentData.NraType)       +','+ // [DF_DEVICETYPE] int DEFAULT 0 NOT NULL,
    StrToSQL(CCIdentData.FMSerial, 10)  +','+ // [DF_MFM] varchar(10) NOT NULL,
    IntToSql(DevModel)                  +','+ // [DF_MODEL] int NOT NULL,
    StrToSQL(CCIdentData.ModSerial, 10) +','+ // [DF_GPRSDEVICE] varchar(10) NOT NULL,
    StrToSQL(CCIdentData.SimImsi, 32)   +','+ // [DF_SIMIMSI] varchar(32) NOT NULL,
    StrToSQL(DlrEik, 13)                +','+ // [DF_DEALEREIK] varchar(13) NOT NULL,
    IntToSql(DlrBrnch)                  +','+ // [DF_DEALERBRANCH] int NOT NULL,
    StrToSQL(CustEik, 13)               +','+ // [DF_CUSTEIK] varchar(13) NULL,
    IntToSql(CustSite)                  +','+ // [DF_CUSTSITE] int NULL,
    StrToSQL('Нотификация по ANP2')     +','; // [DF_COMMENT] varchar(max) NULL,

    if CCIdentData.NraRegId > 0 then
     SQL := SQL + DateToSQL(Date)                + ','+ // [DF_FISCALIZATIONDATE] datetime NULL,
                  DateToSQL(Date)                + ','+ // [DF_FIRSTFISCALIZATION] datetime NULL,
                  IntToSql(CCIdentData.NraRegId) + ','+ // [DF_NRAREGID] int NULL,
                  IntToSql(CCIdentData.NraRegId) + ','+ // [DF_FIRSTNRAREGID] int NULL,
                  BoolToSql(CCIdentData.NraRegId<3000000) + ','; // [DF_TESTMODE] int DEFAULT 0 NOT NULL,

    SQL := SQL + IntToSql(DevStatus)             +')'; // [DF_STATUS] int DEFAULT 0 NOT NULL,

    if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Insert DEVICES_FU fail: '+Device.DbInterface.LastError);
   end
  else
   begin
    // проверка дали трябва да се промени нещо
    if (not SameText(DataSet.FieldByName('DF_SIMIMSI').AsString,    CCIdentData.SimImsi))or
       (not SameText(DataSet.FieldByName('DF_GPRSDEVICE').AsString, CCIdentData.ModSerial))or
       (not SameText(DataSet.FieldByName('DF_MFM').AsString,        CCIdentData.FMSerial))or
       ((DataSet.FieldByName('DF_NRAREGID').IsNull)and(CCIdentData.NraRegId > 0)) then
     begin
      SQL := 'UPDATE DEVICES_FU SET '+
             'DF_SIMIMSI = '     + StrToSQL(CCIdentData.SimImsi, 32)  +','+
             'DF_GPRSDEVICE = '  + StrToSQL(CCIdentData.ModSerial, 10)+','+
             'DF_MFM = '         + StrToSQL(CCIdentData.FMSerial, 10) +' ';
      if (CCIdentData.NraRegId > 0) then
       begin
        if DataSet.FieldByName('DF_NRAREGID').IsNull then          SQL := SQL + ', DF_NRAREGID = '+IntToSql(CCIdentData.NraRegId)+
                                                                                ', DF_TESTMODE = '+BoolToSql(CCIdentData.NraRegId < 3000000);
        if DataSet.FieldByName('DF_FIRSTNRAREGID').IsNull then      SQL := SQL + ', DF_FIRSTNRAREGID = '+IntToSql(CCIdentData.NraRegId);
        if DataSet.FieldByName('DF_FISCALIZATIONDATE').IsNull then  SQL := SQL + ', DF_FISCALIZATIONDATE = '+DateToSQL(Date);
        if DataSet.FieldByName('DF_FIRSTFISCALIZATION').IsNull then SQL := SQL + ', DF_FIRSTFISCALIZATION = '+DateToSQL(Date);

       end;
      SQL := SQL + ' WHERE DF_SERIAL = '+ StrToSQL(CCIdentData.FDSerial, 10);

      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update DEVICES_FU fail: '+Device.DbInterface.LastError);
     end;
   end;

  if Ra > 0 then
   begin
     // remove duplicate SIM
     SQL := 'UPDATE DEVICES_FU SET '+
            'DF_SIMIMSI = '+ StrToSQL('000000000000000', 32) +' '+ // [DF_SIMIMSI] varchar(32) NULL,
            'WHERE (DF_SIMIMSI = '+StrToSQL(CCIdentData.SimImsi, 32)+')'+
            'AND(DF_SERIAL <> '+StrToSQL(CCIdentData.FDSerial, 10)+')';
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_GPRS.DC_SIMIMSI Fail: '+Device.DbInterface.LastError);

     // remove duplicate GPRS
     SQL := 'UPDATE DEVICES_FU SET '+
            'DF_GPRSDEVICE = '+ StrToSQL('0000000000', 10) +' '+ // [DF_GPRSDEVICE] varchar(10) NULL,
            'WHERE (DF_GPRSDEVICE = '+StrToSQL(CCIdentData.ModSerial, 10)+')'+
            'AND(DF_SERIAL <> '+StrToSQL(CCIdentData.FDSerial, 10)+')';
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_GPRS.DC_FUDEVICE Fail: '+Device.DbInterface.LastError);
   end;
 finally
  CloseDataSet;
 end;
end;

procedure THndr_CCDevice.FUpdateSim;
var SQL : String;        // Актуализира данни за SIM в базата;
    Ra  : Integer;
begin
 Ra := 0;
 with Device.DbInterface do
 try
  // провекка на данните за ФУ в базата
  SQL := 'SELECT  SIM_FISCALDEVICE, SIM_GPRSDEVICE FROM SIM WHERE SIM_IMSI = '+ StrToSQL(CCIdentData.SimImsi, 32);
  if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail:'+Device.DbInterface.LastError);
  if not DataSet.IsEmpty then
   begin
    // проверка дали трябва да се промени нещо
    if (not SameText(DataSet.FieldByName('SIM_FISCALDEVICE').AsString, CCIdentData.FDSerial))or
       (not SameText(DataSet.FieldByName('SIM_GPRSDEVICE').AsString,   CCIdentData.ModSerial))then
     begin
      SQL := 'UPDATE SIM SET '+
             'SIM_FISCALDEVICE = ' + StrToSQL(CCIdentData.FDSerial, 10)      +','+
             'SIM_GPRSDEVICE = '   + StrToSQL(CCIdentData.ModSerial, 10)     +','+
             'SIM_TESTMODE = '     + BoolToSql(CCIdentData.NraRegId<3000000) +' '+
             'WHERE SIM_IMSI = '+ StrToSQL(CCIdentData.SimImsi, 32);
      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update SIM fail: '+Device.DbInterface.LastError);
     end;
   end;

  // проверка за извадена карта от друго устройство
  if Ra > 0 then
   begin
     // remove duplicate SIM
     SQL := 'UPDATE SIM SET '+
            'SIM_FISCALDEVICE = NULL '+
            'WHERE (SIM_FISCALDEVICE = '+StrToSQL(CCIdentData.FDSerial, 10)+')'+
            'AND(SIM_IMSI <> '+StrToSQL(CCIdentData.SimImsi, 32)+')';
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM.SIM_FISCALDEVICE Fail: '+Device.DbInterface.LastError);

     // remove duplicate GPRS
     SQL := 'UPDATE SIM SET '+
            'SIM_GPRSDEVICE = NULL '+
            'WHERE (SIM_GPRSDEVICE = '+StrToSQL(CCIdentData.ModSerial, 10)+')'+
            'AND(SIM_IMSI <> '+StrToSQL(CCIdentData.SimImsi, 32)+')';
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM.SIM_GPRSDEVICE Fail: '+Device.DbInterface.LastError);
   end;
 finally
  CloseDataSet;
 end;
end;

procedure THndr_CCDevice.FUpdateApnTraffic(ApnId: Byte; CCApn: TCCApn; NewLimit: Word; CustEik, DealerEik: String; TaskId: Integer);
var SQL : String;
    Ra  : Integer;
begin
 with Device.DbInterface do
 try
  SQL := 'INSERT INTO CC_TRAFFIC(CTF_MODSERIAL, CTF_FDSERIAL, CTF_SIMIMSI, CTF_APNID, CTF_NEWLIMIT, CTF_BYTESLIMIT, '+
         'CTF_BYTESREAD, CTF_BYTESSEND, CTF_DATETIME, CTF_CUSTOMEREIK, CTF_DEALEREIK, CTF_TASKHDRID)VALUES('+
         StrToSQL(CCIdentData.ModSerial, 10) +','+ // [CTF_MODSERIAL] varchar(10) NOT NULL,
         StrToSQL(CCIdentData.FDSerial, 10)  +','+ //  [CTF_FDSERIAL] varchar(10) NOT NULL,
         StrToSQL(CCIdentData.SimImsi, 32)   +','+ //  [CTF_SIMIMSI] varchar(32) NOT NULL,
         IntToSql(ApnId)                     +','+ //  [CTF_APNID] int NOT NULL,
         IntToSql(NewLimit)                  +','+ //  [CTF_NEWLIMIT] int NOT NULL,
         IntToStr(CCApn.LimitBytes)          +','+ //  [CTF_BYTESLIMIT] int NULL,
         IntToStr(CCApn.ReceBytes)           +','+ //  [CTF_BYTESREAD] int NULL,
         IntToStr(CCApn.SentBytes)           +','+ //  [CTF_BYTESSEND] int NULL,
         DateTimeToSQL(Now)                  +','+ //  [CTF_DATETIME] datetime NOT NULL,
         StrToSQL(CustEik, 13)               +','+ //  [CTF_CUSTOMEREIK] varchar(13) NULL,
         StrToSQL(DealerEik, 13)             +','+ //  [CTF_DEALEREIK] varchar(13) NULL,
         IntToSql(TaskId)                    +')'; //  [CTF_TASKHDRID] int NOT NULL,
  if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Insert CC_TRAFFIC Fail: '+Device.DbInterface.LastError);
 finally
  CloseDataSet;
 end;
end;

procedure THndr_CCDevice.FInsertDeviceAction(ActType: Integer; ActComment, ActData: String);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
   SQL := 'INSERT INTO DEVICES_FU_ACTIONS (DFA_DEVFUSERIAL, DFA_DEVGPRSSERIAL, DFA_SIMIMSI, '+
          'DFA_DEALEREIK, DFA_CUSTEIK, DFA_DEALERBRANCH, DFA_CUSTSITE, '+
          'DFA_ACTIONTYPE, DFA_COMMENT, DFA_DATA) '+
          'SELECT DF.DF_SERIAL, DF.DF_GPRSDEVICE, DF.DF_SIMIMSI, '+
          'DF.DF_DEALEREIK, DF.DF_CUSTEIK, DF.DF_DEALERBRANCH, DF.DF_CUSTSITE, '+
          IntToSql(ActType)               + ', '+ // [DFA_ACTIONTYPE] int NULL,
          StrToSQL(ActComment, 100, true) + ', '+ // [DFA_COMMENT] varchar(100) NULL,
          StrToSQL(ActData, 0, true)      + '  '+ // [DFA_DATA] varchar(max) NULL,
          'FROM DEVICES_FU DF '+
          'WHERE DF.DF_SERIAL = ' + StrToSQL(CCIdentData.FDSerial, 10);

   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEVICES_FU_ACTIONS: '+Device.DbInterface.LastError);
  end;
end;

function THndr_CCDevice.FAddTaskHeader(TaskType, Period: Integer; Comment: String; Start: TDateTime): Integer;
var SQL : String;
begin
 Result := 0;
 with Device.DbInterface do
 try
    SQL := 'INSERT INTO CC_TASK_HDR(CTH_MODSERIAL, CTH_FDSERIAL, CTH_SIMIMSI, CTH_TYPE, CTH_START, '+
           'CTH_PERIOD, CTH_COMMENT)VALUES('+
           StrToSQL(CCIdentData.ModSerial , 10) +', '+ //  [CTH_MODSERIAL] varchar(10) NOT NULL,
           StrToSQL(CCIdentData.FDSerial , 10)  +', '+ //  [CTH_FDSERIAL] varchar(10) NOT NULL,
           StrToSQL(CCIdentData.SimImsi , 32)   +', '+ //  [CTH_SIMIMSI] varchar(32) NOT NULL,
           IntToSql(TaskType)                   +', '+ //  [CTH_TYPE] int NOT NULL,
           DateTimeToSQL(Start)                 +', '+ //  [CTH_START] datetime NOT NULL,
           IntToSql(Period)                     +', '+ //  [CTH_PERIOD] int NOT NULL,
           StrToSQL(Comment)                    +');'; //  [CTH_COMMENT] varchar(max) NULL,
    SQL := SQL+' SELECT @@IDENTITY AS "CTH_ID";';
    if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
    Result := DataSet.FieldByName('CTH_ID').AsInteger;
    CommitTransaction;
 finally
  CloseDataSet;
 end;
 if Result <= 0 then raise EAbort.Create('System error. Get new CTH_ID fail!');
end;

procedure THndr_CCDevice.FUpdateTaskHeader(TaskID, Period: Integer; Start: TDateTime);
var SQL : String;
    Ra  : Integer;
begin
 with Device.DbInterface do
 begin
  SQL := 'UPDATE CC_TASK_HDR SET '+
         'CTH_START = '+      DateTimeToSQL(start) +','+
         'CTH_PERIOD = '+     IntToSql(Period)     +','+
         'CTH_SEND = NULL '+
         'WHERE CTH_ID = '+   IntToSql(DataSet.FieldByName('CTH_ID').AsInteger);
  if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update CC_TASK_HDR fail:'+Device.DbInterface.LastError);
 end;
end;

procedure THndr_CCDevice.FUpdateTaskComment(TaskID: Integer; Comment: String);
var SQL    : String;     //
    Ra     : Integer;
begin
 with Device.DbInterface do
  begin
   SQL := 'UPDATE CC_TASK_HDR SET '+
          'CTH_COMMENT = CTH_COMMENT + ' + StrToSQL(Comment)+' '+
          'WHERE CTH_ID = ' + IntToSql(TaskID);
   ExecuteSQLStatement(SQL, Ra);
  end;
end;

function THndr_CCDevice.FGetInstantTaskID(Comment: String): Integer;
var SQL : String;    // извлича ID на неизпълнена от ЦЦ задача. Ако няма такава създава нова
    Ra  : Integer;
begin
 Result := 0;
 with Device.DbInterface do
 try
  SQL := 'SELECT CTH_ID, CTH_START, CTH_SEND FROM CC_TASK_HDR '+
         'WHERE (CTH_MODSERIAL = '+StrToSQL(CCIdentData.ModSerial, 10)+')'+
         'AND(CTH_FDSERIAL = '+StrToSQL(CCIdentData.FDSerial, 10)+')'+
         'AND(CTH_TYPE = 0)AND(CTH_RECEIVE IS NULL)';
  if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
  if DataSet.IsEmpty then
   begin
    CloseDataSet;
    Result := FAddTaskHeader(0, 0, Comment, Now);
   end
  else
   begin
    Result := DataSet.FieldByName('CTH_ID').AsInteger;

    // имаме задача която е пратена; Би трябвало да е изпълнена но няма отговор по нея
    // при такава ситуация пращаме задачата отново (може модема да не я е чул)
    // има ситуация при която модема праща два пъти (от 2 нишки) един и същи трафик -
    if (not DataSet.FieldByName('CTH_SEND').IsNull) then
     begin
      if not ExecuteSQLStatement('UPDATE CC_TASK_HDR SET CTH_SEND = NULL WHERE CTH_ID = '+IntToSql(Result), Ra) then
       raise EAbort.Create('Update CC_TASK_HDR.CTH_SEND fail:'+Device.DbInterface.LastError);
     end;
   end;

  if Result <= 0 then raise EAbort.Create('System error. Get new CTH_ID fail!');
 finally
  CloseDataSet;
 end;
end;

procedure THndr_CCDevice.FUpdateTaskOperation(TaskID: Integer; OprType, OprName: String; OprValue: String='');
var SQL    : String;     // Добавя операции към задачата. Ако операциите все още не са изпратени към ЦЦ - се редактират
    Ra     : Integer;
    OprMde : Integer;
begin
 if OprValue = '' then OprMde := 0 // четене на данни
  else OprMde := 1;                // запис на данни

 with Device.DbInterface do
 try
  SQL := 'SELECT CTO_ID FROM CC_TASK_OPR '+
         'WHERE (CTO_TASKID = '+IntToSql(TaskID)+')'+
         'AND(CTO_TYPE = '+StrToSQL(OprType, 20)+')'+
         'AND(CTO_NAME = '+StrToSQL(OprName, 100)+')'+
         'AND(CTO_RESULT IS NULL)';
  if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
  if (not DataSet.IsEmpty)and(DataSet.FieldByName('CTO_ID').AsInteger > 0) then
   begin
    SQL := 'UPDATE CC_TASK_OPR SET '+
           'CTO_MODE = '  + IntToSql(OprMde)            +','+ // [CTO_MODE] int NOT NULL,
           'CTO_VALUE = ' + StrToSQL(OprValue, 0, true) +' '+ // [CTO_VALUE] varchar(max) NULL,
           'WHERE CTO_ID = '+DataSet.FieldByName('CTO_ID').AsString;
   end
  else
   begin
    SQL := 'INSERT INTO CC_TASK_OPR(CTO_TASKID, CTO_MODE, CTO_TYPE, CTO_NAME, CTO_VALUE)VALUES('+
           IntToSql(TaskID)            +','+ // [CTO_TASKID] int NOT NULL,
           IntToSql(OprMde)            +','+ // [CTO_MODE] int NOT NULL,
           StrToSQL(OprType, 20)       +','+ // [CTO_TYPE] varchar(20) NOT NULL,
           StrToSQL(OprName, 100)      +','+ // [CTO_NAME] varchar(100) NOT NULL,
           StrToSQL(OprValue, 0, true) +')'; // [CTO_VALUE] varchar(max) NULL,
   end;
  if not ExecuteSQLStatement(SQL, Ra) then  raise EAbort.Create('Insert/Update CC_TASK_OPR fail: '+Device.DbInterface.LastError);
 finally
  CloseDataSet;
 end;
end;

function THndr_CCDevice.FGenFd_RChangeReq(NewDate: TDateTime; var FileRows, FileRChange: String): Boolean;
var ARows : TNRA_AdvLines;
    AReq  : TNRA_Request;
    RegDta: TRegistrationData;
    SQL   : String;
    I     : Integer;

    function FirstNotEmpty(Str1_, Str2_: String): String;
    begin
     if Str1_ <> '' then Result := Str1_
     else
     if Str2_ <> '' then Result := Str2_
     else
      Result := '';
    end;
    function FormatTaxEIK(Src: String): String;
    begin
     Result := '';
     if Src <> '' then Result := 'ЗДДС N '+Src;
    end;
{    function XmlEncode(SrcXml: String): String;
    var I : Integer;
    begin
     Result := '';
     for I := 1 to length(SrcXml) do
      case SrcXml[I] of
      '&' : Result := Result + '&amp;';
      '<' : Result := Result + '&lt;';
      '>' : Result := Result + '&gt;';
      '"' : Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
      else  Result := Result + SrcXml[I];
      end;
    end;}
    function FormatStreetCode(const StrCode: string): string;
    begin
      Result:= StrCode;
      while Length(Result) < 5 do Result:= '0' + Result;
    end;
begin
 Result     := false;
 FileRows   := '';
 FileRChange:= '';
 try
  // извличане на рекламните редове от базата
  // взема се последната заявка и текущите данни за ФУ
  with Device.DbInterface do
  try
    SQL := 'SELECT DF.DF_SERIAL, DF.DF_DEALEREIK, DF.DF_SIMIMSI, DF.DF_STATUS, '+
           'CD.CUST_EIK, CD.CUST_EIKTYPE, CD.CUST_VATNUMBER, CD.CUST_NAME, CD.CUST_SHORTADDRESS, '+
           'CS.CUSTS_NUMBER, CS.CUSTS_TYPE, CS.CUSTS_NAME, CS.CUSTS_SHORTADDRESS, '+
           'CS.CUSTS_TOWNCODE, CS.CUSTS_TOWNNAME, CS.CUSTS_AREACODE, CS.CUSTS_AREANAME, '+
           'CS.CUSTS_STREETCODE, CS.CUSTS_STREETNAME, CS.CUSTS_STREETNO, CS.CUSTS_BLOCK, '+
           'CS.CUSTS_ENTRANCE, CS.CUSTS_FLOOR, CS.CUSTS_APARTMENT, '+
           'S.SIM_MSISDN, S.SIM_OPERATORCODE, D.D_EIKTYPE, '+
           'DA.DFA_ACTIONTYPE, DA.DFA_DATA '+
           'FROM DEVICES_FU DF '+
           'LEFT JOIN DEVICES_FU_ACTIONS DA ON DF.DF_FISCALIZATIONREQ = DA.DFA_ID '+
           'LEFT JOIN CUSTOMERS CD ON DF.DF_CUSTEIK = CD.CUST_EIK '+
           'LEFT JOIN CUSTOMER_SITES CS ON DF.DF_CUSTSITE = CS.CUSTS_ID '+
           'LEFT JOIN SIM S ON DF.DF_SIMIMSI = S.SIM_IMSI '+
           'LEFT JOIN DEALERS D ON DF.DF_DEALEREIK = D.D_EIK '+
           'WHERE DF.DF_SERIAL = '+StrToSQL(CCIdentData.FDSerial);
    if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail:'+Device.DbInterface.LastError);
    if (not DataSet.IsEmpty)and
       (not DataSet.FieldByName('DFA_DATA').IsNull)and
       (not DataSet.FieldByName('CUSTS_TYPE').IsNull)and
       (DataSet.FieldByName('DF_STATUS').AsInteger = 2)and
       (DataSet.FieldByName('DFA_ACTIONTYPE').AsInteger in [2, 6])and
       (CCIdentData.NraRegId > 0) then
     begin
      RegDta := TRegistrationData.Create;
      try
       if RegDta.LoadFromXML(DataSet.FieldByName('DFA_DATA').AsString) then
        begin
         // Генериране на файл с рекламите редове
         ARows[1] := FirstNotEmpty(RegDta.FiscDevEx.AdvLines[1], DataSet.FieldByName('CUST_NAME').AsString);
         ARows[2] := FirstNotEmpty(RegDta.FiscDevEx.AdvLines[2], DataSet.FieldByName('CUST_SHORTADDRESS').AsString);
         ARows[3] := FirstNotEmpty(RegDta.FiscDevEx.AdvLines[3], DataSet.FieldByName('CUSTS_NAME').AsString);
         ARows[4] := FirstNotEmpty(RegDta.FiscDevEx.AdvLines[4], DataSet.FieldByName('CUSTS_SHORTADDRESS').AsString);
         ARows[5] := FirstNotEmpty(RegDta.FiscDevEx.AdvLines[5], FormatTaxEIK(DataSet.FieldByName('CUST_VATNUMBER').AsString));
         ARows[6] := RegDta.FiscDevEx.AdvLines[6];
         ARows[7] := RegDta.FiscDevEx.AdvLines[7];
         ARows[8] := RegDta.FiscDevEx.AdvLines[8];

         if RegDta.FiscDevEx.LineLength >= 22 then ARows[9] := 'Сервиз ЕИК: '+ DataSet.FieldByName('DF_DEALEREIK').AsString
          else ARows[9] := 'Сервиз#'+ DataSet.FieldByName('DF_DEALEREIK').AsString;
         if RegDta.FiscDevEx.LineLength >= 22 then ARows[10] := 'Договор до: ' + FormatDateTime('DD.MM.YY', NewDate)
          else ARows[10] := 'Дог.до ' + FormatDateTime('DD.MM.YY', NewDate);

         for I := 1 to 10 do ARows[I] := Copy(ARows[I], 1, RegDta.FiscDevEx.LineLength);

         // Генериране на заявката за промяна обстоятелства
         AReq.ReqType := rtChange;

         AReq.FDevType    := RegDta.FiscDev.NRAType;                             // Тип на фискалното устройство 1-ЕКАФП;	2-ФПр; 3-ЕСФП за течни горива; 4-ИАСУТД;
         AReq.CustEIK     := DataSet.FieldByName('CUST_EIK').AsString;           // ЕИК номер; само цифри [9,13]
         AReq.CustEIKType := DataSet.FieldByName('CUST_EIKTYPE').AsInteger;      // ЕИК тип номер 0-Булстат; 1-ЕГН; 2-ЛНЧ; 3-Служебен Номер
         AReq.FDevSerial  := CCIdentData.FDSerial;                               // Идентификационен номер на фискално устройство
         AReq.FDevMFM     := CCIdentData.FMSerial;                               // Идентификационен номер на фискална памет
         AReq.FDevCertN   := RegDta.FiscDevEx.CertifNumber;                      // Номер на свидетелство на ФУ/ИАСУТД.
         AReq.SimIMSI     := DataSet.FieldByName('DF_SIMIMSI').AsString;         // IMSI номер на сим карта
         AReq.SimMSISDN   := DataSet.FieldByName('SIM_MSISDN').AsString;         // MSISDN номер на сим карта във фомрат 359XXXXXXXXX.
         AReq.SimOperN    := DataSet.FieldByName('SIM_OPERATORCODE').AsInteger;  // Мобилен оператор 	0-Mtel;	1-Globul;	2-Vivacom;
         AReq.OrgName     := DataSet.FieldByName('CUST_NAME').AsString;          // Име на фирма
         AReq.PSetNum     := DataSet.FieldByName('CUSTS_NUMBER').AsString;       // Номер на обект
         AReq.PSetType    := DataSet.FieldByName('CUSTS_TYPE').AsInteger;        // Тип на обекта  - по списък
         AReq.PSetName    := DataSet.FieldByName('CUSTS_NAME').AsString;         // Име на обект, напр. Магазин Дондуков.
         AReq.StartDate   := RegDta.Services.StartDate;                          // Дата на встъпване в експлоатация
         AReq.SvceEIK     := DataSet.FieldByName('DF_DEALEREIK').AsString;       // ЕИК номер на сервиз
         AReq.SvceEIKType := DataSet.FieldByName('D_EIKTYPE').AsInteger;         // ЕИК тип номер на сервиз (най-често БУЛСТАТ) 0-Булстат; 1-ЕГН; 2-ЛНЧ; 3-Служебен Номер
         AReq.SvceContrExpire:= NewDate;                                         // Дата на изтичане на договора със сервиза
         AReq.FDRID       := IntToStr(CCIdentData.NraRegId);                     // Регистрационен идентификатор. Генерира се от НАП по време на регистрация.

         AReq.PSetAddr.SettlCode  := Trim(DataSet.FieldByName('CUSTS_TOWNCODE').AsString);   // Код на населеното място по ЕКАТТЕ
         AReq.PSetAddr.SettlName  := Trim(DataSet.FieldByName('CUSTS_TOWNNAME').AsString);   // Settlement - Име на населено място (град или село). Напр. София
         AReq.PSetAddr.AreaCode   := Trim(DataSet.FieldByName('CUSTS_AREACODE').AsString);   // Код на населеното място по ЕКАТТЕ плюс район(area)
         AReq.PSetAddr.AreaName   := Trim(DataSet.FieldByName('CUSTS_AREANAME').AsString);   // Име на район напр. р-н Искър
         AReq.PSetAddr.StrCode    := FormatStreetCode(Trim(DataSet.FieldByName('CUSTS_STREETCODE').AsString)); // Код на пътната артерия от номенклатурата на пътните артерии към служба ГРАО
         AReq.PSetAddr.StrName    := Trim(DataSet.FieldByName('CUSTS_STREETNAME').AsString); // Име на улица
         AReq.PSetAddr.StrNo      := Trim(DataSet.FieldByName('CUSTS_STREETNO').AsString);   // Номер в улица
         AReq.PSetAddr.Block      := Trim(DataSet.FieldByName('CUSTS_BLOCK').AsString);      // Номер на блок
         AReq.PSetAddr.En         := Trim(DataSet.FieldByName('CUSTS_ENTRANCE').AsString);   // Вход в блок
         AReq.PSetAddr.Fl         := Trim(DataSet.FieldByName('CUSTS_FLOOR').AsString);      // Етаж
         AReq.PSetAddr.Ap         := Trim(DataSet.FieldByName('CUSTS_APARTMENT').AsString);  // Номер апартамент

         Result := true;
        end;
      finally
       RegDta.Free;
      end;
     end;
  finally
   CloseDataSet;
  end;

  if Result then
   begin
     with TNRA_Xml.Create() do
     try
      FileRows    := {XmlEncode(}GenerateAdvLines(ARows);
      FileRChange := {XmlEncode(}GenerateNRARequest(AReq);
     finally
      Free;
     end;
   end;
 except
  on E: Exception do
   begin
    Result := false;
    Device.PostEventSystem(C_EvType_Error, 'APN2 Fail generate RCHANGE/ROWS file:'+sLineBreak+
                                            E.Message, Self.ClassName+'/Execute');

   end;
 end;
end;

function THndr_CCDevice.FApnNeedTrafficUpdate(CCApn: TCCApn; NewLimit: Cardinal): Boolean;
begin
 Result := true;
 // при грешни данни правим ъпдейт на броячите
 if CCApn.ErrorData then Exit;

 // Имаме някакъв нов лимит И наличния лимит е паднал под 20%
 if (NewLimit > 0)and(CCApn.LimitLeft < (CCApn.LimitBytes div 5)+10) then Exit;

 // Новия лимит е 0; имали някакъв стар лимит, който вече е изчерпан
 // Ако не е изчерпан пращаме винаги остатъка и зацикляме комуникацията
 if (NewLimit = 0)and(CCApn.LimitBytes > 0)and(CCApn.LimitLeft = 0) then Exit;

 Result := false;
end;

function THndr_CCDevice.FCalcModemPingTime(PingPeriodDays: Integer): TDateTime;
var N1, N2 : Integer;
begin
 // модема праща след като мине периода в минути
 // Тоест първо пращане е часа + период
 if PingPeriodDays <= 0 then
  begin
   Result := EndOfTheYear(IncYear(Now, 3));
  end
 else
  begin
   Result := Now;

   // разпределение на часа спрямо последните 2 цифри от номера
   N1 := StrToIntDef(Copy(CCIdentData.FDSerial, Length(CCIdentData.FDSerial),   1), RandomRange(0, 9));
   N2 := StrToIntDef(Copy(CCIdentData.FDSerial, Length(CCIdentData.FDSerial)-1, 1), RandomRange(0, 9));
   case N2 of
   0: ReplaceTime(Result, EncodeTime(N1,    RandomRange( 0, 11), 0, 0));
   1: ReplaceTime(Result, EncodeTime(N1,    RandomRange(12, 23), 0, 0));
   2: ReplaceTime(Result, EncodeTime(N1,    RandomRange(24, 35), 0, 0));
   3: ReplaceTime(Result, EncodeTime(N1,    RandomRange(36, 47), 0, 0));
   4: ReplaceTime(Result, EncodeTime(N1,    RandomRange(48, 59), 0, 0));
   5: ReplaceTime(Result, EncodeTime(N1+13, RandomRange( 0, 11), 0, 0));
   6: ReplaceTime(Result, EncodeTime(N1+13, RandomRange(12, 23), 0, 0));
   7: ReplaceTime(Result, EncodeTime(N1+13, RandomRange(24, 35), 0, 0));
   8: ReplaceTime(Result, EncodeTime(N1+13, RandomRange(36, 47), 0, 0));
   9: ReplaceTime(Result, EncodeTime(N1+13, RandomRange(48, 59), 0, 0));
   end;
  end;
end;

function THndr_CCDevice.FCalcModemPingPeriod(PingPeriodDays: Integer): Integer;
begin
 // периода се задава в минути
 // за тестови устройства 1 час
 if PingPeriodDays <= 0 then
  begin
   Result := 100 * 24 * 60; // 100 dni
  end
 else
  begin
   Result := PingPeriodDays * 24 * 60;
  end;
end;

function THndr_CCDevice.FCalcModemPingComment(PingPeriodDays: Integer): String;
begin
 Result := 'Ping Eltrade ('+IntToStr(PingPeriodDays)+' days)';
end;

function THndr_CCDevice.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL    : String;
    Ra     : Integer;
    TaskID : Integer;
    SvcEnd : TDateTime;
    PktA1  : Word;
    PktA2  : Word;
    PingD  : Integer;
    DlrEik : String;
    CustEik: String;
    TskCmnt: String;
    SRows  : String;
    SRChge : String;
    LastSes: TDateTime;
begin
 Result    := true;
 ErrCode   := errcode_ExecuteSucceed;
 UserError := '';
 TaskID    := 0;
 TskCmnt   := '';  // Коментар към задачата
 PktA1     := 250; // Пакет в КБ по подразбиране за активиране на изчерпан лимит в CC - НАП
 PktA2     := 0;   // Пакет в КБ по подразбиране за активиране на изчерпан лимит в CC - други нужди
 try
   with Device.DbInterface do
   try
    // запис на основни параметри - статтистика за устройството. Таблица: CC_STATISTICS
    // Така взимаме от базата кога е била последната сесеия с устройството
    // Ако сме се заприказвали и си разменяме множество пакети няма нужда да правим част от проверките
    FUpdateDeviceStatistics(LastSes);

    if MinutesBetween(LastSes, Now) > 5 then
     begin
      // провекка на данните за модем в базата; Прави ъпдейт на някои полета или дабовя нов запис
      FUpdateDeviceGprs; // raise exception on fail

      // проверка на данните за ФУ в базата; Прави ъпдейт на някои полета или дабовя нов запис
      FUpdateDeviceFU; // raise exception on fail

      // проверка на данните за SIM в базата; Прави ъндейт на ФД и Модем ако е нужно
      FUpdateSim; // raise exception on fail
     end;

    // ФОРМИРАНЕ НЕЗАБАВНА ЗАДАЧА ЗА МОДЕМА - тип 0

    if (MinutesBetween(LastSes, Now) >= 1)or(CCStatus.PayedTo < Date) then
     begin
      // проверка на абонамента; задаване на задача за промяната му
      // Четене на валидността на СИМ картата (до кога е платено)
      SQL := 'SELECT SIM_STATUSOPERATOR, SIM_PAYEDTODATE FROM SIM '+
             'WHERE SIM_IMSI = '+StrToSQL(CCIdentData.SimImsi);
      if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail:'+Device.DbInterface.LastError);
      if not DataSet.IsEmpty then
       begin
        // проверка на датата до която е платен SIM
        if DataSet.FieldByName('SIM_PAYEDTODATE').IsNull then SvcEnd := EncodeDate(2010,10,10)
         else SvcEnd := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;

        if Abs(DaysBetween(CCStatus.PayedTo, SvcEnd)) >= 1 then
         begin
          // Имаме промяна във валидността на СИМ картата
          // Вземи ID на неизпълнена затача с незабавно изпълнение
          if TaskID = 0 then TaskID := FGetInstantTaskID('Автоматична при проверка абонамент SIM');

          FUpdateTaskOperation(TaskID,
                               xml_NodeCC_Parameter,    // параметър
                               xml_NodeCC_DevPayedTo,   // карайна дата абонамент СИМ
                               DateTimeToXSDateTime(SvcEnd));
          TskCmnt := TskCmnt + sLineBreak + 'Променен абонамент SIM: '+DateToStr(CCStatus.PayedTo)+' -> '+DateToStr(SvcEnd);

          // Създаване на файл за промяна на рекламните редове
          if (CCIdentData.NraRegId > 0)and                   // ФУ е регистрирано
             (CCStatus.PayedTo > EncodeDate(2011,01,01))and  // картата не е блокирана
             (SvcEnd > Date) then                            // крайната дата е напред във времето
           begin
            // устройството е фискализирано и има зададена крайна дата
            // при това положение явно промяната на датата идва от базата
            // генерираме файловете кото да пратим към ФУ. Ако не усеем поради липса на данни в системата не пращаме нищо
            if FGenFd_RChangeReq(SvcEnd, SRows, SRChge) then
             begin
              // Създаваме задача от тип 5 - startup. Самото изпращане на задачата става като XML през файл
              // Трябва да записваме XML файл в startup директорията на модема
              // XML файла се генерира на базата на създадените задачи тип 5
              // тези задачи (5) не се изпращат директно като задачи, но фигурират като такива в базата защото ще получим отговор по тях
              // Трябва да има задача тип 0 с която да изпратим XML файловете - ако няма такава задача - добавяме
              if TaskID = 0 then TaskID := FGetInstantTaskID('Автоматична за изпращане на startup задачи');
              TskCmnt := TskCmnt + sLineBreak + 'Създадени отложени задачи -> Промяна сервиз: '+DateToStr(SvcEnd)+' + Регистрация НАП';

              RA := FAddTaskHeader(5, 0, 'Промяна дата абонамент СИМ. Изпращане към НАП.', Date);

              FUpdateTaskOperation(RA,
                                   xml_NodeCC_File,     // файл
                                   xml_NodeCC_FileRows, // път и име на файла
                                   SRows);
              FUpdateTaskOperation(RA,
                                   xml_NodeCC_File,      // файл
                                   xml_NodeCC_FileRChge, // път и име на файла
                                   SRChge);
             end;
           end;
         end;
       end
      else
       begin
        // външни карти за системата не ги следим до кога са платени
        if IncDay(Date, 40) > CCStatus.PayedTo then
         begin
          //Остават по-малко от 40 дни до изтичане на периода
          SvcEnd := IncYear(Date);
          if TaskID = 0 then TaskID := FGetInstantTaskID('Автоматична при проверка абонамент SIM (външна карта)');

          FUpdateTaskOperation(TaskID,
                               xml_NodeCC_Parameter,    // параметър
                               xml_NodeCC_DevPayedTo,   // карайна дата абонамент СИМ
                               DateTimeToXSDateTime(SvcEnd));
          TskCmnt := TskCmnt + sLineBreak + 'Променен абонамент SIM (вк): '+DateToStr(CCStatus.PayedTo)+' -> '+DateToStr(SvcEnd);
         end;
       end;
     end;

    if (MinutesBetween(LastSes, Now) >= 1)or
       (CCStatus.Apn1.LimitLeft <= (CCStatus.Apn1.LimitBytes div 3))or
       ((CCStatus.Apn2.LimitBytes > 0)and(CCStatus.Apn2.LimitLeft < (CCStatus.Apn2.LimitBytes div 3))) then
     begin
      // проверка на лимита; задаване на задача за увеличаването му; фактуриране на обема данни
      // ако картата липсва в базата не даваме лимити!!!
      SQL := 'SELECT F.DF_CUSTEIK, F.DF_DEALEREIK, M.DFM_APN1ENABLE, M.DFM_APN2ENABLE, '+
             'F.DF_APN1PKT, F.DF_APN2PKT, C.CUST_APN1PKT, C.CUST_APN2PKT, D.D_APN1PKT, D.D_APN2PKT '+
             'FROM DEVICES_FU F '+
             'LEFT JOIN DEVICES_FU_MODELS M ON F.DF_MODEL = M.DFM_ID '+
             'LEFT JOIN CUSTOMERS C ON F.DF_CUSTEIK = C.CUST_EIK '+
             'LEFT JOIN DEALERS D ON F.DF_DEALEREIK = D.D_EIK '+
             'WHERE F.DF_SERIAL = '+StrToSQL(CCIdentData.FDSerial);
      if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU fail:'+Device.DbInterface.LastError);
      if not DataSet.IsEmpty then
       begin
        DlrEik  := DataSet.FieldByName('DF_DEALEREIK').AsString;
        CustEik := DataSet.FieldByName('DF_CUSTEIK').AsString;
        // извличане на размера на пакета за активиране
        if not DataSet.FieldByName('D_APN1PKT').IsNull then    PktA1 := DataSet.FieldByName('D_APN1PKT').AsInteger;
        if not DataSet.FieldByName('CUST_APN1PKT').IsNull then PktA1 := DataSet.FieldByName('CUST_APN1PKT').AsInteger; // клиентския пакет е с по-висок приоритет
        if not DataSet.FieldByName('DF_APN1PKT').IsNull then   PktA1 := DataSet.FieldByName('DF_APN1PKT').AsInteger;   // пакета за ФУ е с по-висок приоритет
        PktA1 := Round(PktA1 * DataSet.FieldByName('DFM_APN1ENABLE').AsFloat);

        if not DataSet.FieldByName('D_APN2PKT').IsNull then    PktA2 := DataSet.FieldByName('D_APN2PKT').AsInteger;
        if not DataSet.FieldByName('CUST_APN2PKT').IsNull then PktA2 := DataSet.FieldByName('CUST_APN2PKT').AsInteger; // клиентския пакет е с по-висок приоритет
        if not DataSet.FieldByName('DF_APN2PKT').IsNull then   PktA2 := DataSet.FieldByName('DF_APN2PKT').AsInteger;   // пакета за ФУ е с по-висок приоритет
        PktA2 := Round(PktA2 * DataSet.FieldByName('DFM_APN2ENABLE').AsFloat);

        if FApnNeedTrafficUpdate(CCStatus.Apn1, PktA1*1024) then
         begin
          // Имаме нужда от подновяване на лимита
          if TaskID = 0 then TaskID := FGetInstantTaskID('Автоматично генерирана при проверка лимит APN1');

          FUpdateTaskOperation(TaskID,
                               xml_NodeCC_Parameter,   // параметър
                               xml_NodeCC_DevApn1ctr,  // лимит по APN1
                               IntToStr(PktA1*1024+CCStatus.Apn1.LimitLeft));
          FUpdateApnTraffic(1, CCStatus.Apn1, PktA1, CustEik, DlrEik, TaskID);
          TskCmnt := TskCmnt + sLineBreak + 'Добавен лимит по APN1: '+ IntToStr(PktA1)+'KB към '+FormatFloat('0.##', CCStatus.Apn1.LimitLeft/1024)+'KB';
         end;

        if FApnNeedTrafficUpdate(CCStatus.Apn2, PktA2*1024) then
         begin
          // Имаме нужда от подновяване на лимита
          if TaskID = 0 then TaskID := FGetInstantTaskID('Автоматично генерирана при проверка лимит APN2');

          FUpdateTaskOperation(TaskID,
                               xml_NodeCC_Parameter,   // параметър
                               xml_NodeCC_DevApn2ctr,  // лимит по APN2
                               IntToStr(PktA2*1024+CCStatus.Apn2.LimitLeft));
          FUpdateApnTraffic(2, CCStatus.Apn2, PktA2, CustEik, DlrEik, TaskID);
          TskCmnt := TskCmnt + sLineBreak + 'Добавен лимит по APN2: '+ IntToStr(PktA2)+'KB към '+FormatFloat('0.##', CCStatus.Apn2.LimitLeft/1024)+'KB';
         end;
       end; // if not DataSet.IsEmpty then
     end;

    // добавяне на коментарен текст към задачата - какво е направила
    if (TskCmnt <> '')and(TaskID > 0) then FUpdateTaskComment(TaskID, TskCmnt);

    // добавяне на отчетите и периодичните задачи
    if MinutesBetween(LastSes, Now) > 5 then
     begin
      // добавяне като незабавни задачи всички отчети чийто момент е минал
      SQL := 'SELECT CR_ID FROM CC_REPORTS '+
             'WHERE (CR_MODSERIAL = '+StrToSQL(CCIdentData.ModSerial, 10)+')AND'+
             '(CR_FDSERIAL = '+StrToSQL(CCIdentData.FDSerial, 10)+')AND'+
             '(CR_PENDING < '+DateTimeToSQL(IncMinute(Now, 30))+')'+
             'AND(CR_TASKID IS NULL)';
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_REPORTS fail:'+Device.DbInterface.LastError);
      while not DataSet.Eof do
       begin
        // всички отчети чието време е дошло и не са изпратени като задачи
        // Всеки отчет се записва като отделна незабавна задача
        SQL := 'EXEC CC_ConvertReportToTask '+
               '@CCReportID = '  +IntToSql(DataSet.FieldByName('CR_ID').AsInteger) +', '+
               '@TaskType = '    +IntToSql(0)                                      +', '+
               '@TaskStart = '   +DateTimeToSQL(Now)                               +', '+
               '@TaskPeriod = '  +IntToSql(0)                                      +', '+
               '@TaskComment = ' +StrToSQL('Заявка за отчет:')                     +', '+
               '@OperType  = '   +StrToSQL(xml_NodeCC_Report)                      +';';
         if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('EXEC CC_ConvertReportToTask fail:'+Device.DbInterface.LastError);
        DataSet.Next;
       end;

      // ФОРМИРАНЕ НА ПЪРВА ПЕРИОДИЧНА ЗАДАЧА - използва се да ни се обаждат регулярно устройствата
      // Проверка дали има периодична задача - всеки модем трябва да се обажда
      SQL := 'SELECT M.DFM_PINGTASK, T.CTH_ID, T.CTH_START, T.CTH_PERIOD, T.CTH_RECEIVE '+
             'FROM DEVICES_FU D '+
             'LEFT JOIN DEVICES_FU_MODELS M ON D.DF_MODEL = M.DFM_ID '+
             'LEFT JOIN CC_TASK_HDR T ON (D.DF_SERIAL = T.CTH_FDSERIAL)AND '+
             '     (D.DF_GPRSDEVICE = T.CTH_MODSERIAL)AND(T.CTH_TYPE = 1) '+
             'WHERE (D.DF_SERIAL = '+StrToSQL(CCIdentData.FDSerial, 10)+')';
      if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU fail:'+Device.DbInterface.LastError);
      if not DataSet.IsEmpty then
       begin
        PingD := DataSet.FieldByName('DFM_PINGTASK').AsInteger;

        // нямаме дефинирана задача.
        if DataSet.FieldByName('CTH_ID').IsNull then
         begin   //Добавяме такава ако има дефиниран период
          if PingD > 0 then FAddTaskHeader(1, FCalcModemPingPeriod(PingD), FCalcModemPingComment(PingD), FCalcModemPingTime(PingD));
         end
        else // Имаме дефинирана задача
         begin
          // Имаме задача - проверка дали има нужда от редакция
          if (DataSet.FieldByName('CTH_PERIOD').AsInteger <> FCalcModemPingPeriod(PingD))or // променен е периода
             ((DataSet.FieldByName('CTH_RECEIVE').IsNull)and                         // няма отговор въпреки че времето е минало
              (IncMinute(DataSet.FieldByName('CTH_START').AsDateTime, DataSet.FieldByName('CTH_PERIOD').AsInteger+10) < Now)) then
           begin
            FUpdateTaskHeader(DataSet.FieldByName('CTH_ID').AsInteger, FCalcModemPingPeriod(PingD), FCalcModemPingTime(PingD));
           end;
         end;
       end; // if not DataSet.IsEmpty then


      // ФОРМИРАНЕ НА ВТОРА ПЕРИОДИЧНА ЗАДАЧА - използва се периодично пращане на отчети
      // Вземане на най близката по време задача
      SQL := 'SELECT TOP 1 CR_ID, CR_PENDING '+
             'FROM CC_REPORTS '+
             'WHERE (CR_MODSERIAL = '+StrToSQL(CCIdentData.ModSerial, 10)+')AND'+
             '(CR_FDSERIAL = '+StrToSQL(CCIdentData.FDSerial, 10)+')AND'+
             '(CR_PENDING > '+DateTimeToSQL(IncMinute(Now, 10))+')'+
             'AND(CR_TASKID IS NULL)';
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_REPORTS fail:'+Device.DbInterface.LastError);
      if not DataSet.IsEmpty then SvcEnd := DataSet.FieldByName('CR_PENDING').AsDateTime
       else SvcEnd := 0; //EndOfTheYear(IncYear(Now, 3));

      SQL := 'SELECT CTH_ID, CTH_START, CTH_PERIOD, CTH_SEND, CTH_RECEIVE '+
             'FROM CC_TASK_HDR '+
             'WHERE (CTH_MODSERIAL = '+StrToSQL(CCIdentData.ModSerial, 10)+')'+
             'AND(CTH_FDSERIAL = '+StrToSQL(CCIdentData.FDSerial, 10)+')'+
             'AND(CTH_TYPE = 2)AND(CTH_RECEIVE IS NULL)';
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
      if DataSet.IsEmpty then  // нямаме заявка за обаждане
       begin                   // добавяме такава ако има следващ отчет
        if SvcEnd > 0 then FAddTaskHeader(2, 60, 'Заявка за отчет', SvcEnd);
       end
      else                   // имаме заявка за обаждане
       begin                 // проверка дали трябва да я редактираме
        if SvcEnd = 0 then SvcEnd := EndOfTheYear(IncYear(Now, 3));
        if DataSet.FieldByName('CTH_START').AsDateTime <> SvcEnd then
         FUpdateTaskHeader(DataSet.FieldByName('CTH_ID').AsInteger, 60, SvcEnd);
       end;

     end; // if MinutesBetween(LastSes, Now) > 5 then

   finally
    CloseDataSet;
   end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

//**************************************************************************************************
//   TCCTaskHeader
//**************************************************************************************************
constructor TCCTaskHeader.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FTaskId     := 0;
 FTaskType   := 0;
 FTaskStart  := 0;
 FTaskPeriod := 0;
 FTaskOpers  := TObjectList.Create(true);
end;

destructor TCCTaskHeader.Destroy;
begin
 FTaskOpers.Clear;
 FreeAndNil(FTaskOpers);
 inherited Destroy;
end;

procedure TCCTaskHeader.LoadFromXml(iNode: IXMLNode);
var Opr : TCCTaskOperation;
begin
 try
    FTaskType := XML_GetNodeInt(iNode, xml_NodeCC_TaskType);
    FTaskId   := XML_GetNodeInt(iNode, xml_NodeCC_TaskId);

    iNode := iNode.firstChild;
    while iNode <> nil do
     begin
      if (SameText(iNode.nodeName, xml_NodeCC_File))or
         (SameText(iNode.nodeName, xml_NodeCC_Parameter))or
         (SameText(iNode.nodeName, xml_NodeCC_Report))then
       begin
        Opr := TCCTaskOperation.Create;
        FTaskOpers.Add(Opr);

        Opr.OprType  := iNode.nodeName;
        Opr.OprName  := XML_GetNodeText    (iNode, xml_NodeCC_Name,    false, false);
        Opr.OprValue := XML_GetNodeText    (iNode, xml_NodeCC_Value,   false, false);
        Opr.OprFrom  := XML_GetNodeDateTime(iNode, xml_NodeCC_From,    false);
        Opr.OprTo    := XML_GetNodeDateTime(iNode, xml_NodeCC_To,      false);
        Opr.OprErr   := (XML_FindNodeByName(iNode, xml_NodeCC_ErrCode, false) <> nil);

        if Opr.OprErr then
         begin
          if Opr.OprValue <> '' then Opr.OprValue := Opr.OprValue + sLineBreak;
          Opr.OprValue := Opr.OprValue + '[' + XML_GetNodeText(iNode, xml_NodeCC_ErrCode, false, false) + ']' + XML_GetNodeText(iNode, xml_NodeCC_ErrMsg, false, false);
         end;
       end;
      iNode := iNode.nextSibling;
     end;

 except
  on E: Exception do raise EAbort.Create('['+Self.ClassName+']LoadFromXml: '+E.Message);
 end;
end;

procedure TCCTaskHeader.SaveToXML(iNode: IXMLNode);
var I   : Integer;
    Opr : TCCTaskOperation;
begin
 try
    iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_Task));

    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskType)).Text   := IntToStr(FTaskType);
    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskId)).Text     := IntToStr(FTaskId);
    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskStart)).Text  := DateTimeToXSDateTime(FTaskStart);
    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskPeriod)).Text := IntToStr(FTaskPeriod);

    for I := 0 to FTaskOpers.Count - 1 do
     begin
      Opr := TCCTaskOperation(FTaskOpers.Items[I]);
      with iNode.AppendChild(XmlDocument.CreateElement(Opr.OprType)) do
       begin
         AppendChild(XmlDocument.CreateElement(xml_NodeCC_Name)).text  := Opr.OprName;
         if Opr.OprFrom > 0 then    AppendChild(XmlDocument.CreateElement(xml_NodeCC_From)).text  := DateTimeToXSDateTime(Opr.OprFrom);
         if Opr.OprTo > 0 then      AppendChild(XmlDocument.CreateElement(xml_NodeCC_To)).text    := DateTimeToXSDateTime(Opr.OprTo);
         // има значение подредбата на XML
         if Opr.OprValue <> '' then AppendChild(XmlDocument.CreateElement(xml_NodeCC_Value)).text := Opr.OprValue;
       end;
     end;
 except
  on E: Exception do raise EAbort.Create('['+Self.ClassName+']LoadFromXml: '+E.Message);
 end;
end;

procedure TCCTaskHeader.SaveToTextXml(var TextXml: String);
var XMLDoc : IXMLDocument;
    iNode  : IXMLDOMElement;
    I      : Integer;
    Opr    : TCCTaskOperation;
begin
 try
  XMLDoc := CreateXMLDoc;
  try
    iNode := XMLDoc.createElement(xml_NodeCC_Task);
    XMLDoc.documentElement := iNode;

    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskType)).Text   := IntToStr(FTaskType);
    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskId)).Text     := IntToStr(FTaskId);
    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskStart)).Text  := DateTimeToXSDateTime(FTaskStart);
    iNode.AppendChild(XmlDocument.CreateElement(xml_NodeCC_TaskPeriod)).Text := IntToStr(FTaskPeriod);

    for I := 0 to FTaskOpers.Count - 1 do
     begin
      Opr := TCCTaskOperation(FTaskOpers.Items[I]);
      with iNode.AppendChild(XmlDocument.CreateElement(Opr.OprType)) do
       begin
         AppendChild(XmlDocument.CreateElement(xml_NodeCC_Name)).text  := Opr.OprName;
         if Opr.OprFrom > 0 then    AppendChild(XmlDocument.CreateElement(xml_NodeCC_From)).text  := DateTimeToXSDateTime(Opr.OprFrom);
         if Opr.OprTo > 0 then      AppendChild(XmlDocument.CreateElement(xml_NodeCC_To)).text    := DateTimeToXSDateTime(Opr.OprTo);
         // има значение подредбата на XML
         if Opr.OprValue <> '' then AppendChild(XmlDocument.CreateElement(xml_NodeCC_Value)).text := Opr.OprValue;
       end;
     end;

     TextXml := XMLDoc.Xml;
  finally
   FreeXmlDoc(XMLDoc);
  end;
 except
  on E: Exception do raise EAbort.Create('['+Self.ClassName+']SaveToTextXml: '+E.Message);
 end;
end;

//**************************************************************************************************
//   THndr_CCTask
//**************************************************************************************************
constructor THndr_CCTask.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);

 FTaskList := TObjectList.Create(true);
end;

destructor THndr_CCTask.Destroy;
begin
 FTaskList.Clear;
 FreeAndNil(FTaskList);
 inherited Destroy;
end;

function THndr_CCTask.GetRequestFromDocument: Boolean;
var iNode : IXMLNode;
    Tsk   : TCCTaskHeader;
begin
 try
  iNode := XML_GetRootNode;

  LoadIdentFromXML(iNode); // raise exception on fail
  LoadStatusFromXML(iNode); // raise exception on fail

  // четене на всички отговори ипо задачи
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    if SameText(iNode.nodeName, xml_NodeCC_Task) then
     begin
      Tsk := TCCTaskHeader.Create(XmlDocument);
      FTaskList.Add(Tsk);
      Tsk.LoadFromXml(iNode);   // raise exception on fail
     end;
    iNode := iNode.nextSibling;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

function THndr_CCTask.AddAnswerToDocument: Boolean;
var iRoot : IXMLNode;
    I     : Integer;
begin
 try
  if FTaskList.Count > 0 then
   begin
    iRoot := XML_GetRootNode;

    for I := 0 to FTaskList.Count - 1 do  TCCTaskHeader(FTaskList.Items[I]).SaveToXML(iRoot);

   end;
  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_CCTask.FExtractTaskAsXmlText(TaskType: Integer; var TaskId: Integer; var TaskXml: String): Boolean;
var SQL : String;
    Ra  : Integer;
    Tsk : TCCTaskHeader;
    Opr : TCCTaskOperation;
begin
 Result := false;
 try
  with Device.DbInterface do
  try
   SQL := 'SELECT TOP 1 CTH_ID, CTH_TYPE, CTH_START, CTH_PERIOD '+
          'FROM CC_TASK_HDR '+
          'WHERE (CTH_MODSERIAL = '+StrToSQL(FCCIdent.ModSerial, 10)+')'+
          'AND(CTH_FDSERIAL = '+StrToSQL(FCCIdent.FDSerial, 10)+')'+
          'AND(CTH_SEND IS NULL)AND(CTH_TYPE = '+IntToSql(TaskType)+') '+
          'ORDER BY CTH_ID ASC';
   if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
   if not DataSet.IsEmpty then
    begin
     Tsk := TCCTaskHeader.Create(XmlDocument);
     try
      Tsk.FTaskId     := DataSet.FieldByName('CTH_ID').AsInteger;
      Tsk.FTaskType   := DataSet.FieldByName('CTH_TYPE').AsInteger;
      Tsk.FTaskStart  := DataSet.FieldByName('CTH_START').AsDateTime;
      Tsk.FTaskPeriod := DataSet.FieldByName('CTH_PERIOD').AsInteger;

      if Tsk.FTaskType > 2 then Tsk.FTaskType := 0; // модема приема само тип 0..2

      SQL := 'SELECT CTO_MODE, CTO_TYPE, CTO_NAME, CTO_VALUE, CTO_FROM, CTO_TO '+
             'FROM CC_TASK_OPR '+
             'WHERE (CTO_TASKID = '+IntToStr(Tsk.FTaskId)+') '+
             'ORDER BY CTO_ID ASC';
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_OPR fail:'+Device.DbInterface.LastError);
      while not DataSet.Eof do
       begin
        Opr          := TCCTaskOperation.Create;
        Opr.OprType  := DataSet.FieldByName('CTO_TYPE').AsString;
        Opr.OprName  := DataSet.FieldByName('CTO_NAME').AsString;
        Opr.OprValue := '';
        Opr.OprFrom  := 0;
        Opr.OprTo    := 0;
        Opr.OprErr   := False;

        if DataSet.FieldByName('CTO_MODE').AsInteger = 1 then Opr.OprValue := DataSet.FieldByName('CTO_VALUE').AsString;
        if not DataSet.FieldByName('CTO_FROM').IsNull    then Opr.OprFrom  := DataSet.FieldByName('CTO_FROM').AsDateTime;
        if not DataSet.FieldByName('CTO_TO').IsNull      then Opr.OprTo    := DataSet.FieldByName('CTO_TO').AsDateTime;

        Tsk.FTaskOpers.Add(Opr);
        DataSet.Next;
       end;

      SQL := 'UPDATE CC_TASK_HDR SET CTH_SEND = '+DateTimeToSQL(Now)+' WHERE CTH_ID = '+IntToSql(Tsk.FTaskId);
      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update CC_TASK_HDR fail:'+Device.DbInterface.LastError);

      Tsk.SaveToTextXml(TaskXml);
      TaskId := Tsk.FTaskId;
      Result := (TaskXml <> '');
     finally
      Tsk.Free;
     end;
    end;
  finally
   CloseDataSet;
  end;
 except
  on E: Exception do
   begin
    Result := false;
    Device.PostEventSystem(C_EvType_Error, 'Fail extract task as XML'+sLineBreak+
                                             E.Message, Self.ClassName+'/FExtractTaskAsXmlText');
   end;
 end;
end;

procedure THndr_CCTask.FHandleOperationResult(Tsk: TCCTaskHeader; Opr: TCCTaskOperation);
var SQL : String;
    Ra  : Integer;
    NraReq  : TNRA_Request;
    NraAdvL : TNRA_AdvLines;
    XmlFile : String;
begin
 XmlFile := '';
 try
   if SameText(Opr.OprType, xml_NodeCC_File) then
    begin
     with Device.DbInterface do
     try
      SQL := 'SELECT CTO_VALUE FROM CC_TASK_OPR '+
             'WHERE (CTO_TASKID = '+IntToSql(Tsk.FTaskId)+')'+
             'AND(CTO_TYPE = '+StrToSQL(Opr.OprType, 20)+')'+
             'AND(CTO_NAME = '+StrToSQL(Opr.OprName, 100)+')';
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_OPR fail:'+Device.DbInterface.LastError);
      XmlFile := DataSet.FieldByName('CTO_VALUE').AsString;
     finally
      CloseDataSet;
     end;

     if SameText(Opr.OprName, xml_NodeCC_FileRChge) then
      begin
       // имаме файл за регистрация към НАП
       if Opr.OprErr then
        begin
         FInsertDeviceAction(fdevact_RejectOperation, 'НЕУСПЕШНА отдалечена промяна абонамент ФУ в НАП', Opr.OprValue);
        end
       else
        begin
          if XmlFile <> '' then
           with TNRA_Xml.Create() do
           try
            NraReq.ReqType := rtChange;
            LoadRequest(XmlFile, NraReq, SQL);

            with Device.DbInterface do
             begin
              SQL := 'UPDATE DEVICES_FU SET '+
                     'DF_LSRVCONTRNUMB = '    + StrToSQL('AUTO'+IntToStr(Tsk.FTaskId), 10)+','+
                     'DF_LSRVCONTRDATEFROM = '+ DateToSQL(Date)                           +','+
                     'DF_LSRVCONTRDATETO = '  + DateToSQL(NraReq.SvceContrExpire)         +' '+
                     'WHERE DF_SERIAL = '+StrToSQL(CCIdentData.FDSerial);
              if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU fail:'+Device.DbInterface.LastError);
             end;

            FInsertDeviceAction(fdevact_Apn2FiscChangeCommit, 'Промяна абонамент ФУ в НАП.(Дог.N: AUTO'+IntToStr(Tsk.FTaskId)+' / '+
                                                              DateToStr(Date)+'-'+DateToStr(NraReq.SvceContrExpire),
                                                              XmlFile);
           finally
            Free;
           end;
        end;
      end
     else
     if SameText(Opr.OprName, xml_NodeCC_FileRows) then
      begin
       if Opr.OprErr then
        begin
         FInsertDeviceAction(fdevact_RejectOperation, 'НЕУСПЕШНO отдалечено програмиране ФУ ', Opr.OprValue);
        end
       else
        begin
          if XmlFile <> '' then
           with TNRA_Xml.Create() do
           try
            LoadAdvLines(XmlFile, NraAdvL);
            FInsertDeviceAction(fdevact_Apn2FiscChangeCommit, 'Промяна абонамент във ФУ.('+NraAdvL[9]+'/'+NraAdvL[10], XmlFile);
           finally
            Free;
           end;
        end;
      end;
    end;  
 except
  on E: Exception do
   begin
    // raise EAbort.Create('[FHandleOperationResult]'+E.Message);
    Device.PostEventSystem(C_EvType_Error, 'Fail process operation result:'+sLineBreak+
                                           '['+Self.ClassName+'][FHandleOperationResult]'+sLineBreak+
                                             E.Message, Self.ClassName+'/FHandleOperationResult');

   end;
 end;
end;

function THndr_CCTask.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL : String;
    Err : String;
    Tsk : TCCTaskHeader;
    Opr : TCCTaskOperation;
    I   : Integer;
    Ra  : Integer;
    TskCnt : array [0..5] of Integer;
begin
 Result    := true;
 ErrCode   := errcode_ExecuteSucceed;
 UserError := '';
 try
  with Device.DbInterface do
  try
   // Handle incomming task results
    while FTaskList.Count > 0 do
     begin
      Tsk := TCCTaskHeader(FTaskList.Items[0]);
      // проверка дали задачата е за същото устройство и дали не е вече обработена
      SQL := 'SELECT CTH_MODSERIAL, CTH_FDSERIAL, CTH_RECEIVE, CTH_RESULT '+
             'FROM CC_TASK_HDR WHERE CTH_ID = '+IntToSql(Tsk.FTaskId);
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
      if not DataSet.IsEmpty then
       begin
        if (not SameText(DataSet.FieldByName('CTH_MODSERIAL').AsString, CCIdentData.ModSerial))or
           (not SameText(DataSet.FieldByName('CTH_FDSERIAL').AsString, CCIdentData.FDSerial)) then
         raise EHandledException.Create(errcode_CC_TaskDeviceMismatch, 'Wrong device identification',
                                                                      'Device not match in task responce!'+sLineBreak+
                                                                      'TaskID: '+IntToStr(Tsk.FTaskId)+sLineBreak+
                                                                      DataSet.FieldByName('CTH_MODSERIAL').AsString+'<>'+CCIdentData.ModSerial+sLineBreak+
                                                                      DataSet.FieldByName('CTH_FDSERIAL').AsString+'<>'+CCIdentData.FDSerial);
        Err := '';
        while Tsk.FTaskOpers.Count > 0 do
         begin
          Opr := TCCTaskOperation(Tsk.FTaskOpers.Items[0]);
          SQL := 'UPDATE CC_TASK_OPR SET '+
                 'CTO_RECEIVE = getdate(), '+
                 'CTO_RECEIVECNT = CTO_RECEIVECNT + 1, ';
          if Opr.OprErr then
           begin
            SQL := SQL + 'CTO_RESULT = '+StrToSQL(Opr.OprValue);
            Err := Err + Opr.OprValue+ sLineBreak;
           end
          else
           begin
            SQL := SQL +'CTO_RESULT = '+StrToSQL('');
            if Opr.OprValue <> '' then SQL := SQL +',CTO_VALUE = '+StrToSQL(Opr.OprValue);
           end;
          SQL := SQL + ' WHERE (CTO_TASKID = '+IntToSql(Tsk.FTaskId)+')'+
                       'AND(CTO_TYPE = '+StrToSQL(Opr.OprType, 20)+')'+
                       'AND(CTO_NAME = '+StrToSQL(Opr.OprName, 100)+')';
          if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update CC_TASK_OPR fail:'+Device.DbInterface.LastError);

          // обработка на получения резултат
          FHandleOperationResult(Tsk, Opr);
          Tsk.FTaskOpers.Delete(0);
         end;

        SQL := 'UPDATE CC_TASK_HDR SET '+
               'CTH_RECEIVE = '+  DateTimeToSQL(Now)   +','+
               'CTH_RESULT = '+   StrToSQL(Err)        +' '+
               'WHERE CTH_ID = '+ IntToSql(Tsk.FTaskId);
        if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update CC_TASK_OPR fail:'+Device.DbInterface.LastError);
       end;
      FTaskList.Delete(0);
     end; //while FTaskList.Count > 0 do

    // нулираме данните за да ги презаредим за изпращане
    FTaskList.Clear;

    // Обработка на изходящи задачи
    // Извличане на задачите от базата за да бъдат изпратени
    for I := 0 to 5 do TskCnt[I] := 0;
    SQL := 'SELECT CTH_ID, CTH_TYPE, CTH_START, CTH_PERIOD '+
           'FROM CC_TASK_HDR '+
           'WHERE (CTH_MODSERIAL = '+StrToSQL(FCCIdent.ModSerial, 10)+')'+
           'AND(CTH_FDSERIAL = '+StrToSQL(FCCIdent.FDSerial, 10)+')'+
           'AND(CTH_SEND IS NULL) '+
           'ORDER BY CTH_TYPE ASC, CTH_ID ASC';
    if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_HDR fail:'+Device.DbInterface.LastError);
    while not DataSet.Eof do
     begin
      // добавят се задачи от тип 0,1,2
      // Задача тип 5 (startup) се подава като файл през незабавна задача
      case DataSet.FieldByName('CTH_TYPE').AsInteger of
      0..2:
         begin
          Tsk             := TCCTaskHeader.Create(XmlDocument);
          Tsk.FTaskId     := DataSet.FieldByName('CTH_ID').AsInteger;
          Tsk.FTaskType   := DataSet.FieldByName('CTH_TYPE').AsInteger;
          Tsk.FTaskStart  := DataSet.FieldByName('CTH_START').AsDateTime;
          Tsk.FTaskPeriod := DataSet.FieldByName('CTH_PERIOD').AsInteger;
          FTaskList.Add(Tsk);
          Inc(TskCnt[DataSet.FieldByName('CTH_TYPE').AsInteger]);
         end;
      5: begin
          Inc(TskCnt[DataSet.FieldByName('CTH_TYPE').AsInteger]);
         end;
      end;
      DataSet.Next;
     end;

    for I := 0 to FTaskList.Count - 1 do
     begin
      Tsk := TCCTaskHeader(FTaskList.Items[I]);
      SQL := 'SELECT CTO_MODE, CTO_TYPE, CTO_NAME, CTO_VALUE, CTO_FROM, CTO_TO '+
             'FROM CC_TASK_OPR '+
             'WHERE (CTO_TASKID = '+IntToStr(Tsk.FTaskId)+') '+
             'ORDER BY CTO_ID ASC';
      if not FillDataSet(SQL) then raise EAbort.Create('Select CC_TASK_OPR fail:'+Device.DbInterface.LastError);
      while not DataSet.Eof do
       begin
        Opr          := TCCTaskOperation.Create;
        Opr.OprType  := DataSet.FieldByName('CTO_TYPE').AsString;
        Opr.OprName  := DataSet.FieldByName('CTO_NAME').AsString;
        Opr.OprValue := '';
        Opr.OprFrom  := 0;
        Opr.OprTo    := 0;
        Opr.OprErr   := False;

        if DataSet.FieldByName('CTO_MODE').AsInteger = 1 then Opr.OprValue := DataSet.FieldByName('CTO_VALUE').AsString;
        if not DataSet.FieldByName('CTO_FROM').IsNull    then Opr.OprFrom  := DataSet.FieldByName('CTO_FROM').AsDateTime;
        if not DataSet.FieldByName('CTO_TO').IsNull      then Opr.OprTo    := DataSet.FieldByName('CTO_TO').AsDateTime;

        Tsk.FTaskOpers.Add(Opr);
        DataSet.Next;
       end;

      SQL := 'UPDATE CC_TASK_HDR SET CTH_SEND = '+DateTimeToSQL(Now)+' WHERE CTH_ID = '+IntToSql(Tsk.FTaskId);
      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update CC_TASK_HDR fail:'+Device.DbInterface.LastError);

      if (Tsk.FTaskType = 0)and(TskCnt[5] > 0) then
       begin
        // в момента обработваме незабавна задача тип 0
        // имаме и задачи тип 5 (startup) за да ги прикачим към нея
        while FExtractTaskAsXmlText(5, RA, SQL) do
         begin
          Opr          := TCCTaskOperation.Create;
          Opr.OprType  := xml_NodeCC_File;
          Opr.OprName  := xml_NodeCC_FileStpTsk+IntToStr(Ra);
          Opr.OprValue := SQL;
          Opr.OprFrom  := 0;
          Opr.OprTo    := 0;
          Opr.OprErr   := False;

          Tsk.FTaskOpers.Add(Opr);
         end;
       end;
     end;

  finally
   CloseDataSet;
  end;

 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result    := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function THndr_CCTask.FGetSentTasksIdList: String;
var I : Integer;
begin
 Result := '';
 for I := 0 to FTaskList.Count - 1 do Result := Result + ',' + IntToStr(TCCTaskHeader(FTaskList.Items[I]).FTaskId);
 if Result <> '' then Delete(Result, 1, 1);
end;

//**************************************************************************************************
//    THndr_CCEvent
//**************************************************************************************************
constructor THndr_CCEvent.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 SetLength(FEvents, 0);
 FTaskSnd := '';
end;

destructor THndr_CCEvent.Destroy;
begin
 SetLength(FEvents, 0);
 inherited Destroy;
end;

function THndr_CCEvent.GetRequestFromDocument: Boolean;
var iNode : IXMLNode;
    eNode : IXMLNode;
    I     : Integer;
begin
 try
  iNode   := XML_GetRootNode;
  eNode   := XML_FindNodeByName(iNode, xml_NodeCC_Event, false);

  if eNode <> nil then
   begin
    LoadIdentFromXML(iNode); // raise exception on fail
    LoadStatusFromXML(iNode); // raise exception on fail

    iNode := eNode.firstChild;
    while iNode <> nil do
     begin
      I := Length(FEvents);
      SetLength(FEvents, I+1);
      FEvents[I].EvType := iNode.nodeName;
      FEvents[I].EvData := iNode.text;
      iNode := iNode.nextSibling;
     end;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_CCEvent.AddAnswerToDocument: Boolean;
begin
 Result := true;
 try
  // няма данни за изпращане

 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_CCEvent.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL : String;
    Ra  : Integer;
    I   : Integer;
    Cmnt: String;
    ErrS: String;
    ErrC: Integer;
begin
 Result    := true;
 ErrCode   := errcode_ExecuteSucceed;
 UserError := '';
 try
  with Device.DbInterface do
  try
    // Process errors - All error events are captured as one recort into DB
    ErrS := '';
    ErrC := 0;
    for I := 0 to Length(FEvents) - 1 do
     begin
      if SameText(FEvents[I].EvType, 'err') then
       begin
        ErrS := ErrS + '['+FEvents[I].EvData+']';
       end
      else
      if SameText(FEvents[I].EvType, 'msg') then
       begin
        ErrS := ErrS + FEvents[I].EvData + sLineBreak;
        Inc(ErrC);
       end
      else
       begin
        if SameText(FEvents[I].EvType, 'key') then
         begin
          if SameText(FEvents[I].EvData, 'TEST') then Cmnt := Cmnt + 'Тест/първоначално пускане'
          else
          if SameText(FEvents[I].EvData, 'MENU') then Cmnt := Cmnt + 'По желание на потребителя';
         end;

        Cmnt := Cmnt + sLineBreak +
                'Exp date: '+DateToStr(CCStatus.PayedTo)+sLineBreak+
                'Usage Apn1: '+FormatFloat('0.##', CCStatus.Apn1.LimitPerc)+'% ('+FormatFloat('0.00',CCStatus.Apn1.LimitLeft/1024)+'KB)'+sLineBreak+
                'Usage Apn2: '+FormatFloat('0.##', CCStatus.Apn2.LimitPerc)+'% ('+FormatFloat('0.00',CCStatus.Apn2.LimitLeft/1024)+'KB)';

        SQL := 'INSERT INTO CC_EVENTS (CE_MODSERIAL, CE_FDSERIAL, CE_SIMIMSI, CE_TYPE, CE_VALUE, CE_DATA, CE_TASKSNDID)VALUES('+
               StrToSQL(CCIdentData.ModSerial, 10) +','+ // [CE_MODSERIAL] varchar(10) NOT NULL,
               StrToSQL(CCIdentData.FDSerial, 10)  +','+ // [CE_FDSERIAL] varchar(10) NOT NULL,
               StrToSQL(CCIdentData.SimImsi, 32)   +','+ // [CE_SIMIMSI] varchar(32) CNOT NULL,
               StrToSQL(FEvents[I].EvType, 20)     +','+ // [CE_TYPE] varchar(20) NULL,
               StrToSQL(FEvents[I].EvData, 100)    +','+ // [CE_VALUE] varchar(100) NULL,
               StrToSQL(Cmnt)                      +','+ // [CE_DATA] varchar(max) NULL,
               StrToSQL(TaskSentList, 50, true)    +')'; // [CE_TASKSNDID] varchar(50) NULL,
        ExecuteSQLStatement(SQL, Ra);
       end;
     end;

    if ErrS <> '' then
     begin
        Cmnt := 'Exp date: '+DateToStr(CCStatus.PayedTo)+sLineBreak+
                'Usage Apn1: '+FormatFloat('0.##', CCStatus.Apn1.LimitPerc)+'% ('+FormatFloat('0.00',CCStatus.Apn1.LimitLeft/1024)+'KB)'+sLineBreak+
                'Usage Apn2: '+FormatFloat('0.##', CCStatus.Apn2.LimitPerc)+'% ('+FormatFloat('0.00',CCStatus.Apn2.LimitLeft/1024)+'KB)';

        SQL := 'INSERT INTO CC_EVENTS (CE_MODSERIAL, CE_FDSERIAL, CE_SIMIMSI, CE_TYPE, CE_VALUE, CE_DATA, CE_TASKSNDID)VALUES('+
               StrToSQL(CCIdentData.ModSerial, 10) +','+ // [CE_MODSERIAL] varchar(10) NOT NULL,
               StrToSQL(CCIdentData.FDSerial, 10)  +','+ // [CE_FDSERIAL] varchar(10) NOT NULL,
               StrToSQL(CCIdentData.SimImsi, 32)   +','+ // [CE_SIMIMSI] varchar(32) CNOT NULL,
               StrToSQL('err', 20)                 +','+ // [CE_TYPE] varchar(20) NULL,
               StrToSQL('Cnt:'+IntToStr(ErrC), 100)+','+ // [CE_VALUE] varchar(100) NULL,
               StrToSQL(ErrS+sLineBreak+Cmnt)      +','+ // [CE_DATA] varchar(max) NULL,
               StrToSQL(TaskSentList, 50, true)    +')'; // [CE_TASKSNDID] varchar(50) NULL,
        ExecuteSQLStatement(SQL, Ra);
     end;
  finally
   CloseDataSet;
  end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result    := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

//**************************************************************************************************
//     THndr_CCReport
//**************************************************************************************************
constructor THndr_CCReport.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
end;

destructor THndr_CCReport.Destroy;
begin
 inherited Destroy;
end;

function THndr_CCReport.GetRequestFromDocument: Boolean;
var iNode : IXMLNode;
begin
 try
  iNode   := XML_GetRootNode;

  FTaskId  := XML_GetNodeInt(iNode, xml_NodeCC_TaskId);
  FRepText := XML_GetNodeText(iNode, xml_NodeCC_Receipt, true, false);

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_CCReport.AddAnswerToDocument: Boolean;
begin
 Result := true;
 try
  // няма данни за изпращане

 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_CCReport.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL : String;
    Ra  : Integer;
begin
 Result    := true;
 ErrCode   := errcode_ExecuteSucceed;
 UserError := '';
 try
  with Device.DbInterface do
  try
   SQL := 'UPDATE CC_REPORTS SET '+
          'CR_RECEIVED = '+ DateTimeToSQL(Now) +','+
          'CR_DATA = '+     StrToSQL(FRepText) +' '+
          'WHERE (CR_TASKID = '+IntToSql(FTaskId)+')AND(CR_RECEIVED IS NULL)';
   if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Post report text fail: '+Device.DbInterface.LastError);

  finally
   CloseDataSet;
  end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result    := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

end.
