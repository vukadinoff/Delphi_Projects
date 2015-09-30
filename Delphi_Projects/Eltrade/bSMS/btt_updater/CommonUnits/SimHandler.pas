unit SimHandler;

interface

uses SysUtils, Classes, Forms, BaseHandler, DeviceUnit, XMLHandler, ESKHandler,
     NRAHandler;

type
//******************************************************************************
//           TCmd_SimStatus
//******************************************************************************
 TCmd_SimStatusClient = class(TCommandClient)
 private
  FESKLoginHndr  : THndr_ESKLoginClient;
  FImsi          : String;
  FMsisdn        : String;
  FIcc           : String;
  FExternal      : Boolean;
  FStatusNap     : Integer;
  FStatusMob     : Integer;
  FCustEik       : String;
  FFiscDevice    : String;
  FGprsDevice    : String;
  FActivation    : TDateTime;
  FPayedTo       : TDateTime;
  FOperCode      : Integer;
  FOperName      : String;
  FMinChrgPeriod : Integer;
  function FGetStatusMobAsText: String;
  function FGetPayedToAsText: String;
  function FGetLeftTimeAsText: String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
  property Imsi: String read FImsi write FImsi;
  property Msisdn: String read FMsisdn write FMsisdn;
  property Icc: String read FIcc write FIcc;
  property IsExternal: Boolean read FExternal write FExternal;
  property StatusNap: Integer read FStatusNap write FStatusNap;
  property StatusMob: Integer read FStatusMob write FStatusMob;
  property CustEik: String read FCustEik write FCustEik;
  property FiscDevice: String read FFiscDevice write FFiscDevice;
  property GprsDevice: String read FGprsDevice write FGprsDevice;
  property OperCode: Integer read FOperCode write FOperCode;
  property OperName: String read FOperName write FOperName;
  property Activation: TDateTime read FActivation write FActivation;
  property PayedTo: TDateTime read FPayedTo write FPayedTo;
  property MinChrgPeriod: Integer read FMinChrgPeriod write FMinChrgPeriod;
  property StatusMobAsText: String read FGetStatusMobAsText;
  property PayedToAsText: String read FGetPayedToAsText;
  property LeftTimeAsText: String read FGetLeftTimeAsText;
 end;

 TCmd_SimStatusServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr  : THndr_ESKLoginServer;
  FImsi          : String;
  FMsisdn        : String;
  FIcc           : String;
  FExternal      : Boolean;
  FStatusNap     : Integer;
  FStatusMob     : Integer;
  FCustEik       : String;
  FFiscDevice    : String;
  FGprsDevice    : String;
  FActivation    : TDateTime;
  FPayedTo       : TDateTime;
  FOperCode      : Integer;
  FOperName      : String;
  FMinChrgPeriod : Integer;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property Imsi: String read FImsi write FImsi;
  property Msisdn: String read FMsisdn write FMsisdn;
  property Icc: String read FIcc write FIcc;
  property IsExternal: Boolean read FExternal write FExternal;
  property StatusNap: Integer read FStatusNap write FStatusNap;
  property StatusMob: Integer read FStatusMob write FStatusMob;
  property CustEik: String read FCustEik write FCustEik;
  property FiscDevice: String read FFiscDevice write FFiscDevice;
  property GprsDevice: String read FGprsDevice write FGprsDevice;
  property OperCode: Integer read FOperCode write FOperCode;
  property OperName: String read FOperName write FOperName;
  property Activation: TDateTime read FActivation write FActivation;
  property PayedTo: TDateTime read FPayedTo write FPayedTo;
  property MinChrgPeriod: Integer read FMinChrgPeriod write FMinChrgPeriod;
 end;

//******************************************************************************
//           TCmd_SimPayment
//******************************************************************************

 TCmd_SimPaymentClient = class(TCommandClient)
 private
  FESKLoginHndr  : THndr_ESKLoginClient;
  FImsi          : String; // In-Out
  FPayPeriod     : Integer;
  FSource        : String;
  FPayedTo       : TDateTime; // Out
  FAmmount       : Real;
  FStatusMob     : Integer;
  function FGetStatusMobAsText: String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
  property Imsi: String read FImsi write FImsi;
  property PayPeriod: Integer read FPayPeriod write FPayPeriod;
  property Source: String read FSource write FSource;
  property PayedTo: TDateTime read FPayedTo write FPayedTo;
  property Ammount: Real read FAmmount write FAmmount;
  property StatusMob: Integer read FStatusMob write FStatusMob;
  property StatusMobAsText: String read FGetStatusMobAsText;
 end;

 THndr_SimPayments = class(THandlerBase)
 private
  FDevice     : TRemoteDevice;
  FImsi       : String;
  FPayPeriod  : Integer;
  FPayedTo    : TDateTime;
  FAmmount    : Real;
  FStatusMob  : Integer;
  FChargeTo   : TDealerData;
  FChargeEsk  : String;
  FSource     : String;
  FActivation  : TDateTime;
  function FGetStatusMobAsText: String;
 public
  constructor Create(RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function Execute(var ErrCode: Integer; var UserError: String): Boolean;

  property Device: TRemoteDevice read FDevice write FDevice;
  property Imsi: String read FImsi write FImsi;
  property PayPeriod: Integer read FPayPeriod write FPayPeriod;
  property PayedTo: TDateTime read FPayedTo write FPayedTo;
  property Ammount: Real read FAmmount write FAmmount;
  property StatusMob: Integer read FStatusMob write FStatusMob;
  property StatusMobAsText: String read FGetStatusMobAsText;
  property ChargeTo: TDealerData read FChargeTo write FChargeTo;
  property ChargeEsk: String read FChargeEsk write FChargeEsk;
  property Source: String read FSource write FSource;
  property Activation: TDateTime read FActivation write FActivation;
 end;

 TCmd_SimPaymentServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr  : THndr_ESKLoginServer;
  FHndr_SimPay   : THndr_SimPayments;

  function FGetImsi: String;
  procedure FSetImsi(Value: String);
  function FGetPaymentInfo: String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property Imsi: String read FGetImsi write FSetImsi;
  property PaymentInfo: String read FGetPaymentInfo;
 end;

//******************************************************************************
//           TCmd_SimChange
//******************************************************************************

 TCmd_SimChangeClient = class(TCommandClient)
 private
  FESKLoginHndr  : THndr_ESKLoginClient;
  FFDSerial      : String;  // input
  FMDSerial      : String;
  FMDVersion     : String;
  FNewIMSI       : String;
  FOldIMSI       : String;
  FStopOldSim    : Boolean;
  FNewPayPeriod  : Integer;

  FNewMSISDN     : String; // output
  FNewOperNo     : Integer;
  FNewUnlockCode : String;
  FNewApn2Url    : String;
  FNewStatusMob  : Integer;
  FNewPayedTo    : TDateTime;
  FOldPayedTo    : TDateTime;
  FOldStatusMob  : Integer;

  FRequestID     : Integer;

  function FGetNewStatusMobAsText: String;
  function FGetOldStatusMobAsText: String;
  function FGetNewPayedToAsText: String;
  function FGetOldPayedToAsText: String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
  property FiscDevSerial: String read FFDSerial write FFDSerial;
  property ModemSerial: String read FMDSerial write FMDSerial;
  property ModemVersion: String read FMDVersion write FMDVersion;

  property NewSimIMSI: String read FNewIMSI write FNewIMSI;
  property NewPayPeriod: Integer read FNewPayPeriod write FNewPayPeriod;
  property NewSimMSISDN: String read FNewMSISDN;
  property NewSimOperNo: Integer read FNewOperNo;
  property NewSimUnlockCode: String read FNewUnlockCode;
  property NewSimApn2Url: String read FNewApn2Url;
  property NewStatusMob: Integer read FNewStatusMob;
  property NewPayedTo: TDateTime read FNewPayedTo;
  property NewStatusMobAsText: String read FGetNewStatusMobAsText;
  property NewPayedToAsText: String read FGetNewPayedToAsText;

  property OldSimIMSI: String read FOldIMSI write FOldIMSI;
  property OldSimStop: Boolean read FStopOldSim write FStopOldSim;
  property OldStatusMob: Integer read FOldStatusMob;
  property OldPayedTo: TDateTime read FOldPayedTo;
  property OldStatusMobAsText: String read FGetOldStatusMobAsText;
  property OldPayedToAsText: String read FGetOldPayedToAsText;

  property RequestID: Integer read FRequestID;
 end;


 TSIM_Data = record
   IMSI       : String;
   MSISDN     : String;
   ICC        : String;
   Operator   : Integer;
   PayedTo    : TDateTime;
   IsExternal : Boolean;
   Activation : TDateTime;
   OperStat   : Integer;
   Apn2Url    : String;
 end;

 TCmd_SimChangeServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr  : THndr_ESKLoginServer;
  FFDSerial      : String; // input
  FMDSerial      : String;
  FMDVersion     : String;
  FNewIMSI       : String;
  FOldIMSI       : String;
  FStopOldSim    : Boolean;
  FNewPayPeriod  : Integer;

  FNewMSISDN     : String; // output
  FNewOperNo     : Integer;
  FNewUnlockCode : String;
  FNewApn2Url    : String;
  FNewStatusMob  : Integer;
  FNewPayedTo    : TDateTime;

  FOldStatusMob  : Integer;
  FOldPayedTo    : TDateTime;
  FRequestID     : Integer;

  FOwnerEIK      : String; // internal
  FOwnerName     : String;
  FOwnerSite     : Integer;

  procedure FLoadSimData(SimImsi: String; var SimData: TSIM_Data; ValidateStatus: Boolean);
  procedure FInsertDeviceAction(ActType: Integer; ActIMSI, ActComment, ActData: String);
  procedure FRegisterSimToNRA(SimIMSI: String; SIMAction: TSimStatusType);
  procedure FInsertSimPayment(SimImsi, Comment: String; Ammount, PriceStd, PriceDlr: Real;
                              PayPeriod: Integer; PayedFrom, PayedTo: TDateTime; ReNew: Byte);
  procedure FInsertOperatorAction(ActType: Integer; SimImsi, Data: String);
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property FiscDevSerial: String read FFDSerial write FFDSerial;
  property ModemSerial: String read FMDSerial write FMDSerial;
  property ModemVersion: String read FMDVersion write FMDVersion;

  property NewSimIMSI: String read FNewIMSI write FNewIMSI;
  property NewPayPeriod: Integer read FNewPayPeriod write FNewPayPeriod;
  property NewSimMSISDN: String read FNewMSISDN;
  property NewSimOperNo: Integer read FNewOperNo;
  property NewSimUnlockCode: String read FNewUnlockCode;
  property NewSimApn2Url: String read FNewApn2Url;
  property NewStatusMob: Integer read FNewStatusMob;
  property NewPayedTo: TDateTime read FNewPayedTo;

  property OldSimIMSI: String read FOldIMSI write FOldIMSI;
  property OldSimStop: Boolean read FStopOldSim write FStopOldSim;
  property OldStatusMob: Integer read FOldStatusMob;
  property OldPayedTo: TDateTime read FOldPayedTo;
  property RequestID: Integer read FRequestID write FRequestID;
 end;

implementation
uses BillingConstUnit, DBInterfaceUnit, DB, DateUtils, Math, ElBngMOServiceHandler, VersionUtilsUnit;

function StatusMobToText(Stat_: Integer): String;
begin
 case Stat_ of
 0 : Result := 'Нова карта';
 1 : Result := 'Активна карта';
 2 : Result := 'Спряна временно';
 3 : Result := 'ТЕРМИНИРАНА';
 4 : Result := 'Заявка за спиране на СИМ';
 5 : Result := 'Заявка за активиране на СИМ';
 6 : Result := 'Заявка за терминиране на СИМ';
 else Result := '';
 end;
end;

//******************************************************************************
//           TCmd_SimStatusClient
//******************************************************************************
constructor TCmd_SimStatusClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
end;

destructor TCmd_SimStatusClient.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_SimStatusClient.GetCommandName: String;
begin
 Result := cmd_SimStatus;
end;

function TCmd_SimStatusClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FImsi + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatus));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsi)).Text := Imsi;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text    := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text     := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimStatusClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_SimStatus);
  Imsi  := XML_GetNodeText(iNode, xml_Node_SimImsi, false, false);
  if Imsi <> '' then
   begin
    Msisdn        := XML_GetNodeText (iNode, xml_Node_SimMsisdn);
    Icc           := XML_GetNodeText (iNode, xml_Node_SimIcc);
    StatusNap     := XML_GetNodeInt  (iNode, xml_Node_SimStatNap);
    StatusMob     := XML_GetNodeInt  (iNode, xml_Node_SimStatMob);
    CustEik       := XML_GetNodeText (iNode, xml_Node_SimCustEik, true, false);
    FiscDevice    := XML_GetNodeText (iNode, xml_Node_SimDevFisc, true, false);
    GprsDevice    := XML_GetNodeText (iNode, xml_Node_SimDevGprs, true, false);
    Activation    := XML_GetNodeFloat(iNode, xml_Node_SimActivat);
    PayedTo       := XML_GetNodeFloat(iNode, xml_Node_SimPayedTo);
    OperCode      := XML_GetNodeInt  (iNode, xml_Node_SimOprCode);
    OperName      := XML_GetNodeText (iNode, xml_Node_SimOprName, true, false);
    MinChrgPeriod := XML_GetNodeInt  (iNode, xml_Node_SimMinPer);
    IsExternal    := StrToBoolDef(XML_GetNodeText(iNode, xml_Node_SimExternal, false, false), true);
   end;
  SDate := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC  := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(Imsi + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimStatusClient.FGetStatusMobAsText: String;
begin
 Result := '';
 if (not IsExternal) then Result := StatusMobToText(FStatusMob);
end;

function TCmd_SimStatusClient.FGetPayedToAsText: String;
begin
 Result := '';
 if (not IsExternal)and(PayedTo > 0)and(StatusMob > 0) then
  Result := DateToStr(PayedTo);
end;

function TCmd_SimStatusClient.FGetLeftTimeAsText: String;
var M, D: Integer;
begin
 Result := '';
 if (not IsExternal)and(PayedTo > Date)and(StatusMob = 1) then
  begin
   M := MonthsBetween(Date, PayedTo);
   if M > 0 then Result := Result + IntToStr(M)+' мес.';
   D := DaysBetween(IncMonth(Date, M), PayedTo);
   if (Result <> '')and(D > 0) then Result := Result + ' и ';
   if D > 0 then Result := Result + IntToStr(D)+' дни';
  end;
end;

//******************************************************************************
//           TCmd_SimStatusServer
//******************************************************************************
constructor TCmd_SimStatusServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_SimStatusServer.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_SimStatusServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_SimStatus);
  Imsi  := XML_GetNodeText(iNode, xml_Node_SimImsi);
  SDate := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC  := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(Imsi + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimStatusServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 DecimalSeparator := '.';
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(Imsi + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatus));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsi)).Text := Imsi;
  if Imsi <> '' then
   begin
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimMsisdn)).Text  := Msisdn;
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimIcc)).Text     := Icc;
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatNap)).Text := IntToStr(StatusNap);
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatMob)).Text := IntToStr(StatusMob);
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimCustEik)).Text := CustEik;
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimDevFisc)).Text := FiscDevice;
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimDevGprs)).Text := GprsDevice;
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimOprCode)).Text := IntToStr(OperCode);
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimOprName)).Text := OperName;
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimActivat)).Text := FloatToStr(Activation);
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayedTo)).Text := FloatToStr(PayedTo);
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimMinPer)).Text  := IntToStr(MinChrgPeriod);
    iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimExternal)).Text:= BoolToStr(IsExternal);
   end;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimStatusServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL : String;
begin
 Result := true;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    with Device.DbInterface do
    try
     SQL := 'SELECT S.SIM_IMSI, S.SIM_MSISDN, S.SIM_ICC, S.SIM_STATUSNAP, S.SIM_STATUSOPERATOR, '+
            'S.SIM_CUSTEIK, S.SIM_FISCALDEVICE, S.SIM_GPRSDEVICE, S.SIM_OPERATORCODE, '+
            'S.SIM_ACTIVATIONDATE, S.SIM_PAYEDTODATE, O.MOBO_MINCHARGEPERIOD, O.MOBO_NAME '+
            'FROM SIM S '+
            'LEFT JOIN SYSN_MOBILEOPERATORS O ON S.SIM_OPERATORCODE = O.MOBO_ID '+
            'WHERE S.SIM_IMSI = '+StrToSQL(Imsi);

     if not FillDataSet(SQL) then raise EAbort.Create('Read SIM fail: '+Device.DbInterface.LastError);
     if DataSet.RecordCount > 0 then
      begin
       Imsi       := DataSet.FieldByName('SIM_IMSI').AsString;
       Msisdn     := DataSet.FieldByName('SIM_MSISDN').AsString;
       Icc        := DataSet.FieldByName('SIM_ICC').AsString;
       StatusNap  := DataSet.FieldByName('SIM_STATUSNAP').AsInteger;
       StatusMob  := DataSet.FieldByName('SIM_STATUSOPERATOR').AsInteger;
       CustEik    := DataSet.FieldByName('SIM_CUSTEIK').AsString;
       FiscDevice := DataSet.FieldByName('SIM_FISCALDEVICE').AsString;
       GprsDevice := DataSet.FieldByName('SIM_GPRSDEVICE').AsString;
       OperCode   := DataSet.FieldByName('SIM_OPERATORCODE').AsInteger;
       OperName   := DataSet.FieldByName('MOBO_NAME').AsString;
       Activation := DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime;
       PayedTo    := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;
       MinChrgPeriod := DataSet.FieldByName('MOBO_MINCHARGEPERIOD').AsInteger;
       IsExternal := false;
      end
     else
      begin
       // външна карта за системата
       SQL := 'SELECT S.SE_IMSI, S.SE_MSISDN, S.SE_ICC, '+
              'S.SE_CUSTEIK, S.SE_FISCALDEVICE, S.SE_GPRSDEVICE, S.SE_MOBILEOPERATOR, '+
              'O.MOBO_MINCHARGEPERIOD, O.MOBO_NAME '+
              'FROM SIM_EXTERNAL S '+
              'LEFT JOIN SYSN_MOBILEOPERATORS O ON S.SE_MOBILEOPERATOR = O.MOBO_ID '+
              'WHERE S.SE_IMSI = '+StrToSQL(Imsi);
       if not FillDataSet(SQL) then raise EAbort.Create('Read SIM fail: '+Device.DbInterface.LastError);
       if DataSet.RecordCount > 0 then
        begin
         Msisdn     := DataSet.FieldByName('SE_MSISDN').AsString;
         Icc        := DataSet.FieldByName('SE_ICC').AsString;
         CustEik    := DataSet.FieldByName('SE_CUSTEIK').AsString;
         FiscDevice := DataSet.FieldByName('SE_FISCALDEVICE').AsString;
         GprsDevice := DataSet.FieldByName('SE_GPRSDEVICE').AsString;
         OperCode   := DataSet.FieldByName('SE_MOBILEOPERATOR').AsInteger;
         OperName   := DataSet.FieldByName('MOBO_NAME').AsString;
         StatusNap  := 0;
         StatusMob  := 0;
         Activation := 0;
         PayedTo    := 0;
         MinChrgPeriod := 0;
         IsExternal := true;
        end
       else
       Imsi := '';
      end;
    finally
     CloseDataSet;
    end;

    ErrCode   := errcode_ExecuteSucceed;
    UserError := '';
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

//******************************************************************************
//           TCmd_SimPaymentClient
//******************************************************************************
constructor TCmd_SimPaymentClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
 Source        := '';
 Imsi          := '';
 PayPeriod     := 0;
end;

destructor TCmd_SimPaymentClient.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_SimPaymentClient.GetCommandName: String;
begin
 Result := cmd_SimPayment;
end;

function TCmd_SimPaymentClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FImsi + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayment));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsi)).Text      := Imsi;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayPeriod)).Text := IntToStr(PayPeriod);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimSource)).Text    := Source;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text    := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text     := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimPaymentClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_SimPayment);

  Imsi      := XML_GetNodeText (iNode, xml_Node_SimImsi);
  PayPeriod := XML_GetNodeInt  (iNode, xml_Node_SimPayPeriod);
  PayedTo   := XML_GetNodeFloat(iNode, xml_Node_SimPayedTo);
  Ammount   := XML_GetNodeFloat(iNode, xml_Node_SimAmmount);
  StatusMob := XML_GetNodeInt  (iNode, xml_Node_SimStatMob);
  Source    := XML_GetNodeText (iNode, xml_Node_SimSource, false, false);

  SDate := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC  := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(Imsi + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimPaymentClient.FGetStatusMobAsText: String;
begin
 Result := StatusMobToText(FStatusMob);
end;

//******************************************************************************
//           TCmd_SimPaymentServer
//******************************************************************************
constructor TCmd_SimPaymentServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
 FHndr_SimPay  := THndr_SimPayments.Create(RemoteDevice);
end;

destructor TCmd_SimPaymentServer.Destroy;
begin
 FESKLoginHndr.Free;
 FHndr_SimPay.Free;
 inherited Destroy;
end;

function TCmd_SimPaymentServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_SimPayment);
  FHndr_SimPay.Imsi      := XML_GetNodeText(iNode, xml_Node_SimImsi);
  FHndr_SimPay.PayPeriod := XML_GetNodeInt (iNode, xml_Node_SimPayPeriod);
  FHndr_SimPay.Source    := XML_GetNodeText(iNode, xml_Node_SimSource, false, false);

  SDate := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC  := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(FHndr_SimPay.Imsi + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimPaymentServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 DecimalSeparator := '.';
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FHndr_SimPay.Imsi + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayment));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsi)).Text      := FHndr_SimPay.Imsi;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayPeriod)).Text := IntToStr  (FHndr_SimPay.PayPeriod);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayedTo)).Text   := FloatToStr(FHndr_SimPay.PayedTo);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimAmmount)).Text   := FloatToStr(FHndr_SimPay.Ammount);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatMob)).Text   := IntToStr  (FHndr_SimPay.StatusMob);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimSource)).Text    := FHndr_SimPay.Source;

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimPaymentServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
begin
 Result := true;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    FHndr_SimPay.ChargeEsk := FESKLoginHndr.EskSerial;
    FHndr_SimPay.ChargeTo  := FESKLoginHndr.DealerData;
    if not FHndr_SimPay.Execute(ErrCode, UserError) then raise EAbort.Create(FHndr_SimPay.LastError);
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

function TCmd_SimPaymentServer.FGetImsi: String;
begin
 Result := FHndr_SimPay.Imsi;
end;

procedure TCmd_SimPaymentServer.FSetImsi(Value: String);
begin
 FHndr_SimPay.Imsi := Value;
end;

function TCmd_SimPaymentServer.FGetPaymentInfo: String;
begin
 Result := 'Status: '+FHndr_SimPay.StatusMobAsText+sLineBreak+
           'Payed to: '+DateToStr(FHndr_SimPay.FPayedTo)+sLineBreak+
           'Pay period: '+IntToStr(FHndr_SimPay.FPayPeriod)+sLineBreak+
           'Pay ammount: '+FormatFloat('0.00', FHndr_SimPay.FAmmount);
end;

//******************************************************************************
//  THndr_SimPayments
//******************************************************************************
constructor THndr_SimPayments.Create(RemoteDevice: TRemoteDevice);
begin
 inherited Create;
 FDevice := RemoteDevice;
 FSource := '';
 FImsi   := '';
 FPayPeriod := 0;
end;

destructor THndr_SimPayments.Destroy;
begin
 inherited Destroy;
end;

function THndr_SimPayments.FGetStatusMobAsText: String;
begin
 Result := StatusMobToText(FStatusMob);
end;

function THndr_SimPayments.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL : String;
    RA  : Integer;
    PayedFrom  : TDateTime;
//    ActivatDate: TDateTime;
    FDSerial   : String;
    FDModel    : String;
    ModSerial  : String;
    CustEIK    : String;
    CustName   : String;
    CustSite   : Integer;
    PriceCmnt  : String;
    VerMin     : String;
    VerCurr    : String;

    OperCode   : Integer;
    PriceDlr   : Real;
    PriceStd   : Real;
    StatusMobN : Integer;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result   := true;
 PriceDlr := -1;
 PriceStd := -1;
 PriceCmnt:= '';

 try
    // 1. валидиране на данните
    with Device.DbInterface do
    try
     // get SIM common data
     SQL := 'SELECT SIM_IMSI, SIM_STATUSOPERATOR, SIM_OPERATORCODE, SIM_ACTIVATIONDATE, '+
            'SIM_PAYEDTODATE, SIM_FISCALDEVICE '+
            'FROM SIM '+
            'WHERE SIM_IMSI = '+StrToSQL(Imsi);

     if not FillDataSet(SQL) then raise EAbort.Create('Read SIM data fail: '+Device.DbInterface.LastError);
     if DataSet.RecordCount = 0 then
      raise EHandledException.Create(errcode_Sim_NotExist, 'SIM картата не е намерена в системата!'+sLineBreak+
                                                           'Вероятно тази карта не е предоставена от Елтрейд.'+sLineBreak+
                                                           'Не може да подноовите абонамента и.',
                                                           'SIM payment fail. Missing SIM card in database'+sLineBreak+
                                                           'IMSI: '+Imsi);

     Imsi       := DataSet.FieldByName('SIM_IMSI').AsString;
     FDSerial   := DataSet.FieldByName('SIM_FISCALDEVICE').AsString;
     StatusMob  := DataSet.FieldByName('SIM_STATUSOPERATOR').AsInteger;
     OperCode   := DataSet.FieldByName('SIM_OPERATORCODE').AsInteger;
     if DataSet.FieldByName('SIM_ACTIVATIONDATE').IsNull then FActivation := 0
      else FActivation := DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime;
     if DataSet.FieldByName('SIM_PAYEDTODATE').IsNull then PayedTo := 0
      else PayedTo := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;

     // валидация статуса на СИМа
     case StatusMob of //  0-нова карта; 1-активирана; 2-спряна временно; 3-спряна за вечни времена;  4-заявка за спиране; 5-заявка за пускане;
     0,
     2: begin
         //
        end;
     1: begin
         if (PayedTo > Date)and(MonthsBetween(Date, PayedTo) > 10) then
          raise EHandledException.Create(errcode_Sim_PeriodError, 'SIM картата е платена за повече от 10 месеца.'+sLineBreak+
                                                                  'Подновяване на абонамент не се допуска.',
                                                                  '');
        end;
     3: begin
         // терминирана карта не подлежи на пускане. Номерата които имаме са вече невалидни
         raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е терминирана!'+sLineBreak+
                                                              'Пускането на такава карта е невъзможно.'+sLineBreak+
                                                              'Необходима е физическа подмяна на картата.',
                                                              '');
        end;
     4: begin
          // има заявка за спиране; трябва картата да бъде спряна и чак тогава може да я пуснем наново
          raise EHandledException.Create(errcode_Sim_WaitApproval, 'SIM картата е в процес на спиране!'+sLineBreak+
                                                                   'Моля опитайте по-късно...',
                                                                   '');
        end;
     5: begin
          // има заявка за пускане. Рано или късно картата ще бъде пусната
        end;
     6: begin
         raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е в процес на терминиранe!'+sLineBreak+
                                                              'Пускането на такава карта е невъзможно.'+sLineBreak+
                                                              'Необходима е физическа подмяна на картата.',
                                                              '');
        end;
     else raise EHandledException.Create(errcode_Sim_NotExist, 'SIM картата е с невалидно състояние!'+sLineBreak+
                                                               'Моля обърнете се към сервиза на Елтрейд.',
                                                               'SIM with invalid SIM_STATUSOPERATOR: '+IntToStr(StatusMob)+sLineBreak+
                                                               'IMSI: '+Imsi);
     end;

     if FDSerial = '' then raise EHandledException.Create(ErrCode_Sim_NotAssigned, 'SIM картата не е присвоена към ФУ!'+sLineBreak+
                                                                                   'С тази SIM карта не е извършена регистрация.'+sLineBreak+
                                                                                   'Не може да подновите абонамент на карта, '+sLineBreak+
                                                                                   'която не е присвоена към фискално устройство.',
                                                                                   '');
     // check fiscal device assigned to SIM
     SQL := 'SELECT D.DF_SERIAL, D.DF_GPRSDEVICE, D.DF_CUSTEIK, D.DF_CUSTSITE, D.DF_SIMIMSI, '+
            'M.DFM_SVIDMODEL, M.DFM_MODELNAME, C.CUST_NAME, G.DC_VERSION '+
            'FROM DEVICES_FU D '+
            'LEFT JOIN DEVICES_FU_MODELS M ON M.DFM_ID = D.DF_MODEL '+
            'LEFT JOIN DEVICES_GPRS G ON G.DC_SERIAL = D.DF_GPRSDEVICE '+
            'LEFT JOIN CUSTOMERS C ON C.CUST_EIK = D.DF_CUSTEIK  '+
            'WHERE D.DF_SERIAL = '+StrToSQL(FDSerial);

     if not FillDataSet(SQL) then raise EAbort.Create('Load DEVICES_FU fail: '+Device.DbInterface.LastError);
     if DataSet.IsEmpty then
      raise EHandledException.Create(ErrCode_Sim_NotAssigned, 'SIM картата не е присвоена към ФУ!'+sLineBreak+
                                                              'С тази SIM карта не е извършена регистрация.'+sLineBreak+
                                                              'Не може да подновите абонамент на карта, '+sLineBreak+
                                                              'която не е присвоена към фискално устройство.',
                                                              'SIM is assigned to missing FD'+sLineBreak+
                                                              'IMSI: '+Imsi+sLineBreak+
                                                              'FD: '+FDSerial);
     ModSerial := DataSet.FieldByName('DF_GPRSDEVICE').AsString;
     CustEIK   := DataSet.FieldByName('DF_CUSTEIK').AsString;
     CustSite  := DataSet.FieldByName('DF_CUSTSITE').AsInteger;
     CustName  := DataSet.FieldByName('CUST_NAME').AsString;
     FDModel   := DataSet.FieldByName('DFM_SVIDMODEL').AsString;
     if FDModel = '' then FDModel   := DataSet.FieldByName('DFM_MODELNAME').AsString;

     if DataSet.FieldByName('DF_SIMIMSI').AsString <> Imsi then
      raise EHandledException.Create(ErrCode_Sim_NotAssigned, 'SIM картата не е присвоена към ФУ!'+sLineBreak+
                                                              'С тази SIM карта не е извършена регистрация.'+sLineBreak+
                                                              'Не може да подновите абонамент на карта, '+sLineBreak+
                                                              'която не е присвоена към фискално устройство.',
                                                              'SIM is assigned to FD which has different SIM'+sLineBreak+
                                                              'IMSI: '+Imsi+sLineBreak+
                                                              'FD: '+FDSerial+sLineBreak+
                                                              'FDSIM: '+DataSet.FieldByName('DF_SIMIMSI').AsString);
     // проверка версията на модема
     if DataSet.FieldByName('DC_VERSION').AsString <> '' then
      begin
       VerMin  := GetMajorVersion(DataSet.FieldByName('DC_VERSION').AsString)+'.1206.0.0';
       VerCurr := DataSet.FieldByName('DC_VERSION').AsString;
{       if CompareVersions(VerMin, VerCurr) < 0 then
        raise EHandledException.Create(errcode_Fisc_ModemNeedUpdate,
                                       'Фирмуера на модема има нужда от обновяване!'+sLineBreak+
                                       'Вашата версия: '+ VerCurr+sLineBreak+
                                       'Задължителна версия: '+VerMin+sLineBreak+
                                       ''+sLineBreak+
                                       'Не се разрешава подновяване на абонамент'+sLineBreak+
                                       'за устройство със стара версия.', '');
}
      end;

     // Check SIM period !!!
     SQL := 'SELECT M.MOBO_ID, M.MOBO_NAME, M.MOBO_MINCHARGEPERIOD, C.SCP_ID, C.SCP_LENGTH, C.SCP_NAME '+
            'FROM SYSN_MOBILEOPERATORS M '+
            'JOIN SIM_CHARGEPERIODS C ON C.SCP_LENGTH >= M.MOBO_MINCHARGEPERIOD '+
            'WHERE (M.MOBO_ID = '+IntToSql(OperCode)+') '+
            'ORDER BY C.SCP_LENGTH DESC';
     if not FillDataSet(SQL) then raise EAbort.Create('Load SIM_CHARGEPERIODS fail: '+Device.DbInterface.LastError);
     if DataSet.IsEmpty then
      raise EHandledException.Create(errcode_Sim_PriceError, 'Не са дефинирани периоди за плащане!'+sLineBreak+
                                                             'Моля обърнете се към сервиза на Елтрейд.',
                                                             'SIM Charge periods not found.'+sLineBreak+
                                                             'OperCode:'+IntToStr(OperCode));
     // ако зададем период за плащане 0 се взема по подразбиране -> най-големия
     DataSet.First;
     if PayPeriod = 0 then PayPeriod := DataSet.FieldByName('SCP_LENGTH').AsInteger;

     if not DataSet.Locate('SCP_LENGTH', PayPeriod, []) then
       raise EHandledException.Create(errcode_Sim_PeriodError, 'Невалиден срок за активиране на SIM!'+sLineBreak+
                                                               'Минималния период за активиране на карта'+sLineBreak+
                                                               'предоставена от: "'+DataSet.FieldByName('MOBO_NAME').AsString+
                                                               '" e '+DataSet.FieldByName('MOBO_MINCHARGEPERIOD').AsString+' месеца.'+sLineBreak+
                                                               'Моля коригирайте избрания период.',
                                                               'Invalid SIM activation period: '+IntToStr(PayPeriod)+sLineBreak+
                                                               'This period does not exist for operator: '+IntToStr(OperCode)+sLineBreak+
                                                               'IMSI: '+Imsi);

     // Determin period price
     SQL := 'SELECT DP_DEALEREIK, DP_PRICE, DP_CURRENCY '+
            'FROM DEALERS_PRICES '+
            'WHERE ((DP_DEALEREIK IS NULL)OR(DP_DEALEREIK = '+StrToSQL(ChargeTo.CompanyEIK)+'))'+
            'AND(DP_CHARGEPERIOD = '+IntToSql(DataSet.FieldByName('SCP_ID').AsInteger)+')';
     if not FillDataSet(SQL) then raise EAbort.Create('Load DEALERS_PRICES fail: '+Device.DbInterface.LastError);
     if DataSet.IsEmpty then
      raise EHandledException.Create(errcode_Sim_PriceError, 'Не е дефинирана цена за избрания период!'+sLineBreak+
                                                             'Моля обърнете се към сервиза на Елтрейд.',
                                                             'Dealer price not found in database'+sLineBreak+
                                                             'DP_DEALEREIK = '+ChargeTo.CompanyEIK+sLineBreak+
                                                             'DP_CHARGEPERIOD = '+IntToStr(PayPeriod));
     DataSet.First;
     while not DataSet.Eof do
      begin
       if DataSet.FieldByName('DP_DEALEREIK').IsNull then PriceStd := DataSet.FieldByName('DP_PRICE').AsFloat
        else PriceDlr := DataSet.FieldByName('DP_PRICE').AsFloat;
       DataSet.Next;
      end;

     if PriceStd < 0 then
      raise EHandledException.Create(errcode_Sim_PriceError, 'Не е дефинирана цена за избрания период!'+sLineBreak+
                                                             'Моля обърнете се към сервиза на Елтрейд.',
                                                             'Standard price not found in database'+sLineBreak+
                                                             'DP_DEALEREIK = '+ChargeTo.CompanyEIK+sLineBreak+
                                                             'DP_CHARGEPERIOD = '+IntToStr(PayPeriod));
     if PriceDlr < 0 then PriceDlr := PriceStd;

     // Проверка и прилагане на промоция
     SQL := 'SELECT TOP 1 DPA_ID, DPA_TYPE, DPA_VALUE1, DPA_VALUE2, DPA_PRICEPERC, DPA_DESCRIPTION '+
            'FROM DEALERS_PRICES_ADJUSTMENT '+
            'WHERE '+
            '((DPA_DEALEREIK IS NULL)OR(DPA_DEALEREIK = '+StrToSQL(ChargeTo.CompanyEIK)+'))AND'+
            '((DPA_FROMDATE IS NULL)OR(DPA_FROMDATE <= GETDATE()))AND'+
            '((DPA_TODATE IS NULL)OR(DPA_TODATE >= GETDATE())) '+
            'ORDER BY DPA_DEALEREIK DESC, DPA_PRICEPERC ASC';
     if not FillDataSet(SQL) then raise EAbort.Create('Select DEALERS_PRICES_ADJUSTMENT fail');
     DataSet.First;
     while not DataSet.Eof do
      begin
       case DataSet.FieldByName('DPA_TYPE').AsInteger of
       1:begin // корекция на базата възрастта на картата (за нови карти и за стари карти)  за възраст на картата се взема периода от първата активация до днес
          if FActivation = 0 then RA := 0
           else RA := MonthsBetween(FActivation, Now);
          if (DataSet.FieldByName('DPA_VALUE1').AsInteger <= RA)and
             (DataSet.FieldByName('DPA_VALUE2').AsInteger >= RA) then
           begin
             PriceDlr  := PriceDlr + RoundTo(PriceDlr * DataSet.FieldByName('DPA_PRICEPERC').AsFloat /100, -2);
             PriceCmnt := PriceCmnt + DataSet.FieldByName('DPA_DESCRIPTION').AsString+sLineBreak;
           end;
         end;
       end;
       DataSet.Next;
      end;

     // Изчисляване на периода и сумата
     if (PayedTo = 0)or(PayedTo < Date) then PayedTo := Date; // при нова карта или вече спряна такава започваме от днес
     PayedFrom := PayedTo;
     PayedTo   := IncMonth(PayedTo, PayPeriod);
     Ammount   := PriceDlr * PayPeriod;
     if Ammount < 0 then raise EHandledException.Create(errcode_Sim_PriceError, 'Не е дефинирана цена за избрания период!');

     // Проверка на кредитния лимит
     SQL := 'SELECT D.D_EIK, D.D_LIMITAMMOUNT * CD.CUR_RATE "LIMITAMMOUNT", '+
            '(SELECT SUM(P.DSP_AMOUNT * CP.CUR_RATE) '+
            ' FROM DEALERS_SIMPAYMENTS P '+
	          ' LEFT JOIN CURRENCIES CP ON P.DSP_CURRENCY = CP.CUR_ID '+
	          ' WHERE (P.DSP_DEALEREIK = D.D_EIK)AND(P.DSP_NAVPAYMENTN IS NULL)) "DUEAMMOUNT" '+
            'FROM DEALERS D '+
            'LEFT JOIN CURRENCIES CD ON D.D_LIMITCURRENCY = CD.CUR_ID '+
            'WHERE D.D_EIK = '+StrToSQL(ChargeTo.CompanyEIK);
     if not FillDataSet(SQL) then raise EAbort.Create('Select DEALERS_SIMPAYMENTS fail');
     if DataSet.IsEmpty then
      raise EHandledException.Create(errcode_Fisc_DealerLimitExceeded, 'Невалидна конфигурация на кредитен лимит'+sLineBreak+
                                                                       'Моля обърнете се към сервиза на Елтрейд.',
                                                                       'Invalid dealer limit ammount!'+sLineBreak+
                                                                       'Dealier limit not found in DB');
     if (not SameText(DataSet.FieldByName('D_EIK').AsString, ChargeTo.CompanyEIK))or
        (DataSet.FieldByName('LIMITAMMOUNT').AsFloat <= 0) then
      raise EHandledException.Create(errcode_Fisc_DealerLimitExceeded, 'Нямате разрешен кредитен лимит.'+sLineBreak+
                                                                       'Моля обърнете се към сервиза на Елтрейд.',
                                                                       'Dealer payment limit not defined:'+sLineBreak+
                                                                       'EIK'+ChargeTo.CompanyEIK);
     if (DataSet.FieldByName('DUEAMMOUNT').AsFloat + Ammount) > DataSet.FieldByName('LIMITAMMOUNT').AsFloat then
      raise EHandledException.Create(errcode_Fisc_DealerLimitExceeded, 'Превишен кредитен лимит!'+sLineBreak+
                                                                       'Сумата на вашите задължения'+sLineBreak+
                                                                       'надхвърля лимит на стойност: '+
                                                                        FormatFloat('0.00', DataSet.FieldByName('LIMITAMMOUNT').AsFloat)+sLineBreak+
                                                                       'Моля обърнете се към сервиза на Елтрейд.',
                                                                       'Dealer payment limit exceeded.'+sLineBreak+
                                                                       'LIMIT='+DataSet.FieldByName('LIMITAMMOUNT').AsString+sLineBreak+
                                                                       'DUE='+DataSet.FieldByName('DUEAMMOUNT').AsString);

     // Данните са валидирани успешно започва обработката
     // От тук надолу не се допускат потребителски HANDLED грешки
     // Добре е всичко да бъде в една транзакция но някой друг път...

     RA := IfThen((FActivation = 0), 0, 1);   //0-нов абонамент; 1-подновяване; 2-местене на абонамент от друга карта и удължаване заради неуспешна фискализация

     // 2. добавяне на задължение
     SQL := 'INSERT INTO DEALERS_SIMPAYMENTS (DSP_DEALEREIK, DSP_DEALERBRANCH, DSP_DEALERUSERID, '+
            'DSP_DEALERUSERNAME, DSP_DEVICEFU, DSP_DEVICEGPRS, DSP_IMSI, DSP_CUSTEIK, DSP_CUSTSITE, '+
            'DSP_AMOUNT, DSP_PERIOD, DSP_PAYEDFROM, DSP_PAYEDTO, DSP_PRICEBASE, DSP_PRICEDEALER, '+
            'DSP_IS_RENEW, DSP_COMMENT) VALUES ('+
            StrToSQL(ChargeTo.CompanyEIK, 13)        +', '+ // [DSP_DEALEREIK] varchar(13) NOT NULL,
            IntToSQL(ChargeTo.BranchID)              +', '+ // [DSP_DEALERBRANCH] int NOT NULL,
            IntToSQL(ChargeTo.UserID)                +', '+ // [DSP_DEALERUSERID] int NOT NULL,
            StrToSQL(ChargeEsk, 10)                  +', '+ // [DSP_DEALERUSERNAME] varchar(10) NOT NULL,
            StrToSQL(FDSerial, 10)                   +', '+ // [DSP_DEVICEFU] varchar(10) NOT NULL,
            StrToSQL(ModSerial, 10)                  +', '+ // [DSP_DEVICEGPRS] varchar(10) NOT NULL,
            StrToSQL(Imsi, 32)                       +', '+ // [DSP_IMSI] varchar(32) NOT NULL,
            StrToSQL(CustEIK, 13)                    +', '+ // [DSP_CUSTEIK] varchar(13) COLLATE Cyrillic_General_CI_AS NOT NULL,
            IntToSQL(CustSite)                       +', '+ // [DSP_CUSTSITE] int NOT NULL,
            FloatToSql(Ammount)                      +', '+ // [DSP_AMOUNT] float NOT NULL,
            IntToSQL(PayPeriod)                      +', '+ // [DSP_PERIOD] int NOT NULL,
            DateToSQL(PayedFrom)                     +', '+ // [DSP_PAYEDFROM] date NOT NULL,
            DateToSQL(PayedTo)                       +', '+ // [DSP_PAYEDTO] date NOT NULL,
            FloatToSql(PriceStd)                     +', '+ // [DSP_PRICEBASE] float NOT NULL,
            FloatToSql(PriceDlr)                     +', '+ // [DSP_PRICEDEALER] float NOT NULL,
            IntToSql(RA)                             +', '+ // [DSP_IS_RENEW] smallint DEFAULT 0 NOT NULL
            StrToSQL(PriceCmnt)                      +')';  // [DSP_COMMENT] varchar(max)
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEALERS_SIMPAYMENTS fail: '+Device.DbInterface.LastError);

     // update dealer ammount
     // ПРАВИ СЕ НА ТРИГЕР В БАЗАТА
//     SQL := 'UPDATE DEALERS SET '+
//            'D_SIMPAYMENT_TOTAL = D_SIMPAYMENT_TOTAL + '+FloatToSql(Ammount)+', '+
//            'D_SIMPAYMENT_DUE = D_SIMPAYMENT_DUE + '+FloatToSql(Ammount)+' '+
//            'WHERE D_EIK = '+StrToSQL(ChargeTo.CompanyEIK);
//     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEALERS fail: '+Device.DbInterface.LastError);

     // 3. Активирана на картата
     StatusMobN := StatusMob;
     case StatusMob of //  0-нова карта; 1-активирана; 2-спряна временно; 3-спряна за вечни времена;  4-заявка за спиране; 5-заявка за пускане;
     0:begin
        // приемаме че новите карти са предварително активни и не ги активираме
        StatusMobN := 1;
        SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
               StrToSQL(Imsi, 32)                             +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
               IntToSql(simoperact_ActivateNewSim)            +','+ //  [SOA_TYPE] int NOT NULL,
               StrToSQL('Активиран нов СИМ'+sLineBreak+
                        'ФУ: '+FDModel+' ('+FDSerial+' / '+ModSerial+')'+sLineBreak+
                        'Сервиз: '+ChargeTo.CompanyName+' ('+ChargeTo.BranchName+')'+sLineBreak+
                        'Клиент: '+CustName+' ('+CustEIK+')'+sLineBreak+
                        'Заявка: '+Source) +')'; //  [SOA_DATA] varchar(100) NULL,
        if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
       end;
     1:begin
        // картата е активна не правим нищо по активация/деактивация
        StatusMobN := 1;
       end;
     2:begin
        // имаме плащане по спряна карта, правим заявка за пускане
        StatusMobN := 5;
        SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
               StrToSQL(Imsi, 32)                             +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
               IntToSql(simoperact_RequeastActivateSim)       +','+ //  [SOA_TYPE] int NOT NULL,
               StrToSQL('Активиране спрян СИМ'+sLineBreak+
                        'ФУ: '+FDModel+' ('+FDSerial+' / '+ModSerial+')'+sLineBreak+
                        'Сервиз: '+ChargeTo.CompanyName+' ('+ChargeTo.BranchName+')'+sLineBreak+
                        'Клиент: '+CustName+' ('+CustEIK+')'+sLineBreak+
                        'Заявка: '+Source) +')'; //  [SOA_DATA] varchar(100) NULL,
        if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
       end;
     3:begin
        StatusMobN := 3;
       end;
     4:begin
        StatusMobN := 4;
       end;
     5:begin
        StatusMobN := 5;
       end;
     end;
     SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
            StrToSQL(Imsi, 32)                             +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
            IntToSql(simoperact_PaymentRequest)            +','+ //  [SOA_TYPE] int NOT NULL,
            StrToSQL('Подновяване на абонамент SIM.'+sLineBreak+
                     'ФУ: '+FDModel+' ('+FDSerial+' / '+ModSerial+')'+sLineBreak+
                     'Сервиз: '+ChargeTo.CompanyName+' ('+ChargeTo.BranchName+')'+sLineBreak+
                     'Клиент: '+CustName+' ('+CustEIK+')'+sLineBreak+
                     'Период: '+IntToStr(PayPeriod)+'; от:'+DateToStr(PayedFrom)+' до:'+DateToStr(PayedTo)+sLineBreak+
                     'Заявка: '+Source)  +')'; //  [SOA_DATA] varchar(100) NULL,
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);

     // update SIM end date and status
     SQL := 'UPDATE SIM SET '+
            'SIM_PAYEDTODATE = '+DateToSQL(PayedTo);
     if StatusMob <> StatusMobN then SQL := SQL + ', SIM_STATUSOPERATOR = '+IntToSql(StatusMobN);
     if FActivation = 0         then SQL := SQL + ', SIM_ACTIVATIONDATE = '+DateTimeToSQL(Now);
     SQL := SQL + ' WHERE SIM_IMSI = '+StrToSQL(Imsi);
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM_PAYEDTODATE fail: '+Device.DbInterface.LastError);

     SQL := 'INSERT INTO DEVICES_FU_ACTIONS (DFA_DEVFUSERIAL, DFA_DEVGPRSSERIAL, '+
            'DFA_SIMIMSI, DFA_ACTIONTYPE, DFA_COMMENT, DFA_DEALEREIK, DFA_DEALERUSER, DFA_CUSTEIK, '+
            'DFA_DEALERBRANCH, DFA_CUSTSITE, DFA_DATA) VALUES ('+
             StrToSQL(FDSerial, 10)                     +', '+ // [DFA_DEVFUSERIAL] varchar(10) NOT NULL,
             StrToSQL(ModSerial, 10)                    +', '+ // [DFA_DEVGPRSSERIAL] varchar(10) NOT NULL,
             StrToSQL(IMSI, 32)                         +', '+ // [DFA_SIMIMSI] varchar(32) NULL,
             IntToSql(fdevact_InsertPayment)            +', '+ // [DFA_ACTIONTYPE] int NULL,
             StrToSQL('Подновяване на абонамент SIM.', 100) +', '+ // [DFA_COMMENT] varchar(100) NULL,
             StrToSQL(ChargeTo.CompanyEIK, 13)          +', '+ // [DFA_DEALEREIK] varchar(13) NULL,
             StrToSQL(ChargeEsk, 10)                    +', '+ // [DFA_DEALERUSER] varchar(10) NULL,
             StrToSQL(CustEIK, 13)                      +', '+ // [DFA_CUSTEIK] varchar(13) NULL,
             IntToSql(ChargeTo.BranchID)                +', '+ // [DFA_DEALERBRANCH] int NULL,
             IntToSql(CustSite)                                 + ', '+ // [DFA_CUSTSITE] int NULL,
             StrToSQL('Подновяване на абонамент SIM.'+sLineBreak+
                      'ФУ: '+FDModel+' ('+FDSerial+' / '+ModSerial+')'+sLineBreak+
                      'Сервиз: '+ChargeTo.CompanyName+' ('+ChargeTo.BranchName+')'+sLineBreak+
                      'Клиент: '+CustName+' ('+CustEIK+')'+sLineBreak+
                      'Период: '+IntToStr(PayPeriod)+'; от:'+DateToStr(PayedFrom)+' до:'+DateToStr(PayedTo)+sLineBreak+
                      'Заявка: '+Source) + ') '; // [DFA_DATA] varchar(max) NULL,
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEVICES_FU_ACTIONS: '+Device.DbInterface.LastError);

    // преди това ползваме нулата за да разпознаем новата карта
    if FActivation = 0 then FActivation := Now;

    // ако имаме повторно пускане на Вивакомска СИМ карта
    // пращаме команда към сървиса за активирането и
    if (StatusMobN <> StatusMob)and(StatusMobN in [4, 5])and(OperCode = 2) then
      with THndr_ElBngMOService.Create() do
      try
       if not ProcessDBRequests then Device.PostEventSystem(C_EvType_Error, 'Fail send command to MOBO service!'+ LastError);
      finally
        Free;
      end;

    finally
     CloseDataSet;
    end;

  SetErrorCode(errcode_ExecuteSucceed, '');
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

//******************************************************************************
//           TCmd_SimChangeClient
//******************************************************************************
constructor TCmd_SimChangeClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
end;

destructor TCmd_SimChangeClient.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_SimChangeClient.GetCommandName: String;
begin
 Result := cmd_SimChange;
end;

function TCmd_SimChangeClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FFDSerial+FNewIMSI+FOldIMSI+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimReplace));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimDevFisc)).Text      := FFDSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimDevGprs)).Text      := FMDSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimVerGprs)).Text      := FMDVersion;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsi)).Text         := FOldIMSI;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsiNew)).Text      := FNewIMSI;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayPeriod)).Text    := IntToStr(NewPayPeriod);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStopOld)).Text      := BoolToStr(OldSimStop);

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text    := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text     := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimChangeClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_SimReplace);

  FFDSerial    := XML_GetNodeText(iNode, xml_Node_SimDevFisc);
  FMDSerial    := XML_GetNodeText(iNode, xml_Node_SimDevGprs);
  FMDVersion   := XML_GetNodeText(iNode, xml_Node_SimVerGprs);
  FOldIMSI     := XML_GetNodeText(iNode, xml_Node_SimImsi);
  FNewIMSI     := XML_GetNodeText(iNode, xml_Node_SimImsiNew);
  NewPayPeriod := XML_GetNodeInt (iNode, xml_Node_SimPayPeriod);
  OldSimStop   := StrToBool(XML_GetNodeText(iNode, xml_Node_SimStopOld));

  FOldStatusMob  := XML_GetNodeInt  (iNode, xml_Node_SimStatMob);
  FNewStatusMob  := XML_GetNodeInt  (iNode, xml_Node_SimStatMobNew);
  FOldPayedTo    := XML_GetNodeFloat(iNode, xml_Node_SimPayedTo);
  FNewPayedTo    := XML_GetNodeFloat(iNode, xml_Node_SimPayedToNew);
  FRequestID     := XML_GetNodeInt  (iNode, xml_Node_FiscReqID);

  FNewMSISDN     := XML_GetNodeText(iNode, xml_Node_SimMsisdnNew); // новодобавени стойности
  FNewOperNo     := XML_GetNodeInt (iNode, xml_Node_SimOprCodeNew);
  FNewUnlockCode := XML_GetNodeText(iNode, xml_Node_SimUnlockCode, false, false);
  FNewApn2Url    := XML_GetNodeText(iNode, xml_Node_SimApn2Url, false, false);

  SDate := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC  := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(FFDSerial+FNewIMSI+FOldIMSI+SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimChangeClient.FGetNewStatusMobAsText: String;
begin
  Result := StatusMobToText(FNewStatusMob);
end;

function TCmd_SimChangeClient.FGetOldStatusMobAsText: String;
begin
  Result := StatusMobToText(FOldStatusMob);
end;

function TCmd_SimChangeClient.FGetNewPayedToAsText: String;
begin
 Result := '';
 if (NewPayedTo > 0)and(NewStatusMob > 0) then Result := DateToStr(NewPayedTo);
end;

function TCmd_SimChangeClient.FGetOldPayedToAsText: String;
begin
 Result := '';
 if (OldPayedTo > 0)and(OldStatusMob > 0) then Result := DateToStr(OldPayedTo);
end;

//******************************************************************************
//           TCmd_SimChangeServer
//******************************************************************************
constructor TCmd_SimChangeServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_SimChangeServer.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_SimChangeServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_SimReplace);

  FFDSerial    := XML_GetNodeText(iNode, xml_Node_SimDevFisc);
  FMDSerial    := XML_GetNodeText(iNode, xml_Node_SimDevGprs);
  FMDVersion   := XML_GetNodeText(iNode, xml_Node_SimVerGprs, False, False);
  FOldIMSI     := XML_GetNodeText(iNode, xml_Node_SimImsi);
  FNewIMSI     := XML_GetNodeText(iNode, xml_Node_SimImsiNew);
  NewPayPeriod := XML_GetNodeInt (iNode, xml_Node_SimPayPeriod);
  OldSimStop   := StrToBool(XML_GetNodeText(iNode, xml_Node_SimStopOld));

  SDate := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC  := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(FFDSerial+FNewIMSI+FOldIMSI+SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_SimChangeServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 DecimalSeparator := '.';
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FFDSerial+FNewIMSI+FOldIMSI+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimReplace));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimDevFisc)).Text      := FFDSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimDevGprs)).Text      := FMDSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimVerGprs)).Text      := FMDVersion;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsi)).Text         := FOldIMSI;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimImsiNew)).Text      := FNewIMSI;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayPeriod)).Text    := IntToStr(NewPayPeriod);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStopOld)).Text      := BoolToStr(OldSimStop);

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatMob)).Text      := IntToStr(FOldStatusMob);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimStatMobNew)).Text   := IntToStr(FNewStatusMob);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayedTo)).Text      := FloatToStr(FOldPayedTo);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimPayedToNew)).Text   := FloatToStr(FNewPayedTo);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqID)).Text       := IntToStr(FRequestID);

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimMsisdnNew)).text    := FNewMSISDN;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimOprCodeNew)).text   := IntToStr(FNewOperNo);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimUnlockCode)).text   := FNewUnlockCode;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SimApn2Url)).text      := FNewApn2Url;


  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

procedure TCmd_SimChangeServer.FInsertDeviceAction(ActType: Integer; ActIMSI, ActComment, ActData: String);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
   SQL := 'INSERT INTO DEVICES_FU_ACTIONS (DFA_DEVFUSERIAL, DFA_DEVGPRSSERIAL, '+
          'DFA_SIMIMSI, DFA_ACTIONTYPE, DFA_COMMENT, DFA_DEALEREIK, DFA_DEALERUSER, DFA_CUSTEIK, '+
          'DFA_DEALERBRANCH, DFA_CUSTSITE, DFA_DATA) VALUES ('+
           StrToSQL(FFDSerial, 10)                          + ', '+ // [DFA_DEVFUSERIAL] varchar(10) NOT NULL,
           StrToSQL(FMDSerial, 10)                          + ', '+ // [DFA_DEVGPRSSERIAL] varchar(10) NOT NULL,
           StrToSQL(ActIMSI, 32)                            + ', '+ // [DFA_SIMIMSI] varchar(32) NULL,
           IntToSql(ActType)                                + ', '+ // [DFA_ACTIONTYPE] int NULL,
           StrToSQL(ActComment, 100, true)                  + ', '+ // [DFA_COMMENT] varchar(100) NULL,
           StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) + ', '+ // [DFA_DEALEREIK] varchar(13) NULL,
           StrToSQL(ESKLoginData.EskSerial, 10)             + ', '+ // [DFA_DEALERUSER] varchar(10) NULL,
           StrToSQL(FOwnerEIK, 13)                          + ', '+ // [DFA_CUSTEIK] varchar(13) NULL,
           IntToSql(ESKLoginData.DealerData.BranchID)       + ', '+ // [DFA_DEALERBRANCH] int NULL,
           IntToSql(FOwnerSite)                             + ', '+ // [DFA_CUSTSITE] int NULL,
           StrToSQL(ActData, 0, true)                       + ') '; // [DFA_DATA] varchar(max) NULL,
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEVICES_FU_ACTIONS: '+Device.DbInterface.LastError);
  end;
end;

procedure TCmd_SimChangeServer.FRegisterSimToNRA(SimIMSI: String; SIMAction: TSimStatusType);
var RegSimHndr : THndr_NRARegSimServer;
    ErrCode    : Integer;
    ErrMsg     : String;
    STest      : String;
begin
  RegSimHndr := THndr_NRARegSimServer.Create(nil, Device);
  try
   if Device.TestMode then STest := ' [ТЕСТОВ сървър] '
    else STest := '';

   if Device.TestMode then
    begin
     // регистрация на СИМ от името на Мобилен оператор
     RegSimHndr.SimRequestType := srtIMSI_List;
     RegSimHndr.TestRequest    := Device.TestMode;
     RegSimHndr.UserType       := 'D';
     RegSimHndr.UserName       := ESKLoginData.EskSerial;
     RegSimHndr.SimAccount     := satMobileOpr;
     RegSimHndr.SimStatus      := simsActivate;
     RegSimHndr.SimRequestList.Clear;
     RegSimHndr.SimRequestList.Add(SimIMSI);

     if not RegSimHndr.Execute(ErrCode, ErrMsg) then
      raise EAbort.Create('NRA activation fail: '+RegSimHndr.LastError);

     if ErrCode <> errcode_ExecuteSucceed then
      begin
       ErrMsg := 'Неуспешна инициализация на СИМ в НАП!'+STest+sLineBreak+
                 'Операция: '+ RegSimHndr.SimStatusText+sLineBreak+
                 'IMSI: '+SimIMSI+sLineBreak+
                 'Клиент: '+FOwnerName + ' / '+FOwnerEIK+sLineBreak+
                 ''+sLineBreak+
                 'Грешка ['+IntToStr(ErrCode)+']: '+ErrMsg;

       Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
       FInsertDeviceAction(fdevact_SimActivation, SimIMSI, 'Неуспешна инициализация на СИМ в НАП!', ErrMsg);
       raise EHandledException.Create(ErrCode, ErrMsg);
      end;
    end;

   // регистрация на СИМ от името на Елтрейд
   RegSimHndr.SimRequestType := srtIMSI_List;
   RegSimHndr.TestRequest    := Device.TestMode;
   RegSimHndr.UserType       := 'D';
   RegSimHndr.UserName       := ESKLoginData.EskSerial;
   RegSimHndr.SimAccount     := satEltrade;
   RegSimHndr.SimStatus      := SIMAction;
   RegSimHndr.SimRequestList.Clear;
   RegSimHndr.SimRequestList.Add(SimIMSI);
   if not RegSimHndr.Execute(ErrCode, ErrMsg) then
    raise EAbort.Create('NRA activation fail: '+RegSimHndr.LastError);

   if ErrCode <> errcode_ExecuteSucceed then
    begin
     ErrMsg := 'Неуспешна регистрация на СИМ в НАП!'+STest+sLineBreak+
               'Операция: '+ RegSimHndr.SimStatusText+sLineBreak+
               'IMSI: '+SimIMSI+sLineBreak+
               'Клиент: '+FOwnerName + ' / '+FOwnerEIK+sLineBreak+
               ''+sLineBreak+
               'Грешка ['+IntToStr(ErrCode)+']: '+ErrMsg;

//     Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName); // Излишно е само пълни лога ...
     FInsertDeviceAction(fdevact_SimActivation, SimIMSI, 'Неуспешна регистрация на СИМ в НАП!', ErrMsg);
     raise EHandledException.Create(ErrCode, ErrMsg);
    end;

   FInsertDeviceAction(fdevact_SimActivation, SimIMSI,
                       'Регистрация на SIM в НАП'+STest,
                       'Регистрация на SIM в НАП'+STest+sLineBreak+
                       'Операция: '+ RegSimHndr.SimStatusText+STest+sLineBreak+
                       'IMSI: '+SimIMSI+sLineBreak+
                       'Клиент: '+FOwnerName + ' / '+FOwnerEIK);
  finally
   RegSimHndr.Free;
  end;
end;

procedure TCmd_SimChangeServer.FLoadSimData(SimImsi: String; var SimData: TSIM_Data; ValidateStatus: Boolean);
var SQL : String;

    function IncludeTrailingSlash(Src_: String): String;
    begin
     Result := Trim(Src_);
     if Result[Length(Result)] <> '/' then Result := Result + '/';
    end;
begin
  with Device.DbInterface do
  try
    // проверка на новата SIM карта
    SQL := 'SELECT S.SIM_IMSI, S.SIM_MSISDN, S.SIM_ICC, S.SIM_OPERATORNAME, S.SIM_OPERATORCODE, '+
           'S.SIM_STATUSNAP, S.SIM_STATUSOPERATOR, S.SIM_PAYEDTODATE, S.SIM_ACTIVATIONDATE, '+
           'M.MOBO_APN2URL1, M.MOBO_APN2URL2 '+
           'FROM SIM S '+
           'LEFT JOIN SYSN_MOBILEOPERATORS M ON S.SIM_OPERATORCODE = M.MOBO_ID '+
           'WHERE S.SIM_IMSI = '+StrToSQL(SimImsi);
    if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail');
    if DataSet.IsEmpty then  // картата липсва в основните карти - търсим във външните
      begin
       CloseDataSet;
       SQL := 'SELECT S.SE_IMSI, S.SE_MSISDN, S.SE_ICC, S.SE_MOBILEOPERATOR, S.SE_STATUS, S.SE_APN2ENABLED, '+
              'M.MOBO_APN2URL1, M.MOBO_APN2URL2 '+
              'FROM SIM_EXTERNAL S '+
              'LEFT JOIN SYSN_MOBILEOPERATORS M ON S.SE_MOBILEOPERATOR = M.MOBO_ID '+
              'WHERE S.SE_IMSI = '+StrToSQL(SimImsi);
       if not FillDataSet(SQL) then raise EAbort.Create('Select SIMext fail');
       if not DataSet.IsEmpty then
        begin
         SimData.IMSI       := DataSet.FieldByName('SE_IMSI').AsString;
         SimData.MSISDN     := DataSet.FieldByName('SE_MSISDN').AsString;
         SimData.ICC        := DataSet.FieldByName('SE_ICC').AsString;
         SimData.Operator   := DataSet.FieldByName('SE_MOBILEOPERATOR').AsInteger;
         SimData.OperStat   := DataSet.FieldByName('SE_STATUS').AsInteger;
         SimData.PayedTo    := 0;  // външна карта за системата
         SimData.IsExternal := true;
         SimData.Activation := 0;
         SimData.Apn2Url    := '';

         if DataSet.FieldByName('SE_APN2ENABLED').AsInteger > 0 then
          begin
           // разделяне на устройствата към 2 порта
           if (DataSet.FieldByName('MOBO_APN2URL2').AsString <> '')and
              (FFDSerial[Length(FFDSerial)] in ['5'..'9']) then
            SimData.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL2').AsString) + cmd_DevPing
           else
           if (DataSet.FieldByName('MOBO_APN2URL1').AsString <> '') then
            SimData.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL1').AsString) + cmd_DevPing;
          end;

         if (SimData.ICC = '')or(SimData.MSISDN = '') then
          raise EHandledException.Create(errcode_Fisc_InvalidData, 'SIM картата е регистрирана с невалидни данни в системата!');

         if ValidateStatus then
          begin

           case DataSet.FieldByName('SE_STATUS').AsInteger of
           0:begin // Чака одобрение;
              raise EHandledException.Create(errcode_Sim_WaitApproval, 'SIM картата не е регистрирана!'+sLineBreak+
                                                                       'Заявката за подмяна на SIM карта все още'+sLineBreak+
                                                                       'не е одобрена. Моля опитайте по-късно.',
                                                                       'External SIM card waiting for approval!'+sLineBreak+
                                                                       'SIM_EXTERNAL.SE_STATUS=0'+sLineBreak+
                                                                       'IMSI: '+SimImsi);
             end;
           1:begin // Одобрена;
             end;
           2:begin // Фискализирано у-во;
             end;
           3:begin // Забранена
              raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е блокирана!',
                                                                   'Forbidden external SIM card'+sLineBreak+
                                                                   'SIM_EXTERNAL.SE_STATUS=3'+sLineBreak+
                                                                   'IMSI: '+SimImsi);
             end;
           end;
          end;
        end
       else
        raise EHandledException.Create(errcode_Sim_NotExist, 'SIM картата не е намерена в системата!'+sLineBreak+
                                                             'Моля подайте заявка за подмяна на SIM карта.'+sLineBreak+
                                                             'https://partners.eltrade.com/',
                                                             'Missing SIM card in SIM and SIM_EXTERNAL'+sLineBreak+
                                                             'This is external SIM without registration!'+sLineBreak+
                                                             'IMSI: '+SimImsi);
      end
     else  // if DataSet.IsEmpty then
      begin
       SimData.IMSI       := DataSet.FieldByName('SIM_IMSI').AsString;
       SimData.MSISDN     := DataSet.FieldByName('SIM_MSISDN').AsString;
       SimData.ICC        := DataSet.FieldByName('SIM_ICC').AsString;
       SimData.Operator   := DataSet.FieldByName('SIM_OPERATORCODE').AsInteger;
       SimData.OperStat   := DataSet.FieldByName('SIM_STATUSOPERATOR').AsInteger;
       SimData.IsExternal := false;

       if DataSet.FieldByName('SIM_ACTIVATIONDATE').IsNull then SimData.Activation := 0
        else SimData.Activation := DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime;
       if DataSet.FieldByName('SIM_PAYEDTODATE').IsNull then SimData.PayedTo := 0
        else SimData.PayedTo := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;

       // разделяне на устройствата към 2 порта
       if (DataSet.FieldByName('MOBO_APN2URL2').AsString <> '')and
          (FFDSerial[Length(FFDSerial)] in ['5'..'9']) then
        SimData.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL2').AsString) + cmd_DevPing
       else
       if (DataSet.FieldByName('MOBO_APN2URL1').AsString <> '') then
        SimData.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL1').AsString) + cmd_DevPing
       else
        SimData.Apn2Url := '';

       if (DataSet.FieldByName('SIM_ICC').AsString = '')or(DataSet.FieldByName('SIM_MSISDN').AsString = '') then
        raise EHandledException.Create(errcode_Fisc_InvalidData, 'SIM картата е регистрирана с невалидни данни в системата!');

       if ValidateStatus then
        begin
         case DataSet.FieldByName('SIM_STATUSOPERATOR').AsInteger of
         // 2 временно спряна - подлежи на пускане, не е проблем...
//         2: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е временно спряна!'+sLineBreak+
//                                                                 'IMSI: '+SimImsi,
//                                                                 'Forbidden SIM card'+sLineBreak+
//                                                                 'SIM.SIM_STATUSOPERATOR=2'+sLineBreak+
//                                                                 'IMSI: '+SimImsi);
         // спряна за вечни времена - няма как да я пуснем повече...
         3: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е деактивирана!'+sLineBreak+
                                                                 'Тази карта не може да се използва повече.'+sLineBreak+
                                                                 'IMSI: '+SimImsi,
                                                                 'Forbidden SIM card'+sLineBreak+
                                                                 'SIM.SIM_STATUSOPERATOR=3'+sLineBreak+
                                                                 'IMSI: '+SimImsi);
         // Заявка за спиране на СИМ - предстои спиране на картата
         4: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е в процес на спиране!'+sLineBreak+
                                                                 'Моля опитайте по-късно...'+sLineBreak+
                                                                 'IMSI: '+SimImsi,
                                                                 'SIM card is in request mode'+sLineBreak+
                                                                 'SIM.SIM_STATUSOPERATOR=4'+sLineBreak+
                                                                 'IMSI: '+SimImsi);
         // Заявка за активиране на СИМ - така или иначе ще тръгне някога...
//         5: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е в процес на пускане!'+sLineBreak+
//                                                                 'Моля опитайте по-късно...'+sLineBreak+
//                                                                 'IMSI: '+SimImsi,
//                                                                 'SIM card is in request mode'+sLineBreak+
//                                                                 'SIM.SIM_STATUSOPERATOR=4'+sLineBreak+
//                                                                 'IMSI: '+SimImsi);
         end;
        end;
      end;
  finally
   CloseDataSet;
  end;
end;

procedure TCmd_SimChangeServer.FInsertSimPayment(SimImsi, Comment: String; Ammount, PriceStd, PriceDlr: Real;
                                                 PayPeriod: Integer; PayedFrom, PayedTo: TDateTime; ReNew: Byte);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
   SQL := 'INSERT INTO DEALERS_SIMPAYMENTS (DSP_DEALEREIK, DSP_DEALERBRANCH, DSP_DEALERUSERID, '+
          'DSP_DEALERUSERNAME, DSP_DEVICEFU, DSP_DEVICEGPRS, DSP_IMSI, DSP_CUSTEIK, DSP_CUSTSITE, '+
          'DSP_AMOUNT, DSP_PERIOD, DSP_PAYEDFROM, DSP_PAYEDTO, DSP_PRICEBASE, DSP_PRICEDEALER, '+
          'DSP_IS_RENEW, DSP_COMMENT) VALUES ('+
          StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13)        +', '+ // [DSP_DEALEREIK] varchar(13) NOT NULL,
          IntToSQL(ESKLoginData.DealerData.BranchID)              +', '+ // [DSP_DEALERBRANCH] int NOT NULL,
          IntToSQL(ESKLoginData.DealerData.UserID)                +', '+ // [DSP_DEALERUSERID] int NOT NULL,
          StrToSQL(ESKLoginData.EskSerial, 10)                    +', '+ // [DSP_DEALERUSERNAME] varchar(10) NOT NULL,
          StrToSQL(FFDSerial, 10)                                 +', '+ // [DSP_DEVICEFU] varchar(10) NOT NULL,
          StrToSQL(FMDSerial, 10)                                 +', '+ // [DSP_DEVICEGPRS] varchar(10) NOT NULL,
          StrToSQL(SimImsi, 32)                                   +', '+ // [DSP_IMSI] varchar(32) NOT NULL,
          StrToSQL(FOwnerEIK, 13)                                 +', '+ // [DSP_CUSTEIK] varchar(13) COLLATE Cyrillic_General_CI_AS NOT NULL,
          IntToSQL(FOwnerSite)                                    +', '+ // [DSP_CUSTSITE] int NOT NULL,
          FloatToSql(Ammount)                                     +', '+ // [DSP_AMOUNT] float NOT NULL,
          IntToSQL(PayPeriod)                                     +', '+ // [DSP_PERIOD] int NOT NULL,
          DateToSQL(PayedFrom)                                    +', '+ // [DSP_PAYEDFROM] date NOT NULL,
          DateToSQL(PayedTo)                                      +', '+ // [DSP_PAYEDTO] date NOT NULL,
          FloatToSql(PriceStd)                                    +', '+ // [DSP_PRICEBASE] float NOT NULL,
          FloatToSql(PriceDlr)                                    +', '+ // [DSP_PRICEDEALER] float NOT NULL,
          IntToSql(ReNew)                                         +', '+ // [DSP_IS_RENEW] smallint DEFAULT 0 NOT NULL
          StrToSQL(Comment)                                       +')';  // [DSP_COMMENT] varchar(max)
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEALERS_SIMPAYMENTS fail: '+Device.DbInterface.LastError);
  end;
end;


procedure TCmd_SimChangeServer.FInsertOperatorAction(ActType: Integer; SimImsi, Data: String);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
     SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
            StrToSQL(SimImsi, 32)                               +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
            IntToSql(ActType)                                   +','+ //  [SOA_TYPE] int NOT NULL,
            StrToSQL(Data+sLineBreak+
                    'ФУ: '+FFDSerial+' / '+FMDSerial+sLineBreak+
                    'Сервиз: '+ESKLoginData.DealerData.CompanyName+' ('+ESKLoginData.DealerData.BranchName+')'+sLineBreak+
                    'Клиент: '+FOwnerName+' ('+FOwnerEIK+')')   +')'; //  [SOA_DATA] varchar(max) NULL,
    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
  end;
end;

function TCmd_SimChangeServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL      : String;
    RA       : Integer;
    NewSim   : TSIM_Data;
    OldSim   : TSIM_Data;
    PayDays  : Integer;
    StatMobN : Integer;
    MdLastVer: String;
    MdFWFile : String;
begin
 Result  := true;
 PayDays := 0;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then raise EAbort.Create(FESKLoginHndr.LastError);
  if ErrCode <> errcode_ExecuteSucceed then raise EHandledException.Create(ErrCode, UserError);

  if FFDSerial = '' then raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за ФУ',
                                                                                  'Invalid FDSerial: empty');
  if FMDSerial = '' then raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за модем',
                                                                                  'Invalid MDSerial: empty');
  if FNewIMSI = ''  then raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за нов SIM',
                                                                                  'Ivalid NewSimIMSI: NULL');
  if FOldIMSI = ''  then raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за стар SIM',
                                                                                    'Ivalid OldSimIMSI: NULL');
  with Device.DbInterface do
  try
    // проверка на фискалното устройство
    SQL := 'SELECT D.DF_SERIAL, D.DF_DEVICETYPE, D.DF_MODEL, D.DF_GPRSDEVICE, D.DF_DEALEREIK, '+
           'D.DF_DEALERBRANCH, D.DF_CUSTEIK, D.DF_CUSTSITE, D.DF_SIMIMSI, C.CUST_NAME, '+
           'GM.DCM_ID, GM.DCM_MODEL, GF.DCF_LASTVERSION, GF.DCF_FILENAME '+
           'FROM DEVICES_FU D '+
           'LEFT JOIN CUSTOMERS C ON D.DF_CUSTEIK = C.CUST_EIK '+
           'LEFT JOIN DEVICES_GPRS G ON D.DF_GPRSDEVICE = G.DC_SERIAL '+
           'LEFT JOIN DEVICES_GPRS_MODELS GM ON G.DC_MODEL = GM.DCM_ID '+
           'LEFT JOIN DEVICES_GPRS_FIRMWARE GF ON GM.DCM_FIRMWARE = GF.DCF_ID '+
           'WHERE D.DF_SERIAL = '+StrToSQL(FFDSerial);
    if not FillDataSet(SQL) then raise EAbort.Create('Select device FU fail');
    if DataSet.IsEmpty then
     raise EHandledException.Create(errcode_Fisc_DeviceNotExist, 'Устройството не съществува'+sLineBreak+
                                                                 'в системата ('+FFDSerial+')!'+sLineBreak+
                                                                 'Не може да извършите избраната'+sLineBreak+
                                                                 'операция при нерегистрирано устройство.',
                                                                 'FD not found in DEVICES_FU during ChangeSIM');
    FOwnerEIK  := DataSet.FieldByName('DF_CUSTEIK').AsString;
    FOwnerSite := DataSet.FieldByName('DF_CUSTSITE').AsInteger;
    FOwnerName := DataSet.FieldByName('CUST_NAME').AsString;
    MdLastVer  := DataSet.FieldByName('DCF_LASTVERSION').AsString;
    MdFWFile   := DataSet.FieldByName('DCF_FILENAME').AsString;

    // проверка на версията на модема
    if FMDVersion <> '' then // стари версии на прогграмата не пращат версия на модема!!!
     begin
       // ако нямаме изрично зададена версия за този тип модем включваме аварийна проверка
       // аварийната проверка е по първата цифра на версията на устройството
       // това е заради бъг в системата - имаме външни и вътрешни модеми произзведени с еднакъв модел
       if (MdLastVer = '')or(MdFWFile = '') then
        begin
         SQL := 'SELECT DCF_LASTVERSION, DCF_FILENAME FROM DEVICES_GPRS_FIRMWARE '+
                'WHERE (DCF_LASTVERSION IS NOT NULL)and(DCF_LASTVERSION <> '+StrToSQL('')+')and(DCF_FILENAME <> '+StrToSQL('')+')';
         if not FillDataSet(SQL) then raise EAbort.Create('Read DEVICES_GPRS_FIRMWARE fail: '+LastError);
         while not DataSet.Eof do
          begin
           if AnsiSameText(GetMajorVersion(DataSet.FieldByName('DCF_LASTVERSION').AsString), GetMajorVersion(FMDVersion)) then
            begin
             MdLastVer  := DataSet.FieldByName('DCF_LASTVERSION').AsString;
             MdFWFile := DataSet.FieldByName('DCF_FILENAME').AsString
            end;
           DataSet.Next;
          end;
        end;

       if (MdLastVer <> '')and(MdFWFile <> '')and
          (CompareVersions(FMDVersion, MdLastVer) > 0) then
        raise EHandledException.Create(errcode_Fisc_ModemNeedUpdate,
                                       'Фирмуера на модема има нужда от обновяване!'+sLineBreak+
                                       'Вашата версия: '+ FMDVersion+sLineBreak+
                                       'Последна версия: '+ MdLastVer+sLineBreak+
                                       ''+sLineBreak+
                                       'Моля обновете версията на модема за да'+sLineBreak+
                                       'продължите с избраната операция.', '');
     end; // if FMDVersion <> '' then

    // зареждане и проверка данните за СИМ картите (вдига ексепшън при неуспех)
    FLoadSimData(FNewIMSI, NewSim, true);   // новата карта се валидира, старата няма нужда...
    FLoadSimData(FOldIMSI, OldSim, false);

    FNewMSISDN     := NewSim.MSISDN;
    FNewOperNo     := NewSim.Operator;
    FNewUnlockCode := ''; // генериране на код за отключване на СИМ картата
    FNewApn2Url    := NewSim.Apn2Url;

    FInsertDeviceAction(fdevact_ReplaceSIM, OldSim.IMSI, 'Заявка за подмяна на SIM карта.',
                                                         'Подмяна на SIM карта:'+sLineBreak+
                                                         'Стар SIM: '+OldSim.IMSI+sLineBreak+
                                                         'Нов SIM: '+NewSim.IMSI);


    // ъпдейт данните на ФУ
    // ако не го направим сега, не минава плащането на новия абонамент (при нова карта)
    // във функцията за плащане на абонамент се прави проверка за коретни данни в таблица СИМ и КАСИ
    SQL := 'UPDATE DEVICES_FU SET '+
           'DF_GPRSDEVICE = '+   StrToSQL(FMDSerial, 10)                          +','+ // [DF_GPRSDEVICE] varchar(10) NOT NULL,
           'DF_DEALEREIK = '+    StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) +','+ // [DF_DEALEREIK] varchar(13) NOT NULL,
           'DF_DEALERBRANCH = '+ IntToSQL(ESKLoginData.DealerData.BranchID)       +','+ // [DF_DEALERBRANCH] int NOT NULL,
           'DF_SIMIMSI = '+      StrToSQL(FNewIMSI, 32)                           +' '+ // [DF_SIMIMSI] varchar(32) NULL,
           'WHERE DF_SERIAL = '+StrToSQL(FFDSerial);
    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU Fail: '+Device.DbInterface.LastError);

    // Ако картата е извадена от друга каса - изваждаме я и от базата
    SQL := 'UPDATE DEVICES_FU SET '+
           'DF_SIMIMSI = '+ StrToSQL('000000000000000', 32) +' '+ // [DF_SIMIMSI] varchar(32) NULL,
           'WHERE (DF_SIMIMSI = '+StrToSQL(FNewIMSI, 32)+')'+
           'AND(DF_SERIAL <> '+StrToSQL(FFDSerial, 10)+')';
    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU.DF_SIMIMSI Fail: '+Device.DbInterface.LastError);
    if RA > 0 then
     begin
      FInsertDeviceAction(fdevact_MoveSimFromDevice, FNewIMSI, 'СИМ картата е взета от друго устройство', '');
      PostEventUser(0, C_EvType_SIM, 'D', ESKLoginData.EskSerial, C_DeviceType_FD, FFDSerial,
                    'СИМ картата е взета от друго устройство'+sLineBreak+'IMSI: '+FNewIMSI);
     end;


    if not OldSim.IsExternal then
     begin
      // спиране на старата карта
      // Нужно е да има поне един ден платен забонамент за да го прехвърлим...
      // OldSim.PayedTo може да съдържа и часове !!! трябва да следим само целите дни
      if (OldSim.PayedTo > Date)and(DaysBetween(Date, OldSim.PayedTo) > 0)and
         ((FStopOldSim)or(OldSim.OperStat in [2,3])) then
       begin
        // старата карта има предплатен абонамент - местим го към новата карта
        // При спряна или терминирана карта не се интересуваме от чавката
        PayDays := DaysBetween(Date, OldSim.PayedTo);

        SQL := 'UPDATE SIM SET SIM_PAYEDTODATE = GetDate() WHERE SIM_IMSI = '+StrToSQL(OldSim.IMSI);
        if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Chage SIM PAYEDTODATE fail:'+Device.DbInterface.LastError);

        FInsertSimPayment(OldSim.IMSI, 'Преместване абонамент към SIM: '+NewSim.IMSI+sLineBreak+
                                       'Брой дни: '+IntToStr(PayDays)+' ('+DateToStr(OldSim.PayedTo)+' -> '+DateToStr(Date)+')',
                          0, 0, 0, -Abs(MonthsBetween(OldSim.PayedTo, Date)), OldSim.PayedTo, Date, 2); //0-нов абонамент; 1-подновяване; 2-местене на абонамент от друга карта и удължаване заради неуспешна фискализация
        FInsertOperatorAction(simoperact_RequestDeactivateSim, OldSim.IMSI,
                              'Местене aбонамент от стар към нов SIM'+sLineBreak+
                              'Стар SIM: '+OldSim.IMSI+sLineBreak+
                              'Нов SIM: '+NewSim.IMSI+sLineBreak+
                              'Брой дни: '+IntToStr(PayDays)+' ('+DateToStr(OldSim.PayedTo)+' -> '+DateToStr(Date)+')');
        FInsertDeviceAction(fdevact_StopSimDuePayExpire, OldSim.IMSI,
                            'Експорт предплатен абонамент към друга карта.',
                            'Местене aбонамент от стар към нов SIM'+sLineBreak+
                            'Стар SIM: '+OldSim.IMSI+sLineBreak+
                            'Нов SIM: '+NewSim.IMSI+sLineBreak+
                            'Брой дни: '+IntToStr(PayDays)+' ('+DateToStr(OldSim.PayedTo)+' -> '+DateToStr(Date)+')');
       end;

      if (OldSim.OperStat in [0,1])and(FStopOldSim) then
       begin
        FInsertOperatorAction(simoperact_RequestDeactivateSim, OldSim.IMSI,
                              'Спиране на SIM поради подмяна'+sLineBreak+
                              'Стар SIM: '+OldSim.IMSI+sLineBreak+
                              'Нов SIM: '+NewSim.IMSI);

        SQL := 'UPDATE SIM SET SIM_STATUSOPERATOR = 4 WHERE SIM_IMSI = '+ StrToSQL(OldSim.IMSI);
        if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Chage SIM PAYEDTODATE fail:'+Device.DbInterface.LastError);
       end;
     end;

    //ъпдейт на данните за СИМ
    if not NewSim.IsExternal then
     begin
      // промяна на основните данни за картата
      SQL := 'UPDATE SIM SET '+
             'SIM_CUSTEIK = '+      StrToSQL(FOwnerEIK, 13, true) +', '+ // [SIM_CUSTEIK] varchar(13) NOT NULL,
             'SIM_CUSTSITEID = '+   IntToSql(FOwnerSite, true)    +', '+ // [SIM_CUSTSITEID] int NULL,
             'SIM_FISCALDEVICE = '+ StrToSQL(FFDSerial, 10, true) +', '+ // [SIM_FISCALDEVICE] varchar(10) NULL,
             'SIM_GPRSDEVICE = '+   StrToSQL(FMDSerial, 10, true) +', '+ // [SIM_GPRSDEVICE] varchar(10) NULL,
             'SIM_TESTMODE = '+     BoolToSql(Device.TestMode)    +'  '+ // [SIM_TESTMODE] int NULL,
             'WHERE SIM_IMSI = '+   StrToSQL(NewSim.IMSI);
      if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Chage SIM customer fail:'+Device.DbInterface.LastError);

      // Местене на абонамент от старата карта към новата карта
      // Ако новата карта не е активна - това включва и активирането и
      if PayDays > 0 then
       begin
         if NewSim.PayedTo < Date then NewSim.PayedTo := Date;

         // 3. Активирана на картата
         StatMobN := NewSim.OperStat;
         case NewSim.OperStat of //  0-нова карта; 1-активирана; 2-спряна временно; 3-спряна за вечни времена;  4-заявка за спиране; 5-заявка за пускане;
         0:begin
            // приемаме че новите карти са предварително активни и не ги активираме
            StatMobN := 1;
            FInsertOperatorAction(simoperact_ActivateNewSim, NewSim.IMSI,
                                  'Активиран нов СИМ при подмяна.'+sLineBreak+
                                  'Стар SIM: '+OldSim.IMSI);
           end;
         2:begin
            // имаме плащане по спряна карта, правим заявка за пускане
            StatMobN := 5;
            FInsertOperatorAction(simoperact_RequeastActivateSim, NewSim.IMSI,
                                  'Активиране спрян СИМ при подмяна'+sLineBreak+
                                  'Стар SIM: '+OldSim.IMSI);
           end;
         end;
         FInsertSimPayment(NewSim.IMSI, 'Импорт абонамент от SIM: '+OldSim.IMSI,
                          0, 0, 0, Abs(MonthsBetween(NewSim.PayedTo, IncDay(NewSim.PayedTo, PayDays))),
                          NewSim.PayedTo, IncDay(NewSim.PayedTo, PayDays), 2); //0-нов абонамент; 1-подновяване; 2-местене на абонамент от друга карта и удължаване заради неуспешна фискализация

         FInsertOperatorAction(simoperact_PaymentRequest, NewSim.IMSI,
                               'Местене на абонамент от спрян SIM'+sLineBreak+
                               'Стар SIM: '+OldSim.IMSI+sLineBreak+
                               'Нов SIM: '+NewSim.IMSI+sLineBreak+
                               'Брой дни: '+IntToStr(PayDays)+sLineBreak+
                               'Период от: '+DateToStr(NewSim.PayedTo)+' до:'+DateToStr(IncDay(NewSim.PayedTo, PayDays)));

         FInsertDeviceAction(fdevact_InsertPayment, NewSim.IMSI,
                            'Импорт предплатен абонамент от спряна карта.',
                            'Импорт aбонамент от стар към нов SIM'+sLineBreak+
                            'Стар SIM: '+OldSim.IMSI+sLineBreak+
                            'Нов SIM: '+NewSim.IMSI+sLineBreak+
                            'Брой дни: '+IntToStr(PayDays)+sLineBreak+
                            'Период от: '+DateToStr(NewSim.PayedTo)+' до:'+DateToStr(IncDay(NewSim.PayedTo, PayDays)));

         // update SIM end date and status
         SQL := 'UPDATE SIM SET '+
                'SIM_PAYEDTODATE = '+DateToSQL(IncDay(NewSim.PayedTo, PayDays));
         if NewSim.OperStat <> StatMobN then SQL := SQL + ', SIM_STATUSOPERATOR = '+IntToSql(StatMobN);
         if NewSim.Activation = 0       then SQL := SQL + ', SIM_ACTIVATIONDATE = '+DateTimeToSQL(Now);
         SQL := SQL + ' WHERE SIM_IMSI = '+StrToSQL(NewSim.IMSI);
         if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM_PAYEDTODATE fail: '+Device.DbInterface.LastError);


         NewSim.PayedTo  := IncDay(NewSim.PayedTo, PayDays); // обновяване на самата дата - по-надолу се ползва
         NewSim.OperStat := StatMobN;
         if NewSim.Activation = 0 then NewSim.Activation := Now;
       end;

      // активираме абонамент на новата карта ако има нужда
      // при условиве че е нова карта и не е преместен абонамент от старата
      if NewSim.PayedTo <= Date then
       begin
        with THndr_SimPayments.Create(Device) do
        try
          Imsi      := NewSim.IMSI;
          PayPeriod := NewPayPeriod;
          ChargeEsk := FESKLoginHndr.EskSerial;
          ChargeTo  := FESKLoginHndr.DealerData;
          if not Execute(ErrCode, UserError) then raise EAbort.Create(LastError);
          if ErrCode <> errcode_ExecuteSucceed then
           raise EHandledException.Create(ErrCode, 'Неуспешна регистрация на нов'+sLineBreak+
                                                   'абонамент при подмяна на карта.'+sLineBreak+
                                                   UserError);
          NewSim.PayedTo    := PayedTo;
          NewSim.Activation := Activation;
          NewSim.OperStat   := StatusMob;
        finally
         Free;
        end;
       end;
     end
    else // if not NewSim.IsExternal then
     begin
      SQL := 'UPDATE SIM_EXTERNAL SET '+
             'SE_FISCALDEVICE = '+ StrToSQL(FFDSerial, 10, true)   +','+ // [SE_FISCALDEVICE] varchar(10) NULL,
             'SE_GPRSDEVICE = '+   StrToSQL(FMDSerial, 10, true)   +','+ // [SE_GPRSDEVICE] varchar(10) NULL,
             'SE_CUSTEIK = '+      StrToSQL(FOwnerEIK, 13, true)  +','+ // [SE_CUSTEIK] varchar(13) NULL,
             'SE_CUSTSITEID = '+   IntToSql(FOwnerSite, true)     +','+ // [SE_CUSTSITEID] int NULL,
             'SE_TESTMODE = '+     BoolToSql(Device.TestMode)     +' '+ // [SE_TESTMODE] int NULL,
             'WHERE SE_IMSI = '+   StrToSQL(FNewIMSI);
      if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM_EXTERNAL fail: '+Device.DbInterface.LastError);

      SQL := 'INSERT INTO SIM_EXTERNALACTIONS(SEA_IMSI, SEA_ACTIONTYPE, SEA_DATA) VALUES ('+
             StrToSQL(FNewIMSI, 32)                 +','+ //  [SEA_IMSI] varchar(32) NOT NULL,
             IntToSQL(simextact_FDevRegistered)     +','+ //  [SEA_ACTIONTYPE] int NOT NULL,
             StrToSQL('ФУ: '+FFDSerial+sLineBreak+
                      'Дилър:'+ESKLoginData.DealerData.CompanyName+sLineBreak+
                      'Клиент:'+FOwnerName, 200)   +')'; //  [SEA_DATA] varchar(200) NULL,
      if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_EXTERNALACTIONS fail: '+Device.DbInterface.LastError);
      FInsertDeviceAction(fdevact_SimActivation, FNewIMSI, 'Външна СИМ карта. Регистрацията се пропуска!', '');
     end;

    // плащането на абонамент е готово продължаваме напред

    //регистрация на СИМ в НАП - само на собствени карти или чужди в тестовия сървър
    if (not NewSim.IsExternal) then FRegisterSimToNRA(FNewIMSI, simsActivate);

    // Ако старата карта е терминирана отписването в НАП няма да стане!
    // В крайна сметка това отписване не ни интересува особено
    try
     if (not OldSim.IsExternal) then FRegisterSimToNRA(FOldIMSI, simsSuspend);      // дава грешка при терминирана карта
    except
    end;

    // извличане номера на заявката
    SQL := 'SELECT MAX(DFA_ID) "DFA_ID" FROM DEVICES_FU_ACTIONS '+
           'WHERE (DFA_DEVFUSERIAL = '+StrToSQL(FFDSerial)+')'+
           'AND(DFA_DEVGPRSSERIAL = '+StrToSQL(FMDSerial)+')'+
           'AND(DFA_SIMIMSI = '+StrToSQL(OldSim.IMSI)+')'+
           'AND(DFA_ACTIONTYPE = '+IntToSql(fdevact_ReplaceSIM)+')';
   if not FillDataSet(SQL) then raise EAbort.Create('Select Fiscalization request ID fail: '+Device.DbInterface.LastError);
   if DataSet.FieldByName('DFA_ID').IsNull then
    raise EHandledException.Create(errcode_Fisc_InternalError, 'Системна грешка.'+sLineBreak+
                                                               'Неидентифицирана заявка за смяна SIM!',
                                                               'MAX(DFA_ID).DEVICES_FU_ACTIONS not found'+sLineBreak+
                                                               'Unexpected error during: SIM replacement');
   FRequestID    := DataSet.FieldByName('DFA_ID').AsInteger;
   FNewPayedTo   := NewSim.PayedTo;
   FNewStatusMob := NewSim.OperStat;

   FOldPayedTo   := OldSim.PayedTo;
   FOldStatusMob := OldSim.OperStat;
   
  finally
   CloseDataSet;
  end;
 except
  on E: EHandledException do
   begin
    try
     FInsertDeviceAction(fdevact_ReplaceSIM, OldSim.IMSI, 'НЕУСПЕШНА подмяна на SIM карта!',
                                                          'Грешка при подмяна на SIM карта:'+sLineBreak+
                                                          'Стар SIM: '+FOldIMSI+sLineBreak+
                                                          'Нов SIM: '+FNewIMSI+sLineBreak+
                                                          E.Message);
    except
    end;
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


end.
