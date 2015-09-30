unit NRAHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit, XMLHandler, LoginHandler, CryptoHandler,
     IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders;

const
 C_HttpNraReadTimeout_1card  = 5000;
 C_HttpNraReadTimeout_10card = 10000;

type
 TSimRequestType = (srtIMSI_List, srtIMSI_SQL);
 TSimAccountType = (satMobileOpr, satEltrade);
 TSimStatusType = (simsActivate = 1, {Активиране}
						       simsSuspend  = 2, {Временно спиране (suspended) Поради неплащане или друга причина}
						       simsDestroy  = 3, {Деактивиране (прекратяване на договор или друго)srtIMSI_List, srtIMSI_SQL}
                   simsForbidden= 4  {Допълнителен статус на СИМ подаван от производителите (при желание да забранят регистрация на ФУ със
						                          съответната СИМ. Разрешаването става с второ съобщение за съответната СИМ и флаг 1)
						                          При Спиране в полето ЕИК се вписва ЕИК на подателя на съобщението. При активиране се вписва ЕИК на крайния търговец закупил ФУ.
						                          Валидните стойности са});

 THndr_NRARegSimClient = class(THandlerClient)
 private
  FCrcHandler     : TCRC32;
  FSimRequestList : TStrings;
  FSimRequestType : TSimRequestType;
  FSimStatus      : TSimStatusType;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument);
  destructor Destroy; override;

  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property SimRequestList: TStrings read FSimRequestList write FSimRequestList;
  property SimRequestType: TSimRequestType read FSimRequestType write FSimRequestType;
  property SimStatus: TSimStatusType read FSimStatus write FSimStatus;
 end;

 THndr_NRARegSimServer = class(THandlerServer)
 private
  FCrcHandler     : TCRC32;
  FSimRequestList : TStrings;
  FSimRequestType : TSimRequestType;
  FSimStatus      : TSimStatusType;
  FSimAccount     : TSimAccountType;
  FTestRequest    : Boolean;
  FUserType       : String;
  FUserName       : String;
  FHttpClient     : TIdHTTP;
  FSSLIOHandler   : TIdSSLIOHandlerSocket;
  function FGetRequestTypeText: String;
  function FGetSimStatusText: String;
  function FGetSimAccountText: String;
  function FGetTestRequestText: String;
  function CalculateCRC(Source: String): String;
  function ValidateMSISDN(SIM_MSISDN: String): Boolean;
  function ValidateIMSI(SIM_IMSI: String): Boolean;
  function ValidateEIK(EIK, EIKType: String): Boolean;
  function PostEvent(SIM_Imsi, EvType, EvData: String): Boolean;
  function UpdateSimStatus(SimIMSI: String): Boolean;
  function PostXmlToFile(Fname: String; Stream: TStream): Boolean;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property SimRequestList: TStrings read FSimRequestList write FSimRequestList;
  property SimRequestType: TSimRequestType read FSimRequestType write FSimRequestType;
  property SimStatus: TSimStatusType read FSimStatus write FSimStatus;
  property SimStatusText: String read FGetSimStatusText;
  property SimAccount: TSimAccountType read FSimAccount write FSimAccount;
  property SimAccountText: String read FGetSimAccountText;
  property TestRequest: Boolean read FTestRequest write FTestRequest;
  property TestRequestText: String read FGetTestRequestText;
  property UserType: String read FUserType write FUserType;
  property UserName: String read FUserName write FUserName;
 end;

//******************************************************************************
//  NRA Commands
//******************************************************************************

 TCmd_NRARegSimClient = class(TCommandClient)
 private
  FLoginHandler   : THndr_LoginClient;
  FSimRegHandler  : THndr_NRARegSimClient;

  function FGetUserName: String;
  function FGetPassword: String;
  procedure FSetUserName(Value: String);
  procedure FSetPassword(Value: String);
  function FGetSimRequestList: TStrings;
  function FGetSimRequestType: TSimRequestType;
  function FGetSimStatus: TSimStatusType;
  procedure FSetSimRequestList(Value: TStrings);
  procedure FSetSimRequestType(Value: TSimRequestType);
  procedure FSetSimStatus(Value: TSimStatusType);
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property UserName: String read FGetUserName write FSetUserName;
  property Password: String read FGetPassword write FSetPassword;
  property SimRequestList: TStrings read FGetSimRequestList write FSetSimRequestList;
  property SimRequestType: TSimRequestType read FGetSimRequestType write FSetSimRequestType;
  property SimStatus: TSimStatusType read FGetSimStatus write FSetSimStatus;
 end;

 TCmd_NRARegSimServer = class(THandlerServerUserEvent)
 private
  FLoginHandler  : THndr_LoginServer;
  FSimRegHandler : THndr_NRARegSimServer;
  function FGetUserName: String;
  function FGetPassword: String;
  procedure FSetUserName(Value: String);
  procedure FSetPassword(Value: String);
  function FGetSimRequestList: TStrings;
  function FGetSimRequestType: TSimRequestType;
  function FGetSimStatus: TSimStatusType;
  function FGetSimStatusText: String;
  function FGetTestRequest: Boolean;
  procedure FSetSimRequestList(Value: TStrings);
  procedure FSetSimRequestType(Value: TSimRequestType);
  procedure FSetSimStatus(Value: TSimStatusType);
  procedure FSetTestRequest(Value: Boolean);
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property UserName: String read FGetUserName write FSetUserName;
  property Password: String read FGetPassword write FSetPassword;
  property SimRequestList: TStrings read FGetSimRequestList write FSetSimRequestList;
  property SimRequestType: TSimRequestType read FGetSimRequestType write FSetSimRequestType;
  property SimStatus: TSimStatusType read FGetSimStatus write FSetSimStatus;
  property SimStatusText: String read FGetSimStatusText;
  property TestRequest: Boolean read FGetTestRequest write FSetTestRequest;
 end;


implementation
uses BillingConstUnit, XMLHandlerMS, DateUtils, Variants;

//******************************************************************************
// THndr_NRARegSimClient
//******************************************************************************
constructor THndr_NRARegSimClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FCrcHandler     := TCRC32.Create;
 FSimRequestList := TStringList.Create;
end;

destructor THndr_NRARegSimClient.Destroy;
begin
 FCrcHandler.Free;
 FSimRequestList.Free;
 inherited Destroy;
end;

function THndr_NRARegSimClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  :=  CalculateCRC(FSimRequestList.Text + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRASimRegData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRAReqType)).Text   := IntToStr(Integer(FSimRequestType));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRASimStatus)).Text := IntToStr(Integer(FSimStatus));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text         := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text          := SCRC;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRAReqList)) do
   begin
    for I := 0 to FSimRequestList.Count - 1 do
     AppendChild(XmlDocument.CreateElement(xml_Node_NRAReq)).Text  := FSimRequestList.Strings[I];
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_NRARegSimClient.GetAnswerFromDocument: Boolean;
var iNode : IXMLNode;
    iRoot : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  iRoot           := XML_GetRootNode;
  iRoot           := XML_FindNodeByName(iRoot, xml_Node_NRASimRegData);
  FSimRequestType := TSimRequestType(StrToInt(XML_GetNodeText(iRoot, xml_Node_NRAReqType)));
  FSimStatus      := TSimStatusType( StrToInt(XML_GetNodeText(iRoot, xml_Node_NRASimStatus)));
  SDate           := XML_GetNodeText(iRoot, xml_Node_Time);
  SCRC            := XML_GetNodeText(iRoot, xml_Node_CRC);

  FSimRequestList.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_NRAReqResult);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FSimRequestList.Add(iNode.text);
    iNode := iNode.nextSibling;
   end;

  if SCRC <> CalculateCRC(FSimRequestList.Text + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_NRARegSimClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

//******************************************************************************
//   THndr_NRARegSimServer
//******************************************************************************
constructor THndr_NRARegSimServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FCrcHandler     := TCRC32.Create;
 FSimRequestList := TStringList.Create;
 FHttpClient     := TIdHTTP.Create(nil);
 FSslIOHandler   := TIdSSLIOHandlerSocket.Create(nil);

 FTestRequest    := true;
 FSimAccount     := satEltrade;

 FSSLIOHandler.SSLOptions.Method := sslvTLSv1;
 FSSLIOHandler.SSLOptions.Mode   := Unassigned;

 FHttpClient.IOHandler  := FSslIOHandler;
 FHttpClient.HTTPOptions:= [];
 FHttpClient.ReadTimeout:= C_HttpNraReadTimeout_1card;
end;

destructor THndr_NRARegSimServer.Destroy;
begin
 FCrcHandler.Free;
 FSimRequestList.Free;
 FHttpClient.Free;
 FSslIOHandler.Free;
 inherited Destroy;
end;

function THndr_NRARegSimServer.GetRequestFromDocument: Boolean;
var iNode : IXMLNode;
    iRoot : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  iRoot           := XML_GetRootNode;
  iRoot           := XML_FindNodeByName(iRoot, xml_Node_NRASimRegData);
  FSimRequestType := TSimRequestType(StrToInt(XML_GetNodeText(iRoot, xml_Node_NRAReqType)));
  FSimStatus      := TSimStatusType( StrToInt(XML_GetNodeText(iRoot, xml_Node_NRASimStatus)));
  SDate           := XML_GetNodeText(iRoot, xml_Node_Time);
  SCRC            := XML_GetNodeText(iRoot, xml_Node_CRC);

  FSimRequestList.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_NRAReqList);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FSimRequestList.Add(iNode.text);
    iNode := iNode.nextSibling;
   end;

  if SCRC <> CalculateCRC(FSimRequestList.Text + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_NRARegSimServer.ValidateMSISDN(SIM_MSISDN: String): Boolean;
var I : Integer;
begin
 Result := false;

 if Length(SIM_MSISDN) < 12 then Exit;

 for I := 1 to Length(SIM_MSISDN) do
  case I of
  1: if (SIM_MSISDN[I] <> '3') then Exit;
  2: if (SIM_MSISDN[I] <> '5') then Exit;
  3: if (SIM_MSISDN[I] <> '9') then Exit;
  4: if (SIM_MSISDN[I] <> '8') then Exit;
  else if not (SIM_MSISDN[I] in ['0'..'9']) then Exit;
  end;

 Result := true;
end;

function THndr_NRARegSimServer.ValidateIMSI(SIM_IMSI: String): Boolean;
var I : Integer;
begin
 Result := false;

 if Length(SIM_IMSI) < 15 then Exit;

 for I := 1 to Length(SIM_IMSI) do
  begin
   if not (SIM_IMSI[I] in ['0'..'9']) then Exit;
  end;

 Result := true;
end;

function THndr_NRARegSimServer.ValidateEIK(EIK, EIKType: String): Boolean;
var I : Integer;
begin
 Result := false;

 if not (StrToIntDef(EIKType, -1) in [0..3]) then Exit;

 for I := 1 to Length(EIK) do
  begin
   if not (EIK[I] in ['0'..'9']) then Exit;
  end;

 Result := true;
end;

function THndr_NRARegSimServer.PostXmlToFile(Fname: String; Stream: TStream): Boolean;
var S : String;
    F : TFileStream;
begin
 Result := false;
 try
  if Stream <> nil then
   if Stream.Size > 0 then
    begin
     S := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
     S := IncludeTrailingPathDelimiter(S + 'Log');
     S := S + Fname;
     Stream.Position := 0;
     F := TFileStream.Create(S, fmCreate);
     try
      F.CopyFrom(Stream, Stream.Size);
      Result := true;
     finally
      F.Free;
     end;
    end;
 except
 end;
end;

function THndr_NRARegSimServer.FGetRequestTypeText: String;
begin
 case FSimRequestType of
 srtIMSI_List : Result := 'IMSI';
 srtIMSI_SQL  : Result := 'SQL';
 end;
end;

function THndr_NRARegSimServer.FGetSimStatusText: String;
begin
 case FSimStatus of
 simsActivate   : Result := 'Активиране';
 simsSuspend    : Result := 'Временно спиране (suspended) Поради неплащане или друга причина';
 simsDestroy    : Result := 'Деактивиране (прекратяване на договор или друго)';
 simsForbidden  : Result := 'Забрана за регистрация';
 end;
end;

function THndr_NRARegSimServer.FGetSimAccountText: String;
begin
 case FSimAccount of
 satMobileOpr : Result := 'Тест Моб.оператор';
 satEltrade   : Result := 'Елтрейд';
 end;
end;

function THndr_NRARegSimServer.FGetTestRequestText: String;
begin
 if FTestRequest then Result := '[ТЕСТОВ сървър на НАП]'
  else Result := '';
end;

function THndr_NRARegSimServer.PostEvent(SIM_Imsi, EvType, EvData: String): Boolean;
var SQL : String;
    EIK : String;
    RCnt: Integer;
begin
 try
  with Device do
   begin
    if SIM_Imsi = '' then EIK := 'NULL'
     else EIK := '(SELECT SIM_CUSTEIK FROM SIM WHERE SIM_IMSI = '+DbInterface.StrToSQL(SIM_Imsi)+')';

    SQL := 'INSERT INTO EVENTS_NRA (EN_DATETIME, EN_MODULE, EN_SUBMODULE, EN_REMOTEIP, '+
           'EN_DEVTYPE, EN_DEVSERIAL, EN_TYPE, EN_USERTYPE, EN_USERNAME, EN_SIM_IMSI, '+
           'EN_SIM_EIK, EN_DATA) VALUES ('+
           DBInterface.DateTimeToSQL(Now)                    +','+ // EN_DATETIME DATETIME
           DBInterface.StrToSQL(C_ModuleNameDev, 20)         +','+ // EN_MODULE VARCHAR(20)
           DBInterface.StrToSQL(ServerName, 50)              +','+ // EN_SUBMODULE VARCHAR(50)
           DBInterface.StrToSQL(ConnectionInfo.RemoteIP, 20) +','+ // EN_REMOTEIP VARCHAR(20)
           DBInterface.StrToSQL(DeviceInfo.Dev_Type, 10)     +','+ // EN_DEVTYPE VARCHAR(10)
           DBInterface.StrToSQL(DeviceInfo.Dev_Serial, 10)   +','+ // EN_DEVSERIAL VARCHAR(10)
           DBInterface.StrToSQL(EvType, 10)                  +','+ // EN_TYPE VARCHAR(10)
           DBInterface.StrToSQL(FUserType , 1)               +','+ // EN_USERTYPE VARCHAR(1)
           DBInterface.StrToSQL(FUserName , 50)              +','+ // EN_USERNAME VARCHAR(50)
           DBInterface.StrToSQL(SIM_Imsi , 20, true)         +','+ // EN_SIM_IMSI VARCHAR(20)
           EIK                                               +','+ // EN_SIM_EIK VARCHAR(20)
           DBInterface.StrToSQL(EvData)                      +')'; // EN_DATA VARCHAR(1024)
   end;

  Result := Device.DbInterface.ExecuteSQLStatement(SQL, RCnt);
  if not Result then LastError := 'Post NRAEvent fail: '+Device.DBInterface.LastError;
 except
  on E: Exception do
   begin
    Result    := False;
    LastError := 'Post NRAEvent fail: '+E.Message;
   end;
 end;
end;

function THndr_NRARegSimServer.UpdateSimStatus(SimIMSI: String): Boolean;
var SQL : String;
    RCnt: Integer;
begin
 try
   SQL := 'UPDATE SIM SET SIM_STATUSNAP = ';
   case FSimStatus of
   simsActivate  : SQL := SQL + '2';
   simsSuspend   : SQL := SQL + '3';
   simsDestroy   : SQL := SQL + '4';
   simsForbidden : SQL := SQL + '1';
   end;
   SQL := SQL + ' WHERE SIM_IMSI = '+ Device.DbInterface.StrToSQL(SimIMSI);

   Result := Device.DbInterface.ExecuteSQLStatement(SQL, RCnt);
   if not Result then LastError := 'Change SIM_NRASTATUS fail: '+Device.DBInterface.LastError;
 except
  on E: Exception do
   begin
    Result    := False;
    LastError := 'Change SIM_NRASTATUS fail: '+E.Message;
   end;
 end;
end;

function THndr_NRARegSimServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var Xml_Doc: IXMLDocument;
    iRoot  : IXMLNode;
    iNode  : IXMLNode;
    S,S1   : String;
    Url    : String;
    I, Cnt : Integer;
    C1, C2 : Integer;
    ST     : TDateTime;
    Source : TStringStream;
    Resp   : TStringStream;

const
    // данни за мобилен оператор в тестовия сървър
    C_Nra_MobOprBulstat   : String = '123123123';
    C_Nra_MobOprUserName  : String = 'U123123123';
    C_Nra_MobOprPassword  : String = 'TEST6382937402';

    // данни за Елтрейд в тесттовия и продуктивния сървър
    C_Nra_EltradeBulstat  : String = '832076302';
    C_Nra_EltradeUserName : String = 'U832076302';
    C_Nra_EltradePassword : String = 'E75RT6432L455N';

    // адреси на сървърите
    C_Nra_Url_Test        : String = 'https://nraapp02.nra.bg/testsimreg/simregport';
    C_Nra_Url_Production  : String = 'https://nraapp02.nra.bg/simreg/simregport';
//    C_Nra_Url_Production  : String = 'https://nraapp02.nra.bg/testsimreg/simregport';
    C_XMLNameSpace        : String = 'nra:fdmon:simreg:jaxbns';

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
    function CreateNode_(NodeName_: String): IXMLNode;
    begin
     Result := XML_Doc.createNode(NODE_ELEMENT, NodeName_, C_XMLNameSpace);
    end;
    function FieldText_(FieldName_: String): String;
    begin
     Result := Trim(Device.DbInterface.DataSet.FieldByName(FieldName_).AsString);
    end;
    function GetNodeText(OnRoot_: IXMLNode; NodeName_: String): String;
    var iN_ : IXMLNode;
    begin
     Result := '';
     if OnRoot_ <> nil then
      begin
       iN_ := OnRoot_.selectSingleNode(NodeName_);
       if iN_ <> nil then Result := iN_.text;
      end;
    end;
    function DoRequest(Url_: String; Src_, Resp_: TStream; TryCnt: Byte; var ErrMsg: String): Boolean;
    var tout : TDateTime;
    begin
     tout   := Now;
     Result := true;
     try
      if FHttpClient.Connected then FHttpClient.Disconnect;
      FHttpClient.DoRequest(hmPost, Url_, Src_, Resp_);
      FHttpClient.Disconnect;
     except
       on E: Exception do
        begin
         Result := false;
         ErrMsg := 'Communication with NRA FAIL.'+sLineBreak+
                   'Url: '+Url+sLineBreak+
                   'Time: '+IntToStr(MilliSecondsBetween(tout, Now))+sLineBreak+
                   E.Message;
         PostEvent('', 'ERROR', '['+IntToStr(errcode_NraSimReg_PostFail)+']['+IntToStr(TryCnt)+' try] '+ ErrMsg);
        end;
     end;
    end;
begin
 Source := nil;
 Resp   := nil;
 C1 := 0; C2 := 0;
 try
  S := '';
  for I := 0 to FSimRequestList.Count - 1 do
   begin
    case FSimRequestType of
    srtIMSI_List: S := S + ','+Device.DbInterface.StrToSQL(FSimRequestList.Strings[I]);
    srtIMSI_SQL : S := S + ' '+FSimRequestList.Strings[I];
    end;
   end;
  Delete(S, 1, 1);
  if S = '' then raise EHandledException.Create(errcode_NraSimReg_InvalidInput, 'Невалидни входни данни!'+sLineBreak+
                                                                                'Тип данни: '+FGetRequestTypeText+sLineBreak+
                                                                                'Операция: '+FGetSimStatusText+sLineBreak+
                                                                                'Данни:'+FSimRequestList.Text);

  FSimRequestList.Clear; // тук ще запишем отговора - резултата от заявката

  if not Device.DbInterface.FillDataSet(
               'SELECT TOP(500) S.SIM_IMSI, S.SIM_MSISDN, S.SIM_CUSTEIK, C.CUST_EIKTYPE '+
               'FROM SIM S '+
               'LEFT JOIN CUSTOMERS C ON C.CUST_EIK = S.SIM_CUSTEIK '+
               'WHERE S.SIM_IMSI IN ('+S+')') then
   raise EAbort.Create('Select SIM fail: '+Device.DbInterface.LastError);

  Cnt := Device.DbInterface.DataSet.RecordCount;
  if Cnt > 0 then
   begin
    ST := Now;
    FSimRequestList.Add(IntToStr(Cnt)+' SIM(s) pending for NRA registration');

    // validate data
    Device.DbInterface.DataSet.First;
    while not Device.DbInterface.DataSet.Eof do
     begin
      if not ValidateIMSI(FieldText_('SIM_IMSI')) then
       raise EHandledException.Create(errcode_NraSimReg_InvalidSimData, 'Неуспешна валидация на СИМ. Невалиден IMSI номер',
                                                                        'SIM validation fail. Invalid SIM_IMSI ['+FieldText_('SIM_IMSI')+']');
      if not ValidateMSISDN(FieldText_('SIM_MSISDN')) then
       raise EHandledException.Create(errcode_NraSimReg_InvalidSimData, 'Неуспешна валидация на СИМ. Невалиден MSISDN номер',
                                                                        'SIM validation fail. Invalid SIM_MSISDN "'+FieldText_('SIM_MSISDN')+'" ['+FieldText_('SIM_IMSI')+']');
      if not ValidateEIK(FieldText_('SIM_CUSTEIK'), FieldText_('CUST_EIKTYPE')) then
       raise EHandledException.Create(errcode_NraSimReg_InvalidSimData, 'Неуспешна валидация на СИМ. Невалиден собственик - ЕИК',
                                                                        'SIM validation fail. Invalid SIM_CUSTEIK "'+FieldText_('SIM_CUSTEIK')+'"('+FieldText_('CUST_EIKTYPE')+') ['+FieldText_('SIM_IMSI')+']');
      Device.DbInterface.DataSet.Next;
     end;

    // generate XML
    Xml_Doc := CreateXMLDoc;
    try
     Xml_Doc.appendChild(Xml_Doc.createProcessingInstruction('xml', 'version="1.0" encoding="windows-1251"'));
     iNode := Xml_Doc.appendChild(CreateNode_('simreg'));

     if (FSimAccount = satMobileOpr)and(FTestRequest) then
      begin
       with (iNode as IXMLElement) do
        begin
         setAttribute('user', C_Nra_MobOprUserName);
         setAttribute('pass', C_Nra_MobOprPassword);
        end;
       iNode.appendChild(CreateNode_('senderbulstat')).text := C_Nra_MobOprBulstat;
      end
     else
      begin
       with (iNode as IXMLElement) do
        begin
         setAttribute('user', C_Nra_EltradeUserName);
         setAttribute('pass', C_Nra_EltradePassword);
        end;
       iNode.appendChild(CreateNode_('senderbulstat')).text := C_Nra_EltradeBulstat;
      end;


     Device.DbInterface.DataSet.First;
     while not Device.DbInterface.DataSet.Eof do
      begin
       with iNode.appendChild(CreateNode_('sim')) do
        begin
         appendChild(CreateNode_('imsi')).text    := FieldText_('SIM_IMSI');
         appendChild(CreateNode_('msisdn')).text  := FieldText_('SIM_MSISDN');

         if (FSimAccount = satMobileOpr)and(FTestRequest) then
          begin
           appendChild(CreateNode_('eik')).text     := C_Nra_EltradeBulstat;
           appendChild(CreateNode_('eiktype')).text := '0';
          end
         else
          begin
           appendChild(CreateNode_('eik')).text     := FieldText_('SIM_CUSTEIK');
           appendChild(CreateNode_('eiktype')).text := FieldText_('CUST_EIKTYPE');
          end;

         if FSimStatus = simsForbidden then
          begin
           appendChild(CreateNode_('simstatus')).text   := '2';
           appendChild(CreateNode_('fdsimstatus')).text := '2';
          end
         else
          begin
           appendChild(CreateNode_('simstatus')).text := IntToStr(Integer(FSimStatus));
           appendChild(CreateNode_('fdsimstatus')).text := '1';
          end;
         appendChild(CreateNode_('dateaction')).text := FormatDateTime('YYYY-MM-DD"T"HH:NN:SS', Now);
        end;
       Device.DbInterface.DataSet.Next;
      end;

     // увеличаване на таймайта при множество карти
     if Device.DbInterface.DataSet.RecordCount > 10 then FHttpClient.ReadTimeout := C_HttpNraReadTimeout_10card
      else FHttpClient.ReadTimeout := C_HttpNraReadTimeout_1card;

     Source := TStringStream.Create(Xml_Doc.xml);
//     Xml_Doc.save('send.xml');
    finally
     FreeXmlDoc(Xml_Doc);
    end;
    Device.DbInterface.CloseDataSet;

    // Send Data
    with FHttpClient.Request do
     begin
      Connection          := 'keep-alive';
      ContentType         := 'text/xml';
      Accept              := '*/*';
      AcceptCharSet       := 'windows-2151;q=0.7,*;q=0.7';
      AcceptLanguage      := 'bg,en-us;q=0.7,en;q=0.3';
      BasicAuthentication := False;
      UserAgent           := 'Eltrade Ltd.';
     end;

    if Source = nil then
     raise EHandledException.Create(errcode_NraSimReg_CreateXMLFail, 'Неуспешно генериране на заявка за SIM към NAP',
                                                                     'Fail generate NRA request. XML is empty');
    Resp := TStringStream.Create('');

    if FTestRequest then Url := C_Nra_Url_Test
     else Url := C_Nra_Url_Production;

    if not DoRequest(Url, Source, Resp, 1, S) then
     begin
      if not DoRequest(Url, Source, Resp, 2, S) then
       begin
        if not DoRequest(Url, Source, Resp, 3, S) then
         raise EHandledException.Create(errcode_NraSimReg_PostFail3times, 'Неуспешна връзка със сървъра на NAP'+sLineBreak+
                                                                          S,
                                                                          '');
//                                                                          'Communication with NRA server FAIL.'+sLineBreak+
//                                                                          S);
       end;
     end;

{    try
     if FTestRequest then Url := C_Nra_Url_Test
      else Url := C_Nra_Url_Production;

       if FHttpClient.Connected then FHttpClient.Disconnect;
       FHttpClient.DoRequest(hmPost, Url, Source, Resp);
       FHttpClient.Disconnect;
    except
     on E: Exception do
      begin
       raise EHandledException.Create(errcode_NraSimReg_PostFail, 'Неуспешна връзка със сървъра на NAP'+sLineBreak+
                                                                  E.Message,
                                                                  'Communication with NRA server FAIL.'+sLineBreak+
                                                                  'Unsuccessfull POST to: '+Url+sLineBreak+
                                                                  E.Message);
      end;
    end;}

    // validate responce
    Xml_Doc := CreateXMLDoc;
    try
     if not Xml_Doc.loadXML(Resp.DataString) then
      begin
       // todo post log
       raise EHandledException.Create(errcode_NraSimReg_InvalidResp, 'Невалиден отговор от НАП. Грешен XML.',
                                                                     'Invalid responce. Error in XML format:'+sLineBreak+
                                                                     'Reason:'+Xml_Doc.parseError.reason+
                                                                     ' [Line:'+IntToStr(Xml_Doc.parseError.line)+
                                                                     ' Linepos:'+IntToStr(Xml_Doc.parseError.linepos)+
                                                                     ' Filepos:'+IntToStr(Xml_Doc.parseError.filepos)+']');
      end;
//     Xml_Doc.save('responce.xml');
     iRoot := Xml_Doc.documentElement;
     if iRoot = nil then
      raise EHandledException.Create(errcode_NraSimReg_InvldXMLContnt, 'Невалиден отговор от НАП. Грешен XML.',
                                                                       'NRA responce error: XML document is empty');
     S := GetNodeText(iRoot, 'errormsg');
     if S <> '' then
      raise EHandledException.Create(errcode_NraSimReg_ReturnError, 'Сървъра на NAP отговаря с грешка:'+sLineBreak+
                                                                    S,
                                                                    '');
//                                                                    'NRA server respond with error:'+sLineBreak+
//                                                                    S);  // няма нужда да пълним системния лог
     S := GetNodeText(iRoot, 'senderbulstat');
     if S = '' then
      raise EHandledException.Create(errcode_NraSimReg_ReturnError, 'Невалиден отговор от НАП. Грешен XML.',
                                                                    'NRA server respоnce mismatch:'+sLineBreak+
                                                                    'Sender bulstat is empty!');
     S := GetNodeText(iRoot, 'numsims');
     if S = '' then
      raise EHandledException.Create(errcode_NraSimReg_ReturnError, 'Невалиден отговор от НАП. Грешен XML.',
                                                                    'NRA server respnd: ZERO SIM registered');

     iNode := iRoot.firstChild;
     while iNode <> nil do
      begin
       if AnsiSameText(iNode.nodeName, 'simres') then
        begin
         S  := GetNodeText(iNode, 'imsi');
         S1 := GetNodeText(iNode, 'status');
         if S = '' then
          raise EHandledException.Create(errcode_NraSimReg_InvldXMLContnt, 'Невалиден отговор от НАП. Грешен XML.',
                                                                           'NRA responce error: Invalid XML content. Empty "simres/imsi"');
         if S1 = '0' then
          begin // всичко е ОК
           Inc(C1);
           if not PostEvent(S, 'REGOK', 'Регистрация SIM.'+TestRequestText+sLineBreak+
                                        'Операция: '+SimStatusText+sLineBreak+
                                        'IMSI: '+S+sLineBreak+
                                        'Подател: '+SimAccountText+sLineBreak+
                                        'Url: '+Url+sLineBreak+
                                        'Done in: '+IntToStr(MilliSecondsBetween(ST, Now))+' msec.') then raise EAbort.Create(LastError);
           if not UpdateSimStatus(S) then raise EAbort.Create(LastError);
          end
         else
          begin // грешка при регистрация на картата
           Inc(C2);
           if not PostEvent(S, 'REGFAIL', 'Сървъра на NAP връща грешка!'+sLineBreak+
                                          '['+S1+'] '+GetNodeText(iNode, 'errormsg')+sLineBreak+
                                          'Url: '+Url) then raise EAbort.Create(LastError);
           if (Cnt = 1)and(StrToIntDef(S1, 0) <> 1) then
            raise EHandledException.Create(errcode_NraSimReg_ReturnError, 'Сървъра на NAP връща грешка!'+sLineBreak+
                                                                          '['+S1+'] '+GetNodeText(iNode, 'errormsg'),
                                                                          'NRA respond with error.'+sLineBreak+
                                                                          'Url: '+Url+sLineBreak+
                                                                          'IMSI: '+S+sLineBreak+
                                                                          '['+S1+'] '+GetNodeText(iNode, 'errormsg'));
          end;
        end;
       iNode := iNode.nextSibling;
      end;
    finally
     FreeXmlDoc(Xml_Doc);
    end;

    if Cnt > 1 then
     begin
      if not PostEvent('', 'REGDONE', 'NRA registration complete. ['+IntToStr(MilliSecondsBetween(ST, Now))+' msec.]'+sLineBreak+
                                      'Url: '+Url+sLineBreak+
                                      IntToStr(Cnt)+ ' SIM pending'+sLineBreak+
                                      IntToStr(C1) + ' SIM succeed'+sLineBreak+
                                      IntToStr(C2) + ' SIM failed') then raise EAbort.Create(LastError);
     end;

    FSimRequestList.Add(IntToStr(C1) + ' cards registered successfully.');
    FSimRequestList.Add(IntToStr(C2) + ' cards failed in registration.');
    FSimRequestList.Add('Done in '+IntToStr(MilliSecondsBetween(ST, Now))+' msec.');
   end
  else  //if Device.DbInterface.DataSet.RecordCount > 0 then
   begin
    FSimRequestList.Add('No SIM found corresponding to:');
    FSimRequestList.Add(S);
    PostEvent('', 'REG0', 'No SIM found corresponding to:'+S)
   end;

  SetErrorCode(errcode_ExecuteSucceed, '');
  Result := true;
 except
  on E: EHandledException do
   begin
    Device.DbInterface.CloseDataSet;
    PostEvent('', 'ERROR', '['+IntToStr(E.ErrorCode)+'] '+ E.Message+sLineBreak+'Url: '+Url);
    if (Source <> nil)and(Resp <> nil) then
     begin
      // Трябва да имаме отговор за да запишем заявката във файл. Иначе ппри липса на сървър само се трупат файлове със заявки...
      if (Source.Size > 0)and(Resp.Size > 0) then
       begin
        S := FormatDateTime('YYMMDD-HHNNSS', Now)+'_NRARequest.xml';
        if PostXmlToFile(S, Source) then PostEvent('', 'ERROR', 'NRA request XML saved to file:'+S);
        S := FormatDateTime('YYMMDD-HHNNSS', Now)+'_NRAResponce.xml';
        if PostXmlToFile(S, Resp) then PostEvent('', 'ERROR', 'NRA responce XML saved to file:'+S);
       end;
     end;
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName);
     end;
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    PostEvent('', 'ERROR', 'Unhandled exception: '+LastError);
    Result    := false;
   end;
 end;
 if Source <> nil then Source.Free;
 if Resp <> nil then Resp.Free;
end;

function THndr_NRARegSimServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  :=  CalculateCRC(FSimRequestList.Text + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRASimRegData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRAReqType)).Text   := IntToStr(Integer(FSimRequestType));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRASimStatus)).Text := IntToStr(Integer(FSimStatus));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text         := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text          := SCRC;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NRAReqResult)) do
   begin
    for I := 0 to FSimRequestList.Count - 1 do
     AppendChild(XmlDocument.CreateElement(xml_Node_NRAReq)).Text  := FSimRequestList.Strings[I];
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

function THndr_NRARegSimServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

//******************************************************************************
//   TCmd_NRARegSimClient
//******************************************************************************
constructor TCmd_NRARegSimClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FLoginHandler  := THndr_LoginClient.Create(XmlDoc);
 FSimRegHandler := THndr_NRARegSimClient.Create(XmlDoc);
end;

destructor TCmd_NRARegSimClient.Destroy;
begin
 FLoginHandler.Free;
 FSimRegHandler.Free;
 inherited Destroy;
end;

function TCmd_NRARegSimClient.GetCommandName: String;
begin
 Result := cmd_NRARegisterSim;
end;

function TCmd_NRARegSimClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FLoginHandler.AddRequestToDocument then raise EAbort.Create(FLoginHandler.LastError);
  if not FSimRegHandler.AddRequestToDocument then raise EAbort.Create(FSimRegHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FLoginHandler.CalculateCRC(SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NraSimReg));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_NRARegSimClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FLoginHandler.GetAnswerFromDocument then raise EAbort.Create(FLoginHandler.LastError);
  if not FSimRegHandler.GetAnswerFromDocument then raise EAbort.Create(FSimRegHandler.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_NraSimReg);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FLoginHandler.CalculateCRC(SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_NRARegSimClient.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_NRARegSimClient.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_NRARegSimClient.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_NRARegSimClient.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

function TCmd_NRARegSimClient.FGetSimRequestList: TStrings;
begin
 Result := FSimRegHandler.SimRequestList;
end;

function TCmd_NRARegSimClient.FGetSimRequestType: TSimRequestType;
begin
 Result := FSimRegHandler.SimRequestType;
end;

function TCmd_NRARegSimClient.FGetSimStatus: TSimStatusType;
begin
 Result := FSimRegHandler.SimStatus;
end;

procedure TCmd_NRARegSimClient.FSetSimRequestList(Value: TStrings);
begin
 FSimRegHandler.SimRequestList.Clear;
 FSimRegHandler.SimRequestList.AddStrings(Value);
end;

procedure TCmd_NRARegSimClient.FSetSimRequestType(Value: TSimRequestType);
begin
 FSimRegHandler.SimRequestType := Value;
end;

procedure TCmd_NRARegSimClient.FSetSimStatus(Value: TSimStatusType);
begin
 FSimRegHandler.SimStatus := Value;
end;

//******************************************************************************
//   TCmd_NRARegSimServer
//******************************************************************************
constructor TCmd_NRARegSimServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FLoginHandler := THndr_LoginServer.Create(XmlDoc, RemoteDevice);
 FSimRegHandler:= THndr_NRARegSimServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_NRARegSimServer.Destroy;
begin
 FLoginHandler.Free;
 FSimRegHandler.Free;
 inherited Destroy;
end;

function TCmd_NRARegSimServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FLoginHandler.GetRequestFromDocument then raise EAbort.Create(FLoginHandler.LastError);
  if not FSimRegHandler.GetRequestFromDocument then raise EAbort.Create(FSimRegHandler.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_NraSimReg);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FLoginHandler.CalculateCRC(SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_NRARegSimServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result := true;
 try
  if not FLoginHandler.Execute(ErrCode, UserError) then raise EAbort.Create(FLoginHandler.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    FSimRegHandler.UserType := 'A';
    FSimRegHandler.UserName := FLoginHandler.UserName;
    if not FSimRegHandler.Execute(ErrCode, UserError) then raise EAbort.Create(FSimRegHandler.LastError);
   end;

 except
  on E: EHandledException do
   begin
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_NRARegSimServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  FLoginHandler.XmlDocument  := Self.XmlDocument; // много е важно да превключим документа
  FSimRegHandler.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FLoginHandler.AddAnswerToDocument then raise EAbort.Create(FLoginHandler.LastError);
  if not FSimRegHandler.AddAnswerToDocument then raise EAbort.Create(FSimRegHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FLoginHandler.CalculateCRC(SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_NraSimReg));
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

function TCmd_NRARegSimServer.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_NRARegSimServer.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_NRARegSimServer.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_NRARegSimServer.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

function TCmd_NRARegSimServer.FGetSimRequestList: TStrings;
begin
 Result := FSimRegHandler.SimRequestList;
end;

function TCmd_NRARegSimServer.FGetSimRequestType: TSimRequestType;
begin
 Result := FSimRegHandler.SimRequestType;
end;

function TCmd_NRARegSimServer.FGetSimStatus: TSimStatusType;
begin
 Result := FSimRegHandler.SimStatus;
end;

function TCmd_NRARegSimServer.FGetSimStatusText: String;
begin
 Result := FSimRegHandler.SimStatusText;
end;

function TCmd_NRARegSimServer.FGetTestRequest: Boolean;
begin
 Result := FSimRegHandler.TestRequest;
end;

procedure TCmd_NRARegSimServer.FSetSimRequestList(Value: TStrings);
begin
 FSimRegHandler.SimRequestList.Clear;
 FSimRegHandler.SimRequestList.AddStrings(Value);
end;

procedure TCmd_NRARegSimServer.FSetSimRequestType(Value: TSimRequestType);
begin
 FSimRegHandler.SimRequestType := Value;
end;

procedure TCmd_NRARegSimServer.FSetSimStatus(Value: TSimStatusType);
begin
 FSimRegHandler.SimStatus := Value;
end;

procedure TCmd_NRARegSimServer.FSetTestRequest(Value: Boolean);
begin
 FSimRegHandler.TestRequest := Value;
end;


initialization
 IdSSLOpenSSLHeaders.Load;

finalization
 IdSSLOpenSSLHeaders.Unload;

end.
