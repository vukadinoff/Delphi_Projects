unit DeviceUnit;

interface

uses Classes, SysUtils, DBInterfaceUnit, IdTCPServer, MyIdCustomHttpServer, ActiveX,
     BillingConstUnit, XMLHandler;

type
 TRemoteDevice = class;

 TConnectionInfo = record
  RemoteHost    : String;
  RemoteIP      : String;
  RemotePort    : Integer;
  LocalPort     : Integer;
  ConnectTime   : TDateTime;
  DisconnectTime: TDateTime;
  LastAction    : TDateTime;
 end;

 TDeviceInfo = record
  Dev_Type    : String;
  Dev_Serial  : String;
  Dev_Version : String;
 end;

 TCommandType   = (cmdPing, cmdTest, cmdUpdate, cmdFiscalize, cmdSystem);
 TCommandTypes  = Set of TCommandType;

 TBaseProcessor = class(TObject)
 private
  FDBInterface    : TDBInterfaceADO;
  FDevice         : TRemoteDevice;
  FLastError      : String;
  FTestMode       : Boolean;
  FInXmlDoc       : IXMLDocument;
  FOutXmlDoc      : IXMLDocument;
  FInCmdText      : String;
  FInCmdVersion   : String;
  FInCmdAppName   : String;
  procedure FPostErrorFile(Fname, Content: String);
 protected
  function FGetProcessorVersion: String; virtual;
 public
  constructor Create(DBInterface_: TDBInterfaceADO; RemoteDevice_: TRemoteDevice);
  destructor Destroy; override;

  property DBInterface: TDBInterfaceADO read FDBInterface write FDBInterface;
  property Device: TRemoteDevice read FDevice write FDevice;
  property XmlInDocument: IXMLDocument read FInXmlDoc write FInXmlDoc;
  property XmlOutDocument: IXMLDocument read FOutXmlDoc write FOutXmlDoc;
  property ProcessorVersion: String read FGetProcessorVersion;
  property LastError: String read FLastError write FLastError;
  property TestMode: Boolean read FTestMode write FTestMode;
  property InCommandText: String read FInCmdText write FInCmdText;
  property InCommandVersion: String read FInCmdVersion write FInCmdVersion;
  property InCommandAppname: String read FInCmdAppName write FInCmdAppName;

  function InDoc_Load(Source: String): Boolean; virtual; abstract;
  function OutDoc_InitHeader(Command: String): Boolean; virtual; abstract;
  function OutDoc_SetResponceCode(Code: Integer; ErrorMessage: String): Boolean; virtual; abstract;
  function ProcessData(var InfoMsg: String): Boolean; virtual; abstract;
 end;

 TDataProcessor = class(TBaseProcessor)
 protected
  function FGetProcessorVersion: String; override;
 public
  function InDoc_Load(Source: String): Boolean; override;
  function OutDoc_InitHeader(Command: String): Boolean; override;
  function OutDoc_SetResponceCode(Code: Integer; ErrorMessage: String): Boolean; override;
 end;

 TDataProcessorCCDevice = class(TBaseProcessor)
 private
 protected
  function FGetProcessorVersion: String; override;
  procedure PostLogToDB(DevSerial, Request, Responce: String; Err: Integer);
 public
  function InDoc_Load(Source: String): Boolean; override;
  function OutDoc_InitHeader(Command: String): Boolean; override;
  function OutDoc_SetResponceCode(Code: Integer; ErrorMessage: String): Boolean; override;
 end;

 TProcessorClass = class of TBaseProcessor;

{ TDataProcessorUsrEvnt = class(TDataProcessor)
 public
  function PostEventUser(ErrorCode: Integer; EvType, UserType, UserName, DevType, DevSerial, EvData: String): Boolean;
  function SkipSomeErrors(ErrCode: Integer): Integer;
 end;}

 TRemoteDevice = class(TObject)
 private
  FDbInterface    : TDBInterfaceADO;
  FCommandList    : TStrings;
  FServerName     : String;
  FLogPath        : String;
  FTestMode       : Boolean;
  FConnectionInfo : TConnectionInfo;
  FDeviceInfo     : TDeviceInfo;
  FAcceptCmds     : TCommandTypes;
  FDataProc       : TBaseProcessor;

  function FGetDeviceInfoAsText: String;
  function FGetConnectedTimeSec: Integer;
  function FGetConnectedTimeMSecs: Int64;
  function FGetIdleTimeSec: Integer;
 public
  constructor Create(DBConnectionString, ServerName, LogPath: String; AcceptedCmds: TCommandTypes;
                     TestMode: Boolean);
  destructor Destroy; override;

  property ConnectionInfo: TConnectionInfo read FConnectionInfo write FConnectionInfo;
  property DeviceInfo: TDeviceInfo read FDeviceInfo write FDeviceInfo;
  property DeviceInfoAsText: String read FGetDeviceInfoAsText;
  property ConnectedTimeSec: Integer read FGetConnectedTimeSec;
  property ConnectedTimeMSecs: Int64 read FGetConnectedTimeMSecs;
  property IdleTimeSec: Integer read FGetIdleTimeSec;
  property AcceptedCommands: TCommandTypes read FAcceptCmds write FAcceptCmds;
  property ServerName: String read FServerName write FServerName;
//  property DataProcessor: TDataProcessor read FDataProc write FDataProc;
  property DbInterface: TDBInterfaceADO read FDbInterface write FDbInterface;
  property TestMode: Boolean read FTestMode write FTestMode;

  function PostEventSystem(EvType, EvData: String; EvCommand: String=''): Boolean;
//  function StreamAsText(Stream_: TStream; Length_: Word): String;
//  procedure WriteStringToStream(Str: String; Stream: TStream);
//  procedure ReadStringFromStream(Stream_: TStream; var Str_: String; Length_: Word);

  function ProcessData(RequestInfo: TIdHTTPRequestInfo; var Responce: String; var ErrorMsg: String): Boolean;

  function DoHttpConnect(Thread: TIdPeerThread; var ErrMsg: String): Boolean;
  function DoHttpGetRequest(Thread: TIdPeerThread; RequestInfo: TIdHTTPRequestInfo; var ErrMsg: String): Boolean;
  function DoHttpDisconnect(Thread: TIdPeerThread): Boolean;
 end;

implementation
uses DateUtils, CryptoHandler,
     DP_FiscUnit, DP_PingUnit, DP_UpdateUnit, DP_SystemUnit, DP_ESKUnit, DP_NRAUnit,
  XMLHandlerMS;


//*********************************************************************************
//                        TBaseProcessor
//*********************************************************************************
constructor TBaseProcessor.Create(DBInterface_: TDBInterfaceADO; RemoteDevice_: TRemoteDevice);
begin
 inherited Create();
 FDBInterface := DBInterface_;
 FDevice      := RemoteDevice_;
 FInXmlDoc    := CreateXMLDoc;
 FOutXmlDoc   := CreateXMLDoc;
end;

destructor TBaseProcessor.Destroy;
begin
 FreeXmlDoc(FInXmlDoc);
 FreeXmlDoc(FOutXmlDoc);
 inherited Destroy;
end;

function TBaseProcessor.FGetProcessorVersion: String;
begin
 Result := '1.0';
end;

procedure TBaseProcessor.FPostErrorFile(Fname, Content: String);
var F : TextFile;
begin
try
 AssignFile(F, Fname);
 try
  Rewrite(F);
  Write(F, Content);
 finally
  CloseFile(F);
 end;
except
end;
end;

//*********************************************************************************
//                        TDataProcessor
//*********************************************************************************
function TDataProcessor.FGetProcessorVersion: String;
begin
 Result := '1.0';
end;

function TDataProcessor.OutDoc_InitHeader(Command: String): Boolean;
var iNode: IXMLElement;
    iPI  : IXMLProcessingInstruction;
begin
 try
   if FOutXmlDoc.documentElement = nil then
    begin
     iNode := FOutXmlDoc.createElement(xml_Node_DocumentRoot);
     iNode.setAttribute(xml_Attr_Command,     Command);
     iNode.setAttribute(xml_Attr_Version,     ProcessorVersion);
     iNode.setAttribute(xml_Attr_Application, ExtractFileName(ParamStr(0)));
     FOutXmlDoc.documentElement := iNode;
     iPI := FOutXmlDoc.createProcessingInstruction('xml', xml_ProcessInstruction);
     FOutXmlDoc.insertBefore(iPI, iNode);
     Result := true;
    end
   else
    Result := true;
 except
  on E: Exception do
   begin
    FLastError := '[InitXMLDocument]'+E.Message;
    Result := false;
   end;
 end;
end;

function TDataProcessor.OutDoc_SetResponceCode(Code: Integer; ErrorMessage: String): Boolean;
var iNode : IXMLNode;
    I     : Integer;
    S     : String;
begin
 try
  iNode := FOutXmlDoc.documentElement;
  if iNode = nil then raise EAbort.Create('XML document is not initialized');

  iNode.appendChild(FOutXmlDoc.createElement(xml_Node_ResultCode)).Text := IntToStr(Code);
  if Code <> 0 then
   begin
    S := '';
    for I := 1 to Length(ErrorMessage) do
     begin
      case ErrorMessage[I] of
      #01..#12: S := S + '';
      #13     : S := S + '|';
      #14..#31: S := S + '';
      else      S := S + ErrorMessage[I];
      end;
     end;
    iNode.appendChild(FOutXmlDoc.createElement(xml_Node_ErrorMessage)).Text := S;
   end;
  Result := true;
 except
  on E: Exception do
   begin
    FLastError := '[SetXMLResponceCode]'+E.Message;
    Result := false;
   end;
 end;
end;

function TDataProcessor.InDoc_Load(Source: String): Boolean;
var iNode: IXMLElement;
    Step : String;
    Fname: String;
begin
 Result := true;
 if Source = '' then Exit;

 try
  Step   := 'Parse XML';
  Result := FInXmlDoc.loadXML(Source);
  if not Result then
   begin
    Fname := 'XmlErr_'+FormatDateTime('YYMMDD-HHHHNN', Now)+'.xml';
    FPostErrorFile(Device.FLogPath + Fname, Source);
    raise EAbort.Create(FInXmlDoc.parseError.reason+
                        ' [Line:'+IntToStr(FInXmlDoc.parseError.line)+
                        ' Linepos:'+IntToStr(FInXmlDoc.parseError.linepos)+
                        ' Filepos:'+IntToStr(FInXmlDoc.parseError.filepos)+']['+Fname+']');
   end;

  iNode := FInXmlDoc.documentElement;
  if iNode = nil then raise EAbort.Create('XML document is empty');

  Step := 'Get Root Attributes';
  InCommandText    := iNode.getAttribute(xml_Attr_Command);
  InCommandVersion := iNode.getAttribute(xml_Attr_Version);
  InCommandAppname := iNode.getAttribute(xml_Attr_Application);
 except
  on E: Exception do
   begin
    FLastError := '[LoadIncommingXML]['+Step+']'+E.Message;
    Result := false;
   end;
 end;
end;

//*********************************************************************************
//                        TDataProcessorCCDevice
//*********************************************************************************
function TDataProcessorCCDevice.FGetProcessorVersion: String;
begin
 Result := '1.0';
end;

function TDataProcessorCCDevice.InDoc_Load(Source: String): Boolean;
var iNode: IXMLElement;
    Step : String;
    Fname: String;
begin
 Result := true;
 if Source = '' then Exit;
 try
  Step   := 'Parse XML';
  Result := FInXmlDoc.loadXML(Source);
  if not Result then
   begin
    Fname := 'XmlErr_'+FormatDateTime('YYMMDD-HHHHNN', Now)+'.xml';
    FPostErrorFile(Device.FLogPath + Fname, Source);
    raise EAbort.Create(FInXmlDoc.parseError.reason+
                            ' [Line:'+IntToStr(FInXmlDoc.parseError.line)+
                            ' Linepos:'+IntToStr(FInXmlDoc.parseError.linepos)+
                            ' Filepos:'+IntToStr(FInXmlDoc.parseError.filepos)+']['+Fname+']');
   end;

  iNode := FInXmlDoc.documentElement;
  if iNode = nil then raise EAbort.Create('XML document is empty');

  InCommandText    := iNode.nodeName;
  InCommandVersion := Device.DeviceInfo.Dev_Version;
  InCommandAppname := C_InCommandAppname_CC;
 except
  on E: Exception do
   begin
    FLastError := '[LoadIncommingXML]['+Step+'] '+E.Message;
    Result := false;
   end;
 end;
end;

function TDataProcessorCCDevice.OutDoc_InitHeader(Command: String): Boolean;
var iNode: IXMLElement;
    iPI  : IXMLProcessingInstruction;
begin
 try
   if FOutXmlDoc.documentElement = nil then
    begin
     iNode := FOutXmlDoc.createElement(xml_NodeCC_PingRes);
     FOutXmlDoc.documentElement := iNode;
     iPI := FOutXmlDoc.createProcessingInstruction('xml', xml_ProcessInstruction);
     FOutXmlDoc.insertBefore(iPI, iNode);
     Result := true;
    end
   else
    Result := true;
 except
  on E: Exception do
   begin
    FLastError := '[InitXMLDocument]'+E.Message;
    Result := false;
   end;
 end;
end;

function TDataProcessorCCDevice.OutDoc_SetResponceCode(Code: Integer; ErrorMessage: String): Boolean;
var iNode : IXMLNode;
    I     : Integer;
    S     : String;
begin
 try
  iNode := FOutXmlDoc.documentElement;
  if iNode = nil then raise EAbort.Create('XML document is not initialized');

  iNode.appendChild(FOutXmlDoc.createElement(xml_NodeCC_DevSerMod)).Text := Device.DeviceInfo.Dev_Serial;
  if Code <> errcode_ExecuteSucceed then
   begin
    S := '';
    for I := 1 to Length(ErrorMessage) do
     begin
      case ErrorMessage[I] of
      #01..#12: S := S + '';
      #13     : S := S + '|';
      #14..#31: S := S + '';
      else      S := S + ErrorMessage[I];
      end;
     end;
    iNode.appendChild(FOutXmlDoc.createElement(xml_NodeCC_ErrCode)).Text := IntToStr(Code);
    iNode.appendChild(FOutXmlDoc.createElement(xml_NodeCC_ErrMsg)).Text  := ErrorMessage;
   end;
  Result := true;
 except
  on E: Exception do
   begin
    FLastError := '[SetXMLResponceCode]'+E.Message;
    Result := false;
   end;
 end;
end;

procedure TDataProcessorCCDevice.PostLogToDB(DevSerial, Request, Responce: String; Err: Integer);
var SQL : String;
    Ra  : Integer;
begin
 with DBInterface do
  begin
   SQL := 'INSERT INTO CC_LOG(CL_LOCALPORT, CL_DATETIME, CL_DEVSERIAL, CL_ERROR, CL_REQUEST, CL_RESPONCE) VALUES ('+
          IntToSql(Device.ConnectionInfo.LocalPort) + ','+ // [CL_LOCALPORT] int NULL,
          DateTimeToSQL(Now)                        + ','+ // [CL_DATETIME] datetime DEFAULT getdate() NOT NULL,
          StrToSQL(DevSerial, 10)                   + ','+ // [CL_DEVSERIAL] varchar(10) NOT NULL,
          IntToSql(Err, true)                       + ','+ // [CL_ERROR] int NULL,
          StrToSQL(Request)                         + ','+ // [CL_REQUEST] varchar(max) NULL,
          StrToSQL(Responce)                        + ')'; // [CL_RESPONCE] varchar(max) NULL,
   ExecuteSQLStatement(SQL, Ra);
  end;
end;

//*********************************************************************************
//                        TRemoteDevice
//*********************************************************************************
constructor TRemoteDevice.Create(DBConnectionString, ServerName, LogPath: String; AcceptedCmds: TCommandTypes;
                                 TestMode: Boolean);
begin
 inherited Create;
 FServerName   := ServerName;
 FLogPath      := LogPath;
 FAcceptCmds   := AcceptedCmds;
 FDbInterface  := TDBInterfaceADO.Create(DBConnectionString, false);
 FCommandList  := TStringList.Create;
 FDataProc     := nil;
 FTestMode     := TestMode;
 FConnectionInfo.ConnectTime := Now;
end;

destructor TRemoteDevice.Destroy;
begin
 if FDataProc <> nil then FreeAndNil(FDataProc);
 FDbInterface.Free;
 FCommandList.Free;
 inherited Destroy;
end;

function TRemoteDevice.FGetDeviceInfoAsText: String;
begin
 Result := 'Start:'+FormatDateTime('NNSS.zzz', FConnectionInfo.ConnectTime)+' ';

 if FConnectionInfo.DisconnectTime <> 0 then Result := Result + 'End:'+FormatDateTime('NNSS.zzz', FConnectionInfo.DisconnectTime)+' ';

 Result := Result +  'Time:'+IntToStr(ConnectedTimeMSecs)+' '+
                     'Dev:'+DeviceInfo.Dev_Type+'/'+DeviceInfo.Dev_Serial+'('+DeviceInfo.Dev_Version+') '+
                     'RH:'  +ConnectionInfo.RemoteIP+' '+
                     'R/LP:'  +IntToStr(ConnectionInfo.RemotePort)+'/'+IntToStr(ConnectionInfo.LocalPort);
end;

function TRemoteDevice.FGetConnectedTimeSec: Integer;
begin
 Result := SecondsBetween(ConnectionInfo.ConnectTime, Now)
end;

function TRemoteDevice.FGetConnectedTimeMSecs: Int64;
begin
 Result := MilliSecondsBetween(ConnectionInfo.ConnectTime, Now)
end;

function TRemoteDevice.FGetIdleTimeSec: Integer;
begin
 Result := SecondsBetween(ConnectionInfo.LastAction, Now);
end;

{procedure TRemoteDevice.WriteStringToStream(Str: String; Stream: TStream);
var StrSize : Longint;
    PValue  : PChar;
begin
 if Stream = nil then Exit;
 StrSize := Length(Str);
 if StrSize > 0 then
  begin
    PValue := Pchar(Str);
    Stream.Write(PValue^, StrSize);
  end;
end;

procedure TRemoteDevice.ReadStringFromStream(Stream_: TStream; var Str_: String; Length_: Word);
var PValue  : PChar;
begin
  Str_ := '';
  if Stream_ = nil then Exit;
  if (Length_ = 0)or(Length_ > (Stream_.Size - Stream_.Position)) then
   Length_ := Stream_.Size - Stream_.Position;

  GetMem(PValue, Length_);
  try
   Stream_.Read(PValue^, Length_);
   Str_ := Copy(StrPas(PValue), 1, Length_);
  finally
   FreeMem(PValue);
  end;
end;

function TRemoteDevice.StreamAsText(Stream_: TStream; Length_: Word): String;
begin
 Stream_.Position := 0;
 ReadStringFromStream(Stream_, Result, Length_);
end;}

function TRemoteDevice.PostEventSystem(EvType: String; EvData: String; EvCommand: String=''): Boolean;
var SQL : String;
    RCnt: Integer;
begin
 with FDbInterface do
  begin
   SQL := 'INSERT INTO EVENTS_SYSTEM (ES_DATETIME, ES_MODULE, ES_SUBMODULE, ES_TYPE, '+
          'ES_DEVICE_FU, ES_DEVICE_GPRS, ES_COMMAND, ES_CONNECTTIME, ES_DISCONNECTTIME, '+
          'ES_DATA)VALUES('+
          DateTimeToSQL(Now)                   +','+ // ES_DATETIME, datetime
          StrToSQL(C_ModuleNameDev, 20)        +','+ // ES_MODULE, varchar(20)
          StrToSQL(FServerName, 50)            +','+ // ES_SUBMODULE, varchar (50)
          StrToSQL(EvType, 10)                 +','+ // ES_TYPE, varchar(10)
          StrToSQL(DeviceInfo.Dev_Type, 10)    +','+ // ES_DEVICE_FU, varchar(10)
          StrToSQL(DeviceInfo.Dev_Serial, 10)  +','+ // ES_DEVICE_GPRS, varchar(10)
          StrToSQL(EvCommand, 50, true)        +','+ // ES_COMMAND varchar(50)
          DateTimeToSQL(ConnectionInfo.ConnectTime )   +','+ // ES_CONNECTTIME, datatime
          DateTimeToSQL(ConnectionInfo.DisconnectTime) +','+ // ES_DISCONNECTTIME, datatime
          StrToSQL(EvData)                     +')'; // ES_DATA, varchar(1024)

   Result := ExecuteSQLStatement(SQL, RCnt);
  end;
end;

function TRemoteDevice.DoHttpConnect(Thread: TIdPeerThread; var ErrMsg: String): Boolean;
begin
 Result := false;

 with FConnectionInfo do
  begin
   RemoteHost := Thread.Connection.LocalName;
   RemoteIP   := Thread.Connection.Socket.Binding.PeerIP;
   RemotePort := Thread.Connection.Socket.Binding.PeerPort;
   LocalPort  := Thread.Connection.Socket.Binding.Port;

   ConnectTime    := Now;
   DisconnectTime := 0;
   LastAction     := Now;
  end;

 CoInitializeEx(nil, 0);
 {
 FDbInterface.CreateConnectionObject;

 if not FDbInterface.OpenDatabase then
  begin
   ErrMsg := FDbInterface.LastError;
   Exit;
  end;
  }

 Result := true;
end;

function TRemoteDevice.DoHttpDisconnect(Thread: TIdPeerThread): Boolean;
begin
 Result := true;
 FConnectionInfo.DisconnectTime := Now;

 if FCommandList.Count > 0 then
  PostEventSystem(C_EvType_Comm, 'Comm. finished.['+DeviceInfoAsText+']'+sLineBreak+
                                 'Cmd count: '+IntToStr(FCommandList.Count)+sLineBreak+
                                 FCommandList.Text)
 else
  begin
   // добре е да има черен списък s IP адреси и да се филтрират
   Result := false;
   PostEventSystem(C_EvType_Comm0, 'Comm. finished.['+DeviceInfoAsText+']'+sLineBreak+
                                   'Cmd count: '+IntToStr(FCommandList.Count));
  end;

 FDbInterface.CloseDatabase;
 CoUninitialize;
end;

function TRemoteDevice.DoHttpGetRequest(Thread: TIdPeerThread; RequestInfo: TIdHTTPRequestInfo; var ErrMsg: String): Boolean;
begin
 Result := false;
 with FDeviceInfo do
  begin
   Dev_Type    := RequestInfo.RawHeaders.Values['DevType'];
   Dev_Serial  := RequestInfo.RawHeaders.Values['DevSer'];
   Dev_Version := RequestInfo.RawHeaders.Values['DevVer'];
  end;
 FConnectionInfo.LastAction  := Now;

 // check input params
 if (DeviceInfo.Dev_Type = '') then
  begin
   ErrMsg := 'Device without "Dev_Type" reach the server. Disconnect. ['+DeviceInfoAsText+']';
   Exit;
  end;
 if (DeviceInfo.Dev_Serial = '') then
  begin
   ErrMsg := 'Device without SN reach the server. Disconnect. ['+DeviceInfoAsText+']';
   Exit;
  end;

 if RequestInfo.ContentType <> 'text/xml' then
  begin
   ErrMsg := 'Request with wrong ContentType received: "'+RequestInfo.ContentType+'" Must disconnect.  ['+DeviceInfoAsText+']';
   Exit;
  end;

// if RequestInfo.ContentVersion <> '1.0' then
//  begin
//   ErrMsg := 'Request with unsupported ContentVersion received: "'+RequestInfo.ContentVersion+'" Must disconnect. ['+DeviceInfoAsText+']';
//   Exit;
//  end;

 FCommandList.Add(RequestInfo.Command+':'+RequestInfo.Document);

 Result := true;
end;

function TRemoteDevice.ProcessData(RequestInfo: TIdHTTPRequestInfo; var Responce: String; var ErrorMsg: String): Boolean;
var ST     : TDateTime;
    Doc    : String;
    InfoMsg: String;
    CrHnd  : TCrypto_TEA;

    procedure CreateDataProc(DPClass: TProcessorClass);
    begin
     FDataProc := DPClass.Create(DbInterface, Self);
     FDataProc.TestMode := FTestMode;
    end;
begin
 ST   := Now;
 CrHnd:= nil;
 try
   Doc  := RequestInfo.Document;
   if Copy(Doc, 1, 1) = '/' then Delete(Doc, 1, 1);
   Responce := '';

   {
   if not DBInterface.DBConnected then
    begin
     if not DBInterface.OpenDatabase then raise EAbort.Create(DBInterface.LastError);
    end;
    }
   // acceptable commands for the server
//   if AnsiSameText(RequestInfo.Command, 'get') then
//    begin
//    end
//   else
   if AnsiSameText(RequestInfo.Command, 'post') then
    begin
     // functions from PC
     if AnsiSameText(Doc, cmd_PCTest)and
        ((cmdSystem in FAcceptCmds)or(cmdFiscalize in FAcceptCmds)) then CreateDataProc(TDP_PCTest) // OK
     else
     if AnsiSameText(Doc, cmd_SysSendMessage)and
        ((cmdSystem in FAcceptCmds)or(cmdFiscalize in FAcceptCmds)) then CreateDataProc(TDP_PCMessage)
     else
     if AnsiSameText(Doc, cmd_SysSendError)and
        ((cmdSystem in FAcceptCmds)or(cmdFiscalize in FAcceptCmds)) then CreateDataProc(TDP_PCErrorLog)
     else
     ////////////////////////////////////
     if AnsiSameText(Doc, cmd_SysCheckUpdate)and
        ((cmdSystem in FAcceptCmds)or(cmdFiscalize in FAcceptCmds)) then CreateDataProc(TDP_SysCheckUpdate)
     else
     if AnsiSameText(Doc, cmd_SysDownloadFile)and
        ((cmdSystem in FAcceptCmds)or(cmdFiscalize in FAcceptCmds)) then CreateDataProc(TDP_SysDownloadFile)
     ///////////////////////////////////
    end
   else
    raise EAbort.Create('Unsupported incomming command: '+RequestInfo.Command);

   if FDataProc = nil then raise EAbort.Create('Invalid command/document for this server: ['+RequestInfo.Command+RequestInfo.Document+']');

   if AnsiSameText(RequestInfo.ContentLanguage, 'encrypted') then
    begin
     CrHnd := TCrypto_TEA.Create;
     RequestInfo.FormParams := CrHnd.DecryptStringEx(RequestInfo.FormParams, DeviceInfo.Dev_Type + DeviceInfo.Dev_Serial + DeviceInfo.Dev_Version);
    end;

   try
    if not FDataProc.InDoc_Load(RequestInfo.FormParams) then raise EAbort.Create(FDataProc.LastError);
    if not AnsiSameText(FDataProc.InCommandText, Doc) then raise EAbort.Create('Difference between XML and URL commands: '+FDataProc.InCommandText+' - '+Doc);
    if not FDataProc.OutDoc_InitHeader(Doc) then  raise EAbort.Create(FDataProc.LastError);
    if not FDataProc.ProcessData(InfoMsg) then raise EAbort.Create(FDataProc.LastError);
   except
    on E: Exception do
     begin
      // когато има проблем с разчитане на документа
      // паращаме обща грешка
      if FDataProc is TDataProcessorCCDevice then
       begin
         // изтриване на документа
         if FDataProc.XmlOutDocument <> nil then FreeXmlDoc(FDataProc.XmlOutDocument);
         FDataProc.XmlOutDocument := CreateXMLDoc;
         FDataProc.OutDoc_InitHeader(Doc);
         FDataProc.OutDoc_SetResponceCode(errcode_CC_XMLParseFail, 'General system error!');

         PostEventSystem(C_EvType_Error, 'CC request general FAIL ['+RequestInfo.Command+RequestInfo.Document+']'+sLineBreak+
                                         'General error returned!'+sLineBreak+
                                         E.Message, Doc);
       end
      else
       raise EAbort.Create(E.Message);
     end;
   end;

   Responce := FDataProc.XmlOutDocument.xml;

   if CrHnd <> nil then  Responce := CrHnd.EncryptStringEx(Responce, DeviceInfo.Dev_Type + DeviceInfo.Dev_Serial + DeviceInfo.Dev_Version);

   InfoMsg := 'Executed['+RequestInfo.Command+RequestInfo.Document+']['+FDataProc.InCommandAppname+'/'+FDataProc.InCommandVersion+']('+IntToStr(MilliSecondsBetween(ST, Now))+'ms)'+sLineBreak+InfoMsg;
   PostEventSystem(C_EvType_Exec, InfoMsg, Doc);

   FreeAndNil(FDataProc);
   Result := true;
 except
  on E: Exception do
   begin
    ErrorMsg := '['+Self.ClassName+']';
    if FDataProc <> nil then ErrorMsg := ErrorMsg + '['+FDataProc.ClassName+']';
    ErrorMsg := ErrorMsg + ' '+E.Message;
    Result   := false;
    Responce := '';
//    PostEvent(C_EvType_Error, 'Data processor fail('+IntToStr(MilliSecondsBetween(ST, Now))+'ms) '+
//                              '['+RequestInfo.Command+RequestInfo.Document+']'+InfoMsg);
   end;
 end;
 if CrHnd <> nil then CrHnd.Free;
end;


end.
