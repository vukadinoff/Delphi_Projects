unit BaseHandler;

interface

uses SysUtils, DeviceUnit, XMLHandler;

type
 THandlerBase = class(TObject)
 private
  FLastError : String;
 public
  property LastError: String read FLastError write FLastError;
 end;

 THandlerXML = class(THandlerBase)
 private
  FXmlDoc : IXMLDocument;
 protected
  function XSDateTimeToDateTime(XSDateTime: String): TDateTime;
  function DateTimeToXSDateTime(ADateTime: TDateTime): String;

  function XML_GetRootNode(RaiseExceptionOnFail: Boolean = true): IXMLElement;
  function XML_FindNodeByName(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): IXMLNode;
  function XML_GetNodeText(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true;
                           CheckForEmptyValue: Boolean=true): String;
  function XML_GetNodeInt(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Integer;
  function XML_GetNodeFloat(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Double;
  function XML_GetNodeDateTime(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): TDateTime;
 public
  constructor Create(XmlDoc: IXMLDocument);

  property XmlDocument: IXMLDocument read FXmlDoc write FXmlDoc;
 end;

 THandlerClient = class(THandlerXML)
 public
  function AddRequestToDocument: Boolean; virtual; abstract;
  function GetAnswerFromDocument: Boolean; virtual; abstract;
 end;

 TCommandClient = class(THandlerClient)
 private
  FResultCode : Integer;
  FErrorMsg   : String;
 public
  constructor Create(XmlDoc: IXMLDocument); virtual;
  function GetCommandName: String; virtual; abstract;

  property ResultCode: Integer read FResultCode write FResultCode;
  property ErrorMsg: String read FErrorMsg write FErrorMsg;
 end;

 TCommandClientClass = class of TCommandClient;

 THandlerServer = class(THandlerXML)
 private
  FDevice     : TRemoteDevice;
 public
  function GetRequestFromDocument: Boolean; virtual; abstract;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; virtual; abstract;
  function AddAnswerToDocument: Boolean; virtual; abstract;

  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  property Device: TRemoteDevice read FDevice write FDevice;
 end;

 THandlerServerUserEvent = class(THandlerServer)
 public
   function PostEventUser(ErrorCode: Integer; EvType, UserType, UserName,
                          DevType, DevSerial, EvData: String): Boolean;
 end;

 EHandledException = class(EAbort)
  private
    FErrorCode: Integer;
    FSysEvent : String;
  public
    constructor Create(ErrCode: Integer; Msg: String; ErrSysEvent: String='');
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property SystemMessage: String read FSysEvent write FSysEvent;
  end;

implementation
uses XMLHandlerMS, BillingConstUnit, DBInterfaceUnit, XSBuiltIns;

//******************************************************************************
//   THandlerServerUserEvent
//******************************************************************************

function THandlerServerUserEvent.PostEventUser(ErrorCode: Integer; EvType, UserType, UserName,
                                             DevType, DevSerial, EvData: String): Boolean;
var SQL : String;
    RCnt: Integer;

    function IsNormalError(ErrCode_: Integer): Boolean;
    begin
     Result := not (ErrorCode in [errcode_ExecuteSucceed, errcode_ModemFwUpdateFail]);
    end;
    function IsCriticalError(ErrCode_: Integer): Boolean;
    begin
     Result := (ErrorCode in [errcode_ModemHardwareTestFail, errcode_CommitOperationFail]);
    end;
begin
 Result := true;
 try
   if Device = nil then raise EAbort.Create('Device object is not assigned');
   if Device.DbInterface = nil then raise EAbort.Create('Device.DBInterface object is not assigned');

   if DevSerial = '' then DevType := '';
   with Device.DbInterface do
    begin
     // запис на потребителско събитие
     SQL := 'INSERT INTO EVENTS_USER (EU_DATATIME, EU_MODULE, EU_SUBMODULE, EU_TYPE, '+
            'EU_REMOTEIP, EU_USERTYPE, EU_USERNAME, EU_DATA, EU_ERRORFLAG, EU_DEVTYPE, '+
            'EU_DEVSERIAL) VALUES ('+
             DateTimeToSQL(Now)                   +','+ // [EU_DATATIME] datetime NOT NULL,
             StrToSQL(C_ModuleNameDev, 20)        +','+ // [EU_MODULE] varchar(20) NOT NULL,
             StrToSQL(Device.ServerName, 50)      +','+ // [EU_SUBMODULE] varchar(50) NULL,
             StrToSQL(EvType, 10)                 +','+ // [EU_TYPE] varchar(10) NOT NULL,
             StrToSQL(Device.ConnectionInfo.RemoteIP, 20)+','+ // [EU_REMOTEIP] varchar(20) NULL,
             StrToSQL(UserType, 1)                +','+ // [EU_USERTYPE] varchar(1) NOT NULL,
             StrToSQL(UserName, 50, true)         +','+ // [EU_USERNAME] varchar(50) NULL,
             StrToSQL(EvData)                     +','+ // [EU_DATA] varchar(max) NULL,
             IntToSql(ErrorCode)                  +','+ // [EU_ERRORFLAG] int DEFAULT 0 NOT NULL,
             StrToSQL(DevType, 1, true)           +','+ // [EU_DEVTYPE] varchar(1) NULL,
             StrToSQL(DevSerial, 32, true)        +')'; // [EU_DEVSERIAL] varchar(32) NULL
     if not ExecuteSQLStatement(SQL, RCnt) then raise EAbort.Create('Post User Event fail:' + Device.DbInterface.LastError);

     // броене на проблемните устройства
     if (IsNormalError(ErrorCode))and(DevType <> '')and(DevSerial <> '') then
      begin
       if not FillDataSet('SELECT MAX(EE_ID) "MAXID" FROM EVENTS_ERRCNTR '+
                          'WHERE (EE_DEVTYPE = '+StrToSQL(DevType, 1)+')AND'+
                          '(EE_DEVSERIAL = '+StrToSQL(DevSerial, 32)+')AND(EE_ACTIVE = 1)') then
         raise EAbort.Create('Check error counter fail: '+Device.DbInterface.LastError);
       try
         if DataSet.FieldByName('MAXID').AsInteger > 0 then
          begin // update counters
           SQL := 'UPDATE EVENTS_ERRCNTR SET EE_ERRCNT_N = EE_ERRCNT_N + 1';
           if IsCriticalError(ErrorCode) then SQL := SQL + ', EE_ERRCNT_C = EE_ERRCNT_C + 1';
           SQL := SQL + ' WHERE EE_ID = '+IntToSql(DataSet.FieldByName('MAXID').AsInteger);
          end
         else
          begin // insert new event
           if UserType <> 'D' then UserName := '';
           SQL := 'INSERT INTO EVENTS_ERRCNTR(EE_DEVTYPE, EE_DEVSERIAL, EE_ESKSERIAL, EE_ERRCNT_N, EE_ERRCNT_C) VALUES ('+
                  StrToSQL(DevType, 1)         +','+  // EE_DEVTYPE
                  StrToSQL(DevSerial, 32)      +','+  // EE_DEVSERIAL
                  StrToSQL(UserName, 10, true) +','+  // EE_ESKSERIAL
                  '1'                          +',';  // EE_ERRCNT_N
           if IsCriticalError(ErrorCode) then SQL := SQL + '1)'   // EE_ERRCNT_C
            else SQL := SQL + '0)';
          end;
        if not ExecuteSQLStatement(SQL, RCnt) then raise EAbort.Create('Post EVENTS_ERRCNTR fail: '+Device.DbInterface.LastError);
       finally
         CloseDataSet;
       end;
      end;
    end;
 except
  on E: Exception do
   begin
    Result    := false;
    LastError := LastError + E.Message;
   end;
 end;
end;

//******************************************************************************
// EHandledException
//******************************************************************************
constructor EHandledException.Create(ErrCode: Integer; Msg: String; ErrSysEvent: String='');
begin
 FErrorCode := ErrCode;
 FSysEvent  := ErrSysEvent;
 inherited Create(Msg);
end;

//******************************************************************************
//   THandlerXML
//******************************************************************************
constructor THandlerXML.Create(XmlDoc: IXMLDocument);
begin
 inherited Create;
 FXmlDoc := XmlDoc;
end;

function THandlerXML.XSDateTimeToDateTime(XSDateTime: String): TDateTime;
begin
  with TXSDateTime.Create do
  try
   XSToNative(XSDateTime);
   Result := AsUTCDateTime;
  finally
   Free;
  end;
end;

function THandlerXML.DateTimeToXSDateTime(ADateTime: TDateTime): String;
begin
  Result:= FormatDateTime('YYYY-MM-DD"T"HH:NN:SS', ADateTime);

{ tova vru6ta i timezone, ne namerih kak da formatiram datata
  with TXSDateTime.Create do
    try
      AsDateTime:= ADateTime;
      Result:= NativeToXS;
    finally
      Free;
    end; }
end;

function THandlerXML.XML_GetRootNode(RaiseExceptionOnFail: Boolean = true): IXMLElement;
begin
  Result := nil;
  if XmlDocument = nil then
   begin
    if RaiseExceptionOnFail then raise EAbort.Create('XML document is not assigned');
   end
  else
   begin
    Result := XmlDocument.documentElement;
    if Result = nil then
     begin
      if RaiseExceptionOnFail then raise EAbort.Create('XML document is empty');
     end;
   end;
end;

function THandlerXML.XML_FindNodeByName(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean = true): IXMLNode;
begin
 Result := nil;
 if OnRoot = nil then
  begin
   if RaiseExceptionOnFail then raise EAbort.Create('XML search root is not assigned');
  end
 else
  begin
   Result := OnRoot.selectSingleNode(NodeName);
   if Result = nil then
    begin
     if RaiseExceptionOnFail then raise EAbort.Create('"'+NodeName+'" does not exist in XML document');
    end;
  end;
end;

function THandlerXML.XML_GetNodeText(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean = true;
                                     CheckForEmptyValue: Boolean = true): String;
var iNode : IXMLNode;
begin
 Result := '';
 if OnRoot = nil then
  begin
   if RaiseExceptionOnFail then raise EAbort.Create('XML search root is not assigned');
  end
 else
  begin
   iNode := OnRoot.selectSingleNode(NodeName);
   if iNode = nil then
    begin
     if RaiseExceptionOnFail then raise EAbort.Create('XML node "'+NodeName+'" not found');
    end
   else
    Result := iNode.text;
  end;
 if (Result = '')and(RaiseExceptionOnFail)and(CheckForEmptyValue) then
  raise EAbort.Create('XML node "'+NodeName+'" is empty');
end;

function THandlerXML.XML_GetNodeInt(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Integer;
var S : String;
begin
 Result := 0;
 S := XML_GetNodeText(OnRoot, NodeName, RaiseExceptionOnFail);
 if (not TryStrToInt(S, Result))and(RaiseExceptionOnFail) then
  raise EAbort.Create('XML node "'+NodeName+'" is not integer: '+S);
end;

function THandlerXML.XML_GetNodeFloat(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Double;
var S : String;
begin
 DecimalSeparator := '.';
 Result := 0;
 S := XML_GetNodeText(OnRoot, NodeName, RaiseExceptionOnFail);
 if (not TryStrToFloat(S, Result))and(RaiseExceptionOnFail) then
  raise EAbort.Create('XML node "'+NodeName+'" is not float: '+S);
end;

function THandlerXML.XML_GetNodeDateTime(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): TDateTime;
var S : String;
begin
 Result := 0;
 S := Trim(XML_GetNodeText(OnRoot, NodeName, RaiseExceptionOnFail, RaiseExceptionOnFail));
 if S <> '' then Result := XSDateTimeToDateTime(S)
end;
//******************************************************************************
//   THandlerServer
//******************************************************************************
constructor THandlerServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc);
 FDevice := RemoteDevice;
end;

//******************************************************************************
//   TCommandClient
//******************************************************************************
constructor TCommandClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FResultCode := errcode_ExecuteFail_Internal;
 FErrorMsg   := 'Command still not executed';
end;

end.
