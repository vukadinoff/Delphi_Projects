unit SendMessageHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit, XMLHandler, ESKHandler, ExtDataHandler,
     ErrorLogHandler, CryptoHandler;

type
//******************************************************************************
//           PCMessage Handler
//******************************************************************************
 TCmd_PCMessageClient = class(TCommandClient)
 private
  FESKLoginHndr : THndr_ESKLoginClient;
  FMsgType      : String;
  FMsgText      : TStrings;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property MessageType: String read FMsgType write FMsgType;
  property MessageText: TStrings read FMsgText write FMsgText;
  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
 end;

 TCmd_PCMessageServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr : THndr_ESKLoginServer;
  FMsgType      : String;
  FMsgText      : TStrings;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property MessageType: String read FMsgType write FMsgType;
  property MessageText: TStrings read FMsgText write FMsgText;
  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
 end;


//******************************************************************************
//           PCExtData Handler
//******************************************************************************
 TCmd_PCExtDataClient = class(TCommandClient)
 private
  FEskSerial     : String;
  FDataToServer  : TStrings;
  FCrcHandler    : TCRC32;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property EskSerial: String read FEskSerial write FEskSerial;
  property DataToServer: TStrings read FDataToServer write FDataToServer;
 end;

 TCmd_PCExtDataServer = class(THandlerServerUserEvent)
 private
  FEskSerial     : String;
  FDataFromClient: THndr_ExtDataServer;
  FCrcHandler    : TCRC32;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property EskSerial: String read FEskSerial write FEskSerial;
  property DataFromClient: THndr_ExtDataServer read FDataFromClient write FDataFromClient;
 end;


//******************************************************************************
//           PCErrorLog Handler
//******************************************************************************
 TCmd_PCErrorLogClient = class(TCommandClient)
 private
  FEskSerial     : String;
  FErrorCode     : Integer;
  FDevType       : String;
  FDevSerial     : String;
  FDataToServer  : TStrings;
  FCrcHandler    : TCRC32;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property EskSerial: String read FEskSerial write FEskSerial;
  property DevType: String read FDevType write FDevType;
  property DevSerial: String read FDevSerial write FDevSerial;
  property ErrorCode: Integer read FErrorCode write FErrorCode;
  property ErrorData: TStrings read FDataToServer write FDataToServer;
 end;

 TCmd_PCErrorLogServer = class(THandlerServerUserEvent)
 private
  FClientLog     : THndr_ClientErrLogServer;
  FCrcHandler    : TCRC32;
  function CalculateCRC(Source: String): String;
  function FGetEskSerial: String;
  function FGetDevType: String;
  function FGetDevSerial: String;
  function FGetErrorCode: Integer;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property EskSerial: String read FGetEskSerial;
  property DevType: String read FGetDevType;
  property DevSerial: String read FGetDevSerial;
  property ErrorCode: Integer read FGetErrorCode;
  property ClientLog: THndr_ClientErrLogServer read FClientLog write FClientLog;
 end;



implementation
uses BillingConstUnit, ZLib, SZCodeBaseX, DBInterfaceUnit;

//******************************************************************************
//   TCmd_PCMessageClient
//******************************************************************************
constructor TCmd_PCMessageClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
 FMsgText      := TStringList.Create;
end;

destructor TCmd_PCMessageClient.Destroy;
begin
 FESKLoginHndr.Free;
 FMsgText.Free;
 inherited Destroy;
end;

function TCmd_PCMessageClient.GetCommandName: String;
begin
 Result := cmd_SysSendMessage;
end;

function TCmd_PCMessageClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    iElem : IXMLElement;
    SDate : String;
    SCRC  : String;
    Strm  : TStringStream;
    CStrm : TCompressionStream;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(MessageType + MessageText.Text + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SendMessage));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_MessageType)).Text := MessageType;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text        := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text         := SCRC;

  Strm := TStringStream.Create('');
  try
   CStrm := TCompressionStream.Create(clFastest, Strm);
   try
    FMsgText.SaveToStream(CStrm);
   finally
    CStrm.Free;
   end;

   iElem := XmlDocument.CreateElement(xml_Node_MessageBody);
   iElem.Set_dataType('bin.base64');
   iElem.nodeTypedValue := SZFullEncodeBase64(Strm.DataString);
   iNode.appendChild(iElem);
  finally
   Strm.Free;
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

function TCmd_PCMessageClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    Strm   : TStringStream;
    B      : Char;
    S      : String;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_SendMessage);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);
  FMsgType := XML_GetNodeText(iNode, xml_Node_MessageType);
  FMsgText.Clear;

  Strm := TStringStream.Create('');
  try
   Strm.WriteString(SZDecodeBase64(XML_GetNodeText(iNode, xml_Node_MessageBody)));
   Strm.Position := 0;

   S := '';
   with TDecompressionStream.Create(Strm) do
   try
    while Read(B, 1) = 1 do S := S + B;
   finally
    Free;
   end;
  finally
   Strm.Free;
  end;

  FMsgText.Text := S;
  if SCRC <> FESKLoginHndr.CalculateCRC(MessageType + MessageText.Text + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//   TCmd_PCMessageServer
//******************************************************************************
constructor TCmd_PCMessageServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
 FMsgText      := TStringList.Create;
end;

destructor TCmd_PCMessageServer.Destroy;
begin
 FESKLoginHndr.Free;
 FMsgText.Free;
 inherited Destroy;
end;

function TCmd_PCMessageServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    Strm   : TStringStream;
    B      : Char;
    S      : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_SendMessage);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);
  FMsgType := XML_GetNodeText(iNode, xml_Node_MessageType);
  FMsgText.Clear;

  Strm := TStringStream.Create('');
  try
   Strm.WriteString(SZDecodeBase64(XML_GetNodeText(iNode, xml_Node_MessageBody)));
   Strm.Position := 0;

   S := '';
   with TDecompressionStream.Create(Strm) do
   try
    while Read(B, 1) = 1 do S := S + B;
   finally
    Free;
   end;
  finally
   Strm.Free;
  end;

  FMsgText.Text := S;
  if SCRC <> FESKLoginHndr.CalculateCRC(MessageType + MessageText.Text + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_PCMessageServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var Ra   : Integer;
    SQL  : String;
    Size : Integer;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result := true;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then  raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    with Device.DbInterface do
     begin
      SQL := 'INSERT INTO EVENTS_DEALER(ED_DATETIME, ED_MODULE, ED_SUBMODULE, ED_MESSAGETYPE, '+
             'ED_DEALEREIK, ED_ESKSERIAL, ED_REMOTEIP, ED_DATA) VALUES ('+
             DateTimeToSQL(Now)                                + ', '+ // [ED_DATETIME] datetime NOT NULL,
             StrToSQL(C_ModuleNameDev, 20)                     + ', '+ // [ED_MODULE] varchar(20)
             StrToSQL(Device.ServerName, 50)                   + ', '+ // [ED_SUBMODULE] varchar(50)
             StrToSQL(MessageType, 20)                         + ', '+ // [ED_MESSAGETYPE] varchar(20)
             StrToSQL(FESKLoginHndr.DealerData.CompanyEIK, 13) + ', '+ // [ED_DEALEREIK] varchar(13)
             StrToSQL(FESKLoginHndr.EskSerial, 10)             + ', '+ // [ED_ESKSERIAL] varchar(10)
             StrToSQL(Device.ConnectionInfo.RemoteIP, 20)      + ', '+ // [ED_REMOTEIP] varchar(20)
             StrToSQL(FMsgText.Text, 0)                        + ')';  // [ED_DATA] varchar(max)

      Size := Length(FMsgText.Text);
      FMsgText.Clear;
      FMsgText.Add('-ELTRADE Fiscalization server-');
      FMsgText.Add('Message accepted! Size of then message: '+IntToStr(Size)+' bytes.');

      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Insert message fail: '+LastError);
      if Ra = 0 then raise EAbort.Create('No records affected after insert message SQL');

      SetErrorCode(errcode_ExecuteSucceed, '');
     end;
   end;
 except
  on E: EHandledException do
   begin
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_PCMessageServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    iElem : IXMLElement;
    SDate : String;
    SCRC  : String;
    Strm  : TStringStream;
    CStrm : TCompressionStream;
begin
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FMsgType + FMsgText.Text + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SendMessage));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_MessageType)).Text := FMsgType;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Strm := TStringStream.Create('');
  try
   CStrm := TCompressionStream.Create(clFastest, Strm);
   try
    FMsgText.SaveToStream(CStrm);
   finally
    CStrm.Free;
   end;

   iElem := XmlDocument.CreateElement(xml_Node_MessageBody);
   iElem.Set_dataType('bin.base64');
   iElem.nodeTypedValue := SZFullEncodeBase64(Strm.DataString);
   iNode.appendChild(iElem);
  finally
   Strm.Free;
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

//******************************************************************************
//        TCmd_PCExtDataClient
//******************************************************************************
constructor TCmd_PCExtDataClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FEskSerial    := '';
 FDataToServer := nil;
 FCrcHandler   := TCRC32.Create;
end;

destructor TCmd_PCExtDataClient.Destroy;
begin
 FCrcHandler.Free;
 inherited Destroy;
end;

function TCmd_PCExtDataClient.GetCommandName: String;
begin
 Result := cmd_SysCommitData;
end;

function TCmd_PCExtDataClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    iElem : IXMLElement;
    SDate : String;
    SCRC  : String;
    Strm  : TStringStream;
    CStrm : TCompressionStream;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(FEskSerial + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CommitData));

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  if (FDataToServer <> nil)and(FDataToServer.Count > 0) then
   begin
    Strm := TStringStream.Create('');
    try
     CStrm := TCompressionStream.Create(clFastest, Strm);
     try
      FDataToServer.SaveToStream(CStrm);
     finally
      CStrm.Free;
     end;

     iElem := XmlDocument.CreateElement(xml_Node_ExtendedData);
     iElem.Set_dataType('bin.base64');
     iElem.nodeTypedValue := SZFullEncodeBase64(Strm.DataString);
     iNode.appendChild(iElem);
    finally
     Strm.Free;
    end;
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

function TCmd_PCExtDataClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_CommitData);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_PCExtDataClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

//******************************************************************************
//        TCmd_PCExtDataServer
//******************************************************************************
constructor TCmd_PCExtDataServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FEskSerial      := '';
 FDataFromClient := nil;
 FCrcHandler     := TCRC32.Create;
end;

destructor TCmd_PCExtDataServer.Destroy;
begin
 if FDataFromClient <> nil then FreeAndNil(FDataFromClient);
 FCrcHandler.Free;
 inherited Destroy;
end;

function TCmd_PCExtDataServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    iChild : IXMLNode;
    SDate  : String;
    SCRC   : String;
    Strm   : TStringStream;
    B      : Char;
    S      : String;
begin
 try
  if FDataFromClient <> nil then FreeAndNil(FDataFromClient);

  iNode        := XML_GetRootNode;
  iNode        := XML_FindNodeByName(iNode, xml_Node_CommitData);
  FEskSerial   := XML_GetNodeText(iNode, xml_Node_ESKSerial);
  SDate        := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC         := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(FEskSerial + SDate) then raise EAbort.Create('Invalid signature');

  iChild := XML_FindNodeByName(iNode, xml_Node_ExtendedData, false);
  if iChild <> nil then
   begin
    Strm := TStringStream.Create('');
    try
     Strm.WriteString(SZDecodeBase64(iChild.text));
     Strm.Position := 0;

     S := '';
     with TDecompressionStream.Create(Strm) do
     try
      while Read(B, 1) = 1 do S := S + B;
     finally
      Free;
     end;
    finally
     Strm.Free;
    end;

    if S <> '' then FDataFromClient := THndr_ExtDataServer.Create(Device, FEskSerial, S);
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

function TCmd_PCExtDataServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var S : String;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result    := true;
 UserError := '';
 ErrCode   := errcode_ExecuteSucceed;
 try

     // update data sent from client
     // не връщаме съобщение за грешка
     if FDataFromClient <> nil then
      begin
       if not FDataFromClient.Execute(S) then
        Device.PostEventSystem(C_EvType_Error, 'Error processing client data:'+sLineBreak+S, Self.ClassName);
      end;


 except
  on E: EHandledException do
   begin
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
    LastError := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_PCExtDataServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CommitData));
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

function TCmd_PCExtDataServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

//******************************************************************************
//        TCmd_PCErrorLogClient
//******************************************************************************
constructor TCmd_PCErrorLogClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FEskSerial    := '';
 FDataToServer := TStringList.Create;
 FCrcHandler   := TCRC32.Create;
end;

destructor TCmd_PCErrorLogClient.Destroy;
begin
 FCrcHandler.Free;
 FDataToServer.Free;
 inherited Destroy;
end;

function TCmd_PCErrorLogClient.GetCommandName: String;
begin
 Result := cmd_SysSendError;
end;

function TCmd_PCErrorLogClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    iElem : IXMLElement;
    SDate : String;
    SCRC  : String;
    Strm  : TStringStream;
    CStrm : TCompressionStream;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(FEskSerial + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SendError));

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DevType)).Text   := FDevType;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DevSerial)).Text := FDevSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ErrorCode)).Text := IntToStr(FErrorCode);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  if (FDataToServer <> nil)and(FDataToServer.Count > 0) then
   begin
    Strm := TStringStream.Create('');
    try
     CStrm := TCompressionStream.Create(clFastest, Strm);
     try
      FDataToServer.SaveToStream(CStrm);
     finally
      CStrm.Free;
     end;

     iElem := XmlDocument.CreateElement(xml_Node_ErrorMessage);
     iElem.Set_dataType('bin.base64');
     iElem.nodeTypedValue := SZFullEncodeBase64(Strm.DataString);
     iNode.appendChild(iElem);
    finally
     Strm.Free;
    end;
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

function TCmd_PCErrorLogClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_SendError);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_PCErrorLogClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

//******************************************************************************
//        TCmd_PCErrorLogServer
//******************************************************************************
constructor TCmd_PCErrorLogServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FClientLog      := THndr_ClientErrLogServer.Create(RemoteDevice);
 FCrcHandler     := TCRC32.Create;
end;

destructor TCmd_PCErrorLogServer.Destroy;
begin
 FClientLog.Free;
 FCrcHandler.Free;
 inherited Destroy;
end;

function TCmd_PCErrorLogServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    iChild : IXMLNode;
    SDate  : String;
    SCRC   : String;
    Strm   : TStringStream;
    B      : Char;
    S      : String;
begin
 try
  iNode        := XML_GetRootNode;
  iNode        := XML_FindNodeByName(iNode, xml_Node_SendError);
  with FClientLog do
   begin
    EskSerial  := XML_GetNodeText(iNode, xml_Node_ESKSerial);
    DevType    := XML_GetNodeText(iNode, xml_Node_DevType, true, false);
    DevSerial  := XML_GetNodeText(iNode, xml_Node_DevSerial, true, false);
    ErrorCode  := StrToIntDef(XML_GetNodeText(iNode, xml_Node_ErrorCode, true, false), 0);
   end;
  SDate        := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC         := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(FClientLog.EskSerial + SDate) then raise EAbort.Create('Invalid signature');

  iChild := XML_FindNodeByName(iNode, xml_Node_ErrorMessage, false);
  if iChild <> nil then
   begin
    Strm := TStringStream.Create('');
    try
     Strm.WriteString(SZDecodeBase64(iChild.text));
     Strm.Position := 0;

     S := '';
     with TDecompressionStream.Create(Strm) do
     try
      while Read(B, 1) = 1 do S := S + B;
     finally
      Free;
     end;
    finally
     Strm.Free;
    end;

    if S <> '' then FClientLog.Text := S;
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

function TCmd_PCErrorLogServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var S : String;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result    := true;
 UserError := '';
 ErrCode   := errcode_ExecuteSucceed;
 try
  if not FClientLog.Execute(S) then
   begin
    Device.PostEventSystem(C_EvType_Error, 'Fail processing client error:'+sLineBreak+S, Self.ClassName);

    ErrCode   := errcode_FailProcessClientError;
    UserError := 'Fail processing client error:'+sLineBreak+S;
   end;

 except
  on E: EHandledException do
   begin
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
    LastError := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_PCErrorLogServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_SendError));
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

function TCmd_PCErrorLogServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

function TCmd_PCErrorLogServer.FGetEskSerial: String;
begin
 Result := FClientLog.EskSerial;
end;

function TCmd_PCErrorLogServer.FGetDevType: String;
begin
 Result := FClientLog.DevType;
end;

function TCmd_PCErrorLogServer.FGetDevSerial: String;
begin
 Result := FClientLog.DevSerial;
end;

function TCmd_PCErrorLogServer.FGetErrorCode: Integer;
begin
 Result := FClientLog.ErrorCode;
end;

end.
