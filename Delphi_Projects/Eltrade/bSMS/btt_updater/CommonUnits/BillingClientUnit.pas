unit BillingClientUnit;

interface
//uses SysUtils, Classes, IdHTTP, IdComponent,XMLHandler;
uses SysUtils, Classes, IdHTTP, IdComponent, Contnrs, BaseHandler, XMLHandler;

const
// Device Types
 dev_ESK  : String = 'ESK';
 dev_Gprs : String = 'GPRS';
 dev_Ecr  : String = 'ECR';

type
 THttpRequest = (hcmGet, hcmPost);
 TBillingClientProgress = procedure(Percent: Integer) of object;

 TBillingClient = class(TObject)
 private
  FHttpClient : TIdHTTP;
  FDevType    : String;
  FDevSerial  : String;
  FDevVer     : String;
  FLastError  : String;
  FContentVer : String;
  FOnProgress : TBillingClientProgress;
  function FGetReadTimeout: Integer;
  procedure FSetReadTimeout(Value: Integer);
  procedure FHTTPWork(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
  procedure FHTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Integer);
  procedure FHTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
 public
  constructor Create(DevType_: String=''; DevSerial_: String=''; DevVer_: String='');
  destructor Destroy; override;

  function GenerateUrl(Host, CommandName: String): String;
  function SendRequest(HttpRequest: THttpRequest; URL: String; Source, Response: TStream;
                       EncryptContent: Boolean=false; CloseAfterRequest: Boolean=true): Boolean;
  procedure CloseConnection;

  property LastError: String read FLastError write FLastError;
  property ReadTimeout: Integer read FGetReadTimeout write FSetReadTimeout;
  property ContentVersion: String read FContentVer write FContentVer;
  property DevType: String read FDevType write FDevType;
  property DevSerial: String read FDevSerial write FDevSerial;
  property DevVer: String read FDevVer write FDevVer;
  property OnReadProgress: TBillingClientProgress read FOnProgress write FOnProgress;
 end;

 TBillingXMLClient = class(TBillingClient)
 private
  FHost     : String;
  FXMLDoc   : IXMLDocument;
  FCommands : TObjectList;
  function FGetVersion: String;
  function FGetCommandResultCode: Integer;
  function FGetServerErrorMessage: String;
  function FGetCommandsCount: Integer;
  function FGetCommand(Index: Integer): TCommandClient;
  function FGetCommandByClass(CommandClass: TCommandClientClass): TCommandClient;
//  procedure FSetCommand(Index: Integer; Value: TCommandClient);
  function FInitXMLDocument(CommandName: String): Boolean;
 public
  constructor Create(Host_: String=''; DevType_: String=''; DevSerial_: String=''; DevVer_: String='');
  destructor Destroy; override;

  function SendCommand(Index: Integer; HttpRequest: THttpRequest; EncryptContent: Boolean=false; CloseAfterRequest: Boolean = true): Integer;
  function AddCommand(CommandClass: TCommandClientClass): TCommandClient; overload;
  procedure ClearCommands;

  property Host: String read FHost write FHost;
  property CommandsCount: Integer read FGetCommandsCount;
  property Commands[Index: Integer]: TCommandClient read FGetCommand; {write FSetCommand;}
  property CommandByClass[CommandClass: TCommandClientClass]: TCommandClient read FGetCommandByClass;
  property XMLDocument: IXMLDocument read FXMLDoc write FXMLDoc;
  property LastCommandResultCode: Integer read FGetCommandResultCode;
  property LastCommandErrorMessage: String read FGetServerErrorMessage;
  property Version: String read FGetVersion;
 end;


implementation
uses IDException, CryptoHandler, BillingConstUnit, Dialogs;

//******************************************************************************
//          TBillingClient
//******************************************************************************
constructor TBillingClient.Create(DevType_: String=''; DevSerial_: String=''; DevVer_: String='');
begin
 inherited Create;
 FDevType    := DevType_;
 FDevSerial  := DevSerial_;
 FDevVer     := DevVer_;
 FContentVer := '1.0';
 FHttpClient := TIdHTTP.Create(nil);
 FHttpClient.ReadTimeout := 25000;
 FHttpClient.OnWorkBegin := FHTTPWorkBegin;
 FHttpClient.OnWork      := FHTTPWork;
 FHttpClient.OnWorkEnd   := FHTTPWorkEnd;
end;

destructor TBillingClient.Destroy;
begin
 if FHttpClient.Connected then FHttpClient.Disconnect;
 FHttpClient.Free;
 inherited Destroy;
end;

procedure TBillingClient.FHTTPWork(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
var Progress: Integer;
begin
 if (Assigned(FOnProgress))and(AWorkMode = wmRead) then
  begin
   if FHttpClient.Response.ContentLength > 0 then
    begin
     Progress := Round((FHttpClient.Response.ContentStream.Size / FHttpClient.Response.ContentLength) * 100);
     FOnProgress(Progress);
    end;
  end;
end;

procedure TBillingClient.FHTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Integer);
begin
 if (Assigned(FOnProgress))and(AWorkMode = wmRead) then FOnProgress(0);
end;

procedure TBillingClient.FHTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
 if (Assigned(FOnProgress))and(AWorkMode = wmRead) then FOnProgress(101);
end;

function TBillingClient.FGetReadTimeout: Integer;
begin
 Result := FHttpClient.ReadTimeout;
end;

procedure TBillingClient.FSetReadTimeout(Value: Integer);
begin
 FHttpClient.ReadTimeout := Value;
end;

function TBillingClient.GenerateUrl(Host, CommandName: String): String;
var I : Integer;
begin
 Result := '';
 // не може да е празно
 if Host = '' then Exit;
 // трабва да започва подобаващо
 if UpperCase(Copy(Host, 1, 7)) <> 'HTTP://' then Host := 'http://'+Host;
 // трябва да завършва подобаващо
 if Copy(Host, Length(Host), 1) <> '/' then Host := Host + '/';
 // документа трябва да бъде само един
 for I := 8 to Length(Host)-1 do
  begin
   if (Host[I] = '/')or(Host[I] = '\') then Exit;
  end;
 Result := Host + CommandName;
end;

function TBillingClient.SendRequest(HttpRequest: THttpRequest; URL: String; Source, Response: TStream;
                                    EncryptContent: Boolean=false; CloseAfterRequest: Boolean = true): Boolean;
var CrH  : TCrypto_TEA;
//    FStrm: TFileStream;
begin
  CrH := nil;

  FHttpClient.Request.ContentType    := 'text/xml';
  FHttpClient.Request.ContentVersion := FContentVer;
  FHttpClient.Request.CustomHeaders.Values['DevType'] := FDevType;
  FHttpClient.Request.CustomHeaders.Values['DevSer']  := FDevSerial;
  FHttpClient.Request.CustomHeaders.Values['DevVer']  := FDevVer;
  if not CloseAfterRequest then FHttpClient.Request.Connection := 'keep-alive'
   else FHttpClient.Request.Connection := '';
  if EncryptContent then FHttpClient.Request.ContentLanguage := 'encrypted'
   else FHttpClient.Request.ContentLanguage := '';

  try
   if (Source = nil)or(Response = nil) then raise EAbort.Create('Internal error. InOut streams are not initialized.');
   Response.Size := 0;

   if EncryptContent then
    begin
     CrH := TCrypto_TEA.Create;
     if not CrH.EncryptStreamEx(Source, FDevType+FDevSerial+FDevVer) then raise EAbort.Create('Fail encrypt source data: '+LastError);
    end;

   case HttpRequest of
   hcmGet : FHttpClient.DoRequest(hmGet,  Url, Source, Response);
   hcmPost: FHttpClient.DoRequest(hmPost, Url, Source, Response);
   end;

   if CloseAfterRequest then CloseConnection;

   if EncryptContent then
    begin
     if not CrH.DecryptStreamEx(Response, FDevType+FDevSerial+FDevVer) then raise EAbort.Create('Fail decrypt responce data: '+LastError);
    end;

   Result := true;
  except
   on E: EIdConnectException do
    begin
     CloseConnection;
     FlastError := 'Неуспешна комуникация със сървъра на Елтрейд!'+sLineBreak+
                   'Моля проверете дали имате интернет свързаност'+sLineBreak+
                   'и опитайте отново.'+sLineBreak+
                   ''+sLineBreak+
                   '[HTTP request] Connection error!'+sLineBreak+
                   E.Message;
     Result     := false;
    end;
   on E: Exception do
    begin
     CloseConnection;
     FlastError := '[HTTPRequest] '+E.Message;
     Result     := false;
    end;
  end;
  if CrH <> nil then CrH.Free;
end;

procedure TBillingClient.CloseConnection;
begin
 if FHttpClient.Connected then FHttpClient.Disconnect;
end;

//******************************************************************************
//          TBillingXMLClient
//******************************************************************************
constructor TBillingXMLClient.Create(Host_: String=''; DevType_: String=''; DevSerial_: String=''; DevVer_: String='');
begin
 inherited Create(DevType_, DevSerial_, DevVer_);
 FHost    := Host_;
 FXMLDoc  := CreateXMLDoc;
 FCommands:= TObjectList.Create;
 FCommands.OwnsObjects := true;
end;

destructor TBillingXMLClient.Destroy;
begin
 FCommands.Clear;
 FCommands.Free;
 FreeXmlDoc(FXMLDoc);
 inherited Destroy;
end;

function TBillingXMLClient.FGetVersion: String;
begin
 Result := '1.0';
end;

function TBillingXMLClient.FInitXMLDocument(CommandName: String): Boolean;
var iNode: IXMLElement;
    iPI  : IXMLProcessingInstruction;
begin
 try
   if FXMLDoc.documentElement = nil then
    begin
     iNode := FXMLDoc.createElement(xml_Node_DocumentRoot);
     iNode.setAttribute(xml_Attr_Command,     CommandName);
     iNode.setAttribute(xml_Attr_Version,     Version);
     iNode.setAttribute(xml_Attr_Application, ExtractFileName(ParamStr(0)));
     FXMLDoc.documentElement := iNode;
     iPI := FXMLDoc.createProcessingInstruction('xml', xml_ProcessInstruction);
     FXMLDoc.insertBefore(iPI, iNode);
    end;
   Result := true;
 except
  on E: Exception do
   begin
    FLastError := '[InitXMLDocument] Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TBillingXMLClient.FGetCommandResultCode: Integer;
var iNode: IXMLNode;
begin
 try
   iNode := FXMLDoc.documentElement;
   if iNode = nil then raise EAbort.Create('XML document is empty');
   iNode := iNode.SelectSingleNode(xml_Node_ResultCode);
   if iNode = nil then raise EAbort.Create('"ResultCode" does not exist in XML document');
   Result := StrToInt(iNode.Text);
 except
  on E: Exception do
   begin
    FLastError := '[GetXMLResultCode]'+E.Message;
    Result     := errcode_ExecuteFail_Internal;
   end;
 end;
end;

function TBillingXMLClient.FGetServerErrorMessage: String;
var iNode: IXMLNode;
    I    : Integer;
    S    : String;
begin
 S      := '';
 Result := '';
 try
   iNode := XmlDocument.documentElement;
   if iNode = nil then raise EAbort.Create('XML document is empty');
   iNode := iNode.selectSingleNode(xml_Node_ErrorMessage);
   if iNode <> nil then S := iNode.Text;

   for I := 1 to length(S) do
    begin
     if S[I] = '|' then Result := Result + sLineBreak
      else Result := Result + S[I];
    end;
 except
  on E: Exception do
   begin
    FLastError := '[GetXMLErrorMessage]'+E.Message;
    Result     := 'Internal error: '+E.Message;
   end;
 end;
end;

function TBillingXMLClient.SendCommand(Index: Integer; HttpRequest: THttpRequest;
                                       EncryptContent: Boolean=false; CloseAfterRequest: Boolean = true): Integer;
var Source  : TStringStream;
    Responce: TStringStream;
    aCommand: TCommandClient;
    URL     : String;
begin
 Source   := nil;
 Responce := nil;
 try
  if (Index < 0)or(Index >= FCommands.Count) then raise EAbort.Create('Invalid command index');
  if not(FCommands.Items[Index] is TCommandClient) then raise EAbort.Create('Invalid command class');
  aCommand := TCommandClient(FCommands.Items[Index]);

  FXMLDoc.loadXML('');
  URL := GenerateUrl(FHost, aCommand.GetCommandName);
  if not FInitXMLDocument(aCommand.GetCommandName) then raise EAbort.Create(LastError);
  if FXMLDoc.DocumentElement = nil then raise EAbort.Create('Source XML document is empty');
  if not aCommand.AddRequestToDocument then raise EAbort.Create(aCommand.LastError);
  if URL = '' then raise EAbort.Create('Invalid URL (Host:'+FHost+' CmdName:'+aCommand.GetCommandName+')');

  Source   := TStringStream.Create(FXMLDoc.xml);
  Responce := TStringStream.Create('');

  if not SendRequest(HttpRequest, URL,  Source, Responce, EncryptContent, CloseAfterRequest) then raise EAbort.Create(LastError);
  if not FXMLDoc.loadXML(Responce.DataString) then raise EAbort.Create('Responce is not valid XML: '+FXMLDoc.parseError.reason);

  Result := FGetCommandResultCode;
  aCommand.ResultCode := Result;
  if Result = errcode_ExecuteSucceed then
   begin
    aCommand.ErrorMsg := '';
    if not aCommand.GetAnswerFromDocument then raise EAbort.Create(aCommand.LastError);
   end
  else
   begin
    aCommand.ErrorMsg := FGetServerErrorMessage;
   end;

 except
  on E: Exception do
   begin
     CloseConnection;
     FLastError := E.Message+sLineBreak+
                   '[HTTP url] '+URL;
     Result     := errcode_ExecuteFail_Internal;
   end;
 end;
 if Source <> nil then Source.Free;
 if Responce <> nil then Responce.Free;
end;

function TBillingXMLClient.FGetCommandsCount: Integer;
begin
 Result := FCommands.Count;
end;

function TBillingXMLClient.FGetCommand(Index: Integer): TCommandClient;
begin
 Result := TCommandClient(FCommands.Items[Index]);
end;

function TBillingXMLClient.FGetCommandByClass(CommandClass: TCommandClientClass): TCommandClient;
var I : Integer;
begin
 Result := nil;
 for I := 0 to FCommands.Count - 1 do
  begin
   if FCommands is CommandClass then
    begin
     Result := TCommandClient(FCommands[I]);
    end;
  end;
end;

{procedure TBillingXMLClient.FSetCommand(Index: Integer; Value: TCommandClient);
begin
 FCommands.Items[Index] := Value;
end;}

function TBillingXMLClient.AddCommand(CommandClass: TCommandClientClass): TCommandClient;
begin
 Result := CommandClass.Create(XMLDocument);
 FCommands.Add(Result);
end;

procedure TBillingXMLClient.ClearCommands;
begin
 FCommands.Clear;
end;

end.
