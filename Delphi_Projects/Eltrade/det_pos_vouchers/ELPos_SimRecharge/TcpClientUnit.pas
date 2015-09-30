unit TcpClientUnit;

interface

uses SysUtils, Classes, IdHTTP, IdComponent, Contnrs, XMLHandler;

type
 THttpRequest = (hcmGet, hcmPost);
 THttpClientProgress = procedure(Percent: Integer) of object;
 THttpRequestResult = (hresOK, hresConnectFail, hresProtocolError, hresSystem);
 TSoapRequestResult = (soapOK, soapConnectFail, soapSystem, soapServerError);

 THttpClient = class(TObject)
 private
  FHttpClient : TIdHTTP;
  FLastError  : String;
  FContentVer : String;
  FOnProgress : THttpClientProgress;
  function FGetReadTimeout: Integer;
  procedure FSetReadTimeout(Value: Integer);
  procedure FHTTPWork(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
  procedure FHTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Integer);
  procedure FHTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
 public
  constructor Create();
  destructor Destroy; override;

  function SendRequest(HttpRequest: THttpRequest; URL: String; Source, Response: TStream;
                       CloseAfterRequest: Boolean=true): THttpRequestResult;
  procedure CloseConnection;

  property LastError: String read FLastError write FLastError;
  property ReadTimeout: Integer read FGetReadTimeout write FSetReadTimeout;
  property ContentVersion: String read FContentVer write FContentVer;
  property OnReadProgress: THttpClientProgress read FOnProgress write FOnProgress;
 end;

 TSoapClient = class(THttpClient)
 private
  FURL   : String;
  FXML   : THandlerXML;
  function FLoadXmlResponce(Src: String): Boolean;
  function FGetReceiveXml: String;
 public
  constructor Create(URL_: String);
  destructor Destroy; override;

  function SendDocument(XML: String; CloseAfterRequest: Boolean=true): TSoapRequestResult;

  property URL: String read FURL write FURL;
  property ReceiveXml: String read FGetReceiveXml;
  property XML: THandlerXML read FXML write FXML;
 end;


implementation
uses IDException;

//******************************************************************************
//          TBillingClient
//******************************************************************************
constructor THttpClient.Create();
begin
 inherited Create;
 FContentVer := '1.0';
 FHttpClient := TIdHTTP.Create(nil);
 FHttpClient.ReadTimeout := 30000;
 FHttpClient.OnWorkBegin := FHTTPWorkBegin;
 FHttpClient.OnWork      := FHTTPWork;
 FHttpClient.OnWorkEnd   := FHTTPWorkEnd;
end;

destructor THttpClient.Destroy;
begin
 if FHttpClient.Connected then FHttpClient.Disconnect;
 FHttpClient.Free;
 inherited Destroy;
end;

procedure THttpClient.FHTTPWork(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
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

procedure THttpClient.FHTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Integer);
begin
 if (Assigned(FOnProgress))and(AWorkMode = wmRead) then FOnProgress(0);
end;

procedure THttpClient.FHTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
 if (Assigned(FOnProgress))and(AWorkMode = wmRead) then FOnProgress(101);
end;

function THttpClient.FGetReadTimeout: Integer;
begin
 Result := FHttpClient.ReadTimeout;
end;

procedure THttpClient.FSetReadTimeout(Value: Integer);
begin
 FHttpClient.ReadTimeout := Value;
end;

function THttpClient.SendRequest(HttpRequest: THttpRequest; URL: String; Source, Response: TStream; CloseAfterRequest: Boolean = true): THttpRequestResult;
//var FStrm : TFileStream;
begin
  FHttpClient.Request.ContentType     := 'text/xml';
  FHttpClient.Request.ContentVersion  := FContentVer;
  if not CloseAfterRequest then FHttpClient.Request.Connection := 'keep-alive'
   else FHttpClient.Request.Connection := '';

  try
   if URL = '' then raise EAbort.Create('Invalid server URL! Please check config file!');
   if (Source = nil)or(Response = nil) then raise EAbort.Create('Internal error. InOut streams are not initialized.');
   Response.Size := 0;

   // za testove
{   FStrm := TFileStream.Create('Send.xml', fmCreate);
   try
    Source.Position := 0;
    FStrm.CopyFrom(Source, Source.Size);
   finally
    FStrm.Free;
   end;
}
   case HttpRequest of
   hcmGet : FHttpClient.DoRequest(hmGet,  Url, Source, Response);
   hcmPost: FHttpClient.DoRequest(hmPost, Url, Source, Response);
   end;

   if CloseAfterRequest then CloseConnection;

      // za testove
{   FStrm := TFileStream.Create('Receive.xml', fmCreate);
   try
    Response.Position := 0;
    FStrm.CopyFrom(Response, Response.Size);
   finally
    FStrm.Free;
   end;
}
   Result := hresOK;
  except
   on E: EIdConnectException do
    begin
     CloseConnection;
     Result := hresConnectFail;
     FlastError := '[HTTP request] Connection error:'+E.Message;
    end;
   on E: EIdHTTPProtocolException do
    begin
     CloseConnection;
     Result := hresProtocolError;
     // Четене на съдържанието
     FLastError := E.ErrorMessage;
     if FLastError <> '' then Response.Write(FLastError[1], Length(FLastError));

     FlastError := '[HTTP request] Server error:'+E.Message;
    end;
   on E: Exception do
    begin
     CloseConnection;
     Result     := hresSystem;
     FlastError := '[HTTPRequest] System error:'+E.Message;
    end;
  end;
end;

procedure THttpClient.CloseConnection;
begin
 if FHttpClient.Connected then FHttpClient.Disconnect;
end;

//******************************************************************************
//          TBillingXMLClient
//******************************************************************************
constructor TSoapClient.Create(URL_: String);
begin
 inherited Create();
 FURL  := URL_;
 FXML  := THandlerXML.Create;
end;

destructor TSoapClient.Destroy;
begin
 FXML.Free;
 inherited Destroy;
end;

function TSoapClient.FLoadXmlResponce(Src: String): Boolean;
begin
 Result := true;
 try
  if not FXML.LoadFromString(Src) then raise EAbort.Create(FXML.LastError);

 except
  on E: Exception do
   begin
    Result := false;
    LastError := '[FLoadXmlResponce] Invalid server responce: '+E.Message;
   end;
 end;
end;

function TSoapClient.FGetReceiveXml: String;
begin
 Result := FXML.XmlString;
end;

function TSoapClient.SendDocument(XML: String; CloseAfterRequest: Boolean=true): TSoapRequestResult;
var Source  : TStringStream;
    Responce: TStringStream;
begin
 Result   := soapSystem;
 Source   := nil;
 Responce := nil;
 try
  if XML = '' then raise EAbort.Create('Invalid XML document. (Empty)');

  Source   := TStringStream.Create(UTF8Encode(XML));
  Responce := TStringStream.Create('');

  case SendRequest(hcmPost, FURL,  Source, Responce, CloseAfterRequest) of
  hresOK:
   begin
    Result := soapServerError;
    if FLoadXmlResponce(Responce.DataString) then Result := soapOK;
   end;
  hresProtocolError:
   begin
    Result := soapServerError;
    // try to load the responce content
    FXML.LoadFromString(Responce.DataString);
   end;
  hresConnectFail:
   begin
    Result := soapConnectFail;
   end;
  hresSystem:
   begin
    Result := soapSystem;
   end;
  end;

 except
  on E: Exception do
   begin
     CloseConnection;
     FLastError := '[SOAP Request]'+E.Message+sLineBreak+
                   '[HTTP url] '+URL;
     Result     := soapSystem;
   end;
 end;
 if Source <> nil then Source.Free;
 if Responce <> nil then Responce.Free;
end;

end.
