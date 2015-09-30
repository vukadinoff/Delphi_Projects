unit MtelSimRechargeUnit;

interface

uses SysUtils, Classes, TcpClientUnit, xmlHandler;

type
  TSimMtelStatus = (resOK, resErrSys, resErrComm);

  TSimRechargeMtel = class(TObject)
  private
    FSoap     : TSoapClient;
    FLastError: String;
    FOnError  : TGetStrProc;
    FMsgCntr  : Integer;
    FLocation : String;

    function    FGetReadTimeout: Integer;
    procedure   FSetReadTimeout(Value: Integer);
    procedure   FPostError(Msg: String);
    procedure   FCheckResponce(XmlHndr: THandlerXML; var ResultCode: Integer);
    function    FCalcSessionId(IncrementCounter: Boolean=false): String;
  public
    constructor Create(URL, Location: String);
    destructor  Destroy; override;

    function    CheckPrima(MSISDN: String; var ResultCode: Integer): TSimMtelStatus;
    function    PayPrima(MSISDN: String; Ammount: Real; var ResultCode: Integer): TSimMtelStatus;
    function    TranslateStatusPrima(ResultCode: Integer; InBulgarian: Boolean = true): String;

    property    ReadTimeout: Integer read FGetReadTimeout write FSetReadTimeout;
    property    LastError: String read FLastError write FLastError;
    property    OnError: TGetStrProc read FOnError write FOnError;
  end;

implementation
uses Math;

const
  C_SessionPrefix = 'BGT';

constructor TSimRechargeMtel.Create(URL, Location: String);
begin
  inherited Create;
  FSoap     := TSoapClient.Create(URL);
  FOnError  := nil;
  FLastError:= '';

  {************************************************************}
  { generate transaction id instead of location (using only for tests) }
  {************************************************************}
  Randomize;
  FMsgCntr  := RandomRange(1, 1000);
  FLocation := Copy(Location, 1, 4);
  {************************************************************}

end;

destructor TSimRechargeMtel.Destroy;
begin
  FSoap.Free;
  inherited Destroy;
end;

function TSimRechargeMtel.FGetReadTimeout: Integer;
begin
  Result := FSoap.ReadTimeout;
end;

procedure TSimRechargeMtel.FSetReadTimeout(Value: Integer);
begin
  FSoap.ReadTimeout := Value;
end;

procedure TSimRechargeMtel.FPostError(Msg: String);
begin
  FLastError := '[' + self.ClassName + ']' + Msg;
  if (Assigned(FOnError)) then FOnError(FLastError);
end;

function TSimRechargeMtel.FCalcSessionId(IncrementCounter: Boolean = false): String;
begin
  if (IncrementCounter) then FMsgCntr := FMsgCntr + 1; // next location
  Result := C_SessionPrefix + FLocation + FormatFloat('000000', FMsgCntr);
end;

procedure TSimRechargeMtel.FCheckResponce(XmlHndr: THandlerXML; var ResultCode: Integer);
var Node : IXMLNode;
    S    : String;
begin
  try
    Node := XmlHndr.XML_GetRootNode;

    s := XmlHndr.XML_GetNodeText(Node, 'SessionID');
    if (not SameText(S, FCalcSessionId)) then raise EAbort.Create('Invalid session ID (' + S + '/' + FCalcSessionId+')');

    if (SameText(Node.nodeName, 'CheckPrima')) then
      ResultCode := XmlHndr.XML_GetNodeInt(Node, 'Status')
    else
      if (SameText(Node.nodeName, 'CheckPrimaResult')) then
        ResultCode := XmlHndr.XML_GetNodeInt(Node, 'Return')
      else
        raise EAbort.Create('Invalid root element: ' + Node.nodeName);
  except
    on E: Exception do raise EAbort.Create('Invalid server responce: ' + E.Message);
  end;
end;

function TSimRechargeMtel.CheckPrima(MSISDN: String; var ResultCode: Integer): TSimMtelStatus;
var Req   : THandlerXML;
    iNode : IXMLElement;
    iPI   : IXMLProcessingInstruction;
    S     : String;
begin
  Result := resErrSys;
  try
    if MSISDN = '' then raise EAbort.Create('Invalid MSISDN');

    Req := THandlerXML.Create;
    try
      iNode := Req.XmlDocument.createElement('CheckPrima');
      Req.XmlDocument.documentElement := iNode;
      iPI := Req.XmlDocument.createProcessingInstruction('xml', 'version="1.0" encoding="UTF-8"');
      Req.XmlDocument.insertBefore(iPI, iNode);

      iNode.appendChild(Req.XmlDocument.createElement('SessionID')).text := FCalcSessionId(true);
      iNode.appendChild(Req.XmlDocument.createElement('MSISDN')).text    := MSISDN;
      iNode.appendChild(Req.XmlDocument.createElement('Timestamp')).text := FormatDateTime('YYYYMMDDHHNNSSzzz', Now);

      S := Req.XmlString;
    finally
      Req.Free;
    end;

    case FSoap.SendDocument(S) of
      soapOK:
        begin
          FCheckResponce(FSoap.XML, ResultCode);
          Result := resOK;
        end;
      soapConnectFail:
        begin
          Result := resErrComm;
          raise EAbort.Create('Connection error: ' + FSoap.LastError);
        end;
      soapSystem     : raise EAbort.Create('System error: ' + FSoap.LastError);
      soapServerError: raise EAbort.Create('Server error: ' + FSoap.LastError);
    end;
  except
    on E: Exception do FPostError('[CheckPrima]' + E.Message);
  end;
end;

function TSimRechargeMtel.PayPrima(MSISDN: String; Ammount: Real; var ResultCode: Integer): TSimMtelStatus;
var Req   : THandlerXML;
    iNode : IXMLElement;
    iPI   : IXMLProcessingInstruction;
    S     : String;
begin
  Result := resErrSys;
  try
    if MSISDN = '' then raise EAbort.Create('Invalid MSISDN');

    Req := THandlerXML.Create;
    try
      iNode := Req.XmlDocument.createElement('PayPrima');
      Req.XmlDocument.documentElement := iNode;
      iPI := Req.XmlDocument.createProcessingInstruction('xml', 'version="1.0" encoding="UTF-8"');
      Req.XmlDocument.insertBefore(iPI, iNode);

      iNode.appendChild(Req.XmlDocument.createElement('SessionID')).text := FCalcSessionId(true);
      iNode.appendChild(Req.XmlDocument.createElement('MSISDN')).text    := MSISDN;
      iNode.appendChild(Req.XmlDocument.createElement('Amount')).text    := FormatFloat('0.00', Ammount);
      iNode.appendChild(Req.XmlDocument.createElement('Timestamp')).text := FormatDateTime('YYYYMMDDHHNNSSzzz', Now);

      S := Req.XmlString;
    finally
      Req.Free;
    end;

    case FSoap.SendDocument(S) of
      soapOK         : begin
                        FCheckResponce(FSoap.XML, ResultCode);
                        Result := resOK;
                       end;
      soapConnectFail: begin
                        Result := resErrComm;
                        raise EAbort.Create('Connection error: '+FSoap.LastError);
                       end;
      soapSystem     : raise EAbort.Create('System error: '+FSoap.LastError);
      soapServerError: raise EAbort.Create('Server error: '+FSoap.LastError);
    end;
  except
    on E: Exception do FPostError('[PayPrima]' + E.Message);
  end;
end;

function TSimRechargeMtel.TranslateStatusPrima(ResultCode: Integer; InBulgarian: Boolean = true): String;
begin
  case ResultCode of
    0 : if (InBulgarian) then Result := 'Операцията изпълнена.' else Result := 'OK';
    13: if (InBulgarian) then Result := 'Невалидна сума.' else Result := 'Invalid amount.';
    14: if (InBulgarian) then Result := 'Невалиден номер.' else Result := 'Invalid phone number.';
    80: if (InBulgarian) then Result := 'Заявката временно не може да бъде изпълнена.'
                         else Result := 'Request temporary can''t be performed.';
    96: if (InBulgarian) then Result := 'Обща грешка.' else Result := 'General error.';
  else
    if (InBulgarian) then Result := 'Непознат код на грешка: ' + IntToStr(ResultCode)
                     else Result := 'Unknown error code: ' + IntToStr(ResultCode);
  end;
end;

end.

