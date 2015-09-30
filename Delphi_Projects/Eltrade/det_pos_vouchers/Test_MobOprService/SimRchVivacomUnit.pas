unit SimRchVivacomUnit;

interface

uses SysUtils, Classes, TcpClientUnit, xmlHandler;

type
  TSimVivacomResult = (resOK, resErrSys, resErrComm);

  TSimRechargeVivacom = class(TObject)
  private
    FSoap     : TSoapClient;
    FLastError: String;
    FOnError  : TGetStrProc;
    FMsgCntr  : Integer;
    FLocation : String;

    function  FGetReadTimeout: Integer;
    procedure FSetReadTimeout(Value: Integer);
    procedure FPostError(Msg: String);
    procedure FCheckResponce(XmlHndr: THandlerXML; var ResultCode: Integer);
    function  FCalcSessionId(IncrementCounter: Boolean=false): String;
  public
    constructor Create(URL, Location: String);
    destructor  Destroy; override;

    function CheckPrePaid(MSISDN: String; var ResultCode: Integer): TSimVivacomResult;
    function PayPrePaid(MSISDN: String; Ammount: Real; var ResultCode: Integer): TSimVivacomResult;
    function TranslateResultCodeCheck(ResultCode: Integer; InBulgarian: Boolean=true): String;
    function TranslateResultCodePay(ResultCode: Integer; InBulgarian: Boolean=true): String;

    property ReadTimeout: Integer read FGetReadTimeout write FSetReadTimeout;
    property LastError: String read FLastError write FLastError;
    property OnError: TGetStrProc read FOnError write FOnError;
  end;

implementation
uses Math;

const
 C_SessionPrefix = 'BGT';

constructor TSimRechargeVivacom.Create(URL, Location: String);
begin
 inherited Create;
 FSoap     := TSoapClient.Create(URL);
 FOnError  := nil;
 FLastError:= '';

 // init session ID -�������� ID �� �����
 // �������� - ��� ��� ��� �� ������
 Randomize;
 FMsgCntr  := RandomRange(1, 1000);
 FLocation := Copy(Location, 1, 4);
end;

destructor TSimRechargeVivacom.Destroy;
begin
 FSoap.Free;
 inherited Destroy;
end;

function TSimRechargeVivacom.FGetReadTimeout: Integer;
begin
 Result := FSoap.ReadTimeout;
end;

procedure TSimRechargeVivacom.FSetReadTimeout(Value: Integer);
begin
 FSoap.ReadTimeout := Value;
end;

procedure TSimRechargeVivacom.FPostError(Msg: String);
begin
 FLastError := '['+self.ClassName+']'+Msg;
 if Assigned(FOnError) then FOnError(FLastError);
end;

function TSimRechargeVivacom.FCalcSessionId(IncrementCounter: Boolean=false): String;
begin
 if IncrementCounter then FMsgCntr := FMsgCntr + 1; // �������������� �� ������
 Result := C_SessionPrefix + FLocation + FormatFloat('000000', FMsgCntr);
end;

procedure TSimRechargeVivacom.FCheckResponce(XmlHndr: THandlerXML; var ResultCode: Integer);
var Nd : IXMLNode;
    S  : String;
begin
 try
  Nd := XmlHndr.XML_GetRootNode;

  S  := XmlHndr.XML_GetNodeText(Nd, 'SessionID');
  if not SameText(S, FCalcSessionId) then raise EAbort.Create('Invalid session ID ('+S+'/'+FCalcSessionId+')');

  if SameText(Nd.nodeName, 'checkPrePaidResponse') then
   ResultCode := XmlHndr.XML_GetNodeInt(Nd, 'Status')
  else
  if SameText(Nd.nodeName, 'payPrePaidResponse') then
   ResultCode := XmlHndr.XML_GetNodeInt(Nd, 'Return')
  else
   raise EAbort.Create('Invalid root element: '+nd.nodeName); 


 except
  on E: Exception do raise EAbort.Create('Invalid server responce: '+E.Message);
 end;
end;

function TSimRechargeVivacom.CheckPrePaid(MSISDN: String; var ResultCode: Integer): TSimVivacomResult;
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
   iNode := Req.XmlDocument.createElement('checkPrePaid');
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
  on E: Exception do FPostError('[CheckPrePaid]'+E.Message);
 end;
end;

function TSimRechargeVivacom.PayPrePaid(MSISDN: String; Ammount: Real; var ResultCode: Integer): TSimVivacomResult;
var Req   : THandlerXML;
    iNode : IXMLElement;
    iPI   : IXMLProcessingInstruction;
    S     : String;
begin
 Result := resErrSys;
 try
  if MSISDN = '' then raise EAbort.Create('Invalid MSISDN');
  if Ammount < 0 then raise EAbort.Create('Invalid ammount');

  Req := THandlerXML.Create;
  try
   iNode := Req.XmlDocument.createElement('payPrePaid');
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
  on E: Exception do FPostError('[PayPrePaid]'+E.Message);
 end;
end;

function TSimRechargeVivacom.TranslateResultCodeCheck(ResultCode: Integer; InBulgarian: Boolean=true): String;
begin
 case ResultCode of
 0:	if InBulgarian then Result := '���������� ���������'
     else               Result := 'OK';

 1:	if InBulgarian then Result := '�������� ������. �������� ���!'
     else               Result := 'System error - try again';

 2:	if InBulgarian then Result := '��������� �����. �������� �� � ��������������!'
     else               Result := 'Incorrect data - contact administrator';

 3:	if InBulgarian then Result := '������ �� ���� �� ���� ������!'+sLineBreak+'(�� ���������� ��� �� � ����������.)'
    else                Result := 'MSISDN not found '+sLineBreak+'(Not existing or Not prepaid)';

 4:	if InBulgarian then Result := '������ �� � �������!'
     else               Result := 'MSISDN is not active';

 else
    if InBulgarian then Result := '�������� ��� �� ������: '+IntToStr(ResultCode)
     else Result := 'Unknown result code: '+IntToStr(ResultCode);
 end;
end;

function TSimRechargeVivacom.TranslateResultCodePay(ResultCode: Integer; InBulgarian: Boolean=true): String;
begin
 case ResultCode of
 0:	if InBulgarian then Result := '���������� ���������'
     else               Result := '��';

 1:	if InBulgarian then Result := '�������� ������. �������� ���!'
     else               Result := 'System error - try again';

 2:	if InBulgarian then Result := '��������� �����. �������� �� � ��������������!'
     else               Result := 'Incorrect data - contact administrator';

 3:	if InBulgarian then Result := '������ � ����� ����������� �������!'
     else               Result := 'Amount is out of range';

 4:	if InBulgarian then Result := '������ �� � �������!'
     else               Result := 'MSISDN is not active';

 5:	if InBulgarian then Result := '������ �� ���� �� ���� ������!'+sLineBreak+'(�� ���������� ��� �� � ����������.)'
     else               Result := 'MSISDN not found '+sLineBreak+'(Not existing or Not prepaid)';

 6:	if InBulgarian then Result := '�������� �������� �����!'
     else               Result := 'Balance exceed or No balance for this shop';

 else
    if InBulgarian then Result := '�������� ��� �� ������: '+IntToStr(ResultCode)
     else Result := 'Unknown result code: '+IntToStr(ResultCode);
 end;
end;

end.

