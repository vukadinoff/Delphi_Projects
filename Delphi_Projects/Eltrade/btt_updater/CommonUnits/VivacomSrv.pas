// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : VivacomSrv.wsdl
//  >Import : VivacomSrv.wsdl:0
// Encoding : utf-8
// Version  : 1.0
// (03.08.2012 10:44:53 - - $Rev: 10138 $)
// ************************************************************************ //

unit VivacomSrv;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

const
  IS_REF  = $0080;


type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Borland types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]

  SuspendResumeRequest = class;                 { "http://www.w3.org/schemas"[Lit][GblElm] }
  SuspendResumeResponse = class;                { "http://www.w3.org/schemas"[Lit][GblElm] }
  StatusCheckRequest   = class;                 { "http://www.w3.org/schemas"[Lit][GblElm] }
  StatusCheckResponse  = class;                 { "http://www.w3.org/schemas"[Lit][GblElm] }

  SessionIDType   =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  TimestampType   =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  BulstatType     =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  ActionType      =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  ICCIDType       =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  IMSIType        =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  MSISDNType      =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  StatusType      =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }
  ErrorCodeType   =  type WideString;      { "http://www.w3.org/schemas"[GblSmpl] }


  // ************************************************************************ //
  // XML       : SuspendResumeRequest, global, <element>
  // Namespace : http://www.w3.org/schemas
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  SuspendResumeRequest = class(TRemotable)
  private
    FSessionID: SessionIDType;
    FTimestamp: TimestampType;
    FBulstat: BulstatType;
    FAction: ActionType;
    FICCID: ICCIDType;
    FIMSI: IMSIType;
    FMSISDN: MSISDNType;
  public
    constructor Create; override;
  published
    property SessionID: SessionIDType  read FSessionID write FSessionID;
    property Timestamp: TimestampType  read FTimestamp write FTimestamp;
    property Bulstat:   BulstatType    read FBulstat write FBulstat;
    property Action:    ActionType     read FAction write FAction;
    property ICCID:     ICCIDType      read FICCID write FICCID;
    property IMSI:      IMSIType       read FIMSI write FIMSI;
    property MSISDN:    MSISDNType     read FMSISDN write FMSISDN;
  end;



  // ************************************************************************ //
  // XML       : SuspendResumeResponse, global, <element>
  // Namespace : http://www.w3.org/schemas
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  SuspendResumeResponse = class(TRemotable)
  private
    FSessionID: SessionIDType;
    FTimestamp: TimestampType;
    FErrorCode: ErrorCodeType;
  public
    constructor Create; override;
  published
    property SessionID: SessionIDType  read FSessionID write FSessionID;
    property Timestamp: TimestampType  read FTimestamp write FTimestamp;
    property ErrorCode: ErrorCodeType  read FErrorCode write FErrorCode;
  end;



  // ************************************************************************ //
  // XML       : StatusCheckRequest, global, <element>
  // Namespace : http://www.w3.org/schemas
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  StatusCheckRequest = class(TRemotable)
  private
    FSessionID: SessionIDType;
    FTimestamp: TimestampType;
    FBulstat: BulstatType;
    FICCID: ICCIDType;
    FIMSI: IMSIType;
    FMSISDN: MSISDNType;
  public
    constructor Create; override;
  published
    property SessionID: SessionIDType  read FSessionID write FSessionID;
    property Timestamp: TimestampType  read FTimestamp write FTimestamp;
    property Bulstat:   BulstatType    read FBulstat write FBulstat;
    property ICCID:     ICCIDType      read FICCID write FICCID;
    property IMSI:      IMSIType       read FIMSI write FIMSI;
    property MSISDN:    MSISDNType     read FMSISDN write FMSISDN;
  end;



  // ************************************************************************ //
  // XML       : StatusCheckResponse, global, <element>
  // Namespace : http://www.w3.org/schemas
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  StatusCheckResponse = class(TRemotable)
  private
    FSessionID: SessionIDType;
    FTimestamp: TimestampType;
    FErrorCode: ErrorCodeType;
    FStatus: StatusType;
  public
    constructor Create; override;
  published
    property SessionID: SessionIDType  read FSessionID write FSessionID;
    property Timestamp: TimestampType  read FTimestamp write FTimestamp;
    property ErrorCode: ErrorCodeType  read FErrorCode write FErrorCode;
    property Status:    StatusType     read FStatus write FStatus;
  end;


  // ************************************************************************ //
  // Namespace : http://www.w3.org/schemas
  // soapAction: urn:FPP_Interface/Msg
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : FPP_SuspendResume
  // service   : FPP_ServiceSuspendResume
  // port      : FPP_SuspendResume_Interface
  // URL       : http://10.64.241.49:4444/fppsimcontrol
  // ************************************************************************ //
  FPP_SuspendResumeType = interface(IInvokable)
  ['{47FF410C-C8C4-0472-B044-F2A9578EFA84}']

    // Cannot unwrap: 
    //     - Input element wrapper name does not match operation's name
    function  Msg(const Parameters: SuspendResumeRequest): SuspendResumeResponse; stdcall;
  end;


  // ************************************************************************ //
  // Namespace : http://www.w3.org/schemas
  // soapAction: urn:FPP_Interface/Msg
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : FPP_Status
  // service   : FPP_ServiceStatus
  // port      : FPP_Status_Interface
  // URL       : http://10.64.241.49:4444/fppsimcontrol
  // ************************************************************************ //
  FPP_StatusType = interface(IInvokable)
  ['{1E136903-2231-FD0D-7D1B-D5968C86E761}']

    // Cannot unwrap: 
    //     - Input element wrapper name does not match operation's name
    //     - More than one strictly out element was found
    function  Msg(const Parameters: StatusCheckRequest): StatusCheckResponse; stdcall;
  end;

function GetFPP_SuspendResumeType(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): FPP_SuspendResumeType;
function GetFPP_StatusType(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): FPP_StatusType;


implementation
  uses SysUtils;

function GetFPP_SuspendResumeType(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): FPP_SuspendResumeType;
const
  defWSDL = 'VivacomSrv.wsdl';
  defURL  = 'http://10.64.241.49:4444/fppsimcontrol';
  defSvc  = 'FPP_ServiceSuspendResume';
  defPrt  = 'FPP_SuspendResume_Interface';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as FPP_SuspendResumeType);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


function GetFPP_StatusType(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): FPP_StatusType;
const
  defWSDL = 'VivacomSrv.wsdl';
  defURL  = 'http://10.64.241.49:4444/fppsimcontrol';
  defSvc  = 'FPP_ServiceStatus';
  defPrt  = 'FPP_Status_Interface';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as FPP_StatusType);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


constructor SuspendResumeRequest.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

constructor SuspendResumeResponse.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

constructor StatusCheckRequest.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

constructor StatusCheckResponse.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(FPP_SuspendResumeType), 'http://www.w3.org/schemas', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(FPP_SuspendResumeType), 'urn:FPP_Interface/Msg');
  InvRegistry.RegisterInvokeOptions(TypeInfo(FPP_SuspendResumeType), ioDocument);
  InvRegistry.RegisterInvokeOptions(TypeInfo(FPP_SuspendResumeType), ioLiteral);
  InvRegistry.RegisterExternalParamName(TypeInfo(FPP_SuspendResumeType), 'Msg', 'Parameters1', 'Parameters');
  InvRegistry.RegisterInterface(TypeInfo(FPP_StatusType), 'http://www.w3.org/schemas', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(FPP_StatusType), 'urn:FPP_Interface/Msg');
  InvRegistry.RegisterInvokeOptions(TypeInfo(FPP_StatusType), ioDocument);
  InvRegistry.RegisterInvokeOptions(TypeInfo(FPP_StatusType), ioLiteral);
  InvRegistry.RegisterExternalParamName(TypeInfo(FPP_StatusType), 'Msg', 'Parameters1', 'Parameters');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SessionIDType), 'http://www.w3.org/schemas', 'SessionIDType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(TimestampType), 'http://www.w3.org/schemas', 'TimestampType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(BulstatType), 'http://www.w3.org/schemas', 'BulstatType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ActionType), 'http://www.w3.org/schemas', 'ActionType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ICCIDType), 'http://www.w3.org/schemas', 'ICCIDType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(IMSIType), 'http://www.w3.org/schemas', 'IMSIType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(MSISDNType), 'http://www.w3.org/schemas', 'MSISDNType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(StatusType), 'http://www.w3.org/schemas', 'StatusType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ErrorCodeType), 'http://www.w3.org/schemas', 'ErrorCodeType');
  RemClassRegistry.RegisterXSClass(SuspendResumeRequest, 'http://www.w3.org/schemas', 'SuspendResumeRequest');
  RemClassRegistry.RegisterSerializeOptions(SuspendResumeRequest, [xoLiteralParam]);
  RemClassRegistry.RegisterXSClass(SuspendResumeResponse, 'http://www.w3.org/schemas', 'SuspendResumeResponse');
  RemClassRegistry.RegisterSerializeOptions(SuspendResumeResponse, [xoLiteralParam]);
  RemClassRegistry.RegisterXSClass(StatusCheckRequest, 'http://www.w3.org/schemas', 'StatusCheckRequest');
  RemClassRegistry.RegisterSerializeOptions(StatusCheckRequest, [xoLiteralParam]);
  RemClassRegistry.RegisterXSClass(StatusCheckResponse, 'http://www.w3.org/schemas', 'StatusCheckResponse');
  RemClassRegistry.RegisterSerializeOptions(StatusCheckResponse, [xoLiteralParam]);

end.