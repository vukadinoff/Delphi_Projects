unit RegistrationXmlUnit;

interface

uses SysUtils, Windows, Classes, ActiveX, XMLHandler, XMLHandlerMS;

type
  TNRA_ReqType = (rtRegister = 1, rtChange = 2, rtUnRegister = 3, rtFDInfo = 98, rtRegistrationResponse = 99);

  { Адрес на обекта. Задължително при регистрация }
  TNRA_SAddr = record
    SettlCode  : String; // Код на населеното място по ЕКАТТЕ
    SettlName  : String; // Settlement - Име на населено място (град или село). Напр. София
    AreaCode   : String; // Код на населеното място по ЕКАТТЕ плюс район(area)
    AreaName   : String; // Име на район напр. р-н Искър
    StrCode    : String; // Код на пътната артерия от номенклатурата на пътните артерии към служба ГРАО
    StrName    : String; // Име на улица
    StrNo      : String; // Номер в улица
    Block      : String; // Номер на блок
    En         : String; // Вход в блок
    Fl         : String; // Етаж
    Ap         : String; // Номер апартамент
  end;

  TNRA_Request = record
    ReqType     : TNRA_ReqType; // Тип на заявката
		FDevType    : Byte;         // Тип на фискалното устройство 1-ЕКАФП;	2-ФПр; 3-ЕСФП за течни горива; 4-ИАСУТД;
		CustEIK     : String;       // ЕИК номер; само цифри [9,13]
		CustEIKType : Byte;         // ЕИК тип номер 0-Булстат; 1-ЕГН; 2-ЛНЧ; 3-Служебен Номер
		FDevSerial  : String;       // Идентификационен номер на фискално устройство
		FDevMFM     : String;       // Идентификационен номер на фискална памет
		FDevCertN   : String;       // Номер на свидетелство на ФУ/ИАСУТД.
		SimIMSI     : String;       // IMSI номер на сим карта
		SimMSISDN   : String;       // MSISDN номер на сим карта във фомрат 359XXXXXXXXX.
		SimOperN    : Byte;         // Мобилен оператор 	0-Mtel;	1-Globul;	2-Vivacom;
		OrgName     : String;       // Име на фирма
		PSetNum     : String;       // Номер на обект
		PSetType    : Word;         // Тип на обекта  - по списък
		PSetAddr    : TNRA_SAddr;   // Адрес на обекта
		PSetName    : String;       // Име на обект, напр. Магазин Дондуков.
		StartDate   : TDateTime;    // Дата на встъпване в експлоатация
		SvceEIK     : String;       // ЕИК номер на сервиз
		SvceEIKType : Byte;         // ЕИК тип номер на сервиз (най-често БУЛСТАТ) 0-Булстат; 1-ЕГН; 2-ЛНЧ; 3-Служебен Номер
		SvceContrExpire: TDateTime; // Дата на изтичане на договора със сервиза

    { Специфично за промяна }
    FDRID       : String;       // Регистрационен идентификатор. Генерира се от НАП по време на регистрация.

    { Специфично за отрегистрация }
    DRegReason  : Byte;         // Причина за от регистрация - от таблица
  end;

  TNRA_Response = record
    ReqType : TNRA_ReqType;
    Status  : Word;
    ErrMsg  : String;
  end;

  TNRA_ZTask = record
    ZTaskTID    : Integer;      // не знам какво значи
    ZTaksTime   : TDateTime;    // Текущо време по сървъра на НАП
    ZTaskDays   : Integer;      // не знам какво значи
  end;

  TNRA_FDINFO = record
		CustEIK     : String;       // ЕИК номер; само цифри [9,13]
		CustEIKType : Byte;         // ЕИК тип номер 0-Булстат; 1-ЕГН; 2-ЛНЧ; 3-Служебен Номер
		FDevSerial  : String;       // Идентификационен номер на фискално устройство
		FDevMFM     : String;       // Идентификационен номер на фискална памет
		SimIMSI     : String;       // IMSI номер на сим карта
		SimMSISDN   : String;       // MSISDN номер на сим карта във фомрат 359XXXXXXXXX.
    SimOperN    : Byte;         // Мобилен оператор 	0-Mtel;	1-Globul;	2-Vivacom;
    FDRID       : String;       // Регистрационен идентификатор. Генерира се от НАП по време на регистрация.
		FDevType    : Byte;         // Тип на фискалното устройство 1-ЕКАФП;	2-ФПр; 3-ЕСФП за течни горива; 4-ИАСУТД;
    RegDate     : TDateTime;    // Дата на регистрация
    NRATime     : TDateTime;    // Текущо време по сървъра на НАП
    ZTask       : TNRA_ZTask;   // Z задача
  end;


  TNRA_FiscalParams = record
    CustEIK   : String;
    OrgName   : String;
    SimIMSI   : String;
    TaxValue  : array [1..8] of Real;
    TaxCountV : Byte;
    DecimalPoint: Boolean;
  end;

  TNRA_AdvLines = array [1..10] of String;

  TNRA_PayTypes = record
    Count: Integer;
    PType: array [1..20] of Byte;
  end;

  TNRA_ExtParams = record
    OwnerEIK        : String;
    OwnerTaxEIK     : String;
    OwnerAddress    : String;
    SiteShAddress   : String;
    SvcContractNumb : String;
    SvcContractFrom : TDateTime;
    SvcContractTo   : TDateTime;
    DeviceComment   : String;
  end;

  TNRA_Xml = class(TObject)
  private
    FNameSpace  : String;

    function FRemoveLFCR(Source_: String): String;
    function FAddLFCR(Source_: String): String;

    { xml node methods }
    function Attribtue(Node: IXMLDOMNode; const Attribtue: string): string;
    function NodeStrValue(Node: IXMLDOMNode): string;
    function NodeIntValue(Node: IXMLDOMNode): Integer;
    function NodeDateTimeValue(Node: IXMLDOMNode): TDateTime;

    function DateTimeToXSDateTime(ADateTime: TDateTime): string;
    function XSDateTimeToDateTime(XSDateTime: string): TDateTime;

    procedure AddSimpleElement(XMLDoc: IXMLDOMDocument; Parent: IXMLDOMNode; const Field, Value: string); overload;
    procedure AddSimpleElement(XMLDoc: IXMLDOMDocument; Parent: IXMLDOMNode; const Field: string; Value: Integer); overload;
    procedure AddSimpleElement(XMLDoc: IXMLDOMDocument; Parent: IXMLDOMNode; const Field: string; Value: TDateTime); overload;

    { Create methods }
    function AddRREG   (XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request; PetrolFormat: Boolean=false): IXMLDOMNode;
    function AddRCHANGE(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request; PetrolFormat: Boolean=false): IXMLDOMNode;
    function AddRDEREG (XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request; PetrolFormat: Boolean=false): IXMLDOMNode;
    function AddFDInfo (XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request): IXMLDOMNode;
    function AddPetrolConfigXml(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; PetrolXML: String): IXMLDOMNode;

    { Parse methods }
    procedure ParsePSAddr(PSAddrNode: IXMLDOMElement; var ARequest: TNRA_Request);
    procedure ParseZTask (PSZRDNode: IXMLDOMElement; var ZTask: TNRA_ZTask);

    procedure ParseRequest  (XmlDoc: IXMLDOMDocument; var ARequest: TNRA_Request; var PetrolXML: String);
    procedure ParseResponse (XmlDoc: IXMLDOMDocument; var AResponse: TNRA_Response);
    procedure ParseFParams  (XmlDoc: IXMLDOMDocument; var AFParams: TNRA_FiscalParams);
    procedure ParseAdvLines (XmlDoc: IXMLDOMDocument; var AAdvLines: TNRA_AdvLines);
    procedure ParseExtParams(XmlDoc: IXMLDOMDocument; var EParams: TNRA_ExtParams);
    procedure ParseFDINFO   (XmlDoc: IXMLDOMDocument; var AFDINFO: TNRA_FDINFO; var PetrolXML: String);
  public
    procedure LoadRequest(const Xml: String; var ARequest: TNRA_Request; var PetrolXML: String); overload;
    procedure LoadRequest(const Xml: string; var ARequest: TNRA_Request); overload;
    procedure LoadFDINFO (const Xml: String; var AFDINFO: TNRA_FDINFO; var PetrolXML: String); 
//    procedure LoadRequestFromFile(const FileName: string; var ARequest: TNRA_Request; var PetrolXML: String);
    procedure LoadResponse(const Xml: String; var AResponse: TNRA_Response);
    procedure LoadFiscalParams(const Xml: String; var AFParams: TNRA_FiscalParams);
    procedure LoadAdvLines(const Xml: String; var AAdvLines: TNRA_AdvLines);
    procedure LoadExtendedParams(const Xml: String; var EParams: TNRA_ExtParams);

    procedure SaveXml(const FileName, AStream: String; Ident: Boolean = True);

    function ValidateXml(Xml: String; var ErrStr: String): Boolean;
    function ChangeNraNameSpace(var Xml, ErrStr: String): Boolean;
    function ChangeNameSpace(ANameSpace: String; var Xml, ErrStr: String): Boolean;

    function GenerateNRARequest(ARequest: TNRA_Request; PetrolFormat: Boolean=false; PetrolXML: String=''): String;
    function GenerateFiscalParams(FParams: TNRA_FiscalParams): String;
    function GenerateAdvLines(ARows: TNRA_AdvLines): String;
    function GenerateExtendedParams(EParams: TNRA_ExtParams): string;

//    property NameSpace: string read FNameSpace write FNameSpace;
//    property NRARequest: TNRA_Request read FNRARequest write FNRARequest;
//    property FiscParams: TNRA_FiscalParams read FFiscParams write FFiscParams;
//    property AdvLines: TNRA_AdvLines read FAdvLines write FAdvLines;
  end;

  ENRAException = class(Exception);

implementation
uses XSBuiltIns;

//resourcestring
//  CannotParse      = 'Cannot parse XML document %s'#13#10'%s';
//  NotAnNRAXml      = 'Xml document doesn''t contain an NRAREQ tag.';
//  NotRREGXml       = 'Xml document doesn''t contain an RREQ tag.';
//  NotRRESXml       = 'Xml document doesn''t contain an RRES tag.';
//  NotAnFUXml       = 'Xml document doesn''t contain an FU tag.';
//  MissingPSAddr    = 'Document is missing PSAddr tag.';

const
  { XML prolog }
  XMLDTDFile      = 'movie-watcher.dtd';
  XMLComment      = ' File "%s" generated on %s ';
  XMLPrologAttrs  = 'version="1.0" encoding="windows-1251"';

  XMLNameSpaceReq = 'nra:fdmon:request';
  XMLSchemaReq    = 'nra:fdmon:request nrareq.xsd';
//  XMLSchemaReqPath= '.\xsd\nrareq.xsd';

  XMLNameSpaceRes = 'nra:fdmon:response';
  XMLSchemaRes    = 'nra:fdmon:response nrares.xsd';
  XMLSchemaResPath= '.\xsd\nrares.xsd';

  XMLSchemaLocationTag  = 'xsi:schemaLocation';
  XMLSchemaLocationPath = 'http://www.w3.org/2001/XMLSchema-instance';

  XMLStyleAttrs   = 'type="text/xsl" href="movie-watcher.xsl"';
  XMLStyleTag     = 'xml:stylesheet';
  XMLTag          = 'xml';

  { XML elements/attributes }

  { ROOT tags }
  C_NRAREQTag   = 'NRAREQ';
  C_NRARESTag   = 'NRARES';
  C_RREGTag     = 'RREG';       // Елемент, който се използува за регистрация.
  C_RCHANGETag  = 'RCHANGE';    // При промяна на фискализацията
  C_RDEREGTag   = 'RDEREG';     // Дерегистрация
  C_FDINFOTag   = 'FDINFO';     // Информация за състояние
  C_RREGTag31   = 'RREG31';     // Елемент, който се използува за регистрация на бензиностанция
  C_RCHANGETag31= 'RCHANGE31';  // При промяна на фискализацията на бензиностанция
//  C_RDEREGTag31 = 'RDEREG31';   // НАМЯ ТАКЪВ ВИД ТАГ !!!
//  C_FDINFOTag31 = 'FDINFO31';   // Информация за състояние
  C_RRESTag     = 'RRes';       // Отговор от регистрация
  C_RRESTag31   = 'RRes31';     // Отговор от регистрация
  C_InfoRes     = 'InfoRes';    // Отговор на FDINFO
  C_FUTag       = 'FU';
  C_ROWSTag     = 'ROWS';
  C_EXTDATATag  = 'ExtData';
  C_TerminalTag = 'Terminal';   // Използва се за описване на бензиностанции
  C_DispenserTag= 'Dispenser';
  C_FStationTag = 'FSTATION';

  { RREG tags }
  C_RTypeTag    = 'RType';    // Tип на заявката: 1 - Регистрация на ФУ/ИАСУТД
  C_FDTypeTag   = 'FDType';
  C_EIKTag      = 'EIK';
  C_EIKTypeTag  = 'EIKType';
  C_FDINTag     = 'FDIN';     // Тук ФУ изпращат номер на фискалното устройство. ИАСУТД изпращат номер на система.
  C_FMINTag     = 'FMIN';     // ИАСУТД не изпращат номер на фискална памет, за ФУ е задължително
  C_FDCertTag   = 'FDCert';
  C_IMSITag     = 'IMSI';
  C_MSISDNTag   = 'MSISDN';
  C_OPIDTag     = 'OPID';
  C_OrgNameTag  = 'OrgName';
  C_PSNumTag    = 'PSNum';
  C_PSTypeTag   = 'PSType';
  C_PSAddrTag   = 'PSAddr';
  C_PSNameTag   = 'PSName';
  C_SODTag      = 'SOD';      // Start Operation Date - Дата на встъпване в експлоатация. Задължително при регистрация
  C_ServiceEIKTag     = 'ServiceEIK';     // ЕИК номер на сервиз
  C_ServiceEIKTypeTag = 'ServiceEIKType'; // ЕИК тип номер на сервиз (най-често БУЛСТАТ)
  C_ServiceContractExpirationTag = 'ServiceContractExpiration'; // Дата на изтичане на договора със сервиза

  { RCHANGE tags }
  C_FDRIDTag    = 'FDRID';

  { RDEREG }
  C_RCFDTag     = 'RCFD';

  { PSAddr tags}
  C_SEKATTETag  = 'SEKATTE';
  C_SettlTag    = 'Settl';
  C_AEkatteTag  = 'AEkatte';
  C_AreaTag     = 'Area';
  C_StreetCodeTag = 'StreetCode';
  C_StreetTag     = 'Street';
  C_StrNoTag      = 'StrNo';
  C_BlockTag      = 'Block';
  C_EnTag         = 'En';
  C_FlTag         = 'Fl';
  C_ApTag         = 'Ap';

  { RRES }
  C_StatusTag     = 'Status';
  C_ErrMsgTag     = 'ErrMsg';

  { InfoRes }
  C_RegDateTag    = 'RegDate';
  C_CTTag         = 'CT';
  C_ZRDTag        = 'ZRD';
  C_TIDTag        = 'TID';
  C_SDTag         = 'SD';
  C_DAYSTag       = 'Days';


  { FU }
  C_TAX_ATag      = 'TAX_A';
  C_TAX_BTag      = 'TAX_B';
  C_TAX_CTag      = 'TAX_C';
  C_TAX_DTag      = 'TAX_D';
  C_TAX_ETag      = 'TAX_E';
  C_TAX_FTag      = 'TAX_F';
  C_TAX_GTag      = 'TAX_G';
  C_TAX_HTag      = 'TAX_H';
  C_TAX_Cnt       = 'TAXN';
  C_DP_Tag        = 'DP';     // decimal point 1-Да; 0-Не

  { ROWS }
  C_ROW0Tag       = 'ROW0';
  C_ROW1Tag       = 'ROW1';
  C_ROW2Tag       = 'ROW2';
  C_ROW3Tag       = 'ROW3';
  C_ROW4Tag       = 'ROW4';
  C_ROW5Tag       = 'ROW5';
  C_ROW6Tag       = 'ROW6';
  C_ROW7Tag       = 'ROW7';
  C_ROW8Tag       = 'ROW8';
  C_ROW9Tag       = 'ROW9';

  { Extended Params }
  C_TagOwnerEIK        = 'ExtP0';
  C_TagOwnerTaxEIK     = 'ExtP1';
  C_TagOwnerAddress    = 'ExtP2';
  C_TagSiteShAddress   = 'ExtP3';
  C_TagSvcContractNumb = 'ExtP4';
  C_TagSvcContractFrom = 'ExtP5';
  C_TagSvcContractTo   = 'ExtP6';
  C_TagDeviceComment   = 'ExtP7';

{ TNRA_Xml }

function TNRA_Xml.Attribtue(Node: IXMLDOMNode; const Attribtue: string): string;
var AttrNode: IXMLDOMNode;
begin
  Result   := '';
  AttrNode := Node.Attributes.GetNamedItem(Attribtue);
  if Assigned(AttrNode) then Result:= AttrNode.Text;
end;

function TNRA_Xml.NodeStrValue(Node: IXMLDOMNode): string;
begin
  Result:= '';
  if (Assigned(Node))and((Node.firstChild <> nil)) then Result:= Node.firstChild.nodeValue;
end;

function TNRA_Xml.NodeIntValue(Node: IXMLDOMNode): Integer;
begin
  Result:= -1;
  if (Assigned(Node))and((Node.firstChild <> nil)) then  Result:= StrToInt(Node.firstChild.nodeValue);
end;

function TNRA_Xml.NodeDateTimeValue(Node: IXMLDOMNode): TDateTime;
begin
  Result:= -1;
  if (Assigned(Node))and((Node.firstChild <> nil)) then  Result:= XSDateTimeToDateTime(Node.firstChild.nodeValue);
end;

function TNRA_Xml.XSDateTimeToDateTime(XSDateTime: String): TDateTime;
begin
  with TXSDateTime.Create do
  try
   XSToNative(XSDateTime);
   Result := AsUTCDateTime;
  finally
   Free;
  end;
end;

function TNRA_Xml.DateTimeToXSDateTime(ADateTime: TDateTime): String;
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

procedure TNRA_Xml.AddSimpleElement(XMLDoc: IXMLDOMDocument; Parent: IXMLDOMNode; const Field, Value: String);
var Internal: IXMLDOMElement;
begin
 Internal := IXMLDOMElement(Parent.appendChild(XMLDoc.createNode(NODE_ELEMENT, Field, FNameSpace))); 
 Internal.appendChild(XMLDoc.createTextNode(Value));
end;

procedure TNRA_Xml.AddSimpleElement(XMLDoc: IXMLDOMDocument; Parent: IXMLDOMNode; const Field: string; Value: Integer);
begin
 AddSimpleElement(XMLDoc, Parent, Field, IntToStr(Value));
end;

procedure TNRA_Xml.AddSimpleElement(XMLDoc: IXMLDOMDocument; Parent: IXMLDOMNode; const Field: string; Value: TDateTime);
begin
 AddSimpleElement(XMLDoc, Parent, Field, DateTimeToXSDateTime(Value));
end;

function TNRA_Xml.AddRCHANGE(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request; PetrolFormat: Boolean=false): IXMLDOMNode;
var PSAddrElement: IXMLDOMNode;
begin
  // Create the RCHAGE node
  if PetrolFormat then
   Result := Parent.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RCHANGETag31, XMLNameSpaceReq))
  else
   Result := Parent.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RCHANGETag, XMLNameSpaceReq));

  with ARequest do
   begin
    if (PetrolFormat)and(FDevType = 3) then FDevType := 31;

    // Add RCHAGE elements
    AddSimpleElement(XMLDoc, Result, C_RTypeTag  , Integer(ReqType));
    AddSimpleElement(XMLDoc, Result, C_FDTypeTag , FDevType);
    AddSimpleElement(XMLDoc, Result, C_EIKTag    , CustEIK);
    AddSimpleElement(XMLDoc, Result, C_EIKTypeTag, CustEIKType);
    AddSimpleElement(XMLDoc, Result, C_FDINTag   , FDevSerial);
    AddSimpleElement(XMLDoc, Result, C_FMINTag   , FDevMFM);
    AddSimpleElement(XMLDoc, Result, C_FDRIDTag  , FDRID);

    if FDevCertN <> '' then AddSimpleElement(XmlDoc, Result, C_FDCertTag , FDevCertN);
    if SimIMSI <> '' then   AddSimpleElement(XMLDoc, Result, C_IMSITag   , SimIMSI);
    if SimMSISDN <> '' then AddSimpleElement(XMLDoc, Result, C_MSISDNTag , SimMSISDN);

    AddSimpleElement(XMLDoc, Result, C_OPIDTag   , Integer(SimOperN));
    if OrgName <> '' then AddSimpleElement(XMLDoc, Result, C_OrgNameTag, OrgName);
    if PSetNum <> '' then AddSimpleElement(XMLDoc, Result, C_PSNumTag  , PSetNum);
    AddSimpleElement(XMLDoc, Result, C_PSTypeTag , PSetType);

    // Create PSAddr tag
    PSAddrElement := Result.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_PSAddrTag, XMLNameSpaceReq));

    // Add PSAddr elements
    AddSimpleElement(XMLDoc, PSAddrElement, C_SEKATTETag    , PSetAddr.SettlCode);
    AddSimpleElement(XMLDoc, PSAddrElement, C_SettlTag      , PSetAddr.SettlName);
    if PSetAddr.AreaCode <> '' then AddSimpleElement(XMLDoc, PSAddrElement, C_AEkatteTag,    PSetAddr.AreaCode);
    if PSetAddr.AreaName <> '' then AddSimpleElement(XMLDoc, PSAddrElement, C_AreaTag,       PSetAddr.AreaName);
    if PSetAddr.StrCode <> '' then  AddSimpleElement(XMLDoc, PSAddrElement, C_StreetCodeTag, PSetAddr.StrCode);
    if PSetAddr.StrName <> '' then  AddSimpleElement(XMLDoc, PSAddrElement, C_StreetTag,     PSetAddr.StrName);
    if PSetAddr.StrNo <> '' then    AddSimpleElement(XMLDoc, PSAddrElement, C_StrNoTag,      PSetAddr.StrNo);
    if PSetAddr.Block <> '' then    AddSimpleElement(XMLDoc, PSAddrElement, C_BlockTag,      PSetAddr.Block);
    if PSetAddr.En <> '' then       AddSimpleElement(XMLDoc, PSAddrElement, C_EnTag,         PSetAddr.En);
    if PSetAddr.Fl <> '' then       AddSimpleElement(XMLDoc, PSAddrElement, C_FlTag,         PSetAddr.Fl);
    if PSetAddr.Ap <> '' then       AddSimpleElement(XMLDoc, PSAddrElement, C_ApTag,         PSetAddr.Ap);

    // Add rest of RREG elements
    if PSetName <> '' then AddSimpleElement(XMLDoc, Result, C_PSNameTag, PSetName);

    AddSimpleElement(XMLDoc, Result, C_ServiceEIKTag,     SvceEIK);
    AddSimpleElement(XMLDoc, Result, C_ServiceEIKTypeTag, SvceEIKType);
    if SvceContrExpire <> MinDateTime then AddSimpleElement(XMLDoc, Result, C_ServiceContractExpirationTag, SvceContrExpire);
  end;
end;

function TNRA_Xml.AddRDEREG(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request; PetrolFormat: Boolean=false): IXMLDOMNode;
begin
  // Create the RDEREG node
  Result := Parent.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RDEREGTag, XMLNameSpaceReq));
  with ARequest do
   begin
    if (PetrolFormat)and(FDevType = 3) then FDevType := 31;

    // Add RDEREG elements
    AddSimpleElement(XMLDoc, Result, C_RTypeTag  , Integer(ReqType));
    AddSimpleElement(XMLDoc, Result, C_FDTypeTag , FDevType);
    AddSimpleElement(XMLDoc, Result, C_EIKTag    , CustEIK);
    AddSimpleElement(XMLDoc, Result, C_EIKTypeTag, CustEIKType);
    AddSimpleElement(XMLDoc, Result, C_FDINTag   , FDevSerial);
    AddSimpleElement(XMLDoc, Result, C_FMINTag   , FDevMFM);
    AddSimpleElement(XMLDoc, Result, C_FDRIDTag  , FDRID);
    AddSimpleElement(XmlDoc, Result, C_RCFDTag   , DRegReason);
    AddSimpleElement(XMLDoc, Result, C_IMSITag   , SimIMSI);

    if SvceEIK <> '' then AddSimpleElement(XMLDoc, Result, C_ServiceEIKTag, SvceEIK);
    if SvceEIK <> '' then AddSimpleElement(XMLDoc, Result, C_ServiceEIKTypeTag, SvceEIKType);
    if SvceContrExpire > MinDateTime then AddSimpleElement(XMLDoc, Result, C_ServiceContractExpirationTag, SvceContrExpire);
   end;
end;

function TNRA_Xml.AddRREG(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request; PetrolFormat: Boolean=false): IXMLDOMNode;
var PSAddrElement: IXMLDOMNode;
begin
  // Create the RREG node
  if PetrolFormat then
   Result := Parent.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RREGTag31, XMLNameSpaceReq))
  else
   Result := Parent.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RREGTag, XMLNameSpaceReq));

  with ARequest do
   begin
    if (PetrolFormat)and(FDevType = 3) then FDevType := 31;

    // Add RREG elements
    AddSimpleElement(XMLDoc, Result, C_RTypeTag  , Integer(ReqType));
    AddSimpleElement(XMLDoc, Result, C_FDTypeTag , FDevType);
    AddSimpleElement(XMLDoc, Result, C_EIKTag    , CustEIK);
    AddSimpleElement(XMLDoc, Result, C_EIKTypeTag, CustEIKType);
    AddSimpleElement(XMLDoc, Result, C_FDINTag   , FDevSerial);
    AddSimpleElement(XMLDoc, Result, C_FMINTag   , FDevMFM);
    AddSimpleElement(XMLDoc, Result, C_FDCertTag , FDevCertN);
    AddSimpleElement(XMLDoc, Result, C_IMSITag   , SimIMSI);
    AddSimpleElement(XMLDoc, Result, C_MSISDNTag , SimMSISDN);
    AddSimpleElement(XMLDoc, Result, C_OPIDTag   , SimOperN);
    AddSimpleElement(XMLDoc, Result, C_OrgNameTag, OrgName);
    AddSimpleElement(XMLDoc, Result, C_PSNumTag  , PSetNum);
    AddSimpleElement(XMLDoc, Result, C_PSTypeTag , PSetType);

    // Create PSAddr tag
    PSAddrElement:= Result.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_PSAddrTag, XMLNameSpaceReq));

    // Add PSAddr elements
    AddSimpleElement(XMLDoc, PSAddrElement, C_SEKATTETag, PSetAddr.SettlCode);
    AddSimpleElement(XMLDoc, PSAddrElement, C_SettlTag,   PSetAddr.SettlName);
    if PSetAddr.AreaCode <> '' then AddSimpleElement(XMLDoc, PSAddrElement, C_AEkatteTag,    PSetAddr.AreaCode);
    if PSetAddr.AreaName <> '' then AddSimpleElement(XMLDoc, PSAddrElement, C_AreaTag,       PSetAddr.AreaName);
    if PSetAddr.StrCode <> '' then  AddSimpleElement(XMLDoc, PSAddrElement, C_StreetCodeTag, PSetAddr.StrCode);
    if PSetAddr.StrName <> '' then  AddSimpleElement(XMLDoc, PSAddrElement, C_StreetTag,     PSetAddr.StrName);
    if PSetAddr.StrNo <> '' then    AddSimpleElement(XMLDoc, PSAddrElement, C_StrNoTag,      PSetAddr.StrNo);
    if PSetAddr.Block <> '' then    AddSimpleElement(XMLDoc, PSAddrElement, C_BlockTag,      PSetAddr.Block);
    if PSetAddr.En <> '' then       AddSimpleElement(XMLDoc, PSAddrElement, C_EnTag,         PSetAddr.En);
    if PSetAddr.Fl <> '' then       AddSimpleElement(XMLDoc, PSAddrElement, C_FlTag,         PSetAddr.Fl);
    if PSetAddr.Ap <> '' then       AddSimpleElement(XMLDoc, PSAddrElement, C_ApTag,         PSetAddr.Ap);

    // Add rest of RREG elements
    AddSimpleElement(XMLDoc, Result, C_PSNameTag  , PSetName);
    AddSimpleElement(XMLDoc, Result, C_SODTag     , StartDate);

    if SvceEIK <> '' then AddSimpleElement(XMLDoc, Result, C_ServiceEIKTag,     SvceEIK);
    if SvceEIK <> '' then AddSimpleElement(XMLDoc, Result, C_ServiceEIKTypeTag, SvceEIKType);
    if SvceContrExpire <> MinDateTime then AddSimpleElement(XMLDoc, Result, C_ServiceContractExpirationTag, SvceContrExpire);
   end;
end;

function TNRA_Xml.AddFDInfo(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; ARequest: TNRA_Request): IXMLDOMNode;
begin
  // Create the FDINFO node
  Result := Parent.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_FDINFOTag, XMLNameSpaceReq));
  with ARequest do
   begin
    AddSimpleElement(XmlDoc, Result, C_EIKTag,     CustEIK);
    AddSimpleElement(XmlDoc, Result, C_EIKTypeTag, CustEIKType);
    AddSimpleElement(XmlDoc, Result, C_FDINTag,    FDevSerial);
    AddSimpleElement(XmlDoc, Result, C_FMINTag,    FDevMFM);
    AddSimpleElement(XmlDoc, Result, C_IMSITag,    SimIMSI);
    AddSimpleElement(XmlDoc, Result, C_MSISDNTag,  SimMSISDN);
    AddSimpleElement(XmlDoc, Result, C_OPIDTag,    SimOperN);
   end;
end;

function TNRA_Xml.AddPetrolConfigXml(XmlDoc: IXMLDOMDocument; Parent: IXMLDOMNode; PetrolXML: String): IXMLDOMNode;
var PetrolXmlDoc: IXMLDOMDocument;
    iNode       : IXMLNode;
//    PetNodeCnt  : Integer;
begin
  PetrolXmlDoc := CreateXMLDoc;
  try
   try
     // Validate Petrol XML format
     if not PetrolXmlDoc.loadXML(PetrolXML) then
      raise EAbort.Create('Load XML fail:'+PetrolXmlDoc.parseError.reason+
                          ' [Line:'+IntToStr(PetrolXmlDoc.parseError.line)+
                          ' Linepos:'+IntToStr(PetrolXmlDoc.parseError.linepos)+
                          ' Filepos:'+IntToStr(PetrolXmlDoc.parseError.filepos)+']');
     if PetrolXmlDoc.documentElement = nil then raise EAbort.Create('Missing root element of Petrol XML');
     if not PetrolXmlDoc.documentElement.hasChildNodes then raise EAbort.Create('Xml document does not contain any elements.');
     if PetrolXmlDoc.documentElement.namespaceURI <> FNameSpace then raise EAbort.Create('Invalid namespace of petrol XML configuration. Should be: "'+FNameSpace+'"');

     // check regular XML
     if XmlDoc.documentElement = nil then raise Exception.Create('Invalid registration XML document. Root missing!');

      // Change root node name if necessary
     if AnsiSameText(Parent.nodeName, C_RREGTag) then Result := XmlDoc.documentElement.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RREGTag31, XMLNameSpaceReq))
     else
     if AnsiSameText(Parent.nodeName, C_RCHANGETag) then Result := XmlDoc.documentElement.appendChild(XmlDoc.createNode(NODE_ELEMENT, C_RCHANGETag31, XMLNameSpaceReq))
     else
      Result := Parent;

     // move nodes from old root to new one
     if not AnsiSameText(Parent.nodeName, Result.nodeName) then
      begin
       iNode := Parent.firstChild;
       while iNode <> nil do
        begin
         if (iNode.nodeName = C_FDTypeTag)and(iNode.text = '3') then iNode.text := '31';
         Result.appendChild(iNode.cloneNode(true));
         iNode := iNode.nextSibling;
        end;
       Parent.parentNode.removeChild(Parent);
      end;

     // Change Device NRA Type
     iNode := Result.selectSingleNode(C_FDTypeTag);
     if (iNode <> nil)and(iNode.text = '3') then iNode.text := '31';

     // add petrol nodes - Only new/edit registratrion
//     PetNodeCnt := 0;
     iNode := PetrolXmlDoc.documentElement.firstChild;
     while iNode <> nil do
      begin
       if (AnsiSameText(iNode.nodeName, C_TerminalTag))or
          (AnsiSameText(iNode.nodeName, C_DispenserTag)) then
        begin
         Result.appendChild(iNode.cloneNode(true));
//         Inc(PetNodeCnt);
        end;
       iNode := iNode.nextSibling;
      end;

   except
    on E: Exception do raise Exception.Create('ERROR adding petrol configuration to registration file!!! '+sLineBreak+E.Message);
   end;
  finally
    XmlDoc := nil;
  end;
end;

function TNRA_Xml.GenerateNRARequest(ARequest: TNRA_Request; PetrolFormat: Boolean=false; PetrolXML: String=''): String;
var RootElement    : IXMLDOMNode;
    SchemaLocation : IXMLDOMNode;
    XmlDoc         : IXMLDOMDocument;
begin
  FNameSpace := XMLNameSpaceReq;

  if not (ARequest.ReqType in [rtRegister, rtChange, rtUnRegister]) then
    raise ENRAException.Create('Invalid request type ('+IntToStr(Integer(ARequest.ReqType))+')');

  XmlDoc := CreateXMLDoc;
  try
    with XmlDoc do
     begin
      // create shema information
      SchemaLocation := XmlDoc.createNode(NODE_ATTRIBUTE, XMLSchemaLocationTag, XMLSchemaLocationPath);
      SchemaLocation.nodeValue := XMLSchemaReq;

      // set version and encoding
      appendChild(createProcessingInstruction(XMLTag, XMLPrologAttrs));
//      appendChild(createComment(Format(XMLComment, ['rreg.xml', FormatDateTime('DD.MM.YY HH:NN:SS', Now)])));

      // Create base NRAREQ element
      RootElement := appendChild(createNode(NODE_ELEMENT, C_NRAREQTag, XMLNameSpaceReq));
      (RootElement as IXMLDOMElement).setAttributeNode(IXMLDOMAttribute(SchemaLocation));
     end;

    // Add content
    case ARequest.ReqType of
    rtRegister  : RootElement := AddRREG   (XmlDoc, RootElement, ARequest, PetrolFormat);
    rtChange    : RootElement := AddRCHANGE(XmlDoc, RootElement, ARequest, PetrolFormat);
    rtUnRegister: RootElement := AddRDEREG (XmlDoc, RootElement, ARequest, PetrolFormat);
    rtFDInfo    : RootElement := AddFDInfo (XmlDoc, RootElement, ARequest);
    end;

    // Add additional petrol configuration
    if PetrolXML <> '' then AddPetrolConfigXml(XmlDoc, RootElement, PetrolXML);

    Result:= XmlDoc.xml;

//    XmlDoc.save('test.xml');
  finally
   XmlDoc := nil;
  end;
end;

function TNRA_Xml.GenerateFiscalParams(FParams: TNRA_FiscalParams): string;
var XmlDoc   : IXMLDOMDocument;
    FUElement: IXMLDOMElement;
begin
  Result     := '';
  FNameSpace := '';

  XmlDoc:= CreateXMLDoc;
  try
   with XmlDoc do
    begin
      // set version and encoding
      appendChild(createProcessingInstruction(XMLTag, XMLPrologAttrs));
//      appendChild(createComment(Format(XMLComment, ['fiscal.xml', FormatDateTime('DD.MM.YYYY HH:NN:SS', Now)])));

      // create base element
      FUElement := IXMLDOMElement(appendChild(createElement(C_FUTag)));

      // add data
      AddSimpleElement(XMLDoc, FUElement, C_EIKTag,     FParams.CustEIK);
      AddSimpleElement(XmlDoc, FUElement, C_OrgNameTag, FParams.OrgName);
      AddSimpleElement(XmlDoc, FUElement, C_IMSITag,    FParams.SimIMSI);
      AddSimpleElement(XmlDoc, FUElement, C_TAX_ATag,   FormatFloat('0000', FParams.TaxValue[1]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_BTag,   FormatFloat('0000', FParams.TaxValue[2]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_CTag,   FormatFloat('0000', FParams.TaxValue[3]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_DTag,   FormatFloat('0000', FParams.TaxValue[4]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_ETag,   FormatFloat('0000', FParams.TaxValue[5]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_FTag,   FormatFloat('0000', FParams.TaxValue[6]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_GTag,   FormatFloat('0000', FParams.TaxValue[7]*100));
      AddSimpleElement(XmlDoc, FUElement, C_TAX_HTag,   FormatFloat('0000', FParams.TaxValue[8]*100));
      if FParams.DecimalPoint then AddSimpleElement(XmlDoc, FUElement, C_DP_Tag, '1')
       else AddSimpleElement(XmlDoc, FUElement, C_DP_Tag, '0');
      AddSimpleElement(XmlDoc, FUElement, C_TAX_Cnt, FParams.TaxCountV);
    end;

   Result:= XmlDoc.xml;
  finally
   XmlDoc:= nil;
  end;
end;

function TNRA_Xml.GenerateAdvLines(ARows: TNRA_AdvLines): String;
var XmlDoc    : IXMLDOMDocument;
   RowsElement: IXMLDOMElement;
begin
  Result     := '';
  FNameSpace := '';

  XmlDoc := CreateXMLDoc;
  try
   with XmlDoc do
    begin
      // set version and encoding
      appendChild(createProcessingInstruction(XMLTag, XMLPrologAttrs));
//      appendChild(createComment(Format(XMLComment, ['rows.xml', FormatDateTime('DD.MM.YYYY HH:NN:SS', Now)])));

      // create base element
      RowsElement:= IXMLDOMElement(appendChild(createElement(C_ROWSTag)));

      // add data
      AddSimpleElement(XMLDoc, RowsElement, C_ROW0Tag, ARows[1]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW1Tag, ARows[2]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW2Tag, ARows[3]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW3Tag, ARows[4]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW4Tag, ARows[5]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW5Tag, ARows[6]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW6Tag, ARows[7]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW7Tag, ARows[8]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW8Tag, ARows[9]);
      AddSimpleElement(XMLDoc, RowsElement, C_ROW9Tag, ARows[10]);
    end;

   Result:= XmlDoc.xml;
  finally
   XmlDoc:= nil;
  end;
end;

function TNRA_Xml.FRemoveLFCR(Source_: String): String;
var JJ: Integer;
begin
 Result := '';
 for JJ := 1 to Length(Source_) do
  case Source_[JJ] of
  #0..#12,#14..#19: Result := Result + '';
  #13: Result := Result + '|';
  '|': Result := Result + ' ';
  else Result := Result + Source_[JJ];
  end;
end;

function TNRA_Xml.FAddLFCR(Source_: String): String;
var JJ: Integer;
begin
 Result := '';
 for JJ := 1 to Length(Source_) do
  if Source_[JJ] = '|' then Result := Result + sLineBreak
   else Result := Result + Source_[JJ];
end;

function TNRA_Xml.GenerateExtendedParams(EParams: TNRA_ExtParams): string;
var XmlDoc  : IXMLDOMDocument;
    RowsElm : IXMLDOMElement;
begin
  Result     := '';
  FNameSpace := '';

  XmlDoc:= CreateXMLDoc;
  try
   with XmlDoc do
    begin
      appendChild(createProcessingInstruction(XMLTag, XMLPrologAttrs));
      RowsElm := IXMLDOMElement(appendChild(createElement(C_EXTDATATag)));

      AddSimpleElement(XMLDoc, RowsElm, C_TagOwnerEIK,        EParams.OwnerEIK);
      AddSimpleElement(XMLDoc, RowsElm, C_TagOwnerTaxEIK,     EParams.OwnerTaxEIK);
      AddSimpleElement(XMLDoc, RowsElm, C_TagOwnerAddress,    EParams.OwnerAddress);
      AddSimpleElement(XMLDoc, RowsElm, C_TagSiteShAddress,   EParams.SiteShAddress);
      AddSimpleElement(XMLDoc, RowsElm, C_TagSvcContractNumb, EParams.SvcContractNumb);
      AddSimpleElement(XMLDoc, RowsElm, C_TagSvcContractFrom, EParams.SvcContractFrom);
      AddSimpleElement(XMLDoc, RowsElm, C_TagSvcContractTo,   EParams.SvcContractTo);
      AddSimpleElement(XMLDoc, RowsElm, C_TagDeviceComment,   FRemoveLFCR(EParams.DeviceComment));
    end;

   Result:= XmlDoc.xml;
  finally
   XmlDoc:= nil;
  end;
end;

procedure TNRA_Xml.LoadRequest(const Xml: string; var ARequest: TNRA_Request; var PetrolXML: String);
var XmlDoc: IXMLDOMDocument2;
//    Xsd   : IXMLDOMSchemaCollection2;
begin
  // Create the XML parser
//  Xsd   := CoXMLSchemaCache40.Create;
  XmlDoc := CreateXMLDoc;
  try
    try
//      Xsd.add(XMLNameSpaceReq, XMLSchemaReqPath);

//      XmlDoc.schemas := Xsd;
      XmlDoc.async   := False;
      XmlDoc.validateOnParse := True;
      XmlDoc.resolveExternals:= True;

      if not XmlDoc.loadXML(Xml) then
       raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                              ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                              ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                              ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

      ParseRequest(XmlDoc, ARequest, PetrolXML);
    except
     on E: Exception do
      raise ENRAException.Create('[LoadRequest] Fail: '+sLineBreak+E.Message);
    end;
  finally
//   Xsd    := nil;
   XmlDoc := nil;
  end;
end;

procedure TNRA_Xml.LoadRequest(const Xml: string; var ARequest: TNRA_Request);
var XmlDoc   : IXMLDOMDocument2;
    PetrolXML: String;
//    Xsd   : IXMLDOMSchemaCollection2;
begin
  // Create the XML parser
//  Xsd   := CoXMLSchemaCache40.Create;
  XmlDoc := CreateXMLDoc;
  try
    try
//      Xsd.add(XMLNameSpaceReq, XMLSchemaReqPath);

//      XmlDoc.schemas := Xsd;
      XmlDoc.async   := False;
      XmlDoc.validateOnParse := True;
      XmlDoc.resolveExternals:= True;

      if not XmlDoc.loadXML(Xml) then
       raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                              ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                              ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                              ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

      ParseRequest(XmlDoc, ARequest, PetrolXML);
    except
     on E: Exception do
      raise ENRAException.Create('[LoadRequest] Fail: '+sLineBreak+E.Message);
    end;
  finally
//   Xsd    := nil;
   XmlDoc := nil;
  end;
end;

{procedure TNRA_Xml.LoadRequestFromFile(const FileName: string;  var ARequest: TNRA_Request; var PetrolXML: String);
var XmlDoc: IXMLDOMDocument2;
//    Xsd   : IXMLDOMSchemaCollection2;
begin
  // Create the XML parser
//  Xsd   := CoXMLSchemaCache40.Create;
  XmlDoc:= CoDOMDocument.Create;
  try
    try
//      Xsd.add(XMLNameSpaceReq, XMLSchemaReqPath);

//      XmlDoc.schemas := Xsd;
      XmlDoc.async   := False;
      XmlDoc.validateOnParse  := True;
      XmlDoc.resolveExternals := True;

      if not XmlDoc.load(FileName) then
       raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                              ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                              ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                              ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

      ParseRequest(XmlDoc, ARequest, PetrolXML);
    except
     on E: Exception do
      raise ENRAException.Create('[LoadRequestFromFile] Fail: '+sLineBreak+
                                  'File:'+FileName+sLineBreak+
                                  E.Message);
    end;
  finally
//    Xsd    := nil;
    XmlDoc := nil;
  end;
end;}

procedure TNRA_Xml.LoadFDINFO(const Xml: String; var AFDINFO: TNRA_FDINFO; var PetrolXML: String);
var XmlDoc: IXMLDOMDocument2;
//    Xsd   : IXMLDOMSchemaCollection2;
begin
  // Create the XML parser
//  Xsd   := CoXMLSchemaCache40.Create;
  XmlDoc := CreateXMLDoc;
  try
    try
//      Xsd.add(XMLNameSpaceReq, XMLSchemaReqPath);
//      XmlDoc.schemas := Xsd;
      XmlDoc.async   := False;
      XmlDoc.validateOnParse := True;
      XmlDoc.resolveExternals:= True;

      if not XmlDoc.loadXML(Xml) then
       raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                              ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                              ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                              ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

      ParseFDINFO(XmlDoc, AFDINFO, PetrolXML);
    except
     on E: Exception do
      raise ENRAException.Create('[LoadFDINFO] Fail: '+sLineBreak+E.Message);
    end;
  finally
//   Xsd    := nil;
   XmlDoc := nil;
  end;
end;

procedure TNRA_Xml.LoadResponse(const Xml: string; var AResponse: TNRA_Response);
var XmlDoc: IXMLDOMDocument2;
//    Xsd   : IXMLDOMSchemaCollection2;
begin
  // Create the XML parser
//  Xsd   := CoXMLSchemaCache40.Create;
  XmlDoc:= CoDOMDocument.Create;
  try
    try
//      Xsd.add(XMLNameSpaceReq, XMLSchemaReqPath);

//      XmlDoc.schemas:= Xsd;
      XmlDoc.async  := False;
      XmlDoc.validateOnParse  := True;
      XmlDoc.resolveExternals := True;

      if not XmlDoc.loadXML(Xml) then
       raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                              ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                              ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                              ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

      ParseResponse(XmlDoc, AResponse);
    except
     on E: Exception do
      raise ENRAException.Create('[LoadResponse] Load XML Fail: '+sLineBreak+
                                  E.Message);
    end;
  finally
//   Xsd    := nil;
   XmlDoc := nil;
  end;
end;

procedure TNRA_Xml.LoadFiscalParams(const Xml: string; var AFParams: TNRA_FiscalParams);
var XmlDoc: IXMLDOMDocument2;
begin
  // Create the XML parser
  XmlDoc:= CoDOMDocument.Create;
  try
    try
      if not XmlDoc.loadXML(Xml) then
       raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                              ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                              ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                              ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

      ParseFParams(XmlDoc, AFParams);
    except
     on E: Exception do
      raise ENRAException.Create('[LoadFiscalParams] Fail: '+sLineBreak+
                                  E.Message);
    end;
  finally
    XmlDoc:= nil;
  end;
end;

procedure TNRA_Xml.LoadAdvLines(const Xml: string; var AAdvLines: TNRA_AdvLines);
var XmlDoc: IXMLDOMDocument;
begin
  // Create the XML Parcer
  XmlDoc:= CreateXMLDoc;
  try
   try
     if not XmlDoc.loadXML(Xml) then
      raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                             ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                             ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                             ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

     ParseAdvLines(XmlDoc, AAdvLines);
   except
    on E: Exception do
      raise ENRAException.Create('[LoadAdvLines] Fail: '+sLineBreak+
                                  E.Message);
   end;
  finally
    XmlDoc:= nil;
  end;
end;

procedure TNRA_Xml.LoadExtendedParams(const Xml: String; var EParams: TNRA_ExtParams);
var XmlDoc: IXMLDOMDocument;
begin
  // Create the XML Parcer
  XmlDoc:= CreateXMLDoc;
  try
   try
     if not XmlDoc.loadXML(Xml) then
      raise Exception.Create('[loadXML]:'+XmlDoc.parseError.reason+
                             ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                             ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                             ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

     ParseExtParams(XmlDoc, EParams);
   except
    on E: Exception do
      raise ENRAException.Create('[LoadExtParams] Fail: '+sLineBreak+
                                  E.Message);
   end;
  finally
    XmlDoc:= nil;
  end;
end;

procedure TNRA_Xml.ParsePSAddr(PSAddrNode: IXMLDOMElement; var ARequest: TNRA_Request);
var Node: Integer;
begin
  if not PSAddrNode.hasChildNodes then raise ENRAException.Create('Document is missing "PSAddr" tag.');

  with PSAddrNode.childNodes, ARequest do
    for Node:= 0 to length - 1 do
     begin
      if item[Node].nodeName = C_SEKATTETag then    PSetAddr.SettlCode := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_SettlTag then      PSetAddr.SettlName := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_AEKatteTag then    PSetAddr.AreaCode  := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_AreaTag then       PSetAddr.AreaName  := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_StreetCodeTag then PSetAddr.StrCode   := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_StreetTag then     PSetAddr.StrName   := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_StrNoTag then      PSetAddr.StrNo     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_BlockTag then      PSetAddr.Block     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_EnTag then         PSetAddr.En        := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_FlTag then         PSetAddr.Fl        := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_ApTag then         PSetAddr.Ap        := NodeStrValue(item[Node]);
     end;
end;

procedure TNRA_Xml.ParseZTask(PSZRDNode: IXMLDOMElement; var ZTask: TNRA_ZTask);
var Node: Integer;
begin
  if not PSZRDNode.hasChildNodes then raise ENRAException.Create('Document is missing "ZRD" tag.');

  with PSZRDNode.childNodes, ZTask do
    for Node:= 0 to length - 1 do
     begin
      if item[Node].nodeName = C_TIDTag then    ZTask.ZTaskTID   := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_SDTag then      ZTask.ZTaksTime := NodeDateTimeValue(item[Node])
      else
      if item[Node].nodeName = C_DAYSTag then    ZTask.ZTaskDays := NodeIntValue(item[Node])
     end;
end;

procedure TNRA_Xml.ParseRequest(XmlDoc: IXMLDOMDocument; var ARequest: TNRA_Request; var PetrolXML: String);
var Node      : Integer;
    PetrolDoc : IXMLDocument;
begin
  PetrolXML := '';
  PetrolDoc := nil;

  // Check root element
  if XmlDoc.documentElement.nodeName <> C_NRAREQTag then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_NRAREQTag+'" tag.');

  if (not XmlDoc.documentElement.hasChildNodes) then
   raise ENRAException.Create('Xml document doesn''t contain any tags.');

  case ARequest.ReqType of
    rtRegister:
      begin
        // Check operation element
        if (XmlDoc.documentElement.childNodes.item[0].nodeName <> C_RREGTag)and
           (XmlDoc.documentElement.childNodes.item[0].nodeName <> C_RREGTag31) then
         raise ENRAException.Create('Xml document doesn''t contain an "'+C_RREGTag+'" tag.');
      end;
    rtChange:
      begin
        // Check operation element
        if (XmlDoc.documentElement.childNodes.item[0].nodeName <> C_RCHANGETag)and
           (XmlDoc.documentElement.childNodes.item[0].nodeName <> C_RCHANGETag31) then
         raise ENRAException.Create('Xml document doesn''t contain an "'+C_RCHANGETag+'" tag.');
      end;
    rtUnRegister:
      begin
        // Check operation element
        if (XmlDoc.documentElement.childNodes.item[0].nodeName <> C_RDEREGTag) then
         raise ENRAException.Create('Xml document doesn''t contain an "'+C_RDEREGTag+'" tag.');
      end;
    else
      raise ENRAException.Create('Unknown child[0] element ('+IntToStr(Integer(ARequest.ReqType))+')');
  end;

  with XmlDoc.documentElement.childNodes.item[0].childNodes, ARequest do
    for Node:= 0 to Length - 1 do
     begin
      { RREG }
      if Item[Node].nodeName = C_RTypeTag then    ReqType     := TNRA_ReqType(NodeIntValue(item[Node]))
      else
      if item[Node].nodeName = C_FDTypeTag then   FDevType    := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_EIKTag then      CustEIK     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_EIKTypeTag then  CustEIKType := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_FDINTag then     FDevSerial  := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_FMINTag then     FDevMFM     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_FDCertTag then   FDevCertN   := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_IMSITag then     SimIMSI     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_MSISDNTag then   SimMSISDN   := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_OPIDTag then     SimOperN    := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_OrgNameTag then  OrgName     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_PSNumTag then    PSetNum     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_PSTypeTag then   PSetType    := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_PSNameTag then   PSetName    := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_SODTag then      StartDate   := NodeDateTimeValue((item[Node]))
      else
      if item[Node].nodeName = C_ServiceEIKTag then     SvceEIK    := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_ServiceEIKTypeTag then SvceEIKType:= NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_ServiceContractExpirationTag then SvceContrExpire := NodeDateTimeValue((item[Node]))
      else       { RCHANGE }
      if item[Node].nodeName = C_FDRIDTag then    FDRID       := NodeStrValue(item[Node])
      else      { RDEREG }
      if item[Node].nodeName = C_RCFDTag then     DRegReason  := NodeIntValue(item[Node])
      else      { PSAddr }
      if item[Node].nodeName = C_PSAddrTag then   ParsePSAddr(IXMLDOMElement(item[Node]), ARequest)
      else
      if (item[Node].nodeName = C_TerminalTag)or(item[Node].nodeName = C_DispenserTag) then
       begin
        if PetrolDoc = nil then
         begin
          PetrolDoc := CreateXMLDoc;
          PetrolDoc.appendChild(PetrolDoc.createProcessingInstruction(XMLTag, XMLPrologAttrs));
          PetrolDoc.appendChild(PetrolDoc.createNode(NODE_ELEMENT, C_FStationTag, XMLNameSpaceReq));
         end;
        PetrolDoc.documentElement.appendChild(item[Node].cloneNode(true));
       end
      else
      if (item[Node].nodeType = NODE_ENTITY_REFERENCE)and
         (item[Node].nodeName = C_PSAddrTag) then ParsePSAddr(IXMLDOMElement(item[Node]), ARequest);
     end;
  if PetrolDoc <> nil then
   begin
    PetrolXML := PetrolDoc.xml;
    PetrolDoc := nil;
   end;
end;

procedure TNRA_Xml.ParseFDINFO(XmlDoc: IXMLDOMDocument; var AFDINFO: TNRA_FDINFO; var PetrolXML: String);
var Node      : Integer;
    PetrolDoc : IXMLDocument;
begin
  PetrolXML := '';
  PetrolDoc := nil;

  // Check root element
  if XmlDoc.documentElement.nodeName <> C_NRARESTag then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_NRARESTag+'" tag.');

  if (not XmlDoc.documentElement.hasChildNodes) then
   raise ENRAException.Create('Xml document doesn''t contain any tags.');

  // Check operation element
  if (XmlDoc.documentElement.childNodes.item[0].nodeName <> C_InfoRes) then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_InfoRes+'" tag.');

  with XmlDoc.documentElement.childNodes.item[0].childNodes, AFDINFO do
    for Node:= 0 to Length - 1 do
     begin
      if item[Node].nodeName = C_FDTypeTag then   FDevType    := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_EIKTag then      CustEIK     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_EIKTypeTag then  CustEIKType := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_FDINTag then     FDevSerial  := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_FMINTag then     FDevMFM     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_IMSITag then     SimIMSI     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_MSISDNTag then   SimMSISDN   := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_OPIDTag then     SimOperN    := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_RegDateTag then  RegDate     := NodeDateTimeValue((item[Node]))
      else
      if item[Node].nodeName = C_CTTag then       NRATime     := NodeDateTimeValue(item[Node])
      else
      if item[Node].nodeName = C_FDRIDTag then    FDRID       := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_ZRDTag then      ParseZTask(IXMLDOMElement(item[Node]), ZTask)
      else
      if (item[Node].nodeName = C_TerminalTag)or(item[Node].nodeName = C_DispenserTag) then
       begin
        if PetrolDoc = nil then
         begin
          PetrolDoc := CreateXMLDoc;
          PetrolDoc.appendChild(PetrolDoc.createProcessingInstruction(XMLTag, XMLPrologAttrs));
          PetrolDoc.appendChild(PetrolDoc.createNode(NODE_ELEMENT, C_FStationTag, XMLNameSpaceReq));
         end;
        PetrolDoc.documentElement.appendChild(item[Node].cloneNode(true));
       end;
     end;  
  if PetrolDoc <> nil then
   begin
    PetrolXML := PetrolDoc.xml;
    PetrolDoc := nil;
   end;
end;

procedure TNRA_Xml.ParseResponse(XmlDoc: IXMLDOMDocument; var AResponse: TNRA_Response);
var Node: Integer;
begin
  // Check root element <NRARES>
  if XmlDoc.documentElement.nodeName <> C_NRARESTag then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_NRARESTag+'" tag.');

  // Check operation element <RRes>
  if (not XmlDoc.documentElement.hasChildNodes) then
   raise ENRAException.Create('Xml document doesn''t contain any tags.');

  if (XmlDoc.documentElement.firstChild.nodeName <> C_RRESTag)and
     (XmlDoc.documentElement.firstChild.nodeName <> C_RRESTag31) then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_RRESTag+'" tag.');

  with XmlDoc.documentElement.childNodes.item[0].childNodes, AResponse do
   begin
    for Node:= 0 to Length - 1 do
     begin
      if item[Node].nodeName = C_RTypeTag then  ReqType := TNRA_ReqType(NodeIntValue(item[Node]))
      else
      if item[Node].nodeName = C_StatusTag then Status  := NodeIntValue(item[Node])
      else
      if item[Node].nodeName = C_ErrMsgTag then ErrMsg  := NodeStrValue(item[Node]);
     end;
   end;
end;

procedure TNRA_Xml.ParseFParams(XmlDoc: IXMLDOMDocument; var AFParams: TNRA_FiscalParams);
var Node: Integer;
begin
  DecimalSeparator := '.';
  // Check root element <NRARES>
  if (XmlDoc.documentElement.nodeName <> C_FUTag) or
     (not XmlDoc.documentElement.hasChildNodes) then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_FUTag+'" tag.');

  with XmlDoc.documentElement.childNodes, AFParams do
   begin
    for Node:= 0 to Length - 1 do
      if item[Node].nodeName = C_EIKTag then      CustEIK    := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_OrgNameTag then  OrgName    := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_IMSITag then     SimIMSI    := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_TAX_ATag then    TaxValue[1]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_BTag then    TaxValue[2]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_CTag then    TaxValue[3]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_DTag then    TaxValue[4]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_ETag then    TaxValue[5]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_FTag then    TaxValue[6]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_GTag then    TaxValue[7]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_TAX_HTag then    TaxValue[8]:= StrToFloatDef(NodeStrValue(item[Node]), 0) / 100
      else
      if item[Node].nodeName = C_DP_Tag then      DecimalPoint:= NodeStrValue(item[Node]) <> '0'
      else
      if item[Node].nodeName = C_TAX_Cnt then     TaxCountV  := StrToIntDef(NodeStrValue(item[Node]), 0);
   end;
end;

procedure TNRA_Xml.ParseAdvLines(XmlDoc: IXMLDOMDocument; var AAdvLines: TNRA_AdvLines);
var Node: Integer;
begin
  // Check root element <NRARES>
  if (XmlDoc.documentElement.nodeName <> C_RowsTag) or
      (not XmlDoc.documentElement.hasChildNodes) then
    raise ENRAException.Create('Xml document doesn''t contain an "'+C_RowsTag+'" tag.');

  with XmlDoc.documentElement.childNodes do
   begin
    for Node:= 0 to Length - 1 do
      if item[Node].nodeName = C_Row0Tag then  AAdvLines[1] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row1Tag then  AAdvLines[2] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row2Tag then  AAdvLines[3] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row3Tag then  AAdvLines[4] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row4Tag then  AAdvLines[5] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row5Tag then  AAdvLines[6] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row6Tag then  AAdvLines[7] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row7Tag then  AAdvLines[8] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row8Tag then  AAdvLines[9] := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_Row9Tag then  AAdvLines[10]:= NodeStrValue(item[Node])
   end;
end;

procedure TNRA_Xml.ParseExtParams(XmlDoc: IXMLDOMDocument; var EParams: TNRA_ExtParams);
var Node: Integer;
begin
  // Check root element <NRARES>
  if (XmlDoc.documentElement.nodeName <> C_EXTDATATag) or
     (not XmlDoc.documentElement.hasChildNodes) then
   raise ENRAException.Create('Xml document doesn''t contain an "'+C_EXTDATATag+'" tag.');

  with XmlDoc.documentElement.childNodes do
   begin
    for Node:= 0 to Length - 1 do
      if item[Node].nodeName = C_TagOwnerEIK then         EParams.OwnerEIK        := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_TagOwnerTaxEIK then      EParams.OwnerTaxEIK     := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_TagOwnerAddress then     EParams.OwnerAddress    := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_TagSiteShAddress then    EParams.SiteShAddress   := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_TagSvcContractNumb then  EParams.SvcContractNumb := NodeStrValue(item[Node])
      else
      if item[Node].nodeName = C_TagSvcContractFrom then  EParams.SvcContractFrom := NodeDateTimeValue((item[Node]))
      else
      if item[Node].nodeName = C_TagSvcContractTo then    EParams.SvcContractTo   := NodeDateTimeValue((item[Node]))
      else
      if item[Node].nodeName = C_TagDeviceComment then    EParams.DeviceComment   := FAddLFCR( NodeStrValue(item[Node]));
   end;
end;

procedure TNRA_Xml.SaveXml(const FileName, AStream: string; Ident: Boolean = True);
var XMLDoc    : IXMLDOMDocument;
    PersStream: IPersistStreamInit;
    rdt       : SAXXMLReader;
    wrt       : MXXMLWriter;
    Stream    : IStream;
    FS        : TFileStream;
begin
  if not DirectoryExists(ExtractFilePath(FileName)) then CreateDir(ExtractFilePath(FileName));

  XmlDoc := CreateXMLDoc;
  FS     := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  Stream := TStreamAdapter.Create(FS);
  try
    XMLDoc.loadXML(AStream);

    if not Ident then
     begin
      XmlDoc.QueryInterface(IPersistStreamInit, PersStream);
      PersStream.Save(Stream, True);
     end
    else
     begin
      wrt:= CoMXXMLWriter.Create;
      rdt:= CoSAXXMLReader.Create;
      try
        wrt.byteOrderMark:= True;
        wrt.omitXMLDeclaration:= False;
        wrt.encoding:= 'windows-1251';
        wrt.indent:= True;
        wrt.standalone:= False;
        wrt.version:= '1.0';

        rdt.contentHandler:= wrt as IVBSAXContentHandler;
        rdt.dtdHandler    := wrt as IVBSAXDTDHandler;
        rdt.errorHandler  := wrt as IVBSAXErrorHandler;
        rdt.putProperty('http://xml.org/sax/properties/lexical-handler', wrt);
        rdt.putProperty('http://xml.org/sax/properties/declaration-handler', wrt);

        wrt.output := Stream;
        rdt.parse(XmlDoc);
        wrt.flush;
      finally
        rdt:= nil;
        wrt:= nil;
      end;
     end;
  finally
    XMLDoc := nil;
//    Stream := nil;
    FS.Free;
  end;
end;

function TNRA_Xml.ValidateXml(Xml: String; var ErrStr: String): Boolean;
var XmlDoc: IXMLDOMDocument;
begin
  ErrStr := '';
  Result := true;
  XmlDoc := CreateXMLDoc;
  try
   try
     if not XmlDoc.loadXML(Xml) then
      raise EAbort.Create('Error loading XML: '+XmlDoc.parseError.reason+
                          ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                          ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                          ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');

     if XmlDoc.documentElement = nil then raise EAbort.Create('Missing XML root element. File is empty.');
     if not XmlDoc.documentElement.hasChildNodes then raise EAbort.Create('Xml document does not contain any elements.');
   except
    on E: Exception do
     begin
      Result := false;
      ErrStr := 'Invalid XML format. '+sLineBreak+E.Message;
     end;
   end;
  finally
    XmlDoc := nil;
  end;
end;

function TNRA_Xml.ChangeNraNameSpace(var Xml, ErrStr: String): Boolean;
begin
 Result := ChangeNameSpace(XMLNameSpaceReq, Xml, ErrStr);
end;

function TNRA_Xml.ChangeNameSpace(ANameSpace: String; var Xml, ErrStr: String): Boolean;
var XmlDoc: IXMLDOMDocument;
    S     : String;
    I     : Integer;

    procedure LoadXML_;
    begin
     if not XmlDoc.loadXML(Xml) then
      raise EAbort.Create('Error loading XML: '+XmlDoc.parseError.reason+
                          ' [Line:'+IntToStr(XmlDoc.parseError.line)+
                          ' Linepos:'+IntToStr(XmlDoc.parseError.linepos)+
                          ' Filepos:'+IntToStr(XmlDoc.parseError.filepos)+']');
    end;
begin
  ErrStr := '';
  Result := true;
  XmlDoc := CreateXMLDoc;
  try
   try
     LoadXML_;
     if XmlDoc.documentElement = nil then raise EAbort.Create('Missing XML root element. File is empty.');
     if not XmlDoc.documentElement.hasChildNodes then raise EAbort.Create('Xml document does not contain any elements.');

     // check Petrol XML file namespace
     if XmlDoc.documentElement.namespaceURI <> ANameSpace then
      begin
       if XmlDoc.documentElement.namespaceURI <> '' then
        begin
         S := 'xmlns="'+XmlDoc.documentElement.namespaceURI+'"';
         I := Pos(S, Xml);
         if I > 0 then Delete(Xml, I, Length(S));
         LoadXML_;
        end;
       if XmlDoc.documentElement.namespaceURI <> '' then raise EAbort.Create('Fail removing existing namespace from XML.');

       XmlDoc.documentElement.setAttribute('xmlns', ANameSpace);
       Xml := XmlDoc.xml;
       LoadXML_;

       if XmlDoc.documentElement.namespaceURI <> ANameSpace then raise EAbort.Create('Fail adding "'+ANameSpace+'" namespace to XML.');
       Xml := XmlDoc.xml;
      end;
   except
    on E: Exception do
     begin
      Result := false;
      ErrStr := 'Invalid XML namespace. '+sLineBreak+E.Message;
     end;
   end;
  finally
    XmlDoc := nil;
  end;
end;


initialization
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
