unit FiscalizationTypes;

interface

uses SysUtils, Classes, XMLHandler, CryptoHandler;


const
 C_StreetCodeUnknown = '999999';        //  с 5 девятки НАП връща:   ERROR 16 Код на грешката от НАП :99.Статус на операцията Друго Съобщение за грешка от НАП - "Невалиден код на улица (StreetCode)." 

type
//******************************************************************************
//           Fiscalization - Common data types
//******************************************************************************
 // base classes
 TDeviceData = class(TObject)
  Serial   : String;
  Model    : String;
  Version  : String;
  ModelId  : Integer;
 end;

 TCompanyData = class(TObject)
  EIKType : Integer;
  EIK     : String;
  Name    : String;
 end;

 TCodeName = record
  Code : String;
  Pref : String;
  Name : String;
 end;

 TAddressData = record
  Region : TCodeName;
  Town   : TCodeName;
  Area   : TCodeName;
  Street : TCodeName;
  StrNo  : String;
  Block  : String;
  Entr   : String;
  Floor  : String;
  App    : String;
 end;

 TPayTypesMapp  = array [1..8] of Byte;
 TTaxGroupsVal  = array [1..8] of Real;
 TAdvLines      = array [1..10] of String;

 TRegD_ModemData = class(TDeviceData)
  LastVersion  : String;
  FirmwareFile : String;
 end;

 // Registratin data
 TRegD_FDData = class(TDeviceData)
  NRAType  : Integer;
  MFM      : String;
 end;

 TRegD_FDDataEx = class(TObject)
  LineLength    : Byte;
  CertifNumber  : String;
  CertifDate    : String;
  CertifModel   : String;
  NraUrl        : String;
//  ModelId       : Integer;
  PayTypesCount : Byte;
  PayTypesMapp  : TPayTypesMapp;
  TaxGroupsVal  : TTaxGroupsVal;
  TaxGroupsCntV : Byte;
  AdvLines      : TAdvLines;
  DecimalSep    : Boolean;
  Comment       : String;
 end;

 TRegD_SIMData = class(TObject)
  IMSI      : String;
  MSISDN    : String;
  ICC       : String;
  Operat    : Integer;
  PayedTo   : TDateTime;
  UnlockCode: String;
  IsExternal: Boolean;    // ново поле
  Activation: TDateTime;  // ново поле
  StatusOper: Integer;    // ново поле - за вътрешна употреба в сървъра
  Apn2Url   : String;     // ново поле - хост лъм нашия сървър през втория АПН
 end;

 TRegD_OwnerData = class(TCompanyData)
  TaxEik    : String;
  DistrName : String;
  DistrCode : String;
  TownName  : String;    //Ново
  TownCode  : Integer;   //Ново
  Address   : String;    //Ново
  ShortAdr  : String;    //Ново
  MOL       : String;    //Ново
  PhoneG1   : String;    //Ново
  PhoneG2   : String;    //Ново
  PhoneM1   : String;    //Ново
  PhoneM2   : String;    //Ново
  Email     : String;    //Ново
  Contact   : String;    //Ново
  Comment   : String;    //Ново
 end;

 TRegD_DealerData = class(TCompanyData);

 TRegD_SiteData = class(TObject)
  SiteType : TCodeName;
  SiteName : String;
  Number   : String;
  Address  : TAddressData;
  ShortAddr: String;
 end;

 TRegD_ServiceData = class(TObject)
  StartDate    : TDateTime;
  SvcContrNo   : String;
  SvcContrFrom : TDateTime;
  SvcContrTo   : TDateTime;
  SvcContrLen  : Integer;
  SimPeriodId  : Integer;
  SimPeriodLen : Integer;
 end;

 TRegD_DeregData = class(TObject)
  DeregReasonId   : Integer;
  DeregREasonName : String;
  NRARegID        : String;
 end;

 TRegistrationData = class(TObject)
 private
  FLastError : String;
  FCrcHndr   : TCRC32;

  FFiscDev   : TRegD_FDData;
  FFiscDevEx : TRegD_FDDataEx;
  FModem     : TRegD_ModemData;
  FSim       : TRegD_SIMData;
  FOwner     : TRegD_OwnerData;
  FDealer    : TRegD_DealerData;
  FSite      : TRegD_SiteData;
  FServices  : TRegD_ServiceData;
  FDeregData : TRegD_DeregData;

  function FGetInfoAsText: String;
  function FRemoveLFCR(Source_: String): String;
  function FAddLFCR(Source_: String): String;
  function FGetLastError: String;
  function FCalculateCRC(Source: String): String;
 public
  constructor Create;
  destructor Destroy; override;

  procedure Clear;

  function SaveToStrings(Dest_: TStrings; FullData: Boolean=false): Boolean;
  function SaveToFile(Fname_: String): Boolean;
  function SaveToXML(Doc: IXMLDocument; Root: IXMLNode): Boolean;

  function LoadFromStrings(Source_: TStrings): Boolean;
  function LoadFromFile(Fname_: String): Boolean;
  function LoadFromXML(Root: IXMLNode): Boolean; overload;
  function LoadFromXML(Xml: String): Boolean; overload;

  function AddFiscDev   : TRegD_FDData;
  function AddFiscDevEx : TRegD_FDDataEx;
  function AddModem     : TRegD_ModemData;
  function AddSim       : TRegD_SIMData;
  function AddOwner     : TRegD_OwnerData;
  function AddDealer    : TRegD_DealerData;
  function AddSite      : TRegD_SiteData;
  function AddServices  : TRegD_ServiceData;
  function AddDeregData : TRegD_DeregData;

  procedure RemoveFiscDev;
  procedure RemoveFiscDevEx;
  procedure RemoveModem;
  procedure RemoveSim;
  procedure RemoveOwner;
  procedure RemoveDealer;
  procedure RemoveSite;
  procedure RemoveServices;
  procedure RemoveDeregData;

  property LastError: String read FGetLastError;
  property FiscDev   : TRegD_FDData read FFiscDev write FFiscDev;
  property FiscDevEx : TRegD_FDDataEx read FFiscDevEx write FFiscDevEx;
  property Modem     : TRegD_ModemData read FModem write FModem;
  property Sim       : TRegD_SIMData read FSim write FSim;
  property Owner     : TRegD_OwnerData read FOwner write FOwner;
  property Dealer    : TRegD_DealerData read FDealer write FDealer;
  property Site      : TRegD_SiteData read FSite write FSite;
  property Services  : TRegD_ServiceData read FServices write FServices;
  property DeregData : TRegD_DeregData read FDeregData write FDeregData;

  property DeviceInfo: String read FGetInfoAsText;

  function DataAsText: String;
  function TextForCRC: String;
  function DataAsXML(RootName: String): String;
 end;

implementation
uses IniFiles, DateUtils, XMLHandlerMS, BillingConstUnit;
//******************************************************************************
//           TRegistrationData
//******************************************************************************

const
 C_Section_FiscDevice   = 'FiscalDevice';
 C_Name_FDSerial        = 'FDSerial';
 C_Name_FDModel         = 'FDModel';
 C_Name_FDVersion       = 'FDVersion';
 C_Name_FDMFM           = 'FDMFM';
 C_Name_FDNraType       = 'FDNRAType';

 C_Section_FiscDeviceEx = 'FiscalDeviceExtended';
 C_Name_FDLineLen       = 'FDLineLength';
 C_Name_FDCertN         = 'FDCertNo';
 C_Name_FDCertD         = 'FDCertDate';
 C_Name_FDCertM         = 'FDCertModel';
 C_Name_FDPayCount      = 'FDPayTypeCount';
 C_Name_FDPayMap        = 'FDPayTypeMap';
 C_Name_FDPayItm        = 'PayItm';
 C_Name_FDTaxGroups     = 'FDTaxGroups';
 C_Name_FDTagGrpV       = 'FDTaxGrpV';  // новодобавен параметър
 C_Name_FDTaxGrp        = 'TaxItem';
 C_Name_FDAdvLines      = 'FDAdvLines';
 C_Name_FDAdvLine       = 'AdvLine';
 C_Name_FDDecimal       = 'FDDecimal';
 C_Name_FDComment       = 'FDComment';
 C_Name_FDNraUrl        = 'FDNraUrl';

 C_Section_Modem        = 'GPRSModem';
 C_Name_MSerial         = 'MSerial';
 C_Name_MModel          = 'MModel';
 C_Name_MVersion        = 'MVersion';

 C_Section_SIM          = 'SIM';
 C_Name_SIMIMSI         = 'SimIMSI';
 C_Name_SIMMSISDN       = 'SimMSISDN';
 C_Name_SIMICC          = 'SimICC';
 C_Name_SIMOper         = 'SimOperator';
 C_Name_SIMPayedTo      = 'SimPayedTo';
 C_Name_SIMUnlockCode   = 'SimUnlockC';
 C_Name_SIMExternal     = 'SimExternal';
 C_Name_SIMActivation   = 'SimActivation';
 C_Name_SIMApn2Url      = 'SimApn2Url';

 C_Section_Owner        = 'Owner';
 C_Name_OwnerEikT       = 'OwnerEikT';
 C_Name_OwnerEik        = 'OwnerEik';
 C_Name_OwnerName       = 'OwnerName';
 C_Name_OwnerAddr       = 'OwnerAddr';
 C_Name_OwnerAddrSh     = 'OwnerAddrSh';
 C_Name_OwnerVatEik     = 'OwnerVatEik';
 C_Name_OwnerDistrN     = 'OwnerDistrN';
 C_Name_OwnerDistrC     = 'OwnerDistrC';
 C_Name_OwnerTownN      = 'OwnerTownN';
 C_Name_OwnerTownC      = 'OwnerTownC';
 C_Name_OwnerMOL        = 'OwnerMol';
 C_Name_OwnerPhnG1      = 'OwnerPhG1';
 C_Name_OwnerPhnG2      = 'OwnerPhG2';
 C_Name_OwnerPhnM1      = 'OwnerPhM1';
 C_Name_OwnerPhnM2      = 'OwnerPhM2';
 C_Name_OwnerEmail      = 'OwnerEmail';
 C_Name_OwnerContact    = 'OwnerContact';
 C_Name_OwnerComment    = 'OwnerComment';

 C_Section_Dealer       = 'Dealer';
 C_Name_DlrEikT         = 'DlrEikT';
 C_Name_DlrEik          = 'DlrEik';
 C_Name_DlrName         = 'DlrName';

 C_Section_Site         = 'Site';
 C_Name_StTypeCode      = 'StTypeCode';
 C_Name_StTypeName      = 'StTypeName';
 C_Name_StNumber        = 'StNumber';
 C_Name_StName          = 'StName';
 C_Name_StAdrRegionC    = 'StAdrRegionCode';
 C_Name_StAdrRegionP    = 'StAdrRegionPref';
 C_Name_StAdrRegionN    = 'StAdrRegionName';
 C_Name_StAdrTownC      = 'StAdrTownCode';
 C_Name_StAdrTownP      = 'StAdrTownPref';
 C_Name_StAdrTownN      = 'StAdrTownName';
 C_Name_StAdrAreaC      = 'StAdrAreaCode';
 C_Name_StAdrAreaP      = 'StAdrAreaPref';
 C_Name_StAdrAreaN      = 'StAdrAreaName';
 C_Name_StAdrStreetC    = 'StAdrStrCode';
 C_Name_StAdrStreetP    = 'StAdrStrPref';
 C_Name_StAdrStreetN    = 'StAdrStrName';
 C_Name_StAdrStreetNo   = 'StAdrStrNo';
 C_Name_StAdrBlock      = 'StAdrBlock';
 C_Name_StAdrEntr       = 'StAdrEntr';
 C_Name_StAdrFloor      = 'StAdrFloor';
 C_Name_StAdrApp        = 'StAdrApp';
 C_Name_StShortAddr     = 'StAdrShortAddr';

 C_Section_Services     = 'Services';
 C_Name_SvcStartDate    = 'SvcStartDate';
 C_Name_SvcContrNo      = 'SvcContrNo';
 C_Name_SvcContrFrom    = 'SvcContrFrom';
 C_Name_SvcContrTo      = 'SvcContrTo';
 C_Name_SvcContrLen     = 'SvcContrLen';
 C_Name_SvcSimPeriodId  = 'SvcSimPeriodId';
 C_Name_SvcSimPeriodLen = 'SvcSimPeriodLen';

 C_Section_DeregData    = 'DeregData';
 C_Name_DeregCode       = 'DRegCode';
 C_Name_DeregName       = 'DRegName';
 C_Name_NraRegId        = 'FDRID';

 C_Section_FileInfo     = 'FileInfo';
 C_Name_CRC             = 'Signature';



constructor TRegistrationData.Create;
begin
 inherited Create;
 FCrcHndr   := TCRC32.Create;
 FLastError := '';

 FFiscDev   := nil;
 FFiscDevEx := nil;
 FModem     := nil;
 FSim       := nil;
 FOwner     := nil;
 FDealer    := nil;
 FSite      := nil;
 FServices  := nil;
 FDeregData := nil;
end;

destructor TRegistrationData.Destroy;
begin
 Clear;
 FCrcHndr.Free;
 inherited Destroy;
end;

procedure TRegistrationData.Clear;
begin
  RemoveFiscDev;
  RemoveFiscDevEx;
  RemoveModem;
  RemoveSim;
  RemoveOwner;
  RemoveDealer;
  RemoveSite;
  RemoveServices;
  RemoveDeregData;
end;

function TRegistrationData.FCalculateCRC(Source: String): String;
begin
 FCrcHndr.Reset;
 FCrcHndr.Process(Source[1], Length(Source));
 Result := FCrcHndr.AsString;
end;

function TRegistrationData.FGetLastError: String;
begin
 if FLastError <> '' then Result := '['+Self.ClassName+']'+FLastError
  else Result := '';
end;

function TRegistrationData.AddFiscDev: TRegD_FDData;
begin
 if FFiscDev = nil then FFiscDev := TRegD_FDData.Create;
 Result := FFiscDev;
end;

procedure TRegistrationData.RemoveFiscDev;
begin
 if FFiscDev <> nil then FreeAndNil(FFiscDev);
end;

function TRegistrationData.AddFiscDevEx: TRegD_FDDataEx;
begin
 if FFiscDevEx = nil then FFiscDevEx := TRegD_FDDataEx.Create;
 Result := FFiscDevEx;
end;

procedure TRegistrationData.RemoveFiscDevEx;
begin
 if FFiscDevEx <> nil then FreeAndNil(FFiscDevEx);
end;

function TRegistrationData.AddModem: TRegD_ModemData;
begin
 if FModem = nil then FModem := TRegD_ModemData.Create;
 Result := FModem;
end;

procedure TRegistrationData.RemoveModem;
begin
 if FModem <> nil then FreeAndNil(FModem);
end;

function TRegistrationData.AddSim: TRegD_SIMData;
begin
 if FSim = nil then FSim := TRegD_SIMData.Create;
 Result := FSim;
end;

procedure TRegistrationData.RemoveSim;
begin
 if FSim <> nil then FreeAndNil(FSim);
end;

function TRegistrationData.AddOwner: TRegD_OwnerData;
begin
 if FOwner = nil then FOwner := TRegD_OwnerData.Create;
 Result := FOwner;
end;

procedure TRegistrationData.RemoveOwner;
begin
 if FOwner <> nil then FreeAndNil(FOwner);
end;

function TRegistrationData.AddDealer: TRegD_DealerData;
begin
 if FDealer = nil then FDealer := TRegD_DealerData.Create;
 Result := FDealer;
end;

procedure TRegistrationData.RemoveDealer;
begin
 if FDealer <> nil then FreeAndNil(FDealer);
end;

function TRegistrationData.AddSite: TRegD_SiteData;
begin
 if FSite = nil then FSite := TRegD_SiteData.Create;
 Result := FSite;
end;

procedure TRegistrationData.RemoveSite;
begin
 if FSite <> nil then FreeAndNil(FSite);
end;

function TRegistrationData.AddServices: TRegD_ServiceData;
begin
 if FServices = nil then FServices := TRegD_ServiceData.Create;
 Result := FServices;
end;

procedure TRegistrationData.RemoveServices;
begin
 if FServices <> nil then FreeAndNil(FServices);
end;

function TRegistrationData.AddDeregData: TRegD_DeregData;
begin
 if FDeregData = nil then FDeregData := TRegD_DeregData.Create;
 Result := FDeregData;
end;

procedure TRegistrationData.RemoveDeregData;
begin
 if FDeregData <> nil then FreeAndNil(FDeregData);
end;

function TRegistrationData.FRemoveLFCR(Source_: String): String;
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

function TRegistrationData.FAddLFCR(Source_: String): String;
var JJ: Integer;
begin
 Result := '';
 for JJ := 1 to Length(Source_) do
  if Source_[JJ] = '|' then Result := Result + sLineBreak
   else Result := Result + Source_[JJ];
end;

//******************************************************************************
//   HIGH LEVEL
//******************************************************************************
function TRegistrationData.SaveToStrings(Dest_: TStrings; FullData: Boolean=false): Boolean;
var I   : Integer;
    IniF: TMemIniFile;

    procedure WriteStr_(Section_, Name_, Value_: String);
    begin
     IniF.WriteString(Section_, Name_, Trim(Value_));
    end;
begin
 DecimalSeparator := '.';
 Result := true;
 try
  if Dest_ = nil then raise EAbort.Create('Container for operation is no assigned!');

  IniF := TMemIniFile.Create('');
  try
   if (FFiscDev <> nil)and(FullData) then
    begin
     WriteStr_(C_Section_FiscDevice, C_Name_FDSerial,  FFiscDev.Serial);
     WriteStr_(C_Section_FiscDevice, C_Name_FDModel,   FFiscDev.Model);
     WriteStr_(C_Section_FiscDevice, C_Name_FDVersion, FFiscDev.Version);
     WriteStr_(C_Section_FiscDevice, C_Name_FDMFM,     FFiscDev.MFM);
     WriteStr_(C_Section_FiscDevice, C_Name_FDNraType, IntToStr(FFiscDev.NRAType));
    end;
   if FFiscDevEx <> nil then
    begin
     WriteStr_(C_Section_FiscDeviceEx, C_Name_FDLineLen,  IntToStr(FFiscDevEx.LineLength));
     WriteStr_(C_Section_FiscDeviceEx, C_Name_FDPayCount, IntToStr(FFiscDevEx.PayTypesCount));
     WriteStr_(C_Section_FiscDeviceEx, C_Name_FDDecimal,  BoolToStr(FFiscDevEx.DecimalSep, false));
     WriteStr_(C_Section_FiscDeviceEx, C_Name_FDComment,  FRemoveLFCR(FFiscDevEx.Comment));
     WriteStr_(C_Section_FiscDeviceEx, C_Name_FDTagGrpV,  IntToStr(FFiscDevEx.TaxGroupsCntV));
     for I := 1 to 8 do  WriteStr_(C_Section_FiscDeviceEx+C_Name_FDPayMap, C_Name_FDPayItm+IntToStr(I), IntToStr(FFiscDevEx.PayTypesMapp[I]));
     for I := 1 to 8 do  WriteStr_(C_Section_FiscDeviceEx+C_Name_FDTaxGroups, C_Name_FDTaxGrp+IntToStr(I), FloatToStr(FFiscDevEx.TaxGroupsVal[I]));
     for I := 1 to 10 do WriteStr_(C_Section_FiscDeviceEx+C_Name_FDAdvLines, C_Name_FDAdvLine+IntToStr(I), FFiscDevEx.AdvLines[I]);
     if FullData then WriteStr_(C_Section_FiscDeviceEx, C_Name_FDCertN,  FFiscDevEx.CertifNumber);
     if FullData then WriteStr_(C_Section_FiscDeviceEx, C_Name_FDCertD,  FFiscDevEx.CertifDate);
     if FullData then WriteStr_(C_Section_FiscDeviceEx, C_Name_FDCertM,  FFiscDevEx.CertifModel);
     if FullData then WriteStr_(C_Section_FiscDeviceEx, C_Name_FDNraUrl, FFiscDevEx.NraUrl);
    end;
   if (FModem <> nil)and(FullData) then
    begin
     WriteStr_(C_Section_Modem, C_Name_MSerial,  FModem.Serial);
     WriteStr_(C_Section_Modem, C_Name_MModel,   FModem.Model);
     WriteStr_(C_Section_Modem, C_Name_MVersion, FModem.Version);
    end;
   if (FSim <> nil)and(FullData) then
    begin
     WriteStr_(C_Section_SIM, C_Name_SIMIMSI,       FSim.IMSI);
     WriteStr_(C_Section_SIM, C_Name_SIMMSISDN,     FSim.MSISDN);
     WriteStr_(C_Section_SIM, C_Name_SIMICC,        FSim.ICC);
     WriteStr_(C_Section_SIM, C_Name_SIMOper,       IntToStr(FSim.Operat));
     WriteStr_(C_Section_SIM, C_Name_SIMPayedTo,    FloatToStr(FSim.PayedTo));
     WriteStr_(C_Section_SIM, C_Name_SIMUnlockCode, FSim.UnlockCode);
     WriteStr_(C_Section_SIM, C_Name_SIMExternal,   BoolToStr(FSim.IsExternal));
     WriteStr_(C_Section_SIM, C_Name_SIMActivation, FloatToStr(FSim.Activation));
     WriteStr_(C_Section_SIM, C_Name_SIMApn2Url,    FSim.Apn2Url);
    end;
   if FOwner <> nil then
    begin
     WriteStr_(C_Section_Owner, C_Name_OwnerEikT,   IntToStr(FOwner.EIKType));
     WriteStr_(C_Section_Owner, C_Name_OwnerEik,    FOwner.EIK);
     WriteStr_(C_Section_Owner, C_Name_OwnerName,   FOwner.Name);
     WriteStr_(C_Section_Owner, C_Name_OwnerAddr,   FOwner.Address);
     WriteStr_(C_Section_Owner, C_Name_OwnerAddrSh, FOwner.ShortAdr);
     WriteStr_(C_Section_Owner, C_Name_OwnerVatEik, FOwner.TaxEik);
     WriteStr_(C_Section_Owner, C_Name_OwnerDistrN, FOwner.DistrName);
     WriteStr_(C_Section_Owner, C_Name_OwnerTownN,  FOwner.TownName);
     WriteStr_(C_Section_Owner, C_Name_OwnerMOL,    FOwner.MOL);
     WriteStr_(C_Section_Owner, C_Name_OwnerPhnG1,  FOwner.PhoneG1);
     WriteStr_(C_Section_Owner, C_Name_OwnerPhnG2,  FOwner.PhoneG2);
     WriteStr_(C_Section_Owner, C_Name_OwnerPhnM1,  FOwner.PhoneM1);
     WriteStr_(C_Section_Owner, C_Name_OwnerPhnM2,  FOwner.PhoneM2);
     WriteStr_(C_Section_Owner, C_Name_OwnerEmail,  FOwner.Email);
     WriteStr_(C_Section_Owner, C_Name_OwnerContact,FOwner.Contact);
     WriteStr_(C_Section_Owner, C_Name_OwnerComment,FOwner.Comment);
    end;
   if (FDealer <> nil)and(FullData) then
    begin
     WriteStr_(C_Section_Dealer, C_Name_DlrEikT, IntToStr(FDealer.EIKType));
     WriteStr_(C_Section_Dealer, C_Name_DlrEik,  FDealer.EIK);
     WriteStr_(C_Section_Dealer, C_Name_DlrName, FDealer.Name);
    end;
   if FSite <> nil then
    begin
     WriteStr_(C_Section_Site, C_Name_StTypeName,    FSite.SiteType.Name);
     WriteStr_(C_Section_Site, C_Name_StNumber,      FSite.Number);
     WriteStr_(C_Section_Site, C_Name_StName,        FSite.SiteName);
     WriteStr_(C_Section_Site, C_Name_StAdrRegionN,  FSite.Address.Region.Name);
     WriteStr_(C_Section_Site, C_Name_StAdrTownN,    FSite.Address.Town.Name);
     WriteStr_(C_Section_Site, C_Name_StAdrAreaN,    FSite.Address.Area.Name);
     WriteStr_(C_Section_Site, C_Name_StAdrStreetN,  FSite.Address.Street.Name);
     WriteStr_(C_Section_Site, C_Name_StAdrStreetNo, FSite.Address.StrNo);
     WriteStr_(C_Section_Site, C_Name_StAdrBlock,    FSite.Address.Block);
     WriteStr_(C_Section_Site, C_Name_StAdrEntr,     FSite.Address.Entr);
     WriteStr_(C_Section_Site, C_Name_StAdrFloor,    FSite.Address.Floor);
     WriteStr_(C_Section_Site, C_Name_StAdrApp,      FSite.Address.App);
     WriteStr_(C_Section_Site, C_Name_StShortAddr,   FSite.ShortAddr);

     if FullData then WriteStr_(C_Section_Site, C_Name_StTypeCode,    FSite.SiteType.Code);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrRegionC,  FSite.Address.Region.Code);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrTownC,    FSite.Address.Town.Code);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrAreaC,    FSite.Address.Area.Code);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrStreetC,  FSite.Address.Street.Code);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrRegionP,  FSite.Address.Region.Pref);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrTownP,    FSite.Address.Town.Pref);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrAreaP,    FSite.Address.Area.Pref);
     if FullData then WriteStr_(C_Section_Site, C_Name_StAdrStreetP,  FSite.Address.Street.Pref);
    end;
   if FServices <> nil then
    begin
     WriteStr_(C_Section_Services, C_Name_SvcContrNo,      FServices.SvcContrNo);
     WriteStr_(C_Section_Services, C_Name_SvcContrLen,     IntToStr(FServices.SvcContrLen));
     WriteStr_(C_Section_Services, C_Name_SvcSimPeriodId,  IntToStr(FServices.SimPeriodId));
     WriteStr_(C_Section_Services, C_Name_SvcSimPeriodLen, IntToStr(FServices.SimPeriodLen));
     WriteStr_(C_Section_Services, C_Name_SvcStartDate,    FloatToStr(FServices.StartDate));
     WriteStr_(C_Section_Services, C_Name_SvcContrFrom,    FloatToStr(FServices.SvcContrFrom));
     WriteStr_(C_Section_Services, C_Name_SvcContrTo,      FloatToStr(FServices.SvcContrTo));
    end;
   if FDeregData <> nil then
    begin
     WriteStr_(C_Section_DeregData, C_Name_DeregCode,      IntToStr(FDeregData.DeregReasonId));
     WriteStr_(C_Section_DeregData, C_Name_DeregName,      FDeregData.DeregREasonName);
     WriteStr_(C_Section_DeregData, C_Name_NraRegId,       FDeregData.NRARegID);
    end;


   Dest_.Clear;
   IniF.GetStrings(Dest_);
   IniF.WriteString(C_Section_FileInfo, C_Name_CRC, FCalculateCRC(Trim(Dest_.Text)));
   Dest_.Clear;
   IniF.GetStrings(Dest_);
  finally
   IniF.Free;
  end;
 except
  on E: Exception do
   begin
    Result     := False;
    FLastError := '[SaveToStrings]'+E.Message;
   end;
 end;
end;

function TRegistrationData.SaveToFile(Fname_: String): Boolean;
var StrList : TStrings;
begin
 Result := true;
 try
  StrList := TStringList.Create;
  try
   if not SaveToStrings(StrList) then raise EAbort.Create(FLastError);
   StrList.SaveToFile(Fname_);
  finally
   StrList.Free;
  end;
 except
  on E: Exception do
   begin
    Result := False;
    FLastError := '[SaveToFile]'+E.Message;
   end;
 end;
end;

function TRegistrationData.SaveToXML(Doc: IXMLDocument; Root: IXMLNode): Boolean;
var I : Integer;
begin
 DecimalSeparator := '.';
 Result := true;
 try
  if Doc = nil then raise EAbort.Create('XML document is not assigned');
  if Root = nil then raise EAbort.Create('XML root element is not assigned');

  if FFiscDev <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_FDSerial)).Text  := FFiscDev.Serial;
    Root.AppendChild(Doc.CreateElement(C_Name_FDModel)).Text   := FFiscDev.Model;
    Root.AppendChild(Doc.CreateElement(C_Name_FDVersion)).Text := FFiscDev.Version;
    Root.AppendChild(Doc.CreateElement(C_Name_FDMFM)).Text     := FFiscDev.MFM;
    Root.AppendChild(Doc.CreateElement(C_Name_FDNraType)).Text := IntToStr(FFiscDev.NRAType);
   end;
  if FFiscDevEx <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_FDLineLen)).Text  := IntToStr(FFiscDevEx.LineLength);
    Root.AppendChild(Doc.CreateElement(C_Name_FDCertN)).Text    := FFiscDevEx.CertifNumber;
    Root.AppendChild(Doc.CreateElement(C_Name_FDCertD)).Text    := FFiscDevEx.CertifDate;
    Root.AppendChild(Doc.CreateElement(C_Name_FDCertM)).Text    := FFiscDevEx.CertifModel;
    Root.AppendChild(Doc.CreateElement(C_Name_FDNraUrl)).Text   := FFiscDevEx.NraUrl;
    Root.AppendChild(Doc.CreateElement(C_Name_FDPayCount)).Text := IntToStr(FFiscDevEx.PayTypesCount);
    with Root.AppendChild(Doc.CreateElement(C_Name_FDPayMap)) do
     begin
      for I := 1 to 8 do appendChild(Doc.createElement(C_Name_FDPayItm+IntToStr(I))).text := IntToStr(FFiscDevEx.PayTypesMapp[I]);
     end;
    with Root.AppendChild(Doc.CreateElement(C_Name_FDTaxGroups)) do
     begin
      for I := 1 to 8 do AppendChild(Doc.CreateElement(C_Name_FDTaxGrp+IntToStr(I))).Text := FormatFloat('0.00', FFiscDevEx.TaxGroupsVal[I]);
     end;
    with Root.AppendChild(Doc.CreateElement(C_Name_FDAdvLines)) do
     begin
      for I := 1 to 10 do AppendChild(Doc.CreateElement(C_Name_FDAdvLine+IntToStr(I))).Text := FFiscDevEx.AdvLines[I];
     end;
    Root.AppendChild(Doc.CreateElement(C_Name_FDDecimal)).Text := BoolToStr(FFiscDevEx.DecimalSep, false);
    Root.AppendChild(Doc.CreateElement(C_Name_FDComment)).Text := FRemoveLFCR(FFiscDevEx.Comment);
    Root.AppendChild(Doc.CreateElement(C_Name_FDTagGrpV)).text := IntToStr(FFiscDevEx.TaxGroupsCntV);
   end;
  if FModem <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_MSerial)).Text  := FModem.Serial;;
    Root.AppendChild(Doc.CreateElement(C_Name_MModel)).Text   := FModem.Model;
    Root.AppendChild(Doc.CreateElement(C_Name_MVersion)).Text := FModem.Version;
   end;
  if FSim <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_SIMIMSI)).Text       := FSim.IMSI;
    Root.AppendChild(Doc.CreateElement(C_Name_SIMMSISDN)).Text     := FSim.MSISDN;
    Root.AppendChild(Doc.CreateElement(C_Name_SIMICC)).Text        := FSim.ICC;
    Root.AppendChild(Doc.CreateElement(C_Name_SIMOper)).Text       := IntToStr(FSim.Operat);
    Root.AppendChild(Doc.CreateElement(C_Name_SIMPayedTo)).Text    := FloatToStr(FSim.PayedTo);
    Root.AppendChild(Doc.CreateElement(C_Name_SIMUnlockCode)).Text := FSim.UnlockCode;
    Root.AppendChild(Doc.CreateElement(C_Name_SIMExternal)).Text   := BoolToStr(FSim.IsExternal);
    Root.AppendChild(Doc.CreateElement(C_Name_SIMActivation)).Text := FloatToStr(FSim.Activation);
    Root.AppendChild(Doc.CreateElement(C_Name_SIMApn2Url)).Text    := FSim.Apn2Url;
   end;
  if FOwner <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerEikT)).Text    := IntToStr(FOwner.EIKType);
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerEik)).Text     := FOwner.EIK;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerName)).Text    := FOwner.Name;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerAddr)).Text    := FOwner.Address;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerAddrSh)).Text  := FOwner.ShortAdr;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerVatEik)).Text  := FOwner.TaxEik;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerDistrN)).Text  := FOwner.DistrName;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerDistrC)).Text  := FOwner.DistrCode;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerTownN)).Text   := FOwner.TownName;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerTownC)).Text   := IntToStr(FOwner.TownCode);
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerMOL)).Text     := FOwner.MOL;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerPhnG1)).Text   := FOwner.PhoneG1;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerPhnG2)).Text   := FOwner.PhoneG2;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerPhnM1)).Text   := FOwner.PhoneM1;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerPhnM2)).Text   := FOwner.PhoneM2;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerEmail)).Text   := FOwner.Email;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerContact)).Text := FOwner.Contact;
    Root.AppendChild(Doc.CreateElement(C_Name_OwnerComment)).Text := FOwner.Comment
   end;
  if FDealer <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_DlrEikT)).Text    := IntToStr(FDealer.EIKType);
    Root.AppendChild(Doc.CreateElement(C_Name_DlrEik)).Text     := FDealer.EIK;
    Root.AppendChild(Doc.CreateElement(C_Name_DlrName)).Text    := FDealer.Name;
   end;
  if FSite <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_StTypeCode)).Text    := FSite.SiteType.Code;
    Root.AppendChild(Doc.CreateElement(C_Name_StTypeName)).Text    := FSite.SiteType.Name;
    Root.AppendChild(Doc.CreateElement(C_Name_StNumber)).Text      := FSite.Number;
    Root.AppendChild(Doc.CreateElement(C_Name_StName)).Text        := FSite.SiteName;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrRegionC)).Text  := FSite.Address.Region.Code;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrRegionP)).Text  := FSite.Address.Region.Pref;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrRegionN)).Text  := FSite.Address.Region.Name;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrTownC)).Text    := FSite.Address.Town.Code;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrTownP)).Text    := FSite.Address.Town.Pref;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrTownN)).Text    := FSite.Address.Town.Name;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrAreaC)).Text    := FSite.Address.Area.Code;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrAreaP)).Text    := FSite.Address.Area.Pref;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrAreaN)).Text    := FSite.Address.Area.Name;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrStreetC)).Text  := FSite.Address.Street.Code;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrStreetP)).Text  := FSite.Address.Street.Pref;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrStreetN)).Text  := FSite.Address.Street.Name;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrStreetNo)).Text := FSite.Address.StrNo;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrBlock)).Text    := FSite.Address.Block;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrEntr)).Text     := FSite.Address.Entr;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrFloor)).Text    := FSite.Address.Floor;
    Root.AppendChild(Doc.CreateElement(C_Name_StAdrApp)).Text      := FSite.Address.App;
    Root.AppendChild(Doc.CreateElement(C_Name_StShortAddr)).Text   := FSite.ShortAddr;
   end;
  if FServices <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_SvcStartDate)).Text    := FloatToStr(FServices.StartDate);
    Root.AppendChild(Doc.CreateElement(C_Name_SvcContrNo)).Text      := FServices.SvcContrNo;
    Root.AppendChild(Doc.CreateElement(C_Name_SvcContrFrom)).Text    := FloatToStr(FServices.SvcContrFrom);
    Root.AppendChild(Doc.CreateElement(C_Name_SvcContrTo)).Text      := FloatToStr(FServices.SvcContrTo);
    Root.AppendChild(Doc.CreateElement(C_Name_SvcContrLen)).Text     := IntToStr(FServices.SvcContrLen);
    Root.AppendChild(Doc.CreateElement(C_Name_SvcSimPeriodId)).Text  := IntToStr(FServices.SimPeriodId);
    Root.AppendChild(Doc.CreateElement(C_Name_SvcSimPeriodLen)).Text := IntToStr(FServices.SimPeriodLen);
   end;
  if FDeregData <> nil then
   begin
    Root.AppendChild(Doc.CreateElement(C_Name_DeregCode)).Text  := IntToStr(FDeregData.DeregReasonId);
    Root.AppendChild(Doc.CreateElement(C_Name_DeregName)).Text  := FDeregData.DeregREasonName;
    Root.AppendChild(Doc.CreateElement(C_Name_NraRegId)).Text   := FDeregData.NRARegID;
   end;
 except
  on E: Exception do
   begin
    Result := False;
    FLastError := '[SaveToXML]'+E.Message;
   end;
 end;
end;

function TRegistrationData.LoadFromStrings(Source_: TStrings): Boolean;
var I      : Integer;
    IniFile: TMemIniFile;
    CRC    : String;

    function GetStrValue(Section_, Name_: String): String;
    begin
     Result := IniFile.ReadString(Section_, Name_, '');
     CRC    := CRC + Result;
    end;
    function GetIntValue(Section_, Name_: String; Def: Integer=0): Integer;
    begin
     Result := StrToIntDef(GetStrValue(Section_, Name_), Def);
    end;
    function GetFloatVal(Section_, Name_: String; Def: Real=0): Real;
    begin
     Result := StrToFloatDef(GetStrValue(Section_, Name_), Def);
    end;
    function GetBooolVal(Section_, Name_: String; Def: Boolean=false): Boolean;
    begin
     Result := StrToBoolDef(GetStrValue(Section_, Name_), Def);
    end;
begin
 DecimalSeparator := '.';
 Clear;
 Result := true;
 try
  if Source_ = nil then raise EAbort.Create('Container for operation is no assigned!');

  IniFile := TMemIniFile.Create('');
  try
    IniFile.SetStrings(Source_);

    CRC := GetStrValue(C_Section_FileInfo, C_Name_CRC);
    if CRC = '' then raise EAbort.Create('Invalid file signature!');
    IniFile.EraseSection(C_Section_FileInfo);
    Source_.Clear;
    IniFile.GetStrings(Source_);
    if CRC <> FCalculateCRC(Trim(Source_.Text)) then raise EAbort.Create('Invalid file signature!');

    if (IniFile.SectionExists(C_Section_FiscDevice)) then
    with AddFiscDev do
     begin
      Serial  := GetStrValue(C_Section_FiscDevice, C_Name_FDSerial);
      Model   := GetStrValue(C_Section_FiscDevice, C_Name_FDModel);
      Version := GetStrValue(C_Section_FiscDevice, C_Name_FDVersion);
      MFM     := GetStrValue(C_Section_FiscDevice, C_Name_FDMFM);
      NRAType := GetIntValue(C_Section_FiscDevice, C_Name_FDNraType, 1);
     end;

    if (IniFile.SectionExists(C_Section_FiscDeviceEx)) then
    with AddFiscDevEx do
     begin
      LineLength    := GetIntValue(C_Section_FiscDeviceEx, C_Name_FDLineLen, 22);
      CertifNumber  := GetStrValue(C_Section_FiscDeviceEx, C_Name_FDCertN);
      CertifDate    := GetStrValue(C_Section_FiscDeviceEx, C_Name_FDCertD);
      CertifModel   := GetStrValue(C_Section_FiscDeviceEx, C_Name_FDCertM);
      NraUrl        := GetStrValue(C_Section_FiscDeviceEx, C_Name_FDNraUrl);
      PayTypesCount := GetIntValue(C_Section_FiscDeviceEx, C_Name_FDPayCount, 4);

      for I := 1 to 8 do
       PayTypesMapp[I] := GetIntValue(C_Section_FiscDeviceEx+C_Name_FDPayMap, C_Name_FDPayItm+IntToStr(I));

      for I := 1 to 8 do
       TaxGroupsVal[I] := GetFloatVal(C_Section_FiscDeviceEx+C_Name_FDTaxGroups, C_Name_FDTaxGrp+IntToStr(I));

      for I := 1 to 10 do
       AdvLines[I]     := GetStrValue(C_Section_FiscDeviceEx+C_Name_FDAdvLines, C_Name_FDAdvLine+IntToStr(I));

      DecimalSep    := StrToBool(GetStrValue(C_Section_FiscDeviceEx, C_Name_FDDecimal));
      Comment       := FAddLFCR(GetStrValue(C_Section_FiscDeviceEx, C_Name_FDComment));
      TaxGroupsCntV := GetIntValue(C_Section_FiscDeviceEx, C_Name_FDTagGrpV, 0);
     end;

    if (IniFile.SectionExists(C_Section_Modem)) then
    with AddModem do
     begin
      Serial  := GetStrValue(C_Section_Modem, C_Name_MSerial);
      Model   := GetStrValue(C_Section_Modem, C_Name_MModel);
      Version := GetStrValue(C_Section_Modem, C_Name_MVersion);
     end;

    if (IniFile.SectionExists(C_Section_SIM)) then
    with AddSim do
     begin
      IMSI       := GetStrValue(C_Section_SIM, C_Name_SIMIMSI);
      MSISDN     := GetStrValue(C_Section_SIM, C_Name_SIMMSISDN);
      ICC        := GetStrValue(C_Section_SIM, C_Name_SIMICC);
      Operat     := GetIntValue(C_Section_SIM, C_Name_SIMOper);
      PayedTo    := GetFloatVal(C_Section_SIM, C_Name_SIMPayedTo);
      UnlockCode := GetStrValue(C_Section_SIM, C_Name_SIMUnlockCode);
      IsExternal := GetBooolVal(C_Section_SIM, C_Name_SIMExternal);
      Activation := GetFloatVal(C_Section_SIM, C_Name_SIMActivation);
      Apn2Url    := GetStrValue(C_Section_SIM, C_Name_SIMApn2Url);
     end;

    if (IniFile.SectionExists(C_Section_Owner)) then
    with AddOwner do
     begin
      EIKType   := GetIntValue(C_Section_Owner, C_Name_OwnerEikT);
      EIK       := GetStrValue(C_Section_Owner, C_Name_OwnerEik);
      Name      := GetStrValue(C_Section_Owner, C_Name_OwnerName);
      Address   := GetStrValue(C_Section_Owner, C_Name_OwnerAddr);
      ShortAdr  := GetStrValue(C_Section_Owner, C_Name_OwnerAddrSh);
      TaxEik    := GetStrValue(C_Section_Owner, C_Name_OwnerVatEik);
      DistrName := GetStrValue(C_Section_Owner, C_Name_OwnerDistrN);
      TownName  := GetStrValue(C_Section_Owner, C_Name_OwnerTownN);
      MOL       := GetStrValue(C_Section_Owner, C_Name_OwnerMOL);
      PhoneG1   := GetStrValue(C_Section_Owner, C_Name_OwnerPhnG1);
      PhoneG2   := GetStrValue(C_Section_Owner, C_Name_OwnerPhnG2);
      PhoneM1   := GetStrValue(C_Section_Owner, C_Name_OwnerPhnM1);
      PhoneM2   := GetStrValue(C_Section_Owner, C_Name_OwnerPhnM2);
      Email     := GetStrValue(C_Section_Owner, C_Name_OwnerEmail);
      Contact   := GetStrValue(C_Section_Owner, C_Name_OwnerContact);
      Comment   := GetStrValue(C_Section_Owner, C_Name_OwnerComment);
     end;

    if (IniFile.SectionExists(C_Section_Dealer)) then
    with AddDealer do
     begin
      EIKType := GetIntValue(C_Section_Dealer, C_Name_DlrEikT);
      EIK     := GetStrValue(C_Section_Dealer, C_Name_DlrEik);
      Name    := GetStrValue(C_Section_Dealer, C_Name_DlrName);
     end;

    if (IniFile.SectionExists(C_Section_Site)) then
    with AddSite do
     begin
      SiteType.Code       := GetStrValue(C_Section_Site, C_Name_StTypeCode);
      SiteType.Name       := GetStrValue(C_Section_Site, C_Name_StTypeName);
      Number              := GetStrValue(C_Section_Site, C_Name_StNumber);
      SiteName            := GetStrValue(C_Section_Site, C_Name_StName);
      Address.Region.Code := GetStrValue(C_Section_Site, C_Name_StAdrRegionC);
      Address.Region.Pref := GetStrValue(C_Section_Site, C_Name_StAdrRegionP);
      Address.Region.Name := GetStrValue(C_Section_Site, C_Name_StAdrRegionN);
      Address.Town.Code   := GetStrValue(C_Section_Site, C_Name_StAdrTownC);
      Address.Town.Pref   := GetStrValue(C_Section_Site, C_Name_StAdrTownP);
      Address.Town.Name   := GetStrValue(C_Section_Site, C_Name_StAdrTownN);
      Address.Area.Code   := GetStrValue(C_Section_Site, C_Name_StAdrAreaC);
      Address.Area.Pref   := GetStrValue(C_Section_Site, C_Name_StAdrAreaP);
      Address.Area.Name   := GetStrValue(C_Section_Site, C_Name_StAdrAreaN);
      Address.Street.Code := GetStrValue(C_Section_Site, C_Name_StAdrStreetC);
      Address.Street.Pref := GetStrValue(C_Section_Site, C_Name_StAdrStreetP);
      Address.Street.Name := GetStrValue(C_Section_Site, C_Name_StAdrStreetN);
      Address.StrNo       := GetStrValue(C_Section_Site, C_Name_StAdrStreetNo);
      Address.Block       := GetStrValue(C_Section_Site, C_Name_StAdrBlock);
      Address.Entr        := GetStrValue(C_Section_Site, C_Name_StAdrEntr);
      Address.Floor       := GetStrValue(C_Section_Site, C_Name_StAdrFloor);
      Address.App         := GetStrValue(C_Section_Site, C_Name_StAdrApp);
      ShortAddr           := GetStrValue(C_Section_Site, C_Name_StShortAddr);
     end;

    if (IniFile.SectionExists(C_Section_Services)) then
    with AddServices do
     begin
      StartDate    := GetFloatVal(C_Section_Services, C_Name_SvcStartDate);
      SvcContrNo   := GetStrValue(C_Section_Services, C_Name_SvcContrNo);
      SvcContrFrom := GetFloatVal(C_Section_Services, C_Name_SvcContrFrom);
      SvcContrTo   := GetFloatVal(C_Section_Services, C_Name_SvcContrTo);
      SvcContrLen  := GetIntValue(C_Section_Services, C_Name_SvcContrLen);
      SimPeriodId  := GetIntValue(C_Section_Services, C_Name_SvcSimPeriodId);
      SimPeriodLen := GetIntValue(C_Section_Services, C_Name_SvcSimPeriodLen);
     end;

    if (IniFile.SectionExists(C_Section_DeregData)) then
    with AddDeregData do
     begin
      DeregReasonId   := GetIntValue(C_Section_DeregData, C_Name_DeregCode);
      DeregREasonName := GetStrValue(C_Section_DeregData, C_Name_DeregName);
      NRARegID        := GetStrValue(C_Section_DeregData, C_Name_NraRegId);
     end;

  finally
   IniFile.Free;
  end;
 except
  on E: Exception do
   begin
    Result     := False;
    FLastError := '[LoadFromStrings]'+E.Message;
   end;
 end;
end;

function TRegistrationData.LoadFromXML(Root: IXMLNode): Boolean;
var Node: IXMLNode;
    I   : Integer;

    function GetNodeText(OnRoot_: IXMLNode; Node_: String; Required_: Boolean=true): String;
    var iNode_ : IXMLNode;
    begin
     Result := '';
     try
      if OnRoot_ = nil then raise EAbort.Create('XML search root is not assigned');
      iNode_ := OnRoot_.selectSingleNode(Node_);
      if iNode_ = nil then raise Exception.Create('XML node "'+Node_+'" not found');
      Result := iNode_.text;
     except
      if Required_ then raise;
     end;
    end;
    function GetNodeInt(OnRoot_: IXMLNode; Node_: String; Def_: Integer=0; Required_: Boolean=true): Integer;
    begin
     Result := StrToIntDef(GetNodeText(OnRoot_, Node_, Required_), Def_);
    end;
    function GetNodeFloat(OnRoot_: IXMLNode; Node_: String; Def_: Real=0; Required_: Boolean=true): Real;
    begin
     Result := StrToFloatDef(GetNodeText(OnRoot_, Node_, Required_), Def_);
    end;
    function GetNodeBool(OnRoot_: IXMLNode; Node_: String; Def_: Boolean=false; Required_: Boolean=true): Boolean;
    begin
     Result := StrToBoolDef(GetNodeText(OnRoot_, Node_, Required_), Def_);
    end;
begin
 DecimalSeparator := '.';
 Clear;
 try
  if Root = nil then raise EAbort.Create('XML root element is not assigned');

  if (Root.selectSingleNode(C_Name_FDSerial) <> nil) then
  with AddFiscDev do
   begin
    Serial  := GetNodeText(Root, C_Name_FDSerial);
    Model   := GetNodeText(Root, C_Name_FDModel);
    Version := GetNodeText(Root, C_Name_FDVersion);
    MFM     := GetNodeText(Root, C_Name_FDMFM);
    NRAType := GetNodeInt (Root, C_Name_FDNraType);
   end;

  if (Root.selectSingleNode(C_Name_FDLineLen) <> nil) then
  with AddFiscDevEx do
   begin
    LineLength    := GetNodeInt (Root, C_Name_FDLineLen);
    CertifNumber  := GetNodeText(Root, C_Name_FDCertN);
    CertifDate    := GetNodeText(Root, C_Name_FDCertD);
    CertifModel   := GetNodeText(Root, C_Name_FDCertM);
    NraUrl        := GetNodeText(Root, C_Name_FDNraUrl);
    PayTypesCount := GetNodeInt (Root, C_Name_FDPayCount);

    Node := Root.selectSingleNode(C_Name_FDPayMap);
    if Node = nil then raise EAbort.Create('XML node "'+C_Name_FDPayMap+'" not found');
    for I := 1 to 8 do PayTypesMapp[I] := GetNodeInt(Node, C_Name_FDPayItm+IntToStr(I));

    Node := Root.selectSingleNode(C_Name_FDTaxGroups);
    if Node = nil then raise EAbort.Create('XML node "'+C_Name_FDTaxGroups+'" not found');
    for I := 1 to 8 do TaxGroupsVal[I] := GetNodeFloat(Node, C_Name_FDTaxGrp+IntToStr(I));

    Node := Root.selectSingleNode(C_Name_FDAdvLines);
    if Node = nil then raise EAbort.Create('XML node "'+C_Name_FDAdvLines+'" not found');
    for I := 1 to 10 do AdvLines[I] := GetNodeText(Node, C_Name_FDAdvLine+IntToStr(I));

    DecimalSep    := StrToBool(GetNodeText(Root, C_Name_FDDecimal));
    Comment       := FAddLFCR (GetNodeText(Root, C_Name_FDComment));
    TaxGroupsCntV := GetNodeInt(Root, C_Name_FDTagGrpV, 0, false);
   end;

  if (Root.selectSingleNode(C_Name_MSerial) <> nil) then
  with AddModem do
   begin
    Serial  := GetNodeText(Root, C_Name_MSerial);
    Model   := GetNodeText(Root, C_Name_MModel);
    Version := GetNodeText(Root, C_Name_MVersion);
   end;

  if (Root.selectSingleNode(C_Name_SIMIMSI) <> nil) then
  with AddSim do
   begin
    IMSI       := GetNodeText (Root, C_Name_SIMIMSI);
    MSISDN     := GetNodeText (Root, C_Name_SIMMSISDN);
    ICC        := GetNodeText (Root, C_Name_SIMICC);
    Operat     := GetNodeInt  (Root, C_Name_SIMOper);
    PayedTo    := GetNodeFloat(Root, C_Name_SIMPayedTo);
    UnlockCode := GetNodeText (Root, C_Name_SIMUnlockCode);
    IsExternal := GetNodeBool (Root, C_Name_SIMExternal, false, false);
    Activation := GetNodeFloat(Root, C_Name_SIMActivation, 0, false);
    Apn2Url    := GetNodeText (Root, C_Name_SIMApn2Url, false);
   end;

  if (Root.selectSingleNode(C_Name_OwnerEikT) <> nil) then
  with AddOwner do
   begin
    EIKType   := GetNodeInt (Root, C_Name_OwnerEikT);
    EIK       := GetNodeText(Root, C_Name_OwnerEik);
    Name      := GetNodeText(Root, C_Name_OwnerName);
    Address   := GetNodeText(Root, C_Name_OwnerAddr);
    ShortAdr  := GetNodeText(Root, C_Name_OwnerAddrSh);
    TaxEik    := GetNodeText(Root, C_Name_OwnerVatEik);
    DistrName := GetNodeText(Root, C_Name_OwnerDistrN);
    DistrCode := GetNodeText(Root, C_Name_OwnerDistrC);
    TownName  := GetNodeText(Root, C_Name_OwnerTownN);
    TownCode  := GetNodeInt (Root, C_Name_OwnerTownC);
    MOL       := GetNodeText(Root, C_Name_OwnerMOL);
    PhoneG1   := GetNodeText(Root, C_Name_OwnerPhnG1);
    PhoneG2   := GetNodeText(Root, C_Name_OwnerPhnG2);
    PhoneM1   := GetNodeText(Root, C_Name_OwnerPhnM1);
    PhoneM2   := GetNodeText(Root, C_Name_OwnerPhnM2);
    Email     := GetNodeText(Root, C_Name_OwnerEmail);
    Contact   := GetNodeText(Root, C_Name_OwnerContact);
    Comment   := GetNodeText(Root, C_Name_OwnerComment);
   end;

  if (Root.selectSingleNode(C_Name_DlrEikT) <> nil) then
  with AddDealer do
   begin
    EIKType := GetNodeInt (Root, C_Name_DlrEikT);
    EIK     := GetNodeText(Root, C_Name_DlrEik);
    Name    := GetNodeText(Root, C_Name_DlrName);
   end;

  if (Root.selectSingleNode(C_Name_StTypeCode) <> nil) then
  with AddSite do
   begin
    SiteType.Code       := GetNodeText(Root, C_Name_StTypeCode);
    SiteType.Name       := GetNodeText(Root, C_Name_StTypeName);
    Number              := GetNodeText(Root, C_Name_StNumber);
    SiteName            := GetNodeText(Root, C_Name_StName);
    Address.Region.Code := GetNodeText(Root, C_Name_StAdrRegionC);
    Address.Region.Pref := GetNodeText(Root, C_Name_StAdrRegionP);
    Address.Region.Name := GetNodeText(Root, C_Name_StAdrRegionN);
    Address.Town.Code   := GetNodeText(Root, C_Name_StAdrTownC);
    Address.Town.Pref   := GetNodeText(Root, C_Name_StAdrTownP);
    Address.Town.Name   := GetNodeText(Root, C_Name_StAdrTownN);
    Address.Area.Code   := GetNodeText(Root, C_Name_StAdrAreaC);
    Address.Area.Pref   := GetNodeText(Root, C_Name_StAdrAreaP);
    Address.Area.Name   := GetNodeText(Root, C_Name_StAdrAreaN);
    Address.Street.Code := GetNodeText(Root, C_Name_StAdrStreetC);
    Address.Street.Pref := GetNodeText(Root, C_Name_StAdrStreetP);
    Address.Street.Name := GetNodeText(Root, C_Name_StAdrStreetN);
    Address.StrNo       := GetNodeText(Root, C_Name_StAdrStreetNo);
    Address.Block       := GetNodeText(Root, C_Name_StAdrBlock);
    Address.Entr        := GetNodeText(Root, C_Name_StAdrEntr);
    Address.Floor       := GetNodeText(Root, C_Name_StAdrFloor);
    Address.App         := GetNodeText(Root, C_Name_StAdrApp);
    ShortAddr           := GetNodeText(Root, C_Name_StShortAddr);
   end;

  if (Root.selectSingleNode(C_Name_SvcStartDate) <> nil) then
  with AddServices do
   begin
    StartDate    := GetNodeFloat(Root, C_Name_SvcStartDate, Date);
    SvcContrNo   := GetNodeText (Root, C_Name_SvcContrNo);
    SvcContrFrom := GetNodeFloat(Root, C_Name_SvcContrFrom, Date);
    SvcContrTo   := GetNodeFloat(Root, C_Name_SvcContrTo);
    SvcContrLen  := GetNodeInt  (Root, C_Name_SvcContrLen);
    SimPeriodId  := GetNodeInt  (Root, C_Name_SvcSimPeriodId);
    SimPeriodLen := GetNodeInt  (Root, C_Name_SvcSimPeriodLen);
   end;

  if (Root.selectSingleNode(C_Name_DeregCode) <> nil) then
  with AddDeregData do
   begin
    DeregReasonId   := GetNodeInt (Root, C_Name_DeregCode);
    DeregREasonName := GetNodeText(Root, C_Name_DeregName);
    NRARegID        := GetNodeText(Root, C_Name_NraRegId);
   end;

  Result := true;
 except
  on E: Exception do
   begin
    Result := False;
    FLastError := '[LoadFromXML]'+E.Message;
   end;
 end;
end;

function TRegistrationData.LoadFromXML(Xml: String): Boolean;
var XMLDoc : IXMLDocument;
    iNode  : IXMLNode;
begin
 Result := false;
 try
  XMLDoc := CreateXMLDoc;
  try
   if not XMLDoc.loadXML(Xml) then raise EAbort.Create('Invalid XML! Parse error: '+XMLDoc.parseError.reason);

   iNode := XMLDoc.documentElement;
   if iNode = nil then raise EAbort.Create('Invalid XML! No root node.');

   if not LoadFromXML(iNode) then raise EAbort.Create('Data error: '+LastError);
   Result := true;
  finally
   FreeXmlDoc(XMLDoc);
  end;
 except
  on E: Exception do
   begin
    Result := False;
    FLastError := '[LoadFromXML]'+E.Message;
   end;
 end;
end;

function TRegistrationData.LoadFromFile(Fname_: String): Boolean;
var StrList : TStrings;
begin
 Result := true;
 try
  if not FileExists(Fname_) then raise EAbort.Create('File does not exist: '+Fname_);
  StrList := TStringList.Create;
  try
   StrList.LoadFromFile(Fname_);
   if not LoadFromStrings(StrList) then raise EAbort.Create(FLastError);
  finally
   StrList.Free;
  end;
 except
  on E: Exception do
   begin
    Result     := False;
    FLastError := '[LoadFromFile]'+E.Message;
   end;
 end;
end;

function TRegistrationData.DataAsText: String;
var SList : TStrings;
begin
 Result := '';
 SList  := TStringList.Create;
 try
  if SaveToStrings(SList) then Result := SList.Text;
 finally
  SList.Free;
 end;
end;

function TRegistrationData.TextForCRC: String;
begin
 Result := '';
 if FFiscDev <> nil then
   begin
    Result := Result + Trim(FFiscDev.Serial);
    Result := Result + Trim(FFiscDev.Model);
   end;
 if FModem <> nil then
   begin
    Result := Result + Trim(FModem.Serial);
    Result := Result + Trim(FModem.Model);
   end;
 if FOwner <> nil then
   begin
    Result := Result + Trim(FOwner.EIK);
    Result := Result + Trim(FOwner.Name);
   end;
 if FSite <> nil then
   begin
    Result := Result + Trim(FSite.Number);
    Result := Result + Trim(FSite.SiteName);
   end;
end;

function TRegistrationData.DataAsXML(RootName: String): String;
var XMLDoc : IXMLDocument;
    iNode  : IXMLElement;
begin
 Result := '';
 XMLDoc := CreateXMLDoc;
 try
  iNode := XMLDoc.createElement(RootName);
  XMLDoc.documentElement := iNode;
  XMLDoc.insertBefore(XMLDoc.createProcessingInstruction('xml', xml_ProcessInstruction), iNode);
  if SaveToXML(XMLDoc, iNode) then Result := XMLDoc.xml;
 finally
  FreeXmlDoc(XMLDoc);
 end;
end;

function TRegistrationData.FGetInfoAsText: String;
begin
 Result := '';
 if FFiscDev <> nil then
  Result := FFiscDev.Serial+'['+FFiscDev.Model+'/'+FFiscDev.Version+']';
 if FModem <> nil then
  Result := Result + '['+FModem.Model+'/'+FModem.Version+']';
end;

end.
