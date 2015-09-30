unit ConstUnit;

interface

uses SysUtils, Forms, Windows, Graphics, WinUtilsUnit, ESKHandler;

const
  C_DBIniSectionMain      = 'DATABASE OPTIONS';
  C_DBIniSectionSvceApp   = 'DATABASE OPTIONS SERVICE';
  C_DBIniValConn          = 'Connection';
  C_DBIniValUser          = 'UserName';
  C_DBIniValPass          = 'Password';

  C_Section_DefSavePath   = 'DefaultPathSave';
  C_Section_DefLoadPath   = 'DefaultPathLoad';
  C_Section_AutoUpdate    = 'AutoUpdate';
  C_Value_Fiscalization   = 'Fiscalization';
  C_Value_FDBackup        = 'FDBackupData';
  C_Value_FDEKL           = 'FDEKLData';
  C_Value_LastUpdCheck    = 'LastUpdateCheck';
  C_Value_PetrolConfig    = 'PetrolConfig';

  C_FilenameModemUpdate   = 'MPOSUPG.exe';
  C_FilenameModemUpdateOld= 'MPOSUPG_v202.exe';
  C_FilenameXmlFuelTool   = 'XmlFuelDesigner.exe';
  C_FilenameFailedRequest = 'FailedRequests.dat';

  // templates
  C_Template_NewFisc      = 'eftt_NewFiscalization.frf';
  C_Template_EditFisc     = 'eftt_EditFiscalization.frf';
  C_Template_DropFisc     = 'eftt_DropFiscalization.frf';
  C_Template_Svidetelstvo = 'eftt_Svidetelstvo.frf';

  // fiscalization file format
  C_FiscFile_Section    = 'FISCALIZATION DATA';
  C_FiscFile_OName      = 'OwnerName';
  C_FiscFile_OEIKType   = 'OwnerEIKType';
  C_FiscFile_OEIKText   = 'OwnerEIKText';
  C_FiscFile_OAdress    = 'OwnerAddress';
  C_FiscFile_OTaxEIK    = 'OwnerTaxEIK';
  C_FiscFile_SNumb      = 'SiteNumb';
  C_FiscFile_SName      = 'SiteName';
  C_FiscFile_SType      = 'SiteType';
  C_FiscFile_SADistrict = 'SiteDistrict';
  C_FiscFile_SATown     = 'SiteTown';
  C_FiscFile_SAArea     = 'SiteArea';
  C_FiscFile_SAStreet   = 'SiteStreet';
  C_FiscFile_SAStreetN  = 'SiteStreetN';
  C_FiscFile_SABlock    = 'SiteBlock';
  C_FiscFile_SAEntrance = 'SiteEntr';
  C_FiscFile_SAFloor    = 'SiteFloor';
  C_FiscFile_SAAppartm  = 'SiteApp';
  C_FiscFile_SAShortAdr = 'SiteShortAdr';
  C_FiscFile_SimPeriod  = 'SimContractPeriod';
  C_FiscFile_ContractN  = 'ContractNumb';
  C_FiscFile_ContractL  = 'ContractLength';
  C_FiscFile_FULineLen  = 'DevLineLength';
  C_FiscFile_FUDecimal  = 'DevDecimal';
  C_FiscFile_FUComment  = 'Commanet';
  C_FiscFile_SectionTax = 'TAX GROUPS';
  C_FiscFile_FUTaxGrp   = 'Grp';
  C_FiscFile_SectionAdv = 'ADV LINES';
  C_FiscFile_FUAdvLines = 'Line';
  C_FiscFile_SectionPay = 'PAY TYPES';
  C_FiscFile_FUPayType  = 'PType';



type
 TCurrentESK = class(TObject)
 private
  FESKSerial    : String;
  FESKVersion   : String;
  FESKCode      : String;
  FESKKey       : String;
  FESKModules   : String;
  FUserFullName : String;
  FUserLevel    : String;
  FCompanyEIK   : String;
  FCompanyEIKTyp: Byte;
  FCompanyName  : String;
  FCompanyTown  : String;
  FCompanyAddres: String;
  FCompanyPhone : String;
  FCompanyContrN: String;
  FCompanyContrD: String;
  FBranchName   : String;
  FBranchTown   : String;
  FBranchAddres : String;
  FBranchPhone  : String;
  FBranchFiscCtr: String;

  function FGetESKData: TEskData;
  procedure FSetESKData(ESKData: TEskData);
  procedure FLoadDealerDataFromIni;
 public
  constructor Create;
  procedure AssignDealer(DealerData: TDealerData);

  property ESKData: TEskData read FGetESKData write FSetESKData;
  property ESKSerial: String read FESKSerial write FESKSerial;
  property ESKVersion: String read FESKVersion write FESKVersion;
  property ESKCode: String read FESKCode write FESKCode;
  property ESKKey: String read FESKKey write FESKKey;
  property ESKModules: String read FESKModules write FESKModules;
  property UserFullName: String read FUserFullName write FUserFullName;
  property UserLevel: String read FUserLevel write FUserLevel;
  property CompanyEIK: String read FCompanyEIK write FCompanyEIK;
  property CompanyName: String read FCompanyName write FCompanyName;
  property CompanyTown: String read FCompanyTown write FCompanyTown;
  property CompanyAddres: String read FCompanyAddres write FCompanyAddres;
  property CompanyPhone: String read FCompanyPhone write FCompanyPhone;
  property CompanyContrN: String read FCompanyContrN write FCompanyContrN;
  property CompanyContrD: String read FCompanyContrD write FCompanyContrD;
  property BranchName: String read FBranchName write FBranchName;
  property BranchTown: String read FBranchTown write FBranchTown;
  property BranchAddres: String read FBranchAddres write FBranchAddres;
  property BranchPhone: String read FBranchPhone write FBranchPhone;
  property BranchFiscCtr: String read FBranchFiscCtr write FBranchFiscCtr;
  property CompanyEIKType: Byte read FCompanyEIKTyp write FCompanyEIKTyp;
 end;


var
 CurrentESK      : TCurrentESK;
 CurrentHostName : String;
 CurrentAppVers  : String;
 LocalPath       : String;
 AppPath         : String;

 Set_ServerHostWork  : String;
 Set_ServerHostTest  : String;
 Set_ServerHostDevS  : String;
 Set_FontName        : TfontName;
 Set_Charset         : TFontCharset;
 Set_UseSiteTypeNumb : Boolean;
 Set_GenSvidetelstvo : Boolean;

// Когато се появят 2 сесии с еднакви номера на устройства - респективно тапи
// сървъра реже връзката ->> "Devise with same SN. Terminate"
// затова трябва да осигурим само една връзка в даден момент от време
// за целта ще ползваме въпросната глобална променлива, която ще контролира комуникацията със сървъра през нишки
// Използва се от
//          TSrvCommitThread = class(TThread)
//          TCheckUpdateThread = class(TThread)
//          TCheckExtDataThread = class(TThread)
CommunicationActive : Boolean;



function LoadAppSettings(Fname: String = ''): Boolean;
function SaveAppSettings(Fname: String = ''): Boolean;
function CheckFileName(Src_: String): String;
function BoolToInt_(Val: Boolean): Integer;
function StrToBool_(Val: string): Boolean;
function RemoveLFCR(Source_: String): String;
function AddLFCR(Source_: String): String;
function GetApplicationFolder: String;

implementation
uses IniFiles, VersionUtilsUnit;

function CheckFileName(Src_: String): String;
var II: Integer;
begin
 Result := '';
 for II := 1 to length(Src_) do
  begin
   if Src_[II] in [' ', '0'..'9', 'A'..'Z', 'a'..'z', 'А'..'я'] then Result := Result + Src_[II];
  end;
end;

function BoolToInt_(Val: Boolean): Integer;
begin
 if Val then Result := 1
  else Result := 0;
end;

function StrToBool_(Val: string): Boolean;
begin
 Result := (Val = '1');
end;

function RemoveLFCR(Source_: String): String;
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

function AddLFCR(Source_: String): String;
var JJ: Integer;
begin
 Result := '';
 for JJ := 1 to Length(Source_) do
  if Source_[JJ] = '|' then Result := Result + sLineBreak
   else Result := Result + Source_[JJ];
end;

function GetApplicationFolder: String;
begin
 Result := Win_GetAppDataDir;
 if Result <> '' then
  begin
   Result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(Result)+'Eltrade');
   if not DirectoryExists(Result) then CreateDir(Result);
   Result := IncludeTrailingPathDelimiter(Result+ChangeFileExt(ExtractFileName(Application.ExeName), ''));
   if not DirectoryExists(Result) then CreateDir(Result);
  end;
 if not DirectoryExists(Result) then Result := '';
end;

function LoadAppSettings(Fname: String = ''): Boolean;
begin
 if Fname = '' then Fname := ChangeFileExt(Application.ExeName, '.ini');
 Result := FileExists(Fname);
 with TIniFile.Create(Fname) do
 try
  // load communication settings
  if not ValueExists('COMMUNICATION', 'AuthorizationServer') then WriteString('COMMUNICATION', 'AuthorizationServer', 'http://partners.eltrade.com:6009/');
  if not ValueExists('COMMUNICATION', 'TestServer') then          WriteString('COMMUNICATION', 'TestServer', 'http://partners.eltrade.com:6008/');
  if not ValueExists('COMMUNICATION', 'DeviceSetupServer') then   WriteString('COMMUNICATION', 'DeviceSetupServer', 'http://partners.eltrade.com:6007/');


  Set_ServerHostWork  := ReadString('COMMUNICATION', 'AuthorizationServer', '');
  Set_ServerHostTest  := ReadString('COMMUNICATION', 'TestServer',          '');
  Set_ServerHostDevS  := ReadString('COMMUNICATION', 'DeviceSetupServer',   '');

  // load print settings
  if not ValueExists('PRINT', 'FontName') then    WriteString ('PRINT', 'FontName',   'MS Sans Serif');
  if not ValueExists('PRINT', 'FontCharset') then WriteInteger('PRINT', 'FontCharset', DEFAULT_CHARSET);

  Set_FontName        := ReadString ('PRINT', 'FontName',           'MS Sans Serif');
  Set_Charset         := ReadInteger('PRINT', 'FontCharset',        DEFAULT_CHARSET);

  // load other settings
  if not ValueExists('MISC',  'ShowSiteTypeNumber') then WriteBool('MISC',  'ShowSiteTypeNumber', false);

  Set_UseSiteTypeNumb := ReadBool('MISC',  'ShowSiteTypeNumber',   false);
  Set_GenSvidetelstvo := ReadBool('MISC',  'GenerateSvidetelstvo', true);


  // поправка на стара грешка...
  if Set_ServerHostWork = 'http://billing.eltrade.com:6009/' then WriteString('COMMUNICATION', 'AuthorizationServer', 'http://partners.eltrade.com:6009/');
  if Set_ServerHostTest = 'http://billing.eltrade.com:6008/' then WriteString('COMMUNICATION', 'TestServer', 'http://partners.eltrade.com:6008/');
 finally
  Free;
 end;
end;

function SaveAppSettings(Fname: String = ''): Boolean;
begin
 if Fname = '' then Fname := ChangeFileExt(Application.ExeName, '.ini');
 try
   with TIniFile.Create(Fname) do
   try
    WriteString('COMMUNICATION', 'AuthorizationServer',  Set_ServerHostWork);
    WriteString('COMMUNICATION', 'TestServer',           Set_ServerHostTest);
    WriteString('COMMUNICATION', 'DeviceSetupServer',    Set_ServerHostDevS);


    WriteString ('PRINT',         'FontName',             Set_FontName);
    WriteInteger('PRINT',         'FontCharset',          Set_Charset);

    WriteBool   ('MISC',          'ShowSiteTypeNumber',   Set_UseSiteTypeNumb);
    WriteBool   ('MISC',          'GenerateSvidetelstvo', Set_GenSvidetelstvo);
   finally
    Free;
   end;
   Result := FileExists(Fname);
 except
  Result := false;
 end;
end;


//******************************************************************************
//   TCurrentESK
//******************************************************************************
constructor TCurrentESK.Create;
begin
 inherited Create;
 FESKSerial    := '';
 FESKVersion   := '';
 FESKCode      := '';
 FESKKey       := '';
 FESKModules   := '';
 FUserFullName := '';
 FUserLevel    := '';
 FCompanyEIK   := '000000000';
 FCompanyName  := '';
 FCompanyTown  := '';
 FCompanyAddres:= '';
 FCompanyPhone := '';
 FCompanyContrN:= '';
 FCompanyContrD:= '';
 FBranchName   := '';
 FBranchTown   := '';
 FBranchAddres := '';
 FBranchPhone  := '';
 FBranchFiscCtr:= '';
 FCompanyEIKTyp:= 0;
end;

function TCurrentESK.FGetESKData: TEskData;
begin
 Result.Serial  := FESKSerial;
 Result.Version := FESKVersion;
 Result.Code    := FESKCode;
 Result.Key     := FESKKey;
 Result.Modules := FESKModules;
end;

procedure TCurrentESK.FSetESKData(ESKData: TEskData);
begin
 FESKSerial  := ESKData.Serial;
 FESKVersion := ESKData.Version;
 FESKCode    := ESKData.Code;
 FESKKey     := ESKData.Key;
 FESKModules := ESKData.Modules;
 // зареждане на локалните данни за собственика на тази тапа
 FLoadDealerDataFromIni;
end;

procedure TCurrentESK.AssignDealer(DealerData: TDealerData);
begin
 if DealerData = nil then Exit;

 FUserFullName := DealerData.UserFullName;
 FUserLevel    := DealerData.UserLevel;
 FCompanyEIK   := DealerData.CompanyEIK;
 FCompanyName  := DealerData.CompanyName;
 FCompanyTown  := DealerData.CompanyTown;
 FCompanyAddres:= DealerData.CompanyAddres;
 FCompanyPhone := DealerData.CompanyPhone;
 FCompanyContrN:= DealerData.CompanyContrNumb;
 FCompanyContrD:= DealerData.CompanyContrDate;
 FBranchName   := DealerData.BranchName;
 FBranchTown   := DealerData.BranchTown;
 FBranchAddres := DealerData.BranchAddres;
 FBranchPhone  := DealerData.BranchPhone;
 FBranchFiscCtr:= DealerData.BranchFiscCtr;

 // запис на потребителя така че после данните да ги има без достъп до сървъра
 if FESKSerial <> '' then
  begin
   try
     with TIniFile.Create(AppPath + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini')) do
     try
      WriteString('OWNER_'+FESKSerial, 'UserFullName',  FUserFullName);
      WriteString('OWNER_'+FESKSerial, 'UserLevel',     FUserLevel);
      WriteString('OWNER_'+FESKSerial, 'CompanyEIK',    FCompanyEIK);
      WriteString('OWNER_'+FESKSerial, 'CompanyName',   FCompanyName);
      WriteString('OWNER_'+FESKSerial, 'CompanyTown',   FCompanyTown);
      WriteString('OWNER_'+FESKSerial, 'CompanyAddres', FCompanyAddres);
      WriteString('OWNER_'+FESKSerial, 'CompanyPhone',  FCompanyPhone);
      WriteString('OWNER_'+FESKSerial, 'CompanyContrN', FCompanyContrN);
      WriteString('OWNER_'+FESKSerial, 'CompanyContrD', FCompanyContrD);
      WriteString('OWNER_'+FESKSerial, 'BranchName',    FBranchName);
      WriteString('OWNER_'+FESKSerial, 'BranchTown',    FBranchTown);
      WriteString('OWNER_'+FESKSerial, 'BranchAddres',  FBranchAddres);
      WriteString('OWNER_'+FESKSerial, 'BranchPhone',   FBranchPhone);
      WriteString('OWNER_'+FESKSerial, 'BranchFiscCtr', FBranchFiscCtr);
     finally
      Free;
     end;
   except
   end;
  end;
end;

procedure TCurrentESK.FLoadDealerDataFromIni();
begin
 if (FESKSerial <> '')and
    ((FCompanyEIK='000000000')or(FCompanyEIK='')or(FCompanyName='')) then
  begin
   try
     with TIniFile.Create(AppPath + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini')) do
     try
      FUserFullName := ReadString('OWNER_'+FESKSerial, 'UserFullName',  '');
      FUserLevel    := ReadString('OWNER_'+FESKSerial, 'UserLevel',     '');
      FCompanyEIK   := ReadString('OWNER_'+FESKSerial, 'CompanyEIK',    '');
      FCompanyName  := ReadString('OWNER_'+FESKSerial, 'CompanyName',   '');
      FCompanyTown  := ReadString('OWNER_'+FESKSerial, 'CompanyTown',   '');
      FCompanyAddres:= ReadString('OWNER_'+FESKSerial, 'CompanyAddres', '');
      FCompanyPhone := ReadString('OWNER_'+FESKSerial, 'CompanyPhone',  '');
      FCompanyContrN:= ReadString('OWNER_'+FESKSerial, 'CompanyContrN', '');
      FCompanyContrD:= ReadString('OWNER_'+FESKSerial, 'CompanyContrD', '');
      FBranchName   := ReadString('OWNER_'+FESKSerial, 'BranchName',    '');
      FBranchTown   := ReadString('OWNER_'+FESKSerial, 'BranchTown',    '');
      FBranchAddres := ReadString('OWNER_'+FESKSerial, 'BranchAddres',  '');
      FBranchPhone  := ReadString('OWNER_'+FESKSerial, 'BranchPhone',   '');
      FBranchFiscCtr:= ReadString('OWNER_'+FESKSerial, 'BranchFiscCtr', '');
     finally
      Free;
     end;
   except
   end;
  end;
end;


initialization
 CurrentESK      := TCurrentESK.Create;
 LocalPath       := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
 AppPath         := GetApplicationFolder;
 CurrentHostName := Win_GetHostName;
 CurrentAppVers  := GetAppVersion(Application.ExeName);

 Set_FontName        := 'MS Sans Serif';
 Set_Charset         := DEFAULT_CHARSET;
 Set_UseSiteTypeNumb := false;
 Set_GenSvidetelstvo := true;

 if AppPath = '' then AppPath := LocalPath;

finalization
 CurrentESK.Free;
end.
