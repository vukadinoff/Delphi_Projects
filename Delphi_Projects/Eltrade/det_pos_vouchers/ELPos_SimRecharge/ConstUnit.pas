unit ConstUnit;

interface

uses Windows, IniFiles, Classes, SysUtils, Forms;

const
  C_DatabaseIniFName   = 'EBODatabase.ini';
  C_ErrFName           = 'ELPosSimRechargeExcept.txt';
  C_CurrentModuleNumb  = 46;   
  C_CurrentModuleCode  = 'SimRechargeTSC';          // използва се за генериране на ключ
  C_ConfigFileName     = 'ELPos_SimRecharge.ini';
  C_AppTitle           = 'Зареждане на СИМ карти';
  C_MTel               = 1;
  C_Globul             = 2;
  C_Vivacom            = 3;
  C_MTelName           = 'МТел';
  C_GlobulName         = 'Глобул';
  C_VivacomName        = 'Виваком';
  C_POSIniFileName     = 'ELPos_TouchConfig.ini';
  C_InvoiceNumbIFile   = 'InvoiceNumber.ini';
  C_PrnLineLength      = 33;

type
  TStringProcedure = procedure (AString: ShortString) of object;

  TSimRChObj = class(TObject)
   public
    FPhoneNumb  : string;
    FMobOper    : integer;
    FSum        : double;
    FPluNumb    : integer;
    FPluName    : string;
    FVat        : double;
    FTaxGroup   : integer;
//    FBuyPr      : double;
//    FBuyCurrID  : integer;
//    FBuyCCource : double;
//    FSellPr     : double;
//    FSellCurrID : Integer;
//    FSellCCource: Double;
    FMaxSumTr   : Double;
    FMaxSumDay  : Double;
    FDocType    : byte;
    FDocName    : String;
    FPayType    : Integer;
    FPayName    : String;
    FPayPrnNumb : Integer;
    FPayPrnName : String;
    FPayCource  : Double;
    FCustBulst  : String;
    FCustName   : String;
  end;

  TOperObj = class(TObject)
   public
    FNumb  : Integer;
    FName  : String;
  end;

  TCustomerObject = class(TObject)
   public
    FBulstat   : String;
    FBulstatL  : String;
    FTaxNumb   : String;
    FFirmName  : String;
    FTown      : String;
    FAddress   : String;
    FMOL       : String;
    FReceiver  : String;
    FDiscount  : Real;
  end;

var
  LocalPath           : String;
  TempPath            : String;
  ExceptPath          : String;
  LanguagePath        : String;
  IniFName            : String;
  LngFName            : String;
  ScaleFactor         : Real;
  CurrentModuleName   : String;
  CurrentModuleID     : integer;
  CurrentOperID       : integer;
  CurrentOperName     : string;
  CurrentOperFullName : string;
  CurrentWorkStorage  : Integer;
  CurrentStorageName  : String;
  BCurID              : Integer;
  BCurName            : string;
  BCurAbr             : string;
  BCurSign            : string;
  BCurCource          : Real;
  OperatorAccessNumb  : Integer;
  VerApp              : string;
  ParamStarted        : boolean;
  SimRChObj           : TSimRChObj;
  CurrentOfficeNumb   : integer;
  CurrentFactNumb     : Int64;
  LocalIniPath        : string;

  // POS settings
  Set_HideMouse       : Boolean;
  Set_ShutDownPC      : Boolean;
  Set_NumLockControl  : Boolean;
  Set_Resolution      : Integer;
  Set_StatusColor     : Integer;
  Set_CurrentTermN    : Integer;
  Set_CurrentTermName : String;
  Set_PrnPath         : String;
  Set_MsgIconsTheme   : Integer;

  // MOB opr settings
  Set_AllowedPayTypes : String;
  Set_Mtel_PluNumb    : Integer;
  Set_Mtel_URL        : String;
  Set_Globul_PluNumb  : Integer;
  Set_Globul_URL      : String;
  Set_Vivacom_PluNumb : Integer;
  Set_Vivacom_URL     : String;
  Set_Vivacom_MinAmnt : Real;//          = 4;
  Set_Vivacom_MaxAmnt : Real;  //        = 100;





function BoolToInt_(Val: boolean): integer;
function BoolToStr_(Val: boolean): string;
function LoadSettings: Boolean;
function GetHostName: string;
function GetVersion: String;
function StrToSQL_(Val: string): string;
procedure SetCurrentOperData(O_ID: integer; O_Name, O_FullName: string);
function FillLeft(S: String; N: Integer; Ch: Char) : String;
function FillLeft1(S: String; N: Integer; Ch: Char) : String;
function FillRight(S: String; N: Integer) : String;
function RoundFloat(Value: Real): Real;
procedure DisplayErrMsg(const AText: string);
function NormalizeMSISDN(Src: String): String;

implementation

uses MyMessageUnit, DBUtilsUnit, ResStrUnit, DataUnit, MainUnit;

function BoolToInt_(Val: boolean): integer;
begin
 if Val then Result := 1
  else Result := 0;
end;

function BoolToStr_(Val: boolean): string;
begin
 if Val then Result := '1'
  else Result := '0';
end;

function LoadSettings: Boolean;
begin
 Result := (FileExists(LocalPath+C_POSIniFileName));
 try
  with TIniFile.Create(LocalPath+C_POSIniFileName) do
  try
   if not ValueExists('COMMON', 'MsgDlgIconsTheme') then WriteInteger('COMMON', 'MsgDlgIconsTheme', 0);

   Set_Resolution     := ReadInteger('MAIN_WINDOW', 'WorkResolution',      0);
   Set_StatusColor    := ReadInteger('MAIN_WINDOW', 'StatusBar_Color',     12632256);
   Set_HideMouse      := ReadBool   ('COMMON',      'HideMouse',           True);
   Set_ShutDownPC     := ReadBool   ('COMMON',      'ShutDownPC',          False);
   Set_NumLockControl := ReadBool   ('COMMON',      'NumLockSwitch',       False);
   Set_CurrentTermN   := ReadInteger('COMMON',      'CurrentTerminalNumb', 1);
   Set_CurrentTermName:= ReadString ('COMMON',      'POSName',             '');
   Set_MsgIconsTheme  := ReadInteger('COMMON',      'MsgDlgIconsTheme',    0);
   Set_PrnPath        := ReadString ('PRN_RULE_1',  'PR_PATH',             '');
  finally
   Free;
  end;

  with TIniFile.Create(LocalPath+C_ConfigFileName) do
  try
   if not ValueExists('COMMON', 'AllowedPayTypes') then WriteString ('COMMON', 'AllowedPayTypes', '0');

   if not ValueExists('MTEL',    'PluNumb')    then WriteInteger('MTEL',    'PluNumb', -1);
   if not ValueExists('MTEL',    'URL')        then WriteString ('MTEL',    'URL',     'test');

   if not ValueExists('GLOBUL',  'PluNumb')    then WriteInteger('GLOBUL',  'PluNumb', -1);
   if not ValueExists('GLOBUL',  'URL')        then WriteString ('GLOBUL',  'URL',     'test');

   if not ValueExists('VIVACOM', 'PluNumb')    then WriteInteger('VIVACOM', 'PluNumb',    -1);
   if not ValueExists('VIVACOM', 'URL')        then WriteString ('VIVACOM', 'URL',        'test');
   if not ValueExists('VIVACOM', 'MinAmmount') then WriteFloat  ('VIVACOM', 'MinAmmount', 4);
   if not ValueExists('VIVACOM', 'MaxAmmount') then WriteFloat  ('VIVACOM', 'MaxAmmount', 100);


   Set_AllowedPayTypes := ReadString ('COMMON', 'AllowedPayTypes', '0');

   Set_Mtel_PluNumb    := ReadInteger('MTEL',   'PluNumb',  -1);
   Set_Mtel_URL        := ReadString ('MTEL',   'URL',      'test');

   Set_Globul_PluNumb  := ReadInteger('GLOBUL', 'PluNumb',  -1);
   Set_Globul_URL      := ReadString ('GLOBUL', 'URL',      'test');

   Set_Vivacom_PluNumb := ReadInteger('VIVACOM', 'PluNumb',    -1);
   Set_Vivacom_URL     := ReadString ('VIVACOM', 'URL',        'test');
   Set_Vivacom_MinAmnt := ReadFloat  ('VIVACOM', 'MinAmmount', 4);
   Set_Vivacom_MaxAmnt := ReadFloat  ('VIVACOM', 'MaxAmmount', 100);

   Set_AllowedPayTypes := ','+Set_AllowedPayTypes+',';
  finally
   Free;
  end;
 except
 end;
end;

function GetHostName: string;
var host : string;
    host_buff : PChar;
    host_len  : DWORD;
begin
 host_buff := #0;
 host_len  := MAX_COMPUTERNAME_LENGTH + 1;
 try
  GetMem(host_buff, host_len);
  GetComputerName(host_buff, host_len);
  host := StrPas(host_buff);
 finally
   FreeMem(host_buff);
 end;
 Result := UpperCase(host);
end;

function GetVersion: String;
const
  InfoNum = 10;
  InfoStr: array[1..InfoNum] of string = ('CompanyName', 'FileDescription',
                   'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
                   'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var S     : string;
    N, Len: DWORD;
    Buf   : PChar;
    Value : PChar;
    LangCharset: string;
    PCharset   : PLongInt;
    InfoLength : UINT;
begin
 try
  Result := '';
  S := Application.ExeName;
  N := GetFileVersionInfoSize(PChar(S), N);
  if N > 0 then
   begin
    Buf := AllocMem(N);
    GetFileVersionInfo(PChar(S), 0, N, Buf);
    // Get translation info for Language / CharSet IDs
    if VerQueryValue(Buf, '\VarFileInfo\Translation',
      Pointer(PCharset), InfoLength) then
     begin
      LangCharset := Format('%.4x%.4x',[LoWord (PCharset^), HiWord (PCharset^)]);
     end
    else LangCharset := '040904E4';

    if VerQueryValue(Buf, PChar('StringFileInfo\'+LangCharset+'\' + InfoStr[3]), Pointer(Value), Len) then
      Result := StrPas(Value);
    FreeMem(Buf, N);
   end;
 except
 end;
end;

function StrToSQL_(Val: string): string;
begin
 if Val = '' then Result := 'NULL'
  else Result := QuotedStr(Val);
end;

procedure SetCurrentOperData(O_ID: integer; O_Name, O_FullName: string);
begin
 CurrentOperID  := O_ID;
 CurrentOperName:= O_Name;
 CurrentOperFullName := O_FullName;
 if Assigned(MainForm) then MainForm.lOperData.Caption := O_Name;
end;

function FillLeft(S: String; N: Integer; Ch: Char) : String;
begin
 if Length(S) > N then
  begin
   Result := copy(S,1,N);
  end
 else
  begin
   Result := S;
   while Length(Result) < N do
    Result := Ch + Result;
  end;
end;

function FillLeft1(S: String; N: Integer; Ch: Char) : String;
var I : Integer;
begin
 Result := S;
 for I := 1 to N do  Result := Ch + Result;
end;

function FillRight(S: String; N: Integer) : String;
begin
 if Length(S) > N then
  begin
   Result := copy(S,1,N);
  end
 else
  begin
   Result := S;
   while Length(Result) < N do
    Result :=  Result + ' ';
  end;
end;

function RoundFloat(Value: Real): Real;
var Sign : Real;
    SVal : String;
    SRnd : String;
    I    : Integer;
begin
   if Value < 0 then Sign := -0.01
    else Sign := 0.01;

   if Abs(Value) < 0.0001 then Value := 0;

   SVal := FloatToStr(Value);
   I := Pos(DecimalSeparator, SVal);
   if I > 0 then
    begin
     SRnd := Copy(SVal, I+3, 1);
     SVal := Copy(SVal, 1, I+2);
     if StrToIntDef(SRnd, 0) >= 5 then
      Result := StrToFloatDef(SVal,0) + Sign
     else
      Result := StrToFloatDef(SVal,0);
    end
   else
    Result := StrToFloatDef(SVal,0)
end;

procedure DisplayErrMsg(const AText: String);
begin
 if Assigned(MainForm) then MainForm.lErr.Caption := AText;
 Application.ProcessMessages;
end;

function NormalizeMSISDN(Src: String): String;
var I: Integer;
begin
 Result := '';
 for I := 1 to Length(Src) do
  if Src[I] in ['0'..'9'] then Result := Result + Src[I]; 
end;


initialization
 OperatorAccessNumb := 11;

end.
