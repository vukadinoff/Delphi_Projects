program ELPos_SimRecharge;

uses
  Forms,
  Windows,
  SysUtils,
  ResStrUnit in 'ResStrUnit.pas',
  ConstUnit in 'ConstUnit.pas',
  DBUtilsUnit in 'DBUtilsUnit.pas',
  EboTaskManagerUnit in 'EboTaskManagerUnit.pas',
  ProtectUnit in 'ProtectUnit.pas',
  KeyStateUnit in 'KeyStateUnit.pas',
  CheckDigitsUnit in 'CheckDigitsUnit.pas',
  CreateFilesUnit in 'CreateFilesUnit.pas',
  SimRchVivacomUnit in 'SimRchVivacomUnit.pas',
  TcpClientUnit in 'TcpClientUnit.pas',
  XMLHandler in 'XMLHandler.pas',
  XMLHandlerMS in 'XMLHandlerMS.pas',
  DataUnit in 'DataUnit.pas' {DataMod: TDataModule},
  MainUnit in 'MainUnit.pas' {MainForm},
  AboutDlg in 'AboutDlg.pas' {AboutBox},
  MyMessageUnit in 'MyMessageUnit.pas' {MyMessageForm},
  RegistrationUnit in 'RegistrationUnit.pas' {RegistrationForm},
  KBDUnit in 'KBDUnit.pas' {KBDForm},
  LoginUnit in 'LoginUnit.pas' {LoginForm},
  SetNumberUnit in 'SetNumberUnit.pas' {SetNumberForm},
  SetSumUnit in 'SetSumUnit.pas' {SetSumForm},
  SetPaymentUnit in 'SetPaymentUnit.pas' {SetPaymentForm},
  WaitUnit in 'WaitUnit.pas' {WaitForm},
  SearchCustomerUnit in 'SearchCustomerUnit.pas' {SearchCustomerForm},
  CustomersUnit in 'CustomersUnit.pas' {CustomersForm},
  CustomerDataUnit in 'CustomerDataUnit.pas' {CustomerDataForm},
  ErrorsUnit in 'ErrorsUnit.pas' {ErrorsForm};

{$R *.res}

var WinH     : HWND;

begin
 WinH := FindWindow(nil, PChar('ELTRADE store management system - SIM RECHARGE'));
 if (WinH = 0) then
  begin
   Title       := nil;
   VerApp      := GetVersion;
   LocalPath   := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
   TempPath    := IncludeTrailingPathDelimiter(LocalPath + 'Temp');
   ExceptPath  := IncludeTrailingPathDelimiter(LocalPath + 'Exceptions');
   LanguagePath:= IncludeTrailingPathDelimiter(LocalPath + 'Translations');
   LocalIniPath:= IncludeTrailingPathDelimiter(LocalPath + 'LocalIni');

   if not DirectoryExists(TempPath) then CreateDir(TempPath);
   if not DirectoryExists(ExceptPath) then CreateDir(ExceptPath);
   if not DirectoryExists(LanguagePath) then CreateDir(LanguagePath);
   if not DirectoryExists(LocalIniPath) then CreateDir(LocalIniPath);

   IniFName := LocalIniPath + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
   LngFName := ChangeFileExt(ExtractFileName(Application.ExeName), '.lng');
   if not LoadResStrings(LanguagePath + LngFName) then LoadResStrings(LocalPath + LngFName);

   SimRChObj := nil;
   if (Title = nil)and(ParamCount = 0) then
    begin
     Title := TAboutBox.Create(nil);
     Title.Show;
     Title.Update;
    end; 
   Application.Initialize;
   Application.Title := 'ELTRADE store management system - SIM RECHARGE';
   Application.CreateForm(TDataMod, DataMod);
  if Assigned(Title) then Title.Update;
   Application.CreateForm(TMainForm, MainForm);
//   if Assigned(Title) then Title.Update;
//   Application.CreateForm(TKBDForm, KBDForm);
   Application.Run;
  end
 else
  begin
   if IsIconic(WinH) then ShowWindow(WinH, SW_RESTORE)
    else BringWindowToTop(WinH);
   SetForegroundWindow(WinH);
  end;
end.
