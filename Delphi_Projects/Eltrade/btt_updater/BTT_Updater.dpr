program BTT_Updater;

uses
  Forms,
  MAinUnit in 'MainUnit.pas' {MainForm},
  BillingClientUnit in 'CommonUnits\BillingClientUnit.pas',
  MSXML2_TLB in 'CommonUnits\MSXML2_TLB.pas',
  BaseHandler in 'CommonUnits\BaseHandler.pas',
  DeviceUnit in 'CommonUnits\DeviceUnit.pas',
  BillingConstUnit in 'CommonUnits\BillingConstUnit.pas',
  XMLHandler in 'CommonUnits\XMLHandler.pas',
  XMLHandlerMS in 'CommonUnits\XMLHandlerMS.pas',
  CCDeviceHandler in 'CommonUnits\CCDeviceHandler.pas',
  ChangePassHandler in 'CommonUnits\ChangePassHandler.pas',
  CryptoHandler in 'CommonUnits\CryptoHandler.pas',
  DBInterfaceUnit in 'CommonUnits\DBInterfaceUnit.pas',
  DP_ESKUnit in 'CommonUnits\DP_ESKUnit.pas',
  DP_FiscUnit in 'CommonUnits\DP_FiscUnit.pas',
  DP_NRAUnit in 'CommonUnits\DP_NRAUnit.pas',
  DP_PingUnit in 'CommonUnits\DP_PingUnit.pas',
  DP_SystemUnit in 'CommonUnits\DP_SystemUnit.pas',
  DP_UpdateUnit in 'CommonUnits\DP_UpdateUnit.pas',
  EIKHandler in 'CommonUnits\EIKHandler.pas',
  ElBngMOServiceHandler in 'CommonUnits\ElBngMOServiceHandler.pas',
  ErrorLogHandler in 'CommonUnits\ErrorLogHandler.pas',
  ESKHandler in 'CommonUnits\ESKHandler.pas',
  ExportFilesHandler in 'CommonUnits\ExportFilesHandler.pas',
  ExtDataHandler in 'CommonUnits\ExtDataHandler.pas',
  FilesHandler in 'CommonUnits\FilesHandler.pas',
  FiscalizationTypes in 'CommonUnits\FiscalizationTypes.pas',
  FiscalizeHandler in 'CommonUnits\FiscalizeHandler.pas',
  LoginHandler in 'CommonUnits\LoginHandler.pas',
  md5 in 'CommonUnits\md5.pas',
  MyIdCustomHttpServer in 'CommonUnits\MyIdCustomHttpServer.pas',
  MyIdHTTPServer in 'CommonUnits\MyIdHTTPServer.pas',
  NRAHandler in 'CommonUnits\NRAHandler.pas',
  RegistrationXmlUnit in 'CommonUnits\RegistrationXmlUnit.pas',
  SendMessageHandler in 'CommonUnits\SendMessageHandler.pas',
  SimHandler in 'CommonUnits\SimHandler.pas',
  SZCodeBaseX in 'CommonUnits\SZCodeBaseX.pas',
  TCPCommConstUnit in 'CommonUnits\TCPCommConstUnit.pas',
  TCPCommUnit in 'CommonUnits\TCPCommUnit.pas',
  TestHandler in 'CommonUnits\TestHandler.pas',
  VersionUtilsUnit in 'CommonUnits\VersionUtilsUnit.pas',
  VivacomSrv in 'CommonUnits\VivacomSrv.pas',
  WinUtilsUnit in 'CommonUnits\WinUtilsUnit.pas',
  ZLib in 'CommonUnits\ZLib.pas',
  ZLIBArchive in 'CommonUnits\ZLIBArchive.pas',
  WaitUnit in 'WaitUnit.pas' {WaitForm},
  ESKReadUnit in 'CommonUnits\ESKReadUnit.pas',
  StrResUnit in 'CommonUnits\StrResUnit.pas',
  ConstUnit in 'CommonUnits\ConstUnit.pas',
  ReadSignatureUnit in 'ReadSignatureUnit.pas' {frmReadSignature},
  DataUnit in 'DataUnit.pas' {DataMod: TDataModule},
  GetFilesUnit in 'CommonUnits\GetFilesUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Eltrade Updater';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataMod, DataMod);
  //Application.CreateForm(TDataMod, DataMod);
  Application.Run;
end.
