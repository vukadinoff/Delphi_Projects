program MtelSimRechargeTest;

uses
  Forms,
  MtelSimRMainUnit in '..\MtelTestService\MtelSimRMainUnit.pas' {MtelMainForm},
  TcpClientUnit in '..\MtelTestService\TcpClientUnit.pas',
  MtelSimRchUnit in '..\MtelTestService\MtelSimRchUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMtelMainForm, MtelMainForm);
  Application.Run;
end.
