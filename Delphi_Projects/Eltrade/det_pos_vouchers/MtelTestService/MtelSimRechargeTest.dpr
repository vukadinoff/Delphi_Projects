program MtelSimRechargeTest;

uses
  Forms,
  MtelSimRechargeMainUnit in 'MtelSimRechargeMainUnit.pas' {MtelMainForm},
  TcpClientUnit in 'TcpClientUnit.pas',
  MtelSimRechargeUnit in 'MtelSimRechargeUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMtelMainForm, MtelMainForm);
  Application.Run;
end.
