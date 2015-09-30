program SimRechargeTest;

uses
  Forms,
  SimRMainUnit in 'SimRMainUnit.pas' {Form1},
  TcpClientUnit in 'TcpClientUnit.pas',
  SimRchVivacomUnit in 'SimRchVivacomUnit.pas',
  SimRechargeUnit in 'SimRechargeUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
