program bSMSProject;

uses
  Forms,
  bSMSMain in 'bSMSMain.pas' {MainForm},
  bSMSComm in 'bSMSComm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
