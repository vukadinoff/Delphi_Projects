program SQLEventsProject;

uses
  Forms,
  SQLEventsDemo in 'SQLEventsDemo.pas' {MainF};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainF, MainF);
  Application.Run;
end.
