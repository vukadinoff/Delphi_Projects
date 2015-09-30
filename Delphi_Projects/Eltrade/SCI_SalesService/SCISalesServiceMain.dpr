program SCISalesServiceMain;

uses
  Forms,
  SCISalesServiceProjectGroup in 'SCISalesServiceProjectGroup.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
