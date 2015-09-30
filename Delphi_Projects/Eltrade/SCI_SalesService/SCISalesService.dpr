program SCISalesService;

uses
  Forms,
  SCISalesServiceMain in 'SCISalesServiceMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
