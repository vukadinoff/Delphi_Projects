program FirstTask;

uses
  Forms,
  Main in 'Main.pas' {frmMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
