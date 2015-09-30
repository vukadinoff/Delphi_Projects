program EltradeKitchen;

uses
  SysUtils,
  Windows,
  Dialogs,
  Forms,
  Main in 'Main.pas' {MainForm},
  fraItems in 'fraItems.pas' {frItems: TFrame},
  ItemList in 'ItemList.pas',
  UDPThreadUnit in 'UDPThreadUnit.pas',
  MyMessageUnit in 'MyMessageUnit.pas' {MyMessageForm},
  TerminalSettingsUnit in 'TerminalSettingsUnit.pas' {TerminalSettingsForm};

{$R *.res}
var Hnd : Thandle;
begin
  Hnd := FindWindow(nil,PChar('Eltrade Kitchen'));

  if Hnd = 0 then
  begin
    Application.Initialize;
    Application.Title := 'Eltrade Kitchen';

    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TTerminalSettingsForm, TerminalSettingsForm);

    if (MainForm.MonitorIdx < 0) or (MainForm.MonitorIdx > Screen.MonitorCount - 1) then
    begin
      MessageDlg('Не е намерен монитор с индекс "'+IntToStr(MainForm.MonitorIdx + 1)+'".'+#13#10+
        'Приложението ще бъде терминирано.', mtError, [mbOK], 0);
      Application.Terminate();;
    end;


    Application.Run;
  end
  else
  begin
    if IsIconic(Hnd) then ShowWindow(Hnd, SW_RESTORE);
  end;

end.
