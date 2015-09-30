program ElPos_SimRechargeKeyGen;

uses
  Forms,
  Windows,
  KGUnit in 'KGUnit.pas' {KGForm},
  ProtectUnit in '..\ProtectUnit.pas';

{$R *.res}

var HWin : HWND;

begin
 HWin := FindWindow(nil, PChar('ELTRADE store management system - BOS OPERATIONS KEY GEN'));
 if (HWin = 0) then
  begin
  Application.Initialize;
  Application.Title := 'ELTRADE store management system - BOS OPERATIONS KEY GEN';  
  Application.CreateForm(TKGForm, KGForm);
  Application.Run;
  end
 else
  begin
   if IsIconic(HWin) then
    ShowWindow(HWin, SW_RESTORE)
   else
    BringWindowToTop(HWin);
   SetForegroundWindow(HWin);      
  end;
end.
