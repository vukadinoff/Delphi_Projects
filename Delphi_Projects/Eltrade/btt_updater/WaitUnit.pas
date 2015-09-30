unit WaitUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TWaitForm = class(TForm)
    Msg: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowWaitScreen(Msg_: String);
procedure HideWaitScreen;

implementation
{$R *.dfm}

var
  WaitForm: TWaitForm;

procedure ShowWaitScreen(Msg_: String);
begin
 if WaitForm = nil then WaitForm := TWaitForm.Create(Application.MainForm);
 WaitForm.Msg.Caption := Msg_;
// if Application.MainForm <> nil then WaitForm.Parent := Application.MainForm;
 if not WaitForm.Visible then WaitForm.Show;
 WaitForm.Update;
end;

procedure HideWaitScreen;
begin
 if WaitForm <> nil then
  begin
   if WaitForm.Visible then WaitForm.Hide;
   WaitForm.Free;
   WaitForm := nil;
  end;
end;

initialization

  WaitForm := nil;

end.
