(************************************************************************)
(*  Copyright(c) Eltrade Ltd.                                           *)
(*    All Rights Reserved.                                              *)
(*                                                                      *)
(*  Author: Stiliyan Vukadinov                                          *)
(*  Revision History:                                                   *)
(*    << 1.0 08.09.2013 >>                                              *)
(************************************************************************)
(*  File History:                                                       *)
(*    bSMSComm.pas  Version 1.0                                         *)
(*    Checked in    08/09/2013 at 12:39:03                              *)
(*    Retrieved     08/09/2013 at 18:16:48                              *)
(************************************************************************)
(*  Purpose:                                                            *)
(*    This file contains the code to send SMS via TCM - 1 server.       *)
(*                                                                      *)
(*                                                                      *)
(************************************************************************)
(*  Forms:                                                              *)
(*    MainForm -                                                        *)
(*                                                                      *)
(************************************************************************)
(*  Important Conntrols:                                                *)
(*    edtPhone        -                                                 *)
(*    mmoMessage      -                                                 *)
(*    mmoStatus       -                                                 *)
(*    btnSendSMS      -                                                 *)
(*    btnCheckStatus  -                                                 *)
(*    btnNewSMS       -                                                 *)
(************************************************************************)

unit bSMSMain;

interface

uses
  SysUtils, Classes, Controls, Forms, bSMSComm, StdCtrls, Placemnt, IniFiles;

type
  TButtonState = (SendSMS, NewSMS);

  TMainForm = class(TForm)
    { Controls }
    lblPhone   : TLabel;
    lblMessage : TLabel;
    lblStatus: TLabel;

    edtPhone   : TEdit;
    mmoMessage : TMemo;
    mmoStatus  : TMemo;

    btnSendSMS : TButton;
    btnNewSMS  : TButton;
    FormStorage1: TFormStorage;
    { Form create & destroy }
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    { Buttons events }
    procedure btnSendSMSClick(Sender: TObject);
    procedure btnNewSMSClick(Sender: TObject);
  private
    IniFileName: string;
    bSMSid: string;
    bSMS: TbSMS;
    { Procedure & Functions }
    procedure LayoutControls();
    procedure SetButtonsState(btnState: TButtonState);
    function  CalcSMSid(): string;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  { New line }
  sLineBreak = AnsiString(#13#10);
  { Maximum length of sms id }
  MAX_SMS_ID_VALUE = '1000000';
  IniFileSection = 'SETTINGS';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  bSMS := nil;
  IniFileName := ChangeFileExt(Application.ExeName, '.ini');
  FormStorage1.IniFileName := IniFileName;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if (bSMS <> nil) then FreeAndNil(bSMS);
end;

procedure TMainForm.LayoutControls();
begin
  edtPhone.Text := '';
  mmoMessage.Clear;
  mmoStatus.Clear;
  { Todo: This value must be derive from application database }
  bSMSid := CalcSMSid();
  SetButtonsState(SendSMS);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  LayoutControls();
end;

procedure TMainForm.SetButtonsState(btnState: TButtonState);
begin
  btnSendSMS.Enabled := (btnState = SendSMS);
  btnNewSMS.Enabled := (btnState = NewSMS);
  case btnState of
    SendSMS: edtPhone.SetFocus;
    NewSMS : btnNewSMS.SetFocus;
  end;
end;

function TMainForm.CalcSMSid(): string;
begin
  Randomize;
  Result := SysUtils.Format('%.*d', [Length(MAX_SMS_ID_VALUE), Random(StrToInt(MAX_SMS_ID_VALUE))]);
end;

procedure TMainForm.btnSendSMSClick(Sender: TObject);
begin
  if (bSMS <> nil) then FreeAndNil(bSMS);

  bSMS := TbSMS.Create(IniFileName, IniFileSection);
  if (bSMS.SendSMS(edtPhone.Text, mmoMessage.Text, bSMSid)) then
     MainForm.mmoStatus.Lines.Add('Sending SMS is successful.')
  else
     MainForm.mmoStatus.Lines.Add('Sending SMS is failed. ' + bSMS.LastError);

  SetButtonsState(NewSMS);
end;

procedure TMainForm.btnNewSMSClick(Sender: TObject);
begin
  LayoutControls();
  FreeAndNil(bSMS)
end;

end.
