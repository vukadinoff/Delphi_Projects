unit LoginUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, DBUtilsUnit,
  Dialogs, StdCtrls, ExtCtrls, dxCore, dxSpeedButton, ConstUnit, IBSQL, IBDataBase,
  Placemnt;

type
  TLoginForm = class(TForm)
    pLogIn: TPanel;
    pLogInData: TPanel;
    TitleLabel: TLabel;
    NameLabel: TLabel;
    PassLabel: TLabel;
    HandImage: TImage;
    eUserName: TEdit;
    ePass: TEdit;
    pNumpad: TPanel;
    btnPad7: TdxSpeedButton;
    btnPad8: TdxSpeedButton;
    btnPad9: TdxSpeedButton;
    btnPad4: TdxSpeedButton;
    btnPad5: TdxSpeedButton;
    btnPad6: TdxSpeedButton;
    btnPad1: TdxSpeedButton;
    btnPad2: TdxSpeedButton;
    btnPad3: TdxSpeedButton;
    btnPad0: TdxSpeedButton;
    btnDot: TdxSpeedButton;
    btnEsc: TdxSpeedButton;
    btnMinus: TdxSpeedButton;
    btnTab: TdxSpeedButton;
    btnBackspace: TdxSpeedButton;
    FormStorage: TFormStorage;
    btnOK: TdxSpeedButton;
    btnCancel: TdxSpeedButton;
    procedure btnPad0Click(Sender: TObject);
    procedure btnDotClick(Sender: TObject);
    procedure btnEscClick(Sender: TObject);
    procedure eUserNameChange(Sender: TObject);
    procedure btnBackspaceClick(Sender: TObject);
    procedure eUserNameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnTabClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure eUserNameEnter(Sender: TObject);
    procedure ePassEnter(Sender: TObject);
  private
    LastCtrlName : String;
    IBQ : TIBSQL;
    IBT : TIBTransaction;
    function LogIn(IBSQL: TIBSQL): boolean;
    procedure ChangeActiveImage;
    procedure CharPadButtonClick(Caption_: string);
    procedure ESCButtonClick;
    procedure NumPadButtonClick(Btn_: Integer);
    function FindButton(KeyCode: Word): TdxSpeedButton;
    procedure PushBtnDown(Btn_: TdxSpeedButton);
    procedure PushBtnUp(Btn_: TdxSpeedButton; CanClick: Boolean=true);
    { Private declarations }
  public
    procedure FitComponents;
    procedure DoLogin;
    { Public declarations }
  end;
                                              
implementation
uses DataUnit, KBDUnit, ResStrUnit, MyMessageUnit, MainUnit, ProtectUnit,
  SetNumberUnit;

{$R *.dfm}

procedure TLoginForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);

 FormStorage.IniFileName := IniFName;
 if not FormStorage.Active then FormStorage.Active := True;

 DB_CreateIBSQL(IBQ, IBT, DataMod.aIBDatabase);
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;

 MainForm.ScaleControls(Self);

 if (ParamCount = 0) and Set_ShutDownPC then btnCancel.Caption := S_ShutDown
  else btnCancel.Caption := S_ExitOs;
end;

procedure TLoginForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 DB_DestroyIBSQL(IBQ, IBT);
 Action := caFree;
end;

procedure TLoginForm.FitComponents;
begin
 Self.WindowState := wsMaximized;
 Application.ProcessMessages;

 pLogIn.Left := (Self.Width - pLogIn.Width) div 2;
 pLogIn.Visible := true;

 if eUserName.Text = '' then eUserName.SetFocus
  else ePass.SetFocus;
 if ActiveControl <> nil then LastCtrlName := ActiveControl.Name;
 ChangeActiveImage;
 Application.ProcessMessages;
end;

function TLoginForm.LogIn(IBSQL: TIBSQL): boolean;
var O_Access, O_Pass, O_User, O_FullName: string;
    O_ID: integer;
begin
 Result := False;
 try
  try
   if not DB_ExecuteSQL('SELECT * FROM N_OPERATORS'+
                        ' WHERE OPERATOR_USERNAME = '+QuotedStr(eUserName.Text)+
                        ' AND OPERATOR_LEVEL IN (3)', IBSQL, False, 'LogIn') then exit;
   if (IBSQL.RecordCount = 0) then
    begin
     MyMessageDlg(S_UserNameNotFound+' '+QuotedStr(eUserName.Text)+'.', mtWarning, [mbOK], 0);
     eUserName.SetFocus;
     Exit;
    end
   else
    begin
     O_ID       := IBSQL.FieldByName('OPERATOR_ID').AsInteger;
     O_User     := IBSQL.FieldByName('OPERATOR_USERNAME').AsString;
     O_Access   := IBSQL.FieldByName('OPERATOR_ACCESS').AsString;
     O_Pass     := IBSQL.FieldByName('OPERATOR_PASSWORD').AsString;
     O_FullName := IBSQL.FieldByName('OPERATOR_FULLNAME').AsString;
    end;
   DB_CommitIBSQL(IBSQL);

   if (Length(O_Pass) > 0) then
    begin
     if Protection.ValidatePassword(ePass.Text, O_Pass) then
      begin
       if not DB_ExecuteSQL('UPDATE N_OPERATORS SET OPERATOR_LOGON_ = 1'+
                            ' WHERE OPERATOR_ID = '+IntToStr(O_ID), IBSQL, True, 'LogIn') then exit;
       SetCurrentOperData(O_ID, O_User, O_FullName);
      end
     else
      begin
       MyMessageDlg(S_InvalidPassword, mtWarning, [mbOK], 0);
       ePass.Text := '';
       ePass.SetFocus;
       Exit;
      end;
    end
   else
    begin
     MyMessageDlg(S_InvalidPassword, mtWarning, [mbOK], 0);
     ePass.SetFocus;
     Exit;
    end;

   DataMod.PostEvent(3, S_SessionStarted+' ('+CurrentOperName+').');
   Result := True;
  except
   on E: Exception do
    begin
     DataMod.PostException('Login failed for user "'+eUserName.Text+'" '+Trim(E.Message));
     DataMod.PostEvent(13, S_LoginFailed+' ('+eUserName.Text+'): '+Trim(E.Message));
    end;
  end;
 finally
  DB_CommitIBSQL(IBSQL);
 end;
end;

procedure TLoginForm.NumPadButtonClick(Btn_: Integer);
begin
 if eUserName.Focused then
   eUserName.Text := eUserName.Text + IntToStr(Btn_)
 else
 if ePass.Focused then
   ePass.Text := ePass.Text + IntToStr(Btn_);
end;

procedure TLoginForm.CharPadButtonClick(Caption_: string);
begin
 if eUserName.Focused then
   eUserName.Text := eUserName.Text + Caption_
 else
 if ePass.Focused then
   ePass.Text := ePass.Text + Caption_;
end;

procedure TLoginForm.ESCButtonClick;
begin
 if eUserName.Focused then eUserName.Text := ''
 else
 if ePass.Focused then ePass.Text := '';
end;

procedure TLoginForm.ChangeActiveImage;
begin
 HandImage.Visible := (ActiveControl is TEdit);
 if ActiveControl is TEdit then
  begin
   HandImage.Top := TEdit(ActiveControl).Top - 2;
   HandImage.Left:= TEdit(ActiveControl).Left + TEdit(ActiveControl).Width + 5;
  end;
end;

procedure TLoginForm.btnPad0Click(Sender: TObject);
begin
 try
  NumPadButtonClick(StrToInt(Copy(TdxSpeedButton(Sender).Name,7,1)));
 except
 end;
end;

procedure TLoginForm.btnDotClick(Sender: TObject);
begin
 if not (Sender is TdxSpeedButton) then exit;
 try
  CharPadButtonClick(TdxSpeedButton(Sender).Caption);
 except
 end;
end;

procedure TLoginForm.btnEscClick(Sender: TObject);
begin
 ESCButtonClick;
end;

procedure TLoginForm.eUserNameChange(Sender: TObject);
begin
 if not (Sender is TEdit) then exit;
 if TEdit(Sender).Focused then
   TEdit(Sender).SelStart := Length(TEdit(Sender).Text);
end;

procedure TLoginForm.btnBackspaceClick(Sender: TObject);
begin
 if (eUserName.Focused) and (eUserName.Text <> '') then
  eUserName.Text := Copy(eUserName.Text, 1, Length(eUserName.Text)-1)
 else
 if (ePass.Focused) and (ePass.Text <> '') then
  ePass.Text := Copy(ePass.Text, 1, Length(ePass.Text)-1);
end;

procedure TLoginForm.eUserNameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if not (Sender is TEdit) then exit;
 ChangeActiveImage;
 if (TEdit(Sender).Name = LastCtrlName) then InputText(Sender); //KBDForm.InputText(Sender);
 LastCtrlName := TEdit(Sender).Name;
end;

procedure TLoginForm.DoLogin;
begin
   if LogIn(IBQ) then
    begin
     if not MainForm.ChildWindowExist('SetNumberForm', cwaShow) then
      with TSetNumberForm.Create(Self.Owner) do FitComponents;

     Self.Close;
    end;
end;

procedure TLoginForm.PushBtnDown(Btn_ : TdxSpeedButton);
begin
 Btn_.HookMouseEnter;
 Btn_.HookMouseDown;
 if Assigned(Btn_.OnMouseDown) then Btn_.OnMouseDown(Btn_, mbLeft, [], 1, 1);
end;

procedure TLoginForm.PushBtnUp(Btn_ : TdxSpeedButton; CanClick: Boolean=true);
var IsDn : Boolean;
begin
 IsDn := (dsClicked in Btn_.DrawState);
 if (IsDn)and(Assigned(Btn_.OnMouseUp)) then Btn_.OnMouseUp(Btn_, mbLeft, [], 1, 1);
 Btn_.HookMouseUp;
 Btn_.HookMouseLeave;
 if (IsDn)and(CanClick)and(Assigned(Btn_.OnClick))and(Self.Active) then Btn_.Click;
end;

function TLoginForm.FindButton(KeyCode: Word): TdxSpeedButton;
begin
 case KeyCode of
 VK_NUMPAD0..VK_NUMPAD9:  Result := TdxSpeedButton(FindComponent(Format('btnPad%d', [Abs(KeyCode - VK_NUMPAD0)])));
 $30..$39:                Result := TdxSpeedButton(FindComponent(Format('btnPad%d', [Abs(KeyCode - $30)])));
 VK_BACK:                 Result := btnBackspace;
 VK_ESCAPE:               Result := btnEsc;
 VK_SUBTRACT:             Result := btnMinus;
 VK_DECIMAL:              Result := btnDot;
 VK_HOME:                 Result := btnCancel;
 VK_RETURN:               Result := btnOK;
 VK_TAB:                  Result := btnTab;
 else                     Result := nil;
 end;
end;

procedure TLoginForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnDown(Btn);
end;

procedure TLoginForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnUp(Btn, (Btn = btnEsc)or(Btn = btnCancel)or(Btn = btnOK));
end;

procedure TLoginForm.btnTabClick(Sender: TObject);
begin
 if eUserName.Focused then ePass.SetFocus
 else
 if ePass.Focused then eUserName.SetFocus
 else eUserName.SetFocus;
end;

procedure TLoginForm.btnOKClick(Sender: TObject);
begin
 if eUserName.Focused then
  begin
   if eUserName.Focused then ePass.SetFocus
   else
   if ePass.Focused then eUserName.SetFocus
   else eUserName.SetFocus;
  end
 else
  DoLogin;
end;

procedure TLoginForm.btnCancelClick(Sender: TObject);
begin
 PostMessage(Application.Handle, WM_CLOSE, 0, 0);
end;

procedure TLoginForm.eUserNameEnter(Sender: TObject);
begin
 if eUserName.Focused then HandImage.Top := eUserName.Top
  else HandImage.Top := ePass.Top;
end;

procedure TLoginForm.ePassEnter(Sender: TObject);
begin
 if eUserName.Focused then HandImage.Top := eUserName.Top
  else HandImage.Top := ePass.Top;
end;

end.
