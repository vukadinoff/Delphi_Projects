unit KBDUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, Placemnt, StdCtrls, Spin, ExtCtrls, ImgList, Menus,
  RxMenus, RXCtrls, Mask, dxCore, dxSpeedButton, dxBitButton,
  ComCtrls, ConstUnit;

type
  TKBDForm = class(TForm)
    Panel1: TPanel;
    InEdit: TEdit;
    SpeedButton1: TdxSpeedButton;
    SpeedButton2: TdxSpeedButton;
    SpeedButton3: TdxSpeedButton;
    SpeedButton4: TdxSpeedButton;
    SpeedButton5: TdxSpeedButton;
    SpeedButton6: TdxSpeedButton;
    SpeedButton7: TdxSpeedButton;
    SpeedButton8: TdxSpeedButton;
    SpeedButton9: TdxSpeedButton;
    SpeedButton10: TdxSpeedButton;
    SpeedButton11: TdxSpeedButton;
    SpeedButton12: TdxSpeedButton;
    SpeedButton13: TdxSpeedButton;
    SpeedButton14: TdxSpeedButton;
    SpeedButton15: TdxSpeedButton;
    SpeedButton16: TdxSpeedButton;
    SpeedButton17: TdxSpeedButton;
    SpeedButton18: TdxSpeedButton;
    SpeedButton19: TdxSpeedButton;
    SpeedButton20: TdxSpeedButton;
    SpeedButton21: TdxSpeedButton;
    SpeedButton22: TdxSpeedButton;
    SpeedButton23: TdxSpeedButton;
    SpeedButton24: TdxSpeedButton;
    SpeedButton25: TdxSpeedButton;
    SpeedButton26: TdxSpeedButton;
    SpeedButton27: TdxSpeedButton;
    SpeedButton28: TdxSpeedButton;
    SpeedButton29: TdxSpeedButton;
    SpeedButton30: TdxSpeedButton;
    SpeedButton31: TdxSpeedButton;
    SpeedButton32: TdxSpeedButton;
    SpeedButton33: TdxSpeedButton;
    SpeedButton34: TdxSpeedButton;
    SpeedButton35: TdxSpeedButton;
    SpeedButton36: TdxSpeedButton;
    SpeedButton37: TdxSpeedButton;
    SpeedButton48: TdxSpeedButton;
    CapsBtn: TdxSpeedButton;
    SpeedButton53: TdxSpeedButton;
    SpeedButton62: TdxSpeedButton;
    btnCancel: TdxBitButton;
    btnOK: TdxBitButton;
    SpeedButton38: TdxSpeedButton;
    SpeedButton39: TdxSpeedButton;
    SpeedButton40: TdxSpeedButton;
    SpeedButton41: TdxSpeedButton;
    SpeedButton42: TdxSpeedButton;
    SpeedButton43: TdxSpeedButton;
    SpeedButton44: TdxSpeedButton;
    SpeedButton45: TdxSpeedButton;
    SpeedButton46: TdxSpeedButton;
    SpeedButton47: TdxSpeedButton;
    ShiftBtn: TdxSpeedButton;
    CtrlBtn: TdxSpeedButton;
    SpeedButton58: TdxSpeedButton;
    SpeedButton60: TdxSpeedButton;
    SpeedButton63: TdxSpeedButton;
    SpeedButton66: TdxSpeedButton;
    SpeedButton68: TdxSpeedButton;
    SpeedButton69: TdxSpeedButton;
    SpeedButton70: TdxSpeedButton;
    FormPlacement: TFormPlacement;
    LngLabel: TLabel;
    StatusBar: TStatusBar;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CapsBtnClick(Sender: TObject);
    procedure CtrlBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure InEditClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ShiftBtnClick(Sender: TObject);
  private
    OldMessageEvent : TMessageEvent;
    LastLayOutTime  : TDateTime;
    procedure RefreshKeyNames;
    procedure ReadActiveKBDLayout;
    procedure CheckBtnState(Btn_: TdxSpeedButton; State: Boolean);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    { Private declarations }
  public
    SenderForm    : TForm;
    SenderControl : TControl;
    procedure InputText(Sender : TObject);
    { Public declarations }
  end;

var
  KBDForm        : TKBDForm;
  ClickCount     : Integer;
  CurrentLayAut  : String;
  MustRefresh    : Boolean;
  BtnCtrlDown    : Boolean;
  BtnShiftDown   : Boolean;
  BtnCapsDown    : Boolean;

procedure InputText(Sender : TObject);

implementation
uses MainUnit;
{$R *.dfm}


procedure InputText(Sender : TObject);
begin
 if KBDForm = nil then KBDForm := TKBDForm.Create(Application);
 KBDForm.InputText(Sender);
end;

procedure TKBDForm.InputText(Sender : TObject);
var Pctrl : TComponent;
    Cnt   : Integer;
begin
 InEdit.Visible := false;

 if Sender is TEdit then
  begin
   InEdit.Visible      := true;

   InEdit.Text         := TEdit(Sender).Text;
   InEdit.PasswordChar := TEdit(Sender).PasswordChar;
   InEdit.CharCase     := TEdit(Sender).CharCase;
   InEdit.MaxLength    := TEdit(Sender).MaxLength;
   InEdit.Color        := TEdit(Sender).Color;

   Cnt := 0;
   repeat
    Pctrl := TEdit(Sender).Owner;
    if (Pctrl = nil)or(Cnt > 10) then break;
    if Pctrl is TForm then
     begin
      KBDForm.Color := TForm(Pctrl).Color;
      break;
     end;
    Inc(Cnt)
   until false;

   InEdit.Font.Assign(TEdit(Sender).Font);

   KBDForm.ShowModal;
   if KBDForm.ModalResult = mrOK then TEdit(Sender).Text := InEdit.Text;
  end;

 if Sender is TPanel then
  begin
   InEdit.Visible      := true;

   InEdit.Text         := TPanel(Sender).Caption;
   InEdit.PasswordChar := #0;
   InEdit.Color        := TPanel(Sender).Color;

   Cnt := 0;
   repeat
    Pctrl := TPanel(Sender).Owner;
    if (Pctrl = nil)or(Cnt > 10) then break;
    if Pctrl is TForm then
     begin
      KBDForm.Color := TForm(Pctrl).Color;
      break;
     end;
    inc(Cnt)
   until false;

   InEdit.Font.Assign(TPanel(Sender).Font);

   KBDForm.ShowModal;
   if KBDForm.ModalResult = mrOK then TPanel(Sender).Caption := InEdit.Text;
  end;
end;

procedure TKBDForm.SpeedButton1Click(Sender: TObject);
var S : String;
begin
 if Sender is TdxSpeedButton then
  begin
   S:= TSpeedButton(Sender).Hint;
   keybd_event(StrToInt(S),MapVirtualKey(StrToInt(S), 0), 0, 0);
   keybd_event(StrToInt(S),MapVirtualKey(StrToInt(S), 0), KEYEVENTF_KEYUP, 0);
   if BtnShiftDown then
    begin
     ShiftBtnClick(Self);
    end;
  end;
end;

procedure TKBDForm.FormCreate(Sender: TObject);
var I     : Integer;
    Comp_ : TComponent;
begin
 FormPlacement.IniFileName := IniFName;
 LastLayOutTime := 0;
 for I := 0 to Self.ComponentCount -1 do
  begin
   Comp_ := Self.Components[I];
   if Comp_ is TdxSpeedButton then
    begin
     TdxSpeedButton(Comp_).Visible                := false;
     TdxSpeedButton(Comp_).Font.Name              := btnOk.Font.Name;
     TdxSpeedButton(Comp_).Font.Style             := btnOk.Font.Style;
     TdxSpeedButton(Comp_).Font.Charset           := btnOk.Font.Charset;
     TdxSpeedButton(Comp_).Font.Size              := Round(8*1);
     TdxSpeedButton(Comp_).Font.Color             := btnOk.Font.Color;
     TdxSpeedButton(Comp_).Colors.BackgroundFrom  := btnOk.Colors.BackgroundFrom;
     TdxSpeedButton(Comp_).Colors.BackgroundTo    := btnOK.Colors.BackgroundTo;
     TdxSpeedButton(Comp_).Colors.ClickedFrom     := btnOK.Colors.BackgroundTo;
     TdxSpeedButton(Comp_).Colors.ClickedTo       := btnOK.Colors.BackgroundFrom;
     TdxSpeedButton(Comp_).Colors.BorderLine      := 0;
     TdxSpeedButton(Comp_).Visible                := true;
     TdxSpeedButton(Comp_).IsLocked               := false;
    end;
  end;
end;

procedure TKBDForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
var Down : Boolean;
begin
 if Assigned(OldMessageEvent) then OldMessageEvent(Msg,Handled);

 if (Msg.message = WM_KEYDOWN)or(Msg.message = WM_KEYUP) then
  begin
   if Msg.message = WM_KEYDOWN then
    Down := true
   else
    Down := false;
   case Msg.wParam of
   VK_CONTROL:
      begin
       ReadActiveKBDLayout;
       if BtnCtrlDown <> Down then CheckBtnState(CtrlBtn,Down);
       BtnCtrlDown := Down;
       MustRefresh := true;
      end;
   VK_SHIFT:
      begin
       if BtnShiftDown <> Down then CheckBtnState(ShiftBtn,Down);
       BtnShiftDown := Down;
       MustRefresh := true;
      end;
   VK_CAPITAL:
      begin
       if (GetKeyState(VK_CAPITAL) and 1) = 1 then
         Down := true
       else
         Down := False;
       CheckBtnState(CapsBtn,Down);
       MustRefresh := true;
      end;
   end;
  end;

 if MustRefresh then RefreshKeyNames;
 MustRefresh := false;
end;

procedure TKBDForm.RefreshKeyNames;
var I        : Integer;
    KeyCode  : Integer;
    Key_buff : pchar;
    Key_Name : string;
    Key_len  : DWORD;
    KState   : TKeyboardState;
begin
try
 GetKeyboardState(KState);
 for I := 0 to KBDForm.ControlCount - 1 do
  begin
   if KBDForm.Controls[I] is TdxSpeedButton then
    begin
     if (TSpeedButton(KBDForm.Controls[I]).Hint <> '')and
        (TSpeedButton(KBDForm.Controls[I]).Tag < 1) then
      begin
       try
        Key_buff := #0;
        KeyCode := StrToInt(TSpeedButton(KBDForm.Controls[I]).Hint);
        try
         GetMem(Key_buff, 2);
         Key_len := ToAscii(KeyCode,MapVirtualKey(KeyCode,0),KState,Key_buff,0);
         Key_Name := StrPas(Key_buff);
         Key_Name := copy(Key_Name,1,Key_len);
        finally
         FreeMem(Key_buff);
        end;
        TdxSpeedButton(KBDForm.Controls[I]).Caption := Key_Name;
       except
       end;
      end;
    end;
  end;
except
end;
end;

procedure TKBDForm.FormShow(Sender: TObject);
begin
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
 OldMessageEvent := Application.OnMessage;
 Application.OnMessage := AppMessage;
 ReadActiveKBDLayout;
 RefreshKeyNames;
end;

procedure TKBDForm.CheckBtnState(Btn_: TdxSpeedButton; State : Boolean);
begin
  if State then
   begin
    Btn_.HookMouseEnter;
    Btn_.HookMouseDown;
   end
  else
   begin
    Btn_.HookMouseUp;
    Btn_.HookMouseLeave;
   end;
end;

procedure TKBDForm.CapsBtnClick(Sender: TObject);
begin
 keybd_event(VK_CAPITAL,0, 0, 0);
 keybd_event(VK_CAPITAL,0, KEYEVENTF_KEYUP, 0);
end;

procedure TKBDForm.CtrlBtnClick(Sender: TObject);
begin
 keybd_event(VK_CONTROL,0, 0, 0);
 keybd_event(VK_CONTROL,0, KEYEVENTF_KEYUP, 0);
end;

procedure TKBDForm.ReadActiveKBDLayout;
var LayOutName : Pchar;
    S1, S2     : String;
begin
 GetMem(LayOutName, 20);
 try
  GetKeyboardLayoutName(LayOutName);
  S1 :=  StrPas(LayOutName);
 finally
  FreeMem(LayOutName);
 end;

 GetMem(LayOutName, 255);
 try
  VerLanguageName(GetKeyboardLayout(0), LayOutName, 255);
  S2 :=  StrPas(LayOutName);
 finally
  FreeMem(LayOutName);
 end;

 if S1 = '00020402' then LngLabel.Caption := 'BP'
 else
 if S1 = '00000402' then LngLabel.Caption := 'BG'
 else
 LngLabel.Caption := UpperCase(Copy(S2,1,2));
 StatusBar.SimpleText := S2;
 MustRefresh := true;
end;

procedure TKBDForm.FormActivate(Sender: TObject);
begin
 if InEdit.Visible then InEdit.SetFocus;
 InEdit.SelectAll;
end;

procedure TKBDForm.InEditClick(Sender: TObject);
begin
btnOK.Click;
end;

procedure TKBDForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Application.OnMessage := OldMessageEvent;
end;

procedure TKBDForm.ShiftBtnClick(Sender: TObject);
begin
 if GetKeyState(VK_SHIFT) <= -127 then
   keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP, 0)
 else
   keybd_event(VK_SHIFT,0, 0, 0);
 MustRefresh := true;
end;

initialization
 KBDForm := nil;

end.
