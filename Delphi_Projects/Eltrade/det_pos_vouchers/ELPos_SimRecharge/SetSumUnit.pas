unit SetSumUnit;
                                                                                                                                      
interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxCore, dxSpeedButton, ExtCtrls, ConstUnit, StdCtrls, 
  Buttons, Messages, IBSQL, IBDataBase, DBUtilsUnit, CreateFilesUnit;

type
  TSetSumForm = class(TForm)
    pMain: TPanel;
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
    btnBackspace: TdxSpeedButton;
    btnInvoice: TdxSpeedButton;
    btnBon: TdxSpeedButton;
    lSum: TLabel;
    lSumData: TLabel;
    lLimits: TLabel;
    btnCancel: TdxSpeedButton;
    btnMinus: TdxSpeedButton;
    btnPlus: TdxSpeedButton;
    procedure btnPad7Click(Sender: TObject);
    procedure btnBackspaceClick(Sender: TObject);
    procedure btnEscClick(Sender: TObject);
    procedure btnDotClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCancelClick(Sender: TObject);
    procedure btnInvoiceClick(Sender: TObject);
    procedure btnBonClick(Sender: TObject);
  private
    procedure NumPadButtonClick(Btn_: Integer; ToChange: TLabel);
    function DataValidity: boolean;
    function FindButton(KeyCode: Word): TdxSpeedButton;
    procedure PushBtnDown(Btn_: TdxSpeedButton);
    procedure PushBtnUp(Btn_: TdxSpeedButton; CanClick: Boolean=true);
    { Private declarations }
  public
    procedure FitComponents;    
    { Public declarations }
  end;


implementation
uses ResStrUnit, DataUnit, MyMessageUnit, MainUnit, LoginUnit, SetNumberUnit,  SearchCustomerUnit,
     SetPaymentUnit;
{$R *.dfm}

function TSetSumForm.DataValidity: boolean;
var Sum_, DaySum: double;
begin
 Result := False;
 if lSumData.Caption = '' then
  begin
   MyMessageDlg(S_SumMissing, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_SumMissing);
   exit;
  end;
 if (StrToFloatDef(Copy(lSumData.Caption,1,Pos(' ',lSumData.Caption)-1), 0) <= 0) then
  begin
   MyMessageDlg(S_InvalidSumEntNumber, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_InvalidSumEntNumber);
   exit;
  end;
 Sum_ := StrToFloat(Copy(lSumData.Caption,1,Pos(' ',lSumData.Caption)-1));

 if (Sum_ > SimRChObj.FMaxSumTr) and (SimRChObj.FMaxSumTr > 0) then
  begin
   MyMessageDlg(S_SumIsBiggerThanLimit, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_SumIsBiggerThanLimit);
   exit;
  end;
 if (Sum_ < Set_Vivacom_MinAmnt) and (Set_Vivacom_MinAmnt > 0) then
  begin
   MyMessageDlg(S_SumIsSmallerThanLimit, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_SumIsSmallerThanLimit);
   exit;
  end;
 if (Sum_ > Set_Vivacom_MaxAmnt) and (Set_Vivacom_MaxAmnt > 0) then
  begin
   MyMessageDlg(S_SumIsBiggerThanMOLimit, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_SumIsBiggerThanMOLimit);
   exit;
  end;
 DaySum := Sum_;
 if not DataMod.TurnoverForToday(DaySum) then exit;
 if (DaySum > SimRChObj.FMaxSumDay) and (SimRChObj.FMaxSumDay > 0) then
  begin
   MyMessageDlg(S_SumIsBiggerThanDayLimit, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_SumIsBiggerThanDayLimit);
   exit;
  end;
 SimRChObj.FSum := Sum_;

 Result := True;
end;

procedure TSetSumForm.btnPad7Click(Sender: TObject);
begin
 if not (Sender is TdxSpeedButton) then exit;
 try
  NumPadButtonClick(StrToInt(Copy(TdxSpeedButton(Sender).Name,7,1)), lSumData);
 except
 end;
end;

procedure TSetSumForm.NumPadButtonClick(Btn_: Integer; ToChange: TLabel);
var S: string;
    I: integer;
begin
 S := ToChange.Caption;
 S := Copy(S,1,Pos(' ',S)-1);
 I := Pos('.',S);
 if I > 0 then
  begin
   if Length(Copy(S,I+1,Length(S)-I)) >= 2 then exit;
  end;
 ToChange.Caption := S + IntToStr(Btn_) + ' ' + BCurSign;
end;

procedure TSetSumForm.btnBackspaceClick(Sender: TObject);
var S: string;
begin
 try
  if (lSumData.Caption <> '') then
   begin
    S := lSumData.Caption;
    S := Copy(S,1,Pos(' ',S)-2);
    if S <> '' then lSumData.Caption := S+' '+BCurSign
     else lSumData.Caption := '';
   end;

  SimRChObj.FSum := 0;
  if Assigned(MainForm) then MainForm.lSumData.Caption := '';
 except
 end;
end;

procedure TSetSumForm.btnEscClick(Sender: TObject);
begin
 lSumData.Caption := '';
 SimRChObj.FSum := 0;
 if Assigned(MainForm) then MainForm.lSumData.Caption := '';
end;

procedure TSetSumForm.btnDotClick(Sender: TObject);
var S: string;
begin
 if not (Sender is TdxSpeedButton) then exit;
 try
  S := lSumData.Caption;
  S := Copy(S,1,Pos(' ',S)-1);
  lSumData.Caption := S + TdxSpeedButton(Sender).Caption + ' ' + BCurSign;
  if Length(lSumData.Caption) > 20 then lSumData.Caption := '0 ' + BCurSign;
 except
 end; 
end;

procedure TSetSumForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
 MainForm.ScaleControls(Self);
end;

procedure TSetSumForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TSetSumForm.FitComponents;
begin
 Self.WindowState := wsMaximized;
 Application.ProcessMessages;

 pMain.Left := (Self.Width - pMain.Width) div 2;
 pMain.Visible := true;

 btnCancel.Left := Self.ClientWidth -  btnCancel.Width - 10;
 btnCancel.Top  := Self.ClientHeight - btnCancel.Height - 10;
 btnCancel.Visible := true;
 Application.ProcessMessages;
end;

procedure TSetSumForm.PushBtnDown(Btn_ : TdxSpeedButton);
begin
 Btn_.HookMouseEnter;
 Btn_.HookMouseDown;
 if Assigned(Btn_.OnMouseDown) then Btn_.OnMouseDown(Btn_, mbLeft, [], 1, 1);
end;

procedure TSetSumForm.PushBtnUp(Btn_ : TdxSpeedButton; CanClick: Boolean=true);
var IsDn : Boolean;
begin
 IsDn := (dsClicked in Btn_.DrawState);
 if (IsDn)and(Assigned(Btn_.OnMouseUp)) then Btn_.OnMouseUp(Btn_, mbLeft, [], 1, 1);
 Btn_.HookMouseUp;
 Btn_.HookMouseLeave;
 if (IsDn)and(CanClick)and(Assigned(Btn_.OnClick))and(Self.Active) then Btn_.Click;
end;

function TSetSumForm.FindButton(KeyCode: Word): TdxSpeedButton;
begin
 case KeyCode of
 VK_NUMPAD0..VK_NUMPAD9:  Result := TdxSpeedButton(FindComponent(Format('btnPad%d', [Abs(KeyCode - VK_NUMPAD0)])));
 $30..$39:                Result := TdxSpeedButton(FindComponent(Format('btnPad%d', [Abs(KeyCode - $30)])));
 VK_BACK:                 Result := btnBackspace;
 VK_ESCAPE:               Result := btnEsc;
 VK_HOME:                 Result := btnCancel;
 VK_F1:                   Result := btnInvoice;
 VK_F2:                   Result := btnBon;
 else                     Result := nil;
 end;
end;

procedure TSetSumForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnDown(Btn);
end;

procedure TSetSumForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnUp(Btn);
end;

procedure TSetSumForm.btnCancelClick(Sender: TObject);
begin
 DisplayErrMsg('');
 MainForm.ClearInfoPanel(2);
// if MainForm.ChildWindowExist('SetNumberForm', False) then SetNumberForm.FitComponents;
 Self.Close;
end;

procedure TSetSumForm.btnInvoiceClick(Sender: TObject);
begin
 if not (Sender is TdxSpeedButton) then exit;
 if not DataValidity then exit;
 SimRChObj.FDocType := TdxSpeedButton(Sender).Tag;
 SimRChObj.FDocName := TdxSpeedButton(Sender).Caption;
 MainForm.lSumData.Caption := FormatFloat('0.00 ',SimRChObj.FSum)+BCurSign;
 MainForm.lDocData.Caption := SimRChObj.FDocName;
 with TSearchCustomerForm.Create(Self.Owner) do
  begin
   DestObject := MainForm.lCustData;
   FitComponents;
  end;
 DisplayErrMsg('');
end;

procedure TSetSumForm.btnBonClick(Sender: TObject);
begin
 if not (Sender is TdxSpeedButton) then exit;
 if not DataValidity then exit;
 SimRChObj.FDocType := TdxSpeedButton(Sender).Tag;
 SimRChObj.FDocName := TdxSpeedButton(Sender).Caption;
 MainForm.lSumData.Caption := FormatFloat('0.00 ',SimRChObj.FSum)+BCurSign;
 MainForm.lDocData.Caption := SimRChObj.FDocName;

 // печат на бележка за потвърждение на номера
 if Set_PrnPath <> '' then
  begin
   if not Create_Confirm_File(nil, Set_PrnPath, DataMod.PostException) then
    DisplayErrMsg(S_ErrCreatePrnFile);
  end;

 with TSetPaymentForm.Create(Self.Owner) do
  begin
   lSimInfo.Caption := S_Phone   +' '+SimRChObj.FPhoneNumb+sLineBreak+
                       S_Ammount +' '+FormatFloat('0.00', SimRChObj.FSum)+' '+BCurSign;
   FitComponents;
   if not LoadData then Close
    else DisplayErrMsg('');
  end;
end;

end.
