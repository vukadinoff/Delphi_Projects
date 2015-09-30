unit SetPaymentUnit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxCore, dxSpeedButton, ExtCtrls, ConstUnit, StdCtrls, 
  Buttons, Messages, IBSQL, IBDataBase, DBUtilsUnit, CreateFilesUnit,
  ResStrUnit;

type
  TSetPaymentForm = class(TForm)
    pMain: TPanel;
    lTitle: TLabel;
    btnPay1: TdxSpeedButton;
    btnPay2: TdxSpeedButton;
    btnPay3: TdxSpeedButton;
    btnPay4: TdxSpeedButton;
    btnCancel: TdxSpeedButton;
    btnPay5: TdxSpeedButton;
    btnPay6: TdxSpeedButton;
    lSimInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnPay1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnCancelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    function FindButton(KeyCode: Word): TdxSpeedButton;
    procedure PushBtnDown(Btn_: TdxSpeedButton);
    procedure PushBtnUp(Btn_: TdxSpeedButton; CanClick: Boolean=true);
    { Private declarations }
  public
    procedure FitComponents;
    function LoadData: boolean;       
    { Public declarations }
  end;

implementation
uses MyMessageUnit, MainUnit, LoginUnit, DataUnit, SetSumUnit, WaitUnit, SetNumberUnit;

{$R *.dfm}

procedure TSetPaymentForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
 MainForm.ScaleControls(Self);
end;

procedure TSetPaymentForm.FitComponents;
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

function TSetPaymentForm.LoadData: boolean;
var ibSQL: TIBSQL;
    Tr   : TIBTransaction;
    I    : integer;
    Cmp  : TComponent;
begin
 Result := False;
 ibSQL := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(ibSQl, Tr, DataMod.aIBDatabase);
   if not DB_ExecuteSQL('SELECT * FROM N_PAYMENT_TYPES ORDER BY PAYTYPE_NUMB', ibSQL, False, 'GetPays') then raise EAbort.Create('');
   I := 1;
   while not ibSQL.Eof do
    begin
     if (Pos(','+ibSQL.FieldByName('PAYTYPE_NUMB').AsString+',', Set_AllowedPayTypes) > 0)or
        (Pos(',0,', Set_AllowedPayTypes) > 0) then
      begin
       Cmp := Self.FindComponent('btnPay'+IntToStr(I));
       if Cmp is TdxSpeedButton then
        with TdxSpeedButton(Cmp) do
         begin
          Caption := ibSQL.FieldByName('PAYTYPE_NAME').AsString;
          Tag     := ibSQL.FieldByName('PAYTYPE_NUMB').AsInteger;
          Hint    := FormatFloat('0.00',ibSQL.FieldByName('PAYTYPE_COURCE').AsFloat);
          Visible := True;
         end;
       Inc(I);
      end;
     ibSQL.Next;
    end;

   Result := True;
  except
  on E: Exception do
   begin
    DataMod.PostException('Fail loading pay types: '+Trim(E.Message));
    DisplayErrMsg('Fail loading pay types.');
   end; 
  end;
 finally
  DB_DestroyIBSQL(ibSQL, Tr);
 end;
end;

procedure TSetPaymentForm.btnPay1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if not (Sender is TdxSpeedButton) then exit;
 SimRChObj.FPayType  := TdxSpeedButton(Sender).Tag;
 SimRChObj.FPayName  := TdxSpeedButton(Sender).Caption;
 SimRChObj.FPayCource:= StrToFloat(TdxSpeedButton(Sender).Hint);
 if Assigned(MainForm) then MainForm.lPayData.Caption := SimRChObj.FPayName;

 if not DataMod.GetPayPrnNumbAndName(SimRChObj.FPayType, SimRChObj.FPayPrnNumb, SimRChObj.FPayPrnName) then
  begin
    SimRChObj.FPayPrnNumb := SimRChObj.FPayType;
    SimRChObj.FPayPrnName := SimRChObj.FPayName;
  end;

 with TWaitForm.Create(Self.Owner) do
 try
  lMessage.Caption := S_RechargeSim;
  FitComponents;

  // send phone nuber and ammount to the server of Vivacom
  if not DataMod.PayPrePaid(NormalizeMSISDN(SimRChObj.FPhoneNumb), SimRChObj.FSum) then Exit;
  if DataMod.SaveSale then
   begin
    DisplayErrMsg('');
    MainForm.ClearInfoPanel(1);
    MainForm.ChildWindowExist('SetNumberForm',      cwaClose);
    MainForm.ChildWindowExist('SetSumForm',         cwaClose);
    MainForm.ChildWindowExist('SearchCustomerForm', cwaClose);
    MainForm.ChildWindowExist('CustomersForm',      cwaClose);
    MainForm.ChildWindowExist('CustomerDataForm',   cwaClose);

    if ParamCount < 2 then
     begin
      DataMod.LogOut;
      if not MainForm.ChildWindowExist('LoginForm', cwaShow) then
       with TLoginForm.Create(Self.Owner) do FitComponents;
     end
    else
     PostMessage(Application.Handle, WM_CLOSE, 0, 0);

     
   end;
 finally
  Close;
 end;
end;

procedure TSetPaymentForm.btnCancelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 DisplayErrMsg('');
 MainForm.ClearInfoPanel(3);

// if MainForm.ChildWindowExist('SetSumForm', False) then SetSumForm.FitComponents;
 Self.Close;
end;

procedure TSetPaymentForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TSetPaymentForm.PushBtnDown(Btn_ : TdxSpeedButton);
begin
 Btn_.HookMouseEnter;
 Btn_.HookMouseDown;
 if Assigned(Btn_.OnMouseDown) then Btn_.OnMouseDown(Btn_, mbLeft, [], 1, 1);
end;

procedure TSetPaymentForm.PushBtnUp(Btn_ : TdxSpeedButton; CanClick: Boolean=true);
var IsDn : Boolean;
begin
 IsDn := (dsClicked in Btn_.DrawState);
 if (IsDn)and(Assigned(Btn_.OnMouseUp)) then Btn_.OnMouseUp(Btn_, mbLeft, [], 1, 1);
 Btn_.HookMouseUp;
 Btn_.HookMouseLeave;
 if (IsDn)and(CanClick)and(Assigned(Btn_.OnClick))and(Self.Active) then Btn_.Click;
end;

function TSetPaymentForm.FindButton(KeyCode: Word): TdxSpeedButton;
begin
 case KeyCode of
 VK_ESCAPE:               Result := btnCancel;
 VK_HOME:                 Result := btnCancel;
 VK_F1:                   Result := btnPay1;
 VK_F2:                   Result := btnPay2;
 VK_F3:                   Result := btnPay3;
 VK_F4:                   Result := btnPay4;
 VK_F5:                   Result := btnPay5;
 VK_F6:                   Result := btnPay6;
 else                     Result := nil;
 end;
end;

procedure TSetPaymentForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnDown(Btn);
end;

procedure TSetPaymentForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnUp(Btn);
end;

end.
