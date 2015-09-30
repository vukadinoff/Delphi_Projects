unit SetNumberUnit;
                                                                                                                                       
interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxCore, dxSpeedButton, ExtCtrls, ConstUnit, StdCtrls, 
  Buttons, Messages, Mask, IBSQL, IBDataBase, DBUtilsUnit;

const PhoneMask = '359 ___ ___ ___';

type
  TSetNumberForm = class(TForm)
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
    btnEsc: TdxSpeedButton;
    btnBackspace: TdxSpeedButton;
    btnMTel: TdxSpeedButton;
    btnVivacom: TdxSpeedButton;
    lNumber: TLabel;
    lNumberData: TLabel;
    btnCancel: TdxSpeedButton;
    btnDot: TdxSpeedButton;
    btnPlus: TdxSpeedButton;
    btnMinus: TdxSpeedButton;
    procedure btnPad7Click(Sender: TObject);
    procedure btnBackspaceClick(Sender: TObject);
    procedure btnEscClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnVivacomClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure NumPadButtonClick(Btn_: Integer; ToChange: TLabel);
    function DataValidity: boolean;
    function FoundPluInDB(MobOperNumb: integer): boolean;
    procedure PushBtnDown(Btn_: TdxSpeedButton);
    procedure PushBtnUp(Btn_: TdxSpeedButton; CanClick: Boolean=true);
    function FindButton(KeyCode: Word): TdxSpeedButton;
    { Private declarations }
  public
    procedure FitComponents;    
    { Public declarations }
  end;

implementation

uses ResStrUnit, MyMessageUnit, MainUnit, LoginUnit, SetSumUnit, DataUnit, WaitUnit;

{$R *.dfm}

function TSetNumberForm.DataValidity: boolean;
begin
 Result := False;
 // мое валидиране
 if (lNumberData.Caption = '') or (lNumberData.Caption = PhoneMask) then
  begin
   MyMessageDlg(S_PhoneMissing, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_PhoneMissing);
   exit;
  end;
 if Pos('_',lNumberData.Caption) > 0 then
  begin
   MyMessageDlg(S_InvalidPhone, mtWarning, [mbOK], 0);
   DisplayErrMsg(S_InvalidPhone);
   exit;
  end;
 SimRChObj.FPhoneNumb := lNumberData.Caption;

 Result := True;
end;

procedure TSetNumberForm.btnPad7Click(Sender: TObject);
begin
 if not (Sender is TdxSpeedButton) then exit;
 try
  NumPadButtonClick(StrToInt(Copy(TdxSpeedButton(Sender).Name,7,1)), lNumberData);
 except
 end;
end;

procedure TSetNumberForm.NumPadButtonClick(Btn_: Integer; ToChange: TLabel);
var I: integer;
    S: string;
begin
 if (Length(ToChange.Caption) <= 0) then exit;
 S := ToChange.Caption;
 I := Pos('_', S);
 if I > 0 then
  begin
   Delete(S,1,I);
   ToChange.Caption := Copy(ToChange.Caption,1,I-1)+IntToStr(Btn_)+S;
  end; 
end;

procedure TSetNumberForm.btnBackspaceClick(Sender: TObject);
var S, S1, S2, PhonePrefix: string;
    I: integer;
begin
 if not (Sender is TdxSpeedButton) then exit;
 if (Length(lNumberData.Caption) <= 0) or
    (lNumberData.Caption = PhoneMask) then exit;
 try
  PhonePrefix := Copy(PhoneMask,1,4);
  S := '';   S1 := '';   S2 := '';
  S := lNumberData.Caption;
  Delete(S,1,Length(PhonePrefix));
  for I := 1 to Length(S) do
   begin
    if (S[I] <> '_') then
      S1 := S1+S[I]
    else
     begin
      S2 := Copy(S,I,Length(S)-I+1);
      break;
     end;
   end;
  if S1[Length(S1)] = ' ' then
   begin
    Delete(S1,Length(S1)-1,2);
    S1 := S1+'_ ';
   end
  else
   begin
    Delete(S1,Length(S1),1);
    S1 := S1+'_';
   end;
  lNumberData.Caption := PhonePrefix+S1+S2;

  SimRChObj.FPhoneNumb := '';
  if Assigned(MainForm) then MainForm.lPhoneData.Caption := '';
 except
 end; 
end;

procedure TSetNumberForm.btnEscClick(Sender: TObject);
begin
 lNumberData.Caption := PhoneMask;
 SimRChObj.FPhoneNumb := '';
 if Assigned(MainForm) then MainForm.lPhoneData.Caption := '';
end;

procedure TSetNumberForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then
  LoadControlsStrings(LocalPath + LngFName, Self);

 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
 MainForm.ScaleControls(Self);
end;

procedure TSetNumberForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TSetNumberForm.FitComponents;
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

function TSetNumberForm.FoundPluInDB(MobOperNumb: integer): boolean;
var ibSQL : TIBSQL;
    Tr    : TIBTransaction;
    SQL   : String;
begin
 Result := False;
 ibSQL := nil;   Tr := nil;
 try
  try
   DB_CreateIBSQL(ibSQl, Tr, DataMod.aIBDatabase);

   SQL := 'SELECT P.PLU_NUMB, P.PLU_ECR_NAME, P.PLU_NAME, P.PLU_MINQUANT, P.PLU_MAXQUANT, '+
          'P.PLU_TAXGROUP_ID, V.TAXGRP_WAT, P.PLU_SELL_DISABLED_ '+
          'FROM PLUES P '+
          'LEFT JOIN N_WAT V ON P.PLU_TAXGROUP_ID = V.TAXGRP_NUMB '+
          'WHERE P.PLU_NUMB = ';

   case MobOperNumb of
   C_MTel     : SQL := SQL + IntToStr(Set_Mtel_PluNumb);
   C_Globul   : SQL := SQL + IntToStr(Set_Globul_PluNumb);
   C_Vivacom  : SQL := SQL + IntToStr(Set_Vivacom_PluNumb);
   end;

   if not DB_ExecuteSQL(SQL, ibSQL, False, 'FindPLU') then raise EAbort.Create('Fail get PLU');
   if ibSQL.RecordCount = 0 then raise EAbort.Create(S_PluNotFound);

   if (ibSQL.FieldByName('PLU_NUMB').IsNull)or
      (ibSQL.FieldByName('PLU_NUMB').AsInteger <= 0) then
    raise EAbort.Create(S_PluNotFound);



   SimRChObj.FPluNumb    := ibSQL.FieldByName('PLU_NUMB').AsInteger;
   SimRChObj.FMobOper    := MobOperNumb;
   SimRChObj.FPluName    := Copy(ibSQL.FieldByName('PLU_NAME').AsString, 1, 255);
   SimRChObj.FVat        := ibSQL.FieldByName('TAXGRP_WAT').AsFloat;
   SimRChObj.FTaxGroup   := ibSQL.FieldByName('PLU_TAXGROUP_ID').AsInteger;
   SimRChObj.FMaxSumTr   := ibSQL.FieldByName('PLU_MINQUANT').AsFloat;
   SimRChObj.FMaxSumDay  := ibSQL.FieldByName('PLU_MAXQUANT').AsFloat;

   if SimRChObj.FMaxSumTr < 0 then SimRChObj.FMaxSumTr := 0;
   if SimRChObj.FMaxSumDay < 0 then SimRChObj.FMaxSumDay := 0;

//   SimRChObj.FBuyPr      := ibSQL.FieldByName('PLU_BUY_PRICE').AsFloat;
//   SimRChObj.FBuyCurrID  := ibSQL.FieldByName('PLU_BUYCURR_ID').AsInteger;
//   SimRChObj.FBuyCCource := ibSQL.FieldByName('BUYCCOURCE').AsFloat;
//   SimRChObj.FSellPr     := ibSQL.FieldByName('PLU_SELL_PRICE').AsFloat;
//   SimRChObj.FSellCurrID := ibSQL.FieldByName('PLU_SELLCURR_ID').AsInteger;
//   SimRChObj.FSellCCource:= ibSQL.FieldByName('SELLCCOURCE').AsFloat;

   Result := True;

   if ibSQL.FieldByName('PLU_SELL_DISABLED_').AsInteger=0 then
    begin
     if not DB_ExecuteSQL('UPDATE PLUES SET PLUES.PLU_SELL_DISABLED_=1 WHERE PLUES.PLU_NUMB='+ IntToStr(SimRChObj.FPluNumb),
                          ibSQL, true, 'Update plues set disable.')
      then DataMod.PostException('Error on set forbidden for sale for plue '+ IntToStr(SimRChObj.FPluNumb));

    end;
  except
  on E: Exception do
   begin
    if E.Message <> '' then DataMod.PostException(S_ErrSearchPlu+': '+E.Message);
    MyMessageDlg(S_UnsuccSearchPlu+sLineBreak+S_SeeErrForDetails, mtError, [mbOK], 0);
    DisplayErrMsg(S_UnsuccSearchPlu);    
   end;
  end;
 finally
  DB_DestroyIBSQL(ibSQL, Tr);
 end;
end;

procedure TSetNumberForm.PushBtnDown(Btn_ : TdxSpeedButton);
begin
 Btn_.HookMouseEnter;
 Btn_.HookMouseDown;
 if Assigned(Btn_.OnMouseDown) then Btn_.OnMouseDown(Btn_, mbLeft, [], 1, 1);
end;

procedure TSetNumberForm.PushBtnUp(Btn_ : TdxSpeedButton; CanClick: Boolean=true);
var IsDn : Boolean;
begin
 IsDn := (dsClicked in Btn_.DrawState);
 if (IsDn)and(Assigned(Btn_.OnMouseUp)) then Btn_.OnMouseUp(Btn_, mbLeft, [], 1, 1);
 Btn_.HookMouseUp;
 Btn_.HookMouseLeave;
 if (IsDn)and(CanClick)and(Assigned(Btn_.OnClick))and(Self.Active) then Btn_.Click;
end;

function TSetNumberForm.FindButton(KeyCode: Word): TdxSpeedButton;
begin
 case KeyCode of
 VK_NUMPAD0..VK_NUMPAD9:  Result := TdxSpeedButton(FindComponent(Format('btnPad%d', [Abs(KeyCode - VK_NUMPAD0)])));
 $30..$39:                Result := TdxSpeedButton(FindComponent(Format('btnPad%d', [Abs(KeyCode - $30)])));
 VK_BACK:                 Result := btnBackspace;
 VK_ESCAPE:               Result := btnEsc;
 VK_HOME:                 Result := btnCancel;
 VK_F1:                   Result := btnMTel;
 VK_F2:                   Result := btnVivacom;
 else                     Result := nil;
 end;
end;

procedure TSetNumberForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnDown(Btn);
end;

procedure TSetNumberForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var Btn : TdxSpeedButton;
begin
 Btn := FindButton(Key);
 if (Btn <> nil) then PushBtnUp(Btn);
end;

procedure TSetNumberForm.btnVivacomClick(Sender: TObject);
var S: string;
begin
 if not DataValidity then Exit;
 if not FoundPluInDB(TdxSpeedButton(Sender).Tag) then exit;

 with TWaitForm.Create(Application) do
 try
  lMessage.Caption := S_ConnectingToMobOpr;
  FitComponents;
  if not DataMod.CheckPrePaid(NormalizeMSISDN(SimRChObj.FPhoneNumb)) then Exit;
 finally
  Close;
 end;

 case SimRChObj.FMobOper of
  1: S := C_MTelName;
  2: S := C_GlobulName;
  3: S := C_VivacomName;
 else S := '';
 end;
 MainForm.lMobOperData.Caption:= S;
 MainForm.lPhoneData.Caption  := SimRChObj.FPhoneNumb;
 S := DataMod.LimitsInfo;

 if not MainForm.ChildWindowExist('SetSumForm', cwaShow) then
  with TSetSumForm.Create(Application) do
   begin
    lLimits.Caption := S;
    FitComponents;
   end;

 DisplayErrMsg('');
end;

procedure TSetNumberForm.btnCancelClick(Sender: TObject);
begin
 if ParamCount < 2 then
  begin
   DataMod.LogOut;
   DisplayErrMsg('');
   MainForm.ClearInfoPanel(1);

   if not MainForm.ChildWindowExist('LoginForm', cwaShow) then
     with TLoginForm.Create(Self.Owner) do FitComponents;
  end
 else
  PostMessage(Application.Handle, WM_CLOSE, 0, 0);

 Self.Close;
end;

end.

