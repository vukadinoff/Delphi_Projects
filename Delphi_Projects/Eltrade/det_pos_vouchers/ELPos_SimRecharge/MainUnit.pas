unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ConstUnit, ComCtrls, DBUtilsUnit, KeyStateUnit, ImgList, AboutDlg,
  Placemnt, ESKTypesUnit, ESKUnit, EboTaskManagerUnit, dxCore, dxSpeedButton, RXDBCtrl;

var Title : TAboutBox;

type
  TChildWndAction = (cwaNone, cwaShow, cwaClose);

  TMainForm = class(TForm)
    StatusTimer: TTimer;
    ImageList: TImageList;
    ClockTimer: TTimer;
    ESKLock: TESKLock;
    StatusBar: TStatusBar;
    btnClose: TdxSpeedButton;
    pTitle: TPanel;
    Bevel4: TBevel;
    pInfo: TPanel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel5: TBevel;
    lErr: TLabel;
    Bevel6: TBevel;
    pInfo1: TPanel;
    lOper: TLabel;
    lOperData: TLabel;
    pInfo2: TPanel;
    lMobOper: TLabel;
    lMobOperData: TLabel;
    lPhone: TLabel;
    lPhoneData: TLabel;
    pInfo3: TPanel;
    lSumData: TLabel;
    lSum: TLabel;
    lDocData: TLabel;
    lCustData: TLabel;
    pInfo4: TPanel;
    lPay: TLabel;
    lPayData: TLabel;
    Bevel7: TBevel;
    EltradeImage: TImage;
    Bevel8: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StatusTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClockTimerTimer(Sender: TObject);
    procedure pInfo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pInfo2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pInfo3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ESKLockEndWork(Sender: TObject);
    procedure pInfo4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    TaskMan      : TEBO_TaskManager;
    MouseDn      : TPoint;
    KeyState     : TKeyState;

    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DisplayErrors;
    function WindowsExit: boolean;
    procedure RegisterModule(ModilesString: String);
  public
    procedure ScaleControls(Source_: TForm);
    function ChildWindowExist(Name: TComponentName; Action: TChildWndAction=cwaNone): Boolean;
    procedure ClearInfoPanel(Sender: byte);      
  end;

var
  MainForm: TMainForm;


implementation

uses DataUnit, KBDUnit, ErrorsUnit, RegistrationUnit, ResStrUnit,
     LoginUnit, MyMessageUnit, SetNumberUnit, SetSumUnit, SetPaymentUnit,
     WaitUnit;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
 DecimalSeparator      := '.'; // init for local use
 Self.Caption          := C_AppTitle;;
 CurrentModuleName     := S_CurrentModulName;
 Application.OnMessage := AppMessage;

 KeyState := TKeyState.Create;
 TaskMan  := TEBO_TaskManager.Create(true, true);
end;

procedure TMainForm.FormShow(Sender: TObject);
var I: Integer;
begin
 if not LoadSettings then
  begin
   DataMod.PostException('Error loading POS settings file.'+
                         ' Missing files '+LocalPath+C_POSIniFileName+'; '+LocalPath+C_ConfigFileName);
   MyMessageDlg(S_SettingsNotLoaded, mtError, [mbOK], 0);
   Application.Terminate;
   Exit;
  end;

 if not DataMod.LoadDatabaseConfig then
  begin
   DataMod.PostException('Error loading database config file. Check: '+C_DatabaseIniFName);
   MyMessageDlg(S_SettingsNotLoaded, mtError, [mbOK], 0);
   Application.Terminate;
   Exit;
  end;

 if not DataMod.ConnectToDatabase then
  begin
   DataMod.PostException('Fail connecting database: '+DataMod.aIBDatabase.DatabaseName);
   MyMessageDlg(S_DBErrorConnect, mtError, [mbOK], 0);
   Application.Terminate;
   Exit;
  end;

 if not DataMod.GetNomeclatures then
  begin
   MyMessageDlg(S_ErrGetSysNomecl, mtError, [mbOK], 0);
   Application.Terminate;
   exit;
  end;

 ESKLock.Active := True;

// screen resolution
// 0 - 800x600
// 1 - 1024x768
// 2 - 1152x864
// 3 - 1024x600 (широк екран 16:9)
// 4 - 1280x720 (широк екран 16:9)
// 5 - 1365x768 (широк екран 16:9)
// 6 - 1536x864 (широк екран 16:9)
// 7 - 1280x800 (широк екран 16:10)
// 8 - с резолюцията на екрана

  case Set_Resolution of
  0  : Self.Width := 800;
  1,3: Self.Width := 1024;
  2  : Self.Width := 1152;
  4,7: Self.Width := 1280;
  5  : Self.Width := 1365;
  6  : Self.Width := 1536;
  else Self.Width := Screen.Width;
  end;

  case Set_Resolution of
  0,3: Self.Height := 600;
  1,5: Self.Height := 768;
  2,6: Self.Height := 864;
  4  : Self.Height := 720;
  7  : Self.Height := 800;
  else Self.Height := Screen.Height;
  end;

 case Set_Resolution of
  0,3  : ScaleFactor := 1;
  1,5  : ScaleFactor := 1.28;
  2,6  : ScaleFactor := 1.44;
  4    : ScaleFactor := 1.2;
  7    : ScaleFactor := 800/600;
 else ScaleFactor := (Screen.Height / 600);
 end;

 Self.Left  := 0;
 Self.Top   := 0;
 ScaleControls(Self);

 // Format status bar
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;

 if ScaleFactor > 1.1 then
  begin
   for I := 0 to StatusBar.Panels.Count -1 do
    StatusBar.Panels[I].Width := Round(StatusBar.Panels[I].Width * ScaleFactor);
   StatusBar.Font.Size := Round(StatusBar.Font.Size * ScaleFactor);
  end;

 if not ChildWindowExist('LoginForm', cwaShow) then
  with TLoginForm.Create(Self) do
   begin
    FitComponents;
    if ParamStr(1) <> '' then eUserName.Text := ParamStr(1);
    if ParamStr(2) <> '' then ePass.Text     := ParamStr(2);
    if ParamStr(2) <> '' then DoLogin;
   end;

//   SetCurrentOperData(StrToInt(ParamStr(1)), DataMod.GetOperName(StrToInt(ParamStr(1))));
//   if not ChildWindowExist('SetNumberForm', cwaShow) then
//    with TSetNumberForm.Create(Self) do FitComponents;

 if Assigned(Title) then FreeAndNil(Title);

 StatusTimer.Enabled   := True;
 ClockTimer.Enabled    := True;
 StatusTimer.Interval  := 100;
 ClockTimer.Interval   := 100;

 ShowCursor(not Set_HideMouse);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 StatusTimer.Enabled:= False;
 ClockTimer.Enabled := False;

 Application.ProcessMessages; // иначе не се усеща че една от формите е затворена вече 
 while Self.MDIChildCount > 0 do
  begin
   Self.MDIChildren[0].Close;
   Application.ProcessMessages;
  end;

 ShowCursor(True);

 KeyState.Free;
 TaskMan.Free;

 if (ParamCount = 0) and Set_ShutDownPC then WindowsExit;
 Action := caFree;
end;

procedure TMainForm.ScaleControls(Source_: TForm);
var I     : Integer;
    Comp_ : TComponent;
begin
if ScaleFactor <= 0 then Exit;
for I := 0 to Source_.ComponentCount-1 do
 begin
  Comp_ := Source_.Components[I];
  if Comp_ is TControl then
   begin
    case TControl(Comp_).Align of
    alLeft,alRight:
     begin
      if not (Comp_ is TBevel) then TControl(Comp_).Width  := Round(TControl(Comp_).Width  * ScaleFactor);
     end;
    alTop,alBottom:
     begin
      if not (Comp_ is TBevel) then TControl(Comp_).Height := Round(TControl(Comp_).Height * ScaleFactor);
     end;
    alNone:
     begin
      TControl(Comp_).Left   := Round(TControl(Comp_).Left * ScaleFactor);
      TControl(Comp_).Top    := Round(TControl(Comp_).Top  * ScaleFactor);
      TControl(Comp_).Width  := Round(TControl(Comp_).Width  * ScaleFactor);
      TControl(Comp_).Height := Round(TControl(Comp_).Height * ScaleFactor); 
     end;
    end;
   end;

  if (Comp_ is TdxSpeedButton) then TdxSpeedButton(Comp_).HookResized;

  if Comp_ is TdxSpeedButton then TdxSpeedButton(Comp_).Font.Size := Round(TdxSpeedButton(Comp_).Font.Size * ScaleFactor)
  else
  if Comp_ is TPanel         then TPanel(Comp_).Font.Size         := Round(TPanel(Comp_).Font.Size * ScaleFactor)
  else
  if Comp_ is TLabel         then TLabel(Comp_).Font.Size         := Round(TLabel(Comp_).Font.Size * ScaleFactor)
  else
  if Comp_ is TListBox       then TListBox(Comp_).Font.Size       := Round(TListBox(Comp_).Font.Size * ScaleFactor)
  else
  if Comp_ is TMemo          then TMemo(Comp_).Font.Size          := Round(TMemo(Comp_).Font.Size * ScaleFactor)
  else
  if Comp_ is TEdit          then TEdit(Comp_).Font.Size          := Round(TEdit(Comp_).Font.Size * ScaleFactor)
 end;
end;

function TMainForm.WindowsExit: boolean;
var hToken  : THandle;
    tpResult: Boolean;
    TokenPvg, rTokenPvg: TTokenPrivileges;
    pcbtpPreviousRequired: DWORD;
    cbtpPrevious: DWORD;
begin
 if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
   if (OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken)) then
    begin
     tpResult := LookupPrivilegeValue(nil, 'SeShutdownPrivilege', TokenPvg.Privileges[0].Luid);
     TokenPvg.PrivilegeCount := 1;
     TokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
     cbtpPrevious := SizeOf(rTokenPvg) ;
     pcbtpPreviousRequired := 0;
     if tpResult then
       Windows.AdjustTokenPrivileges(hToken,
                                     False,
                                     TokenPvg,
                                     cbtpPrevious,
                                     rTokenPvg,
                                     pcbtpPreviousRequired);
    end;
  end;
 Result := ExitWindowsEx(EWX_SHUTDOWN or EWX_FORCE, 0); 
end;

procedure TMainForm.StatusTimerTimer(Sender: TObject);
var ACount : Integer;
    I      : integer;
    S      : String;
begin
 StatusTimer.Enabled := False;
 StatusTimer.Interval:= 3000;
 
 // UPDATE ERROR ICON
 if FileExists(ExceptPath + C_ErrFName) then StatusBar.Panels[5].Text := '1'
  else StatusBar.Panels[5].Text := '0';

 if KeyState.NumLock then StatusBar.Panels[2].Alignment := taCenter
  else StatusBar.Panels[2].Alignment := taLeftJustify;

 if KeyState.CapsLock then StatusBar.Panels[3].Alignment := taCenter
  else StatusBar.Panels[3].Alignment := taLeftJustify;

 // UPDATE DATABASE STATUS
 if DataMod.DBConnected then
  begin
   ACount := 0;
   for I := 0 to DataMod.aIBDatabase.TransactionCount-1 do
    begin
     if (DataMod.aIBDatabase.Transactions[I] <> nil)and(DataMod.aIBDatabase.Transactions[I].InTransaction) then Inc(ACount);
    end;

   if (DataMod.ServerAddress <> '') then S := S_Server + ': ' + DataMod.ServerAddress
    else S := S_Server + ': ' + S_Local;

    S := S + '; Trs: ' + IntToStr(DataMod.aIBDatabase.TransactionCount)+'('+IntToStr(ACount)+')';
  end
 else
  S := S_Server + ': ' + S_NoConnection;

 StatusBar.Panels[0].Text := S;

 I := Round(TaskMan.ProcessCount * 18 * ScaleFactor);
 if I > 0 then I := I + Round(4 * ScaleFactor);
 StatusBar.Panels[6].Width := I;

 StatusTimer.Enabled := True;
end;

procedure TMainForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
var Mse : TPoint;
begin
 case Msg.Message of
 WM_KEYDOWN,
 WM_SYSKEYDOWN:  begin
                   case Msg.wParam of
                   VK_NUMLOCK:
                      begin
                       if KeyState.NumLock then StatusBar.Panels[2].Alignment := taCenter
                        else StatusBar.Panels[2].Alignment := taLeftJustify;
                      end;
                   VK_CAPITAL:
                      begin
                       if KeyState.CapsLock then StatusBar.Panels[3].Alignment := taCenter
                        else StatusBar.Panels[3].Alignment := taLeftJustify;
                      end;
                   end;
                 end;
 WM_KEYUP,
 WM_SYSKEYUP:    begin
                   if Msg.wParam = VK_CONTROL then
                    begin
                     StatusBar.Panels[4].Text := KeyState.ChangeActiveLayOutPOS;
                     StatusBar.Repaint;
                    end;
                 end;
 WM_LBUTTONDOWN: begin
                  GetCursorPos(MouseDn);
                 end;
 WM_LBUTTONUP:   begin
                  if (MouseDn.X > 0) and (MouseDn.Y > 0) then
                   begin
                    GetCursorPos(Mse);
                    if (Abs(MouseDn.Y - Mse.Y) < 50) and (Abs(MouseDn.X - Mse.X) > 200) then
                     begin  // Местене на екран с мишката
                      if TaskMan.ProcessCount > 1 then
                       begin
                        if MouseDn.X - Mse.X > 0 then TaskMan.ActivatePrevApp
                         else TaskMan.ActivateNextApp;
                       end;
                      Msg.Message := 0;
                     end;
                   end;
                  MouseDn.X := 0; MouseDn.Y := 0;
                 end;
 CM_ACTIVATE:
                 begin
                  // за обнови по-бързо сътоянието на бутоните НУМЛОК
                  StatusTimer.Interval:= 400;
                 end;
 end;
end;

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var ARect: TRect;
    I    : Integer;
    HH   : Integer;
    WW   : Integer;
begin
 ARect := Rect;

 with StatusBar.Canvas do
  case Panel.Index of
  2, 3, 4:  //  NumLock, CapsLock, EN/BG
    begin
     Brush.Color := clBlack;
     FillRect(ARect);
     InflateRect(ARect,-1,-1);
     Brush.Color := StatusBar.Color;
     Font.Style := [];
     case Panel.Index of
     2,3:begin
          if Panel.Alignment = taCenter then Brush.Color := clLime;
         end;
     4:  begin
          if Panel.Text <> 'EN' then Brush.Color := clLime;
          Font.Style := [fsBold];
         end;
     end;
     FillRect(ARect);
     HH := (Arect.Bottom - Arect.Top - TextHeight(Panel.Text)) div 2;
     WW := (Panel.Width - TextWidth(Panel.Text) - 4) div 2;
     TextOut(ARect.Left+WW, ARect.Top+HH, Panel.Text);
    end;
  5:begin // Error Icon
     HH := ((Arect.Bottom - Arect.Top) - ImageList.Height) div 2;
     ImageList.Draw(StatusBar.Canvas, ARect.Left, Arect.Top + HH, StrToIntDef(Panel.Text, 0), True);
    end;
  6:begin // Application icons
     Brush.Color := StatusBar.Color;
     for I := 0 to TaskMan.ProcessCount - 1 do
      begin
       HH := (Arect.Bottom - Arect.Top - 16) div 2;
       WW := ((ARect.Right - ARect.Left) div TaskMan.ProcessCount) * I;
       if TaskMan.Processes[I].Icon <> nil then
        DrawIconEx(StatusBar.Canvas.Handle, ARect.Left+WW, ARect.Top+HH, TaskMan.Processes[I].Icon.Handle, 16, 16, 0, Brush.Handle, DI_NORMAL);
      end;
    end;
  7:begin // DateTime
     Brush.Color := StatusBar.Color;
     Font.Style  := [fsBold];
     HH := ((Arect.Bottom - Arect.Top) - TextHeight(Panel.Text)) div 2;
     TextOut(ARect.Right-TextWidth(Panel.Text)-3, ARect.Top + HH ,Panel.Text);
    end;
 end;
end;

procedure TMainForm.StatusBarMouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var L  : Integer;
    Li : Integer;
begin
 with StatusBar do
  begin
   L := Panels[0].Width + Panels[1].Width;
   if (X > L) and (X < L + Panels[2].Width) then
    begin // Panels[2] - NumLock
      if Panels[2].Alignment = taCenter then Panels[2].Alignment := taLeftJustify
       else Panels[2].Alignment := taCenter;
      KeyState.NumLock := (Panels[2].Alignment = taCenter);
    end;
   L := L + Panels[2].Width;
   if (X > L) and (X < L + Panels[3].Width) then
    begin // Panels[3] - CapsLock
      if Panels[3].Alignment = taCenter then Panels[3].Alignment := taLeftJustify
       else Panels[3].Alignment := taCenter;
      KeyState.CapsLock := (Panels[3].Alignment = taCenter);
    end;
   L := L + Panels[3].Width;
   if (X > L) and (X < L + Panels[4].Width) then
    begin // Panels[4] - EN/BG
     keybd_event(VK_CONTROL,0, 0, 0);
     keybd_event(VK_CONTROL,0, KEYEVENTF_KEYUP, 0);
    end;
   L := L + Panels[4].Width;
   if (X > L) and (X < L + Panels[5].Width) then
    begin // Panels[5] - ErrorIcon
     DisplayErrors;
    end;
   L := L + Panels[5].Width;
   if (X > L) and (X < L + Panels[6].Width) then
    begin // Panels[6] - TaskManIcons
     if TaskMan.ProcessCount > 0 then
      begin
       Li := (Panels[6].Width div TaskMan.ProcessCount);
       Li := (X-L) div Li;
       if TaskMan.Processes[Li] <> nil then TaskMan.Processes[Li].ActivateApp;
      end;
    end;
  end;
end;

procedure TMainForm.ClockTimerTimer(Sender: TObject);
begin
 ClockTimer.Enabled := False;
 ClockTimer.Interval:= 1000;

 StatusBar.Panels[4].Text := KeyState.GetKeyboardLayOutPOS(False);
 StatusBar.Panels[7].Text := FormatDateTime('DD.MM.YYYY  HH:NN',Now);

 ClockTimer.Enabled := True;
end;

procedure TMainForm.DisplayErrors;
begin
 if FileExists(ExceptPath+C_ErrFName) then
  if not ChildWindowExist('ErrorsForm', cwaShow) then
   with TErrorsForm.Create(Self) do FitComponents;
end;

procedure TMainForm.RegisterModule(ModilesString: String);
begin
 // ако модула вече се е регистрирал не правим повече проверки
 if CurrentModuleID > 0 then Exit;

 with TRegistrationForm.Create(Self) do
 try
   if not CheckStatus(ModilesString = '---') then
    begin
     if (ShowModal <> mrOK) then
      begin
       Application.Terminate;
       Exit;
      end;
     if CurrentModuleID < 0 then
      begin
       MyMessageDlg(S_InvalidRegistr, mtWarning, [mbOK], 0);
       Application.Terminate;
       Exit;
      end;
    end;
 finally
  Free;
 end;

 if not DataMod.LogModule(True) then
  begin
   Application.Terminate;
   Exit;
  end;

 // Иначе склада е неустановен! 
 if CurrentWorkStorage <= 0 then CurrentWorkStorage := DataMod.GetFirstStorageNumb;
 CurrentStorageName := DataMod.GetStorageName(CurrentWorkStorage);
end;

function TMainForm.ChildWindowExist(Name: TComponentName; Action: TChildWndAction=cwaNone): Boolean;
var I: Integer;
begin
 Result := False;
 for I := 0 to MDIChildCount-1 do
  if SameText(MDIChildren[I].Name, Name) then
   begin
    case Action of
    cwaShow:  begin
               MDIChildren[I].BringToFront;
               if MDIChildren[I].WindowState <> wsMaximized then MDIChildren[I].WindowState := wsMaximized;
              end;
    cwaClose: begin
               MDIChildren[I].Close;
              end;
    end;
    Result := True;
    Break;
   end;
end;

procedure TMainForm.pInfo1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 DisplayErrMsg('');
 ClearInfoPanel(1);
 if ParamCount < 2 then
  begin
   DataMod.LogOut;

   // closa all child windows
   while Self.MDIChildCount > 0 do
    begin
     Self.MDIChildren[0].Close;
     Application.ProcessMessages;
    end;
   // do login
   with TLoginForm.Create(Self.Owner) do FitComponents;
  end
 else PostMessage(Application.Handle, WM_CLOSE, 0, 0);
end;

procedure TMainForm.pInfo2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 DisplayErrMsg('');
 if Self.ActiveMDIChild.Name <> 'SetNumberForm' then
  begin
   ClearInfoPanel(2);
//   if ChildWindowExist('SetNumberForm', True) then SetNumberForm.FitComponents;

   ChildWindowExist('SetSumForm',         cwaClose);
   ChildWindowExist('SetPaymentForm',     cwaClose);
   ChildWindowExist('SearchCustomerForm', cwaClose);
   ChildWindowExist('CustomersForm',      cwaClose);
   ChildWindowExist('CustomerDataForm',   cwaClose);
  end;
end;

procedure TMainForm.pInfo3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 DisplayErrMsg('');
 if Self.ActiveMDIChild.Name <> 'SetSumForm' then
  begin
   ClearInfoPanel(3);
//   if ChildWindowExist('SetSumForm', True) then SetSumForm.FitComponents;

   ChildWindowExist('SetPaymentForm', cwaClose);
   ChildWindowExist('SearchCustomerForm', cwaClose);
   ChildWindowExist('CustomersForm', cwaClose);
   ChildWindowExist('CustomerDataForm', cwaClose);
  end;
end;

procedure TMainForm.ESKLockEndWork(Sender: TObject);
begin
 Application.ProcessMessages;
 if (ESKLock.CurrentProjectData <> nil)and
    (ESKLock.CurrentProjectData.ModuleByCode['Me.'] <> nil) then
   RegisterModule(ESKLock.CurrentProjectData.ModulesAsString)
 else RegisterModule('---');
end;

procedure TMainForm.pInfo4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 DisplayErrMsg('');
 if Self.ActiveMDIChild.Name <> 'SetPaymentForm' then
  begin
   ClearInfoPanel(4);
   ChildWindowExist('SetPaymentForm', cwaShow);
  end;
end;

procedure TMainForm.ClearInfoPanel(Sender: byte);
begin
 case Sender of
  1: begin
      lOperData.Caption     := '';
      lMobOperData.Caption  := '';
      lPhoneData.Caption    := '';
      lCustData.Caption     := '';
      lDocData.Caption      := '';
      lSumData.Caption      := '';
      lPayData.Caption      := '';

      SimRChObj.FPhoneNumb  := '';
      SimRChObj.FMobOper    := 0;
      SimRChObj.FSum        := 0;
      SimRChObj.FPluNumb    := -1;
      SimRChObj.FPluName    := '';
      SimRChObj.FVat        := 20;
      SimRChObj.FTaxGroup   := 2;
//      SimRChObj.FBuyPr      := 0;
//      SimRChObj.FBuyCurrID  := 1;
//      SimRChObj.FBuyCCource := 0;
//      SimRChObj.FSellPr     := 0;
//      SimRChObj.FSellCurrID := 1;
//      SimRChObj.FSellCCource:= 0;
      SimRChObj.FMaxSumTr   := 0;
      SimRChObj.FMaxSumDay  := 0;
      SimRChObj.FDocType    := 0;
      SimRChObj.FDocName    := '';
      SimRChObj.FPayType    := 0;
      SimRChObj.FPayName    := '';
      SimRChObj.FPayCource  := 1;
      SimRChObj.FCustBulst  := '';
      SimRChObj.FCustName   := '';
      SimRChObj.FPayPrnNumb := 0;
      SimRChObj.FPayPrnName := '';
     end;
  2: begin
      lMobOperData.Caption  := '';
      lCustData.Caption     := '';
      lDocData.Caption      := '';
      lSumData.Caption      := '';
      lPayData.Caption      := '';

      SimRChObj.FMobOper    := 0;
      SimRChObj.FSum        := 0;
      SimRChObj.FPluNumb    := -1;
      SimRChObj.FPluName    := '';
      SimRChObj.FVat        := 20;
      SimRChObj.FTaxGroup   := 2;
//      SimRChObj.FBuyPr      := 0;
//      SimRChObj.FBuyCurrID  := 1;
//      SimRChObj.FBuyCCource := 0;
//      SimRChObj.FSellPr     := 0;
//      SimRChObj.FSellCurrID := 1;
//      SimRChObj.FSellCCource:= 0;
      SimRChObj.FMaxSumTr   := 0;
      SimRChObj.FMaxSumDay  := 0;
      SimRChObj.FDocType    := 0;
      SimRChObj.FDocName    := '';
      SimRChObj.FPayType    := 0;
      SimRChObj.FPayName    := '';
      SimRChObj.FPayCource  := 1;
      SimRChObj.FCustBulst  := '';
      SimRChObj.FCustName   := '';
      SimRChObj.FPayPrnNumb := 0;
      SimRChObj.FPayPrnName := '';
     end;
  3: begin
      lCustData.Caption    := '';
      lDocData.Caption     := '';
      lPayData.Caption     := '';

      SimRChObj.FDocType   := 0;
      SimRChObj.FDocName   := '';
      SimRChObj.FPayType   := 0;
      SimRChObj.FPayName   := '';
      SimRChObj.FPayCource := 1;
      SimRChObj.FCustBulst := '';
      SimRChObj.FCustName  := '';
      SimRChObj.FPayPrnNumb:= 0;
      SimRChObj.FPayPrnName:= '';
     end;
  4: begin
      lPayData.Caption      := '';

      SimRChObj.FPhoneNumb  := '';
      SimRChObj.FMobOper    := 0;
      SimRChObj.FSum        := 0;
      SimRChObj.FPluNumb    := -1;
      SimRChObj.FPluName    := '';
      SimRChObj.FVat        := 20;
      SimRChObj.FTaxGroup   := 2;
//      SimRChObj.FBuyPr      := 0;
//      SimRChObj.FBuyCurrID  := 1;
//      SimRChObj.FBuyCCource := 0;
//      SimRChObj.FSellPr     := 0;
//      SimRChObj.FSellCurrID := 1;
//      SimRChObj.FSellCCource:= 0;
      SimRChObj.FMaxSumTr   := 0;
      SimRChObj.FMaxSumDay  := 0;
      SimRChObj.FDocType    := 0;
      SimRChObj.FDocName    := '';
      SimRChObj.FCustBulst  := '';
      SimRChObj.FCustName   := '';
     end;
 end;
end;

end.
