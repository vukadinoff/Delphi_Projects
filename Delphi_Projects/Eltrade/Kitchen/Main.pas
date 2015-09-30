unit Main;

interface

uses
  Windows, Messages, SysUtils, DateUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DB, IBDatabase, IBCustomDataSet, IBQuery, IniFiles,
  StdCtrls, Buttons, Menus, RXCtrls, Grids, Placemnt, DBGrids, RXDBCtrl,
  RxMemDS, ImgList, ComCtrls, dxCore, dxSpeedButton, KeyStateUnit, UDPThreadUnit, fraItems;

const
  ErrFName = 'CanteenExcept.txt';
  LogFName = 'CanteenLog.txt';

type
  TMainForm = class(TForm)
    pnlUpDown: TPanel;
    pmExit: TPopupMenu;
    NCloseApp: TMenuItem;
    NShutdownPC: TMenuItem;
    btnClose: TdxSpeedButton;
    tmrTmSpooler: TTimer;
    pnlTop: TPanel;
    EltradeImage: TImage;
    stbStatus: TStatusBar;
    btnDown: TdxSpeedButton;
    btnUp: TdxSpeedButton;
    scrItems: TScrollBox;
    pnlItems: TPanel;
    ImageList1: TImageList;
    ImageList: TImageList;
    popupExit: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    IBDatabase: TIBDatabase;
    tmrDbRefresh: TTimer;
    IBQuery: TIBQuery;
    IBTransaction: TIBTransaction;

    procedure FormCreate(Sender: TObject);
    procedure tmrTmSpoolerTimer(Sender: TObject);

    function AddPlu(Plunumb: Integer; Plugrp: Integer; PluEcrName: String; PluName: String): Boolean;
    function AddOperator(OperId: Integer; OperName: String): Boolean;

    procedure LoadFromDB();
    
    procedure AddItem(TS: string;
                      Terminal: Integer;
                      Operator: Integer;
                      OperatorName: string;
                      Table: Integer;
                      Number: Integer;
                      Pnumber: Integer;
                      Pgrp: Integer;
                      DishName: String;
                      DishLongName: String;
                      DishModif: String;
                      Quantity: Integer;
                      Delay1: Integer;
                      Delay2: Integer;
                      MsgTo: string);


    procedure AddFrame(AOwner: TComponent;
                       TS: string;
                       DBId: Integer;
                       Terminal: Integer;
                       Operator: Integer;
                       OperatorName: string;
                       Table: Integer;
                       Number: Integer;
                       Pnumber: Integer;
                       DishName: String;
                       DishModif: String;
                       Quantity: Integer;
                       Delay1: Integer;
                       Delay2: Integer;
                       MsgTo: string);

    procedure RevokeItem(Terminal: Integer;
                         Operator: Integer;
                         Table: Integer;
                         Number: Integer;
                         Pnumber: Integer);

    procedure RevokedFrame(order_id: Integer);

    procedure RemoveItem(id: integer);

    function ExistsItem(TS: string;
                        Terminal: Integer;
                        Operator: Integer;
                        Table: Integer;
                        Number: Integer;
                        Pnumber: Integer;
                        Quantity: Integer): Boolean;

    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure stbStatusDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure stbStatusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ShutDown(MachineName_, ShutMessage_: String;
                        Force_, Reboot_: Boolean; Countdown_: Integer);
    procedure tmrPosInfoTimer(Sender: TObject);

    procedure AddLog(IsError: boolean; const Text: string);
    procedure PostLog(StrLst_: TStrings);
    procedure PostException(S: String);
    procedure FormActivate(Sender: TObject);
    procedure stbStatusDblClick(Sender: TObject);
    procedure IBDatabaseBeforeDisconnect(Sender: TObject);

    function InitializeIBdatabase(Fname_: String): boolean;
    function OpenIBDatabase: Boolean;

    procedure MouseDown();
    procedure MouseUp();
    procedure MouseMove();

  private
    { Private declarations }
    FMonitor: Integer;
    
    LocalPath: string;
    IniFName: string;
    LngFName: string;
    LogList: TStrings;
    LogPath: string;
    ExceptPath: string;

    _HideMouse: Integer;
    _ColorNew: string;
    _ColorDelay: string;
    _ColorExpired: string;
    _ColorRevoked: string;

    _FontSizeDish: Integer;
    _FontSizeTime: Integer;

    _DBConnection: String;
    _spoolerPath: String;
    _readSpooler: Integer;
    _dbrefresh: Integer;
    _delay1: Integer;
    _delay2: Integer;
    _msgTo: string;
    _msgToList: TStringList;

    _Items: Integer;
    _ItemsHeight: Integer;

    _mX: Integer;
    _mY: Integer;
    _deltaX: Integer;
    _deltaY: Integer;
    _deltaYmoved: Integer;
    _mDown: Boolean;
    _mMoved: Boolean;

    procedure ReadSettings();
    procedure ReadSpooler();
    procedure GarbageCollector();

  public
    UDP        : TMyUDPcomm;
    
    procedure AnnounceDish(dishData: String);
    procedure AdjustScroll();

    function Moved(): Boolean;

    property MonitorIdx: Integer read FMonitor write FMonitor;
    
    property HideMouse: Integer read _HideMouse;
    property ColorNew: string read _ColorNew;
    property ColorDelay: string read _ColorDelay;
    property ColorExpired: string read _ColorExpired;
    property ColorRevoked: string read _ColorRevoked;
    
    property FontSizeDish: Integer read _FontSizeDish;
    property FontSizeTime: Integer read _FontSizeTime;
  end;

var
  MainForm: TMainForm;
  Key_NumLock  : Boolean;
  Key_CapsLock : Boolean;
  KeyState   : TKeyState;
  Img_Pos    : Integer;
  Img_Printer: Integer;
  Img_Exit   : Integer;
  Img_ShutDn : Integer;

implementation

uses TerminalSettingsUnit;

{$R *.dfm}
procedure TMainForm.FormCreate(Sender: TObject);
begin
   _deltaYmoved := 10;
   _deltaY := 80;
   _deltaX := 100;
   _mDown := False;
   _mMoved := False;

   LocalPath   := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
   IniFName    := LocalPath+ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
   LngFName    := ChangeFileExt(ExtractFileName(Application.ExeName),'');
   LogPath     := LocalPath + 'Log\';
   ExceptPath  := LocalPath + 'Exceptions\';

   LogList := TStrings.Create;

   KeyState     := TKeyState.Create;
   Key_NumLock  := KeyState.NumLock;
   Key_CapsLock := KeyState.CapsLock;
   Img_Pos      := 0;
   Img_Printer  := 5;
   Img_Exit     := 8;
   Img_ShutDn   := 9;

   _Items := 0;
   _ItemsHeight := 100;

  _spoolerPath := LocalPath + '\Spool';
  _delay1 := 15;
  _delay2 := 25;
  _msgTo := '';
  _DBConnection := LocalPath + '\KITCHEN.GDB';
  _readSpooler := 15;
  _dbrefresh := 120;

  MonitorIdx := 1;
  _HideMouse := 1;
  _ColorNew := '$00D2E6D2';
  _ColorDelay := '$002BFFFF';
  _ColorExpired := '$004848FF';
  _ColorRevoked := '$00FF962D';

  _FontSizeDish := 14;
  _FontSizeTime := 12;

  self.ReadSettings();

  try
   UDP := TMyUDPcomm.Create(6677);
   stbStatus.Panels[1].Text := UDP.LocalHostName;
  except
   on E: Exception do
    begin
     UDP := nil;
     MessageDlg('Грешка при инициализиране на мрежата. Проверете настройките!', mtWarning, [mbOK], 0);
     Application.Terminate();
    end;
  end;

  try

  if InitializeIBdatabase(_DBConnection) then
   begin
    if not MainForm.OpenIBDatabase then
     begin
       MessageDlg('Грешка при инициализиране на базата данни (' + _DBConnection + '). Проверете настройките!', mtWarning, [mbOK], 0);
       Application.Terminate();
     end;
   end
  else
    begin
      MessageDlg('Грешка при инициализиране на базата данни (' + _DBConnection + '). Проверете настройките!', mtWarning, [mbOK], 0);
      Application.Terminate();
    end;

    Self.LoadFromDB();

  except
   on E: Exception do
    begin
     MessageDlg('Грешка при инициализиране на базата данни (' + _DBConnection + '). Проверете настройките! (' + E.Message + ')' , mtWarning, [mbOK], 0);
     Application.Terminate();
    end;
    
   end;

    Self.tmrTmSpooler.Enabled := True;
    Self.tmrDbRefresh.Enabled := True;
end;

procedure TMainForm.ReadSettings();
 var IniFile : TIniFile;
 var i: Integer;
begin
  IniFile := TIniFile.Create(IniFName);

  try
    _DBConnection := IniFile.ReadString('DATABASE', 'DBConnection', _DBConnection);
    _readSpooler := IniFile.ReadInteger('DELAYS', 'ReadSpooler', _readSpooler);
    _dbrefresh := IniFile.ReadInteger('DELAYS', 'DBRefresh', _dbrefresh);
    _spoolerPath := IniFile.ReadString('PATHS', 'Spooler', ExtractFilePath(Application.ExeName) + 'Spool');
    _delay1 := IniFile.ReadInteger('DELAYS', 'Delay1', 15);
    _delay2 := IniFile.ReadInteger('DELAYS', 'Delay2', 25);
    _msgTo := IniFile.ReadString('NET', 'MsgTo', '');

    MonitorIdx := IniFile.ReadInteger('INFO', 'MonitorIndex', MonitorIdx);
    MonitorIdx := MonitorIdx - 1;

    _HideMouse := IniFile.ReadInteger('INFO', 'HideMouse', _HideMouse);
    _ColorNew := IniFile.ReadString('INFO', 'ColorNew', _ColorNew);
    _ColorDelay  := IniFile.ReadString('INFO', 'ColorDelay: ', _ColorDelay);
    _ColorExpired  := IniFile.ReadString('INFO', 'ColorExpired', _ColorExpired);
    _ColorRevoked  := IniFile.ReadString('INFO', 'ColorRevoked', _ColorRevoked);

    _FontSizeDish  := IniFile.ReadInteger('INFO', 'FontSizeDish', _FontSizeDish);
    _FontSizeTime  := IniFile.ReadInteger('INFO', 'FontSizeTime', _FontSizeTime);

    _msgToList := TStringList.Create;

    ExtractStrings([','], [], PChar(_msgTo), _msgToList);

    for i := 0 to _msgToList.Count-1 do
      _msgToList[i] := Trim(_msgToList[i]);

    Self.tmrTmSpooler.Interval := Self._readSpooler * 1000;
    Self.tmrDbRefresh.Interval := Self._dbrefresh * 1000;
    
  finally
    IniFile.Free;
  end;

end;

procedure TMainForm.AddItem(TS: string;
                            Terminal: Integer;
                            Operator: Integer;
                            OperatorName: string;
                            Table: Integer;
                            Number: Integer;
                            Pnumber: Integer;
                            Pgrp: Integer;
                            DishName: String;
                            DishLongName: String;
                            DishModif: String;
                            Quantity: Integer;
                            Delay1: Integer;
                            Delay2: Integer;
                            MsgTo: string);
var new_id: Integer;
begin
  // path - set timestamp to now
  //TS := formatdatetime('dd.mm.yyyy hh:nn:ss', now());
  new_id := 0;

if AddPlu(Pnumber, Pgrp, DishName, DishLongName) and
   AddOperator(Operator, OperatorName) then

begin

  try

    with IBQuery do
    begin

      if Active then Close;

      SQL.Clear;
      SQL.Add('SELECT GEN_ID(GEN_ORDERS_ID, 1) "NEW_ID" FROM RDB$DATABASE');
      Open;
      new_id := FieldByName('NEW_ID').AsInteger;

      if Active then Close;

      SQL.Clear;
      SQL.Add('INSERT INTO ORDERS VALUES(');
      SQL.Add(IntToStr(new_id)                      + ','); // ORDER_ID
      SQL.Add(QuotedStr(UDP.LocalHostName)          + ','); // KITCHEN_TERMINAL_ID
      SQL.Add(IntToStr(Terminal)                    + ','); // TERMINAL_ID
      SQL.Add(QuotedStr(TS)                         + ','); // RECEIVED
      SQL.Add(IntToStr(Operator)                    + ','); // OPERATOR_ID
      SQL.Add(QuotedStr(OperatorName)               + ','); // OPERATOR_NAME
      SQL.Add(IntToStr(Table)                       + ','); // TABLE_NO
      SQL.Add(IntToStr(Pnumber)                     + ','); // PLU_ID
      SQL.Add(IntToStr(Number)                      + ','); // PLU_ORDER_ID
      SQL.Add(IntToStr(Quantity)                    + ','); // QUANTITY
      SQL.Add(QuotedStr(DishName)                   + ','); // PLU_NAME
      SQL.Add(QuotedStr(DishLongName)               + ','); // PLU_LONG_NAME
      SQL.Add(QuotedStr(DishModif)                  + ','); // MODIFICATOR
      SQL.Add('0'                                   + ')'); // REVOKED
      Prepare;
      ExecSQL;
      Transaction.Commit;
    end;

  except
   on E: Exception do
   begin
    AddLog(True, 'Грешка при добавяне на поръчка: '+Trim(E.Message));
    MessageDlg('Грешка при запис на поръчка!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
   end;
  end;

  AddFrame(scrItems,
            TS,
            new_id,
            Terminal,
            Operator,
            OperatorName,
            Table,
            Number,
            Pnumber,
            DishLongName,
            DishModif,
            Quantity,
            Delay1,
            Delay2,
            MsgTo);
end;

end;

procedure TMainForm.AddFrame(AOwner: TComponent;
                       TS: string;
                       DBId: Integer;
                       Terminal: Integer;
                       Operator: Integer;
                       OperatorName: string;
                       Table: Integer;
                       Number: Integer;
                       Pnumber: Integer;
                       DishName: String;
                       DishModif: String;
                       Quantity: Integer;
                       Delay1: Integer;
                       Delay2: Integer;
                       MsgTo: string);
var fr: TfrItems;
begin
  try
    fr := TfrItems.Create(AOwner,
                          DBId,
                          Terminal,
                          Operator,
                          OperatorName,
                          Table,
                          Number,
                          Pnumber,
                          DishName,
                          DishModif,
                          Quantity,
                          Delay1,
                          Delay2,
                          MsgTo);

    fr.pnldetails.Color := StringToColor(ColorNew);
    fr.pnlTm.Color := StringToColor(ColorNew);

    fr.lblTitle.Font.Size := FontSizeDish;
    fr.lblMod.Font.Size := FontSizeDish;
    fr.lblQuantity.Font.Size := FontSizeDish;
    fr.lblTm.Font.Size := FontSizeTime;

    fr.Name := '';
    fr.Width := pnlItems.Width;
    fr.Parent := pnlItems;
    fr.AutoSize := true;
    fr.Top := 0;
    fr.StartCount(TS);

    _Items := _Items + 1;
    _ItemsHeight := fr.Height;

    AdjustScroll();
  except
   on E: Exception do
    begin
     AddLog(True, 'Грешка при добавяне на поръчка: '+Trim(E.Message));
     MessageDlg('Грешка при добавяне на поръчка!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.ReadSpooler();
var
  i: Integer;
  fName: String;
  SR: TSearchRec;
  IniFile: TIniFile;
  Terminal: Integer;
  Operator: Integer;
  OperatorName: string;
  Table: Integer;
  Number: Integer;
  Pnumber: Integer;
  Pgrp: Integer;
  DishName: String;
  DishLongName: String;
  DishModif: String;
  Quantity: Integer;
  Revoked: Integer;
  TS: string;

  begin

  // First clear the trash
  GarbageCollector();
  
  if FindFirst(self._spoolerPath + '\*.*', faAnyFile, SR) = 0 then
  repeat
    if (SR.Attr <> faDirectory) then
    begin

    fName := self._spoolerPath + '\' + SR.Name;
    IniFile := TIniFile.Create(fName);

    try
      TS := IniFile.ReadString('PRN_HEADER','DATE','');
      TS := TS + ' ' + IniFile.ReadString('PRN_HEADER','TIME','');
      Terminal := IniFile.ReadInteger('PRN_HEADER', 'TERM', 0);
      Operator := IniFile.ReadInteger('PRN_HEADER', 'OPER', 0);
      OperatorName := IniFile.ReadString('PRN_HEADER','OPERNM','');
      Table := IniFile.ReadInteger('PRN_HEADER', 'TBL', 0);

      i := 1;

      Number := IniFile.ReadInteger('PRN_DETAIL_' + IntToStr(i), 'NO', 0);

      while Number <> 0 do
      begin
        Pnumber := IniFile.ReadInteger('PRN_DETAIL_' + IntToStr(i), 'PNUMB', 0);
        Pgrp := IniFile.ReadInteger('PRN_DETAIL_' + IntToStr(i), 'PLUGRP', 0);
        DishName :=  IniFile.ReadString('PRN_DETAIL_' + IntToStr(i), 'PNAME', 'Грешка: не е намерено име');
        DishLongName :=  IniFile.ReadString('PRN_DETAIL_' + IntToStr(i), 'PLONGNAME', '');
        DishModif :=  IniFile.ReadString('PRN_DETAIL_' + IntToStr(i), 'MODIF', '');
        Quantity := IniFile.ReadInteger('PRN_DETAIL_' + IntToStr(i), 'QUANT', 1);
        Revoked := IniFile.ReadInteger('PRN_DETAIL_' + IntToStr(i), 'REVOKED', 0);

        if Revoked > 0 then
          RevokeItem(Terminal, Operator, Table, Number, Pnumber)
        else
        begin
         if not ExistsItem(TS,
                          Terminal,
                          Operator,
                          Table,
                          Number,
                          Pnumber,
                          Quantity) then
           AddItem(TS, Terminal, Operator, OperatorName, Table, Number, Pnumber, Pgrp, DishName, DishLongName, DishModif, Quantity, _delay1, _delay2, _msgTo);
        end;

        i := i + 1;
        Number := IniFile.ReadInteger('PRN_DETAIL_' + IntToStr(i), 'NO', 0);
      end;

    except
     on E: Exception do
      begin
       AddLog(True, 'Грешка при изчитане на входяща бележка (' + self._spoolerPath + '): '+Trim(E.Message));
       MessageDlg('Грешка при изчитане на входяща бележка!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
      end;
    end;

    IniFile.Free;
    DeleteFile(fName);
  end;
  until FindNext(SR) <> 0;

  FindClose(SR);
end;


procedure TMainForm.RevokeItem(Terminal: Integer;
                               Operator: Integer;
                               Table: Integer;
                               Number: Integer;
                               Pnumber: Integer);
var frm: TfrItems;
var order_id, i: Integer;
var tmp: string;
begin
  order_id := 0;

   try
    with IBQuery do
     begin
      if Active then Close;
      SQL.Clear;
      SQL.Add('SELECT FIRST 1 ORDER_ID FROM ORDERS');
      SQL.Add('WHERE REVOKED = 0');
      SQL.Add('AND TERMINAL_ID = ' + IntToStr(Terminal));
      SQL.Add('AND OPERATOR_ID = ' + IntToStr(Operator));
      SQL.Add('AND TABLE_NO = ' + IntToStr(Table));
      SQL.Add('AND PLU_ORDER_NO = ' + IntToStr(Number));
      SQL.Add('AND PLU_ID = ' + IntToStr(Pnumber));
      SQL.Add('ORDER BY RECEIVED DESC');
      tmp := SQL.Text;
      Open;
      First;

      if not FieldByName('ORDER_ID').IsNull then
       begin
         order_id := FieldByName('ORDER_ID').AsInteger;

         if Active then Close;

         SQL.Clear;
         SQL.Add('UPDATE ORDERS SET');
         SQL.Add(' REVOKED = 1');
         SQL.Add(' WHERE ORDER_ID = ' + IntToStr(order_id));
         Prepare;
         ExecSQL;
         Transaction.Commit;

       end;
     end;
   except
    on E: Exception do
     begin
      AddLog(True, 'Грешка при сторниране на продукт: '+Trim(E.Message));
      MessageDlg('Грешка при сторниране на продукт!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
     end;
   end;

   RevokedFrame(order_id);

end;

procedure TMainForm.RevokedFrame(order_id: Integer);
var frm: TfrItems;
var i: Integer;
var tmp: string;
begin
   if (order_id > 0) then
   begin
    for i := pnlItems.ControlCount - 1 downto 0 do
    begin
      if (pnlItems.Controls[i].Visible) and (pnlItems.Controls[i] is TfrItems) then
      begin
        frm := TfrItems(pnlItems.Controls[i]);

        if (frm.Tag = order_id) then
        begin
          frm._revoked := True;
          frm.tmr.Enabled := False;
          frm.pnlDetails.Color :=  StringToColor(ColorRevoked);
          frm.pnlTm.Color :=  StringToColor(ColorRevoked);
          frm.lblTitle.Caption := 'СТОРНИРАНА ' + frm.lblTitle.Caption;

          Break;
        end;

      end;
    end;
   end;

end;


procedure TMainForm.GarbageCollector();
var i: Integer;
begin

  for i := pnlItems.ControlCount - 1 downto 0 do
  begin
    if (not pnlItems.Controls[i].Visible) and (pnlItems.Controls[i] is TfrItems) then
    begin
      pnlItems.Controls[i].Free;
    end;
  end;


end;

procedure TMainForm.tmrTmSpoolerTimer(Sender: TObject);
begin
 self.ReadSpooler();
end;

procedure TMainForm.btnUpClick(Sender: TObject);
var i: Integer;
begin
  i := self.scrItems.VertScrollBar.ScrollPos - self.scrItems.VertScrollBar.Increment;
  self.scrItems.VertScrollBar.Position :=  i;
end;


procedure TMainForm.btnDownClick(Sender: TObject);
var i: Integer;
begin
  i := self.scrItems.VertScrollBar.ScrollPos + self.scrItems.VertScrollBar.Increment;
  scrItems.VertScrollBar.Position :=  i;
end;

procedure TMainForm.stbStatusDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var ARect: TRect;
    Height_: Integer;
begin
  ARect := Rect;
   with stbStatus.Canvas do
    begin
     Brush.Color := stbStatus.Color;
     Font.Style := [fsBold];
     TextOut(ARect.Right-TextWidth(Panel.Text)-3,ARect.Top ,Panel.Text);
    end;

   with StatusBar.Canvas do
    begin
     Height_ := ((Arect.Bottom - Arect.Top) - ImageList.Height) div 2;
     ImageList.Draw(StatusBar.Canvas, ARect.Left, ARect.Top + Height_, Img_Exit, True);
    end;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  ShutDown('', '', false, false, 0);
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ShutDown(MachineName_, ShutMessage_: String;
                             Force_, Reboot_: Boolean; Countdown_: Integer);
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
var
  otoken, hToken: THandle;
  tp: TTokenPrivileges;
  h: DWORD;
begin
  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken);
  otoken := htoken;
  LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, tp.Privileges[0].luid);
  tp.privilegecount := 1;
  tp.privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
  h := 0;
  AdjustTokenPrivileges(hToken, False, tp, 0, PTokenPrivileges(nil)^, h);
  InitiateSystemShutdown(PChar(MachineName_), PChar(ShutMessage_), Countdown_, Force_, Reboot_);
  tp.privilegecount := 1;
  tp.privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
  h := 0;
  AdjustTokenPrivileges(oToken, False, tp, 0, PTokenPrivileges(nil)^, h);
  CloseHandle(hToken);
end;


procedure TMainForm.AnnounceDish (dishData: String);
var id: String;
var i: Integer;
begin

  if _msgTo = '' then Exit
  else if _msgTo = '0' then
  begin
    UDP.BroadcastCommand(6678, 'KitchenMSG', dishData);
    Exit;
  end
  else
  begin
    TerminalSettingsForm.LoadPOSTable();

    try
      with TerminalSettingsForm.MemData do
      begin
        if not Active then Open;

        First;

        while not Eof do
        begin
          id := Trim(FieldByName('POS_NUMBER').AsString);

          if (_msgToList.Find(id, i) = true) then
          begin
            Self.UDP.SendCommand(FieldByName('PC_NAME').AsString, 6678, 'KitchenMSG', dishData);
          end;

          Next;
        end;

      end;

    except
     on E: Exception do
      begin
       AddLog(True, 'Грешка при анонсиране на поръчка: '+Trim(E.Message));
      end;
    end;

  end;

  end;

procedure TMainForm.tmrPosInfoTimer(Sender: TObject);
begin
  TerminalSettingsForm.LoadPOSTable();
end;

procedure TMainForm.AddLog(IsError: boolean; const Text: string);
var sErr: string;
begin
 if LogList =  nil then exit;
 if (Text = '') then exit;
 if IsError then sErr := '[Err] '
  else sErr := '';
 LogList.Add(FormatDateTime('DD.MM.YY HH:NN:SS ', Now)+sErr+Text);
 if IsError then PostException(Text)
  else PostLog(LogList);
end;

procedure TMainForm.PostLog(StrLst_: TStrings);
var F: TextFile;
    I: Integer;
begin
 if (StrLst_ = nil) then exit;
 if StrLst_.Count <= 0 then exit;
{$I-}
 AssignFile(F, LogPath + LogFName);
 Append(F);
 if IOResult <> 0 then Rewrite(F);
 for I := 0 to StrLst_.Count-1 do
  begin
   Writeln(F, StrLst_.Strings[I]);
  end;
 I := FileSize(F);
 CloseFile(f);
{$I+}
 StrLst_.Clear;
 if I > 400 then
   RenameFile(LogPath+LogFName, LogPath+ChangeFileExt(LogFName,'')+
              FormatDateTime('_DDMMYY_HHNNSS',Now) + '.txt');
end;

procedure TMainForm.PostException(S: String);
var F: TextFile;
    I: Integer;
begin
{$I-}
 AssignFile(F, ExceptPath+ErrFName);
 Append(F);
 if IOResult <> 0 then Rewrite(F);
 Writeln(F, FormatDateTime('DD.MM.YY HH:NN:SS ', Now)+ S);
 I := FileSize(F);
 CloseFile(F);
{$I+}
 if I > 200 then
   RenameFile(ExceptPath+ErrFName, ExceptPath+ChangeFileExt(ErrFName,'')+
              FormatDateTime('_DDMMYY_HHNNSS',Now)+'.txt');
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Self.Top    := Screen.Monitors[Self.MonitorIdx].Top;
  Self.Left   := Screen.Monitors[Self.MonitorIdx].Left;
  Self.Width  := Screen.Monitors[Self.MonitorIdx].Width;
  Self.Height := Screen.Monitors[Self.MonitorIdx].Height;

  if (Self.HideMouse <> 0) then ShowCursor(false);
end;

procedure TMainForm.stbStatusDblClick(Sender: TObject);
var MousePos: TPoint;
    X: Integer;
begin
  MousePos := Mouse.CursorPos;
  MousePos := stbStatus.ScreenToClient(MousePos);

X := MousePos.x;

with stbStatus do
  begin
   if ((X > (Panels[0].Width +  Panels[1].Width)) and (X < (Panels[0].Width + Panels[1].Width + Panels[2].Width))) then
   begin
       TerminalSettingsForm.Show();
   end;
   end;
end;

procedure TMainForm.stbStatusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
with stbStatus do
  begin
   if (X <= Panels[0].Width) then
    begin
     popupExit.Popup(MainForm.Left + X, MainForm.Top + MainForm.Height - 5 - Y);
    end;
   end;

end;

procedure TMainForm.AdjustScroll();
var i: Integer;
begin
  _Items := 0;

    for i := 0 to pnlItems.ControlCount - 1 do
    begin
      if (pnlItems.Controls[i].Visible = True) and (pnlItems.Controls[i] is TfrItems) then
      begin
        Inc(_Items);
        pnlItems.Controls[i].Top := i*_ItemsHeight;
      end;
    end;

    scritems.VertScrollBar.Range := (_ItemsHeight) * (_Items-1);
    scritems.VertScrollBar.Increment :=  _ItemsHeight;
    scritems.AutoScroll := false;
end;

function TMainForm.InitializeIBdatabase(Fname_: String) : boolean;
begin
 if FileExists(Fname_) then
  begin
   IBDatabase.DatabaseName := 'localhost:' + Fname_;
   result := true;
  end
 else
  begin
   MessageDlg('Некоректен път към базата данни. Проверете настройките!', mtWarning, [mbOK], 0);
   result := false;
  end;
end;

function TMainForm.OpenIBDatabase : Boolean;
begin
 try
  IBDatabase.Open;
  Result := IBDatabase.Connected;
 except
  on E : Exception do
   begin
    MessageDlg('Неуспешно отваряне на базата данни. Проверете настройките!'+#13+#10+
               E.Message, mtWarning, [mbOK], 0);
    Result := false;
   end;
 end;
end;

procedure TMainForm.IBDatabaseBeforeDisconnect(Sender: TObject);
var I : Integer;
begin
 for I := 0 to IBDatabase.TransactionCount - 1 do
  begin
   if IBDatabase.Transactions[I].InTransaction then
     IBDatabase.Transactions[I].Commit;
  end;

end;

function TMainForm.AddPlu(Plunumb: Integer; Plugrp: Integer; PluEcrName: String; PluName: String): Boolean;
begin
 Result := false;

 try
  with IBQuery do
   begin
    if Active then Close;
    SQL.Clear;
    SQL.Add('SELECT * FROM PLUES ');
    SQL.Add('WHERE PLU_NUMB = ' + IntToStr(Plunumb));
    Open;

    if not FieldByName('PLU_NUMB').IsNull then
     begin
      if (Plugrp <> FieldByName('PLU_GROUP_ID').AsInteger) or
         (PluEcrName <> FieldByName('PLU_ECR_NAME').AsString) or
         (PluName <> FieldByName('PLU_NAME').AsString) then
      begin
           if Active then Close;

            SQL.Clear;
            SQL.Add('UPDATE PLUES SET ');
            SQL.Add('PLU_GROUP_ID = ' + IntToStr(Plugrp) + ', ');
            SQL.Add('PLU_ECR_NAME = ' + QuotedStr(PluEcrName) + ', ');
            SQL.Add('PLU_NAME = ' + QuotedStr(PluName));
            SQL.Add(' WHERE PLU_NUMB = ' + IntToStr(Plunumb));
            Prepare;
            ExecSQL;
            Transaction.Commit;
      end;
      Result := True;
     end
    else
     begin
       if Active then Close;

       SQL.Clear;
       SQL.Add('INSERT INTO PLUES VALUES (');
       SQL.Add(IntToStr(Plunumb) + ', ');
       SQL.Add(IntToStr(Plugrp) + ', ');
       SQL.Add(QuotedStr(PluEcrName) + ', ');
       SQL.Add(QuotedStr(PluName) + ')');
       Prepare;
       ExecSQL;
       Transaction.Commit;

       Result := True;
     end;
   end;
 except
  on E: Exception do
   begin
    Result := false;

    AddLog(True, 'Грешка при запис на продукт: '+Trim(E.Message));
    MessageDlg('Грешка при запис на продукт!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
   end;
 end;

end;


function TMainForm.AddOperator(OperId: Integer; OperName: String): Boolean;
begin
 Result := false;

 try
  with IBQuery do
   begin
    if Active then Close;
    SQL.Clear;
    SQL.Add('SELECT * FROM OPERATORS ');
    SQL.Add('WHERE OPER_ID = ' + IntToStr(OperId));
    Open;

    if not FieldByName('OPER_ID').IsNull then
     begin
      if OperName <> FieldByName('OPER_NAME').AsString then
      begin
           if Active then Close;
            SQL.Clear;
            SQL.Add('UPDATE OPERATORS SET ');
            SQL.Add('OPER_NAME = ' + QuotedStr(OperName));
            SQL.Add(' WHERE OPER_ID = ' + IntToStr(OperId));
            Prepare;
            ExecSQL;
            Transaction.Commit;
      end;
      Result := True;
     end
    else
     begin
      if Active then Close;
           if Active then Close;
            SQL.Clear;
            SQL.Add('INSERT INTO OPERATORS VALUES (');
            SQL.Add(IntToStr(OperId) + ', ');
            SQL.Add(QuotedStr(OperName) + ')');
            Prepare;
            ExecSQL;
            Transaction.Commit;
     end;
     Result := True;
   end;
 except
  on E: Exception do
   begin
    Result := false;

    AddLog(True, 'Грешка при запис на оператор: '+Trim(E.Message));
    MessageDlg('Грешка при запис на оператор!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
   end;
 end;

end;

procedure TMainForm.RemoveItem(id: integer);
begin

 try
  with IBQuery do
   begin

    if Active then Close;

    SQL.Clear;
    SQL.Add('DELETE FROM ORDERS ');
    SQL.Add('WHERE ORDER_ID = ' + IntToStr(id));
    Prepare;
    ExecSQL;
    Transaction.Commit;

    _Items := _Items - 1;
    AdjustScroll();
   end;
 except
  on E: Exception do
   begin
    AddLog(True, 'Грешка при потвърждаване на поръчка: '+Trim(E.Message));
    MessageDlg('Грешка при потвърждаване на поръчка!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
   end;
 end;

end;


procedure TMainForm.LoadFromDB();
begin
   try
    with IBQuery do
     begin
      if Active then Close;
      SQL.Clear;
      SQL.Add('SELECT * FROM ORDERS');
      SQL.Add('WHERE KITCHEN_TERMINAL_ID = ' + QuotedStr(UDP.LocalHostName));
      SQL.Add('ORDER BY RECEIVED');
      Open;
      First;

      while not Eof do
      begin
        AddFrame(scrItems,
            FieldByName('RECEIVED').AsString,
            FieldByName('ORDER_ID').AsInteger,
            FieldByName('TERMINAL_ID').AsInteger,
            FieldByName('OPERATOR_ID').AsInteger,
            FieldByName('OPERATOR_NAME').AsString,
            FieldByName('TABLE_NO').AsInteger,
            FieldByName('PLU_ORDER_NO').AsInteger,
            FieldByName('PLU_ID').AsInteger,
            FieldByName('PLU_NAME').AsString,
            FieldByName('MODIFICATOR').AsString,
            FieldByName('QUANTITY').AsInteger,
            Self._delay1,
            Self._delay2,
            Self._msgTo);

        if (FieldByName('REVOKED').AsInteger > 0) then
          RevokedFrame(FieldByName('ORDER_ID').AsInteger);

            Next;
      end;

     end;
   except
    on E: Exception do
     begin
      AddLog(True, 'Грешка при изчитане на поръчки от базата: '+Trim(E.Message));
      MessageDlg('Грешка при изчитане на поръчки от базата!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
     end;
   end;
end;

function TMainForm.ExistsItem(TS: string;
                              Terminal: Integer;
                              Operator: Integer;
                              Table: Integer;
                              Number: Integer;
                              Pnumber: Integer;
                              Quantity: Integer): Boolean;
begin
  Result := false;

   try
    with IBQuery do
     begin
      if Active then Close;
      SQL.Clear;
      SQL.Add('SELECT ORDER_ID FROM ORDERS');
      SQL.Add('WHERE RECEIVED = ' + QuotedStr(TS));
      SQL.Add('AND TERMINAL_ID = ' + IntToStr(Terminal));
      SQL.Add('AND OPERATOR_ID = ' + IntToStr(Operator));
      SQL.Add('AND TABLE_NO = ' + IntToStr(Table));
      SQL.Add('AND PLU_ORDER_NO = ' + IntToStr(Number));
      SQL.Add('AND PLU_ID = ' + IntToStr(Pnumber));
      SQL.Add('AND QUANTITY = ' + IntToStr(Quantity));

      Open;

      if not FieldByName('ORDER_ID').IsNull then Result := True;
      
     end;
   except
    on E: Exception do
     begin
      AddLog(True, 'Грешка при сторниране на продукт: '+Trim(E.Message));
      MessageDlg('Грешка при сторниране на продукт!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
     end;
   end;
end;

procedure TMainForm.MouseDown();
var MousePos: TPoint;
begin
  MousePos := Mouse.CursorPos;

  _mX := MousePos.x;
  _mY := MousePos.y;
  
  _mDown := True;
end;

procedure TMainForm.MouseUp();
var x,y: Integer;
var MousePos: TPoint;
begin
  _mDown := False;
  MousePos := Mouse.CursorPos;

  x := Abs(MousePos.x - _mX);
  y := Abs(MousePos.y - _mY);
end;

procedure TMainForm.MouseMove();
var x,y: Integer;
var MousePos: TPoint;
begin

  if _mDown then
  begin
    MousePos := Mouse.CursorPos;

    x := Abs(MousePos.x - _mX);
    y := Abs(MousePos.y - _mY);

    if (y > _deltaYmoved) then _mMoved := True;

    if (y > _deltaY)  and (x < _deltaX) then
    begin
      if (MousePos.y > _mY) then
      begin
        self.scrItems.VertScrollBar.Position := self.scrItems.VertScrollBar.ScrollPos - self.scrItems.VertScrollBar.Increment;
      end
      else
      begin
        self.scrItems.VertScrollBar.Position := self.scrItems.VertScrollBar.ScrollPos + self.scrItems.VertScrollBar.Increment;
      end;

      _mX := MousePos.x;
      _mY := MousePos.y;
    end;
  end;

end;

function TmainForm.Moved(): Boolean;
begin
  if _mMoved then
  begin
    Result:= True;
    _mMoved := False;
  end
  else
    Result := False;
end;

end.

