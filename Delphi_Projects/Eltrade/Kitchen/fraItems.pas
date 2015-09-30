unit fraItems;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, MyMessageUnit, TerminalSettingsUnit, DateUtils;

type
  TfrItems = class(TFrame)
    pnlDetails: TPanel;
    lblTitle: TLabel;
    lblQuantity: TLabel;
    lblMod: TLabel;
    pnlTm: TPanel;
    lblTm: TLabel;
    tmr: TTimer;

    procedure tmrTimer(Sender: TObject);
    procedure StartCount(ts: string);
    procedure RemoveMe();

    procedure MouseDown();
    procedure MouseUp();
    procedure MouseMove();

    procedure lblTitleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblTitleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDetailsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDetailsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblModMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblModMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblQuantityMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblQuantityMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblTmMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblTmMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblTitleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblModMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblQuantityMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlDetailsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblTmMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    public
    
    _timerStart: TDateTime;
    _ticks: Integer;
    _terminal: Integer;
    _operator: Integer;
    _operatorName: string;
    _table: Integer;
    _number: Integer;
    _pnumber: Integer;
    _dishName: String;
    _dishModif: String;
    _quantity: Integer;
    _delay1: Integer;
    _delay2: Integer;
    _msgTo: string;
    _revoked: Boolean;
    _h: Integer;
    _m: Integer;

    IsTrash: Boolean;

    //constructor Create(AOwner: TComponent); overload;
    { Public declarations }
    constructor Create(AOwner: TComponent;
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
                       MsgTo: string); overload;

    function getTitle(): string;

  end;

implementation

uses Main;

{$R *.dfm}
{
 constructor TfrItems.Create(AOwner: TComponent);
 begin
   inherited Create(AOwner);
   //"OnCreate" code
 end;
 }
constructor TfrItems.Create(AOwner: TComponent;
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
begin
   Self.Create(AOwner);
   Self.Tag := DBId;
   Self._terminal := Terminal;
   Self._operator := Operator;
   Self._operatorName := OperatorName;
   Self._table := Table;
   Self._number := Number;
   Self._pnumber := Pnumber;
   Self._dishName := DishName;
   Self.lblTitle.Caption := DishName;
   Self._dishModif := DishModif;
   Self.lblMod.Caption :=  DishModif;
   Self._quantity := Quantity;
   Self.lblQuantity.Caption := 'количество: ' + IntToStr(Quantity);
   Self._delay1 := Delay1;
   Self._delay2 := Delay2;
   Self._msgTo:= MsgTo;
   Self._revoked := false;
   Self._h := 0;
   Self._m := 0;

   Self.IsTrash := false;
end;


procedure TfrItems.StartCount(ts: string);
var dt: TDateTime;
var  MySettings: TFormatSettings;
begin
  GetLocaleFormatSettings(GetUserDefaultLCID, MySettings);
  MySettings.DateSeparator := '.';
  MySettings.TimeSeparator := ':';
  MySettings.ShortDateFormat := 'dd.mm.yyyy';
  MySettings.ShortTimeFormat := 'hh:nn:ss';

  try
    dt := StrToDateTime(ts, MySettings);
  except
   on E: Exception do
    begin
     MainForm.AddLog(True, 'Грешка при форматиране на дата: '+Trim(E.Message));
     MessageDlg('Грешка при форматиране на дата!'+#13+#10+Trim(E.Message), mtWarning, [mbOK], 0);
    end;
  end;

  self._ticks := SecondsBetween(dt, now());
  self._timerStart := now();
  self.tmr.Interval := 1000;
  self.tmr.Enabled := true;
end;

procedure TfrItems.tmrTimer(Sender: TObject);
var h,m: Integer;
var lbl: string;
begin
  self._ticks := _ticks + 1;

  if not _revoked then
  begin
    if ((_ticks/60) > _delay2) then
    begin
      pnldetails.Color := StringToColor(MainForm.ColorExpired);
      pnlTm.Color := StringToColor(MainForm.ColorExpired);
    end
    else if((_ticks/60) > _delay1) then
    begin
      pnldetails.Color := StringToColor(MainForm.ColorDelay);
      pnlTm.Color := StringToColor(MainForm.ColorDelay);
    end;
  end;

  h := _ticks div 3600;
  m := (_ticks - h*3600) div 60;

  if ((h <> _h) or (m <> _h)) then
  begin
    _h := h;
    _m := m;

    lbl := '';

    if (_h > 0) then
    begin
      lbl := IntToStr(_h) + ' ч. ';
    end;

    if (_m > 0) then
    begin
      lbl := lbl + IntToStr(_m) + ' мин.';
    end;

    lblTm.Caption := lbl;
  end;

  //lblTm.Caption := formatdatetime('nn:ss', _timerStart - now());
end;

procedure TfrItems.RemoveMe();
var buttonSelected : Integer;
var msgData: string;
begin
  if _revoked then
  buttonSelected := MyMessageDlg('Потвърдете сторнирана поръчка!' + AnsiString(#13#10) + AnsiString(#13#10) + self.getTitle(), mtConfirmation , [mbOK, mbCancel], 0)
  else
  buttonSelected := MyMessageDlg('Потвърдете изпълнена поръчка!' + AnsiString(#13#10) + AnsiString(#13#10) + self.getTitle(), mtConfirmation , [mbOK, mbCancel], 0);
  
  if buttonSelected = mrOK then
  begin
    if _revoked then
    msgData := 'Потвърдена сторнирана поръчка от кухнята: опер.: сторно ' + Self._operatorName + ', маса: ' + IntToStr(Self._table) + ', ' + Self._dishName + ' ' + Self._dishModif
    else
    msgData := 'Готова поръчка от кухнята: опер.: ' + Self._operatorName + ', маса: ' + IntToStr(Self._table) + ', ' + Self._dishName + ' ' + Self._dishModif;

    MainForm.RemoveItem(Self.Tag);
    
    Self.tmr.Enabled := False;
    Self.Visible := False;


    MainForm.AnnounceDish(msgData);    
  end;
end;

function TfrItems.getTitle(): string;
begin
 Result := 'Ястие: ' + self._dishName + ' ' + self._dishModif;
end;

procedure TfrItems.MouseUp();
begin
  MainForm.MouseUp();

  if not MainForm.Moved() then
    RemoveMe();
end;

procedure TfrItems.MouseDown();
begin
  MainForm.MouseDown();
end;

procedure TfrItems.MouseMove();
begin
  MainForm.MouseMove();
end;

procedure TfrItems.lblTitleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseUp();
end;

procedure TfrItems.lblTitleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDown();
end;

procedure TfrItems.pnlDetailsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown();
end;

procedure TfrItems.pnlDetailsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseUp();
end;

procedure TfrItems.lblModMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDown();
end;

procedure TfrItems.lblModMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseUp();
end;

procedure TfrItems.lblQuantityMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseUp();
end;

procedure TfrItems.lblQuantityMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown();
end;

procedure TfrItems.lblTmMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MouseUp();
end;

procedure TfrItems.lblTmMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDown();
end;

procedure TfrItems.lblTitleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  MouseMove();
end;

procedure TfrItems.lblModMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MouseMove();
end;

procedure TfrItems.lblQuantityMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseMove();
end;

procedure TfrItems.pnlDetailsMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  MouseMove();
end;

procedure TfrItems.lblTmMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MouseMove();
end;

end.
