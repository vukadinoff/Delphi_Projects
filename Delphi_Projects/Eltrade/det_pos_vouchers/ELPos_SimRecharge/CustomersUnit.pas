unit CustomersUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, dxCore, dxSpeedButton, StdCtrls, IBSQL,
  DBUtilsUnit, ConstUnit;

type
  TCustomersForm = class(TForm)
    pCenter: TPanel;
    btnCancel: TdxSpeedButton;
    btnOK: TdxSpeedButton;
    pRight: TPanel;
    btnOrderByName: TdxSpeedButton;
    btnOrderByBulstat: TdxSpeedButton;
    btnOrderByTaxNumb: TdxSpeedButton;
    btnOrderByMOL: TdxSpeedButton;
    btnNewCustomer: TdxSpeedButton;
    Bevel4: TBevel;
    ListCustomers: TListBox;
    BtnFirst: TdxSpeedButton;
    btnUp: TdxSpeedButton;
    BtnLast: TdxSpeedButton;
    btnDown: TdxSpeedButton;
    lbTitle: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListCustomersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ListCustomersMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure btnCancelClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOrderByNameClick(Sender: TObject);
    procedure btnOrderByBulstatClick(Sender: TObject);
    procedure btnOrderByTaxNumbClick(Sender: TObject);
    procedure btnOrderByMOLClick(Sender: TObject);
    procedure btnNewCustomerClick(Sender: TObject);
    procedure BtnFirstClick(Sender: TObject);
    procedure BtnLastClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pCenterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    procedure MovePageUp;
    procedure MovePageDown;
    procedure AddCustomerToList(Bulst, BulstL, TaxNumb, FirmName, Town, Address, MOL, Receiver: string; Disc: real);
    { Private declarations }
  public
    DestObject : TObject; // записва там данните за избрания клиент

    procedure FitComponents;
    function LoadData(IBSQL: TIBSQL; Whr: string): boolean;    
    { Public declarations }
  end;

implementation
uses DataUnit, ResStrUnit, MyMessageUnit, CustomerDataUnit;

{$R *.dfm}

procedure TCustomersForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 DataMod.ClearList(ListCustomers);
 Action := caFree;
end;

function TCustomersForm.LoadData(IBSQL: TIBSQL; Whr: string): boolean;
var S: string;
begin
 Result := False;
 try
  try
   DataMod.ClearList(ListCustomers);
   S := 'SELECT * FROM CUSTOMERS_DATA WHERE (CUST_BULSTAT <> ''9876543219876'')';
   if Whr <> '' then S := S + ' AND ' + Whr;
   if not DB_ExecuteSQL(S, IBSQL, False, 'GetContragents') then exit;
   if (IBSQL.RecordCount <= 0) then exit;
   while not IBSQL.Eof do
    begin
     AddCustomerToList(IBSQL.FieldByName('CUST_BULSTAT').AsString,
                       IBSQL.FieldByName('CUST_BULST_LETTER').AsString,
                       IBSQL.FieldByName('CUST_TAXNUMB').AsString,
                       IBSQL.FieldByName('CUST_FIRMNAME').AsString,
                       IBSQL.FieldByName('CUST_TOWN').AsString,
                       IBSQL.FieldByName('CUST_FIRMADDRESS').AsString,
                       IBSQL.FieldByName('CUST_MOL').AsString,
                       IBSQL.FieldByName('CUST_RECEIVER').AsString,
                       IBSQL.FieldByName('CUST_DISCOUNT').AsInteger);
     IBSQL.Next;
    end;

   if (ListCustomers.Items.Count > 0) then ListCustomers.ItemIndex := 0;
   Result := True;
  except
  on E: Exception do
   begin
    DataMod.PostException('Fail loading contragents: ' + E.Message);
    DisplayErrMsg('Fail loading contragents.');
   end; 
  end;
 finally
  DB_CommitIBSQL(IBSQL);
 end;
end;

procedure TCustomersForm.AddCustomerToList(Bulst, BulstL, TaxNumb,
 FirmName, Town, Address, MOL, Receiver: string; Disc: real);
var CObj: TCustomerObject;
begin
 CObj := TCustomerObject.Create;
 CObj.FBulstat  := Bulst;
 CObj.FBulstatL := BulstL;
 CObj.FTaxNumb  := TaxNumb;
 CObj.FFirmName := FirmName;
 CObj.FTown     := Town;
 CObj.FAddress  := Address;
 CObj.FMOL      := MOL;
 CObj.FReceiver := Receiver;
 CObj.FDiscount := Disc;

 ListCustomers.AddItem('', CObj);
end;

procedure TCustomersForm.ListCustomersDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var Obj : TObject;
    TPos, Len: Integer;
    S   : String;
begin
 Obj := TListBox(Control).Items.Objects[Index];
 if (Obj <> nil) and (Obj is TCustomerObject) then
  begin
   TListBox(Control).Canvas.Brush.Color := TListBox(Control).Color;
   TListBox(Control).Canvas.Brush.Style := bsSolid;
   TListBox(Control).Canvas.Font.Color  := TListBox(Control).Font.Color;
   TListBox(Control).Canvas.Font.Style  := [];

   if Index = TListBox(Control).ItemIndex then
    begin
     TListBox(Control).Canvas.Pen.Color := clBlack;
     TListBox(Control).Canvas.Pen.Style := psSolid;
     TListBox(Control).Canvas.RoundRect(Rect.Left+1, Rect.Top+1, Rect.Right-1, Rect.Bottom-1, 5, 5);
    end;

   TPos := (Rect.Bottom - Rect.Top) div 2;

   // Line 1
   TListBox(Control).Canvas.Font.Size  := TListBox(Control).Canvas.Font.Size + 2;
   TListBox(Control).Canvas.Font.Style := [fsBold];
   S := TCustomerObject(Obj).FFirmName;
   Len := TListBox(Control).Canvas.TextWidth(S);
   TListBox(Control).Canvas.TextOut(Rect.Left + 4, Rect.Top + 2, S);
   TListBox(Control).Canvas.Font.Size  := TListBox(Control).Canvas.Font.Size - 2;
   TListBox(Control).Canvas.Font.Style := [];

   S := TCustomerObject(Obj).FMOL;
   TListBox(Control).Canvas.TextOut(Rect.Left + Len + 10, Rect.Top + 4, S);

   // Line2
   S := S_Bulstat + TCustomerObject(Obj).FBulstat + ' ' + TCustomerObject(Obj).FBulstatL+'; ';
   S := S + S_VATNo + ' '+ TCustomerObject(Obj).FTaxNumb;
   TListBox(Control).Canvas.TextOut(Rect.Left + 4, Rect.Top + Tpos , S);
  end;
end;

procedure TCustomersForm.ListCustomersMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
 Height := ((Abs(TListBox(Control).Font.Height) + Round(5)) * 2) + Round(8);
end;

procedure TCustomersForm.btnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TCustomersForm.MovePageUp;
begin
 if ListCustomers.ItemIndex > 0 then
   ListCustomers.ItemIndex := ListCustomers.ItemIndex-1;
 InvalidateRect(ListCustomers.Handle, nil, True);
end;

procedure TCustomersForm.MovePageDown;
begin
 if ListCustomers.ItemIndex < ListCustomers.Count-1 then ListCustomers.ItemIndex := ListCustomers.ItemIndex+1;
 InvalidateRect(ListCustomers.Handle, nil, True);
end;

procedure TCustomersForm.btnUpClick(Sender: TObject);
begin
 MovePageUp;
end;

procedure TCustomersForm.btnDownClick(Sender: TObject);
begin
 MovePageDown;
end;

procedure TCustomersForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
 VK_PRIOR,VK_UP,VK_LEFT:
  begin
   if btnUp.Enabled then btnUp.Click;
  end;
 VK_NEXT,VK_DOWN,VK_RIGHT:
  begin
   if btnDown.Enabled then btnDown.Click;
  end;
 end;
end;

procedure TCustomersForm.btnOKClick(Sender: TObject);
var Obj: TObject;
begin
 if (ListCustomers.Items.Count <= 0) then exit;
 if (ListCustomers.ItemIndex < 0) then
  begin
   MyMessageDlg(S_CustomerNotSelect, mtWarning, [mbOK], 0);
   if (ListCustomers.Items.Count > 0) then ListCustomers.ItemIndex := 0;
   Exit;
  end;
 Obj := ListCustomers.Items.Objects[ListCustomers.ItemIndex];
 if (Obj <> nil) and (Obj is TCustomerObject) then
  begin
   DataMod.SetDestObject(DestObject, TCustomerObject(Obj).FFirmName, TCustomerObject(Obj).FBulstat);
  end;
 Close;
end;

procedure TCustomersForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);
end;

procedure TCustomersForm.btnOrderByNameClick(Sender: TObject);
var I  : Integer;
    Obj: TObject;
begin
 ListCustomers.Sorted := False;
 for I := 0 to ListCustomers.Items.Count-1 do
  begin
   Obj := ListCustomers.Items.Objects[I];
   if (Obj <> nil) and (Obj is TCustomerObject) then
    begin
     ListCustomers.Items.Strings[I] := TCustomerObject(Obj).FFirmName;
    end;
  end;
 ListCustomers.Sorted := True;
end;

procedure TCustomersForm.btnOrderByBulstatClick(Sender: TObject);
var I  : Integer;
    Obj: TObject;
begin
 ListCustomers.Sorted := False;
 for I := 0 to ListCustomers.Items.Count-1 do
  begin
   Obj := ListCustomers.Items.Objects[I];
   if (Obj <> nil) and (Obj is TCustomerObject) then
    begin
     ListCustomers.Items.Strings[I] := TCustomerObject(Obj).FBulstat;
    end;
  end;
 ListCustomers.Sorted := True;
end;

procedure TCustomersForm.btnOrderByTaxNumbClick(Sender: TObject);
var I  : Integer;
    Obj: TObject;
begin
 ListCustomers.Sorted := False;
 for I := 0 to ListCustomers.Items.Count-1 do
  begin
   Obj := ListCustomers.Items.Objects[I];
   if (Obj <> nil) and (Obj is TCustomerObject) then
    begin
     ListCustomers.Items.Strings[I] := TCustomerObject(Obj).FTaxNumb+' '+TCustomerObject(Obj).FFirmName;
    end;
  end;
 ListCustomers.Sorted := True;
end;

procedure TCustomersForm.btnOrderByMOLClick(Sender: TObject);
var I  : Integer;
    Obj: TObject;
begin
 ListCustomers.Sorted := False;
 for I := 0 to ListCustomers.Items.Count-1 do
  begin
   Obj := ListCustomers.Items.Objects[I];
   if (Obj <> nil) and (Obj is TCustomerObject) then
    begin
     ListCustomers.Items.Strings[I] := TCustomerObject(Obj).FMOL+' '+TCustomerObject(Obj).FFirmName;
    end;
  end;
 ListCustomers.Sorted := True;
end;

procedure TCustomersForm.btnNewCustomerClick(Sender: TObject);
begin
 with TCustomerDataForm.Create(Self.Owner) do
  begin
   DestObject := Self.DestObject;
   ClearCtrls;
   FitComponents;
  end;
 Self.Close;
end;

procedure TCustomersForm.BtnFirstClick(Sender: TObject);
begin
 if ListCustomers.Items.Count <= 0 then exit;
 ListCustomers.ItemIndex := 0;
 InvalidateRect(ListCustomers.Handle, nil, True);
end;

procedure TCustomersForm.BtnLastClick(Sender: TObject);
begin
 if ListCustomers.Items.Count <= 0 then exit;
 ListCustomers.ItemIndex := ListCustomers.Items.Count-1;
 InvalidateRect(ListCustomers.Handle, nil, True);
end;

procedure TCustomersForm.FormShow(Sender: TObject);
begin
  if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
end;

procedure TCustomersForm.FitComponents;
begin
 WindowState := wsMaximized;
 Application.ProcessMessages;

 pCenter.Top    := lbTitle.Height + 1;
 pCenter.Left   := (Self.ClientWidth  - pCenter.Width - pRight.Width) div 2;
 pCenter.Height := Self.ClientHeight - lbTitle.Height - 2;

 pRight.Top     := lbTitle.Height + 1;
 pRight.Left    := pCenter.Left + pCenter.Width + 1;

 pCenter.Visible := true;
 pRight.Visible  := true;
 Application.ProcessMessages;
end;


procedure TCustomersForm.pCenterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
    I : Integer;
begin
 P.X := X;
 P.Y := Y;
 if pCenter.ControlAtPos(P, true, true) = ListCustomers then
  begin
   P.X := X - ListCustomers.Top;
   P.Y := Y - ListCustomers.Left;
   I := ListCustomers.ItemAtPos(P, True);
   if (I > -1 ) and (I <> ListCustomers.ItemIndex) then
    begin
     ListCustomers.ItemIndex := I;
     InvalidateRect(ListCustomers.Handle, nil, True); 
    end;
  end;
end;

end.
