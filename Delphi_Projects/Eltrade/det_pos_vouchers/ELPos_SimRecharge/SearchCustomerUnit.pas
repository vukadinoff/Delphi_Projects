unit SearchCustomerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, dxCore, dxSpeedButton, ConstUnit, IBSQL,
  IBDataBase, DBUtilsUnit, MyMessageUnit, ImgList;
  
type
  TSearchCustomerForm = class(TForm)
    PanelSearch: TPanel;
    lbTitle: TLabel;
    lbBulstat: TLabel;
    lbTaxNumb: TLabel;
    HandImage: TImage;
    btnCancel: TdxSpeedButton;
    btnSearch: TdxSpeedButton;
    eBulstat: TEdit;
    eTaxNumb: TEdit;
    lbName: TLabel;
    eFirmName: TEdit;
    Bevel1: TBevel;
    imgChkBeginWith: TImage;
    lbBeginWith: TLabel;
    ImageList: TImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure eBulstatEnter(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure eBulstatClick(Sender: TObject);
    procedure imgChkBeginWithClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    function GetRecordCount(IBSQL: TIBSQL; MaxCount: integer): integer;
    { Private declarations }
  public
    DestObject : TObject;
    procedure FitComponents;
    { Public declarations }
  end;


implementation
uses MainUnit, DataUnit, ResStrUnit, CustomerDataUnit, KBDUnit, CustomersUnit;

{$R *.dfm}

procedure TSearchCustomerForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);

 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;

 DestObject := nil;
end;

procedure TSearchCustomerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TSearchCustomerForm.FitComponents;
begin
 WindowState := wsMaximized;
 Application.ProcessMessages;

 PanelSearch.Top  := (Self.ClientHeight - PanelSearch.Height) div 2 - 20;
 PanelSearch.Left := (Self.ClientWidth  - PanelSearch.Width) div 2;

 PanelSearch.Visible := true;
 eBulstat.SetFocus;
 Application.ProcessMessages;
end;

procedure TSearchCustomerForm.FormShow(Sender: TObject);
begin
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
 eBulstat.Text  := '';
 eTaxNumb.Text  := '';
 eFirmName.Text := '';
 HandImage.Top  := eBulstat.Top;
end;

procedure TSearchCustomerForm.eBulstatEnter(Sender: TObject);
begin
 HandImage.Top := TEdit(Sender).Top;
end;

procedure TSearchCustomerForm.btnCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TSearchCustomerForm.btnSearchClick(Sender: TObject);
var S: string;
    IBSQL : TIBSQL;
    IBT   : TIBTransaction;
    RecCnt: integer;
    Simbol: String;
    Whr: string;
begin
 IBSQL:= nil;
 IBT  := nil;
 if (eBulstat.Text = '') and (eTaxNumb.Text = '') and (eFirmName.Text = '') then
  begin
   Close;
   exit;
  end;

 try
  try
   DB_CreateIBSQL(IBSQL, IBT, DataMod.aIBDatabase);
   S := '';   Whr := '';
   if eBulstat.Text <> '' then
    begin
     if Whr <> '' then Whr := Whr + ' AND';
     Whr := Whr + ' (CUST_BULSTAT LIKE ' + QuotedStr('%'+eBulstat.Text+'%') + ')';
    end;
   if eTaxNumb.Text <> '' then
    begin
     if Whr <> '' then Whr := Whr + ' AND';
     Whr := Whr + ' (CUST_TAXNUMB LIKE ' + QuotedStr('%'+eTaxNumb.Text+'%') + ')';
    end;
   if eFirmName.Text <> '' then
    begin
     if imgChkBeginWith.Tag = 1 then Simbol := ''
      else Simbol := '%';
     if Whr <> '' then Whr := Whr + ' AND';
     Whr := Whr + ' ((CUST_FIRMNAME LIKE ' + QuotedStr(Simbol+AnsiLowerCase(eFirmName.Text)+'%') + ') OR';
     Whr := Whr + ' (CUST_FIRMNAME LIKE ' + QuotedStr(Simbol+AnsiUpperCase(eFirmName.Text)+'%') + ') OR';
     Whr := Whr + ' (CUST_FIRMNAME LIKE ' +
          QuotedStr(Simbol+AnsiUpperCase(Copy(eFirmName.Text,1,1))+
                    AnsiLowerCase(Copy(eFirmName.Text,2,Length(eFirmName.Text)-1))+'%') + '))';
    end;

   if (Whr = '') then exit;

   S := 'SELECT CUST_BULSTAT, CUST_TAXNUMB, CUST_FIRMNAME'+
        ' FROM CUSTOMERS_DATA WHERE' + Whr + ' ORDER BY CUST_BULSTAT ASC';
   if not DB_ExecuteSQL(S, IBSQL, False, 'SearchByCust') then exit;
   RecCnt := GetRecordCount(IBSQL, 55);

   if RecCnt = 0 then
    begin
     if MyMessageDlg(S_ContrNotFoundCnf, mtConfirmation, [mbYes,mbNo], 0) = mrNo then exit;
     // add customer                
     with TCustomerDataForm.Create(Self.Owner) do
      begin
       eBulstat.Text  := Self.eBulstat.Text;
       eTaxNumb.Text  := Self.eTaxNumb.Text;
       eFirmName.Text := Self.eFirmName.Text;
       DestObject     := Self.DestObject;
       FitComponents;
      end;
    end
   else
   if RecCnt = 1 then
    begin
     DataMod.SetDestObject(DestObject, IBSQL.FieldByName('CUST_FIRMNAME').AsString,
                           IBSQL.FieldByName('CUST_BULSTAT').AsString);
    end
   else
   if RecCnt > 50 then
    begin
     MyMessageDlg(S_ContrManyFound, mtWarning, [mbOK], 0);
     Exit;
    end
   else
    begin
     //show customers by...
     with TCustomersForm.Create(Self.Owner) do
      begin
       DestObject := Self.DestObject;
       FitComponents;
       if not LoadData(IBSQL, Whr) then Close;
      end;
    end;
  except
  on E: Exception do
   begin
    DataMod.PostException('Fail search cointragent: '+Trim(E.Message));
    DisplayErrMsg('Fail search cointragent.');
   end; 
  end;
 finally
  DB_DestroyIBSQL(IBSQL, IBT);
  Self.Close;
 end;
end;

procedure TSearchCustomerForm.eBulstatClick(Sender: TObject);
begin
 InputText(Sender);
// KBDForm.InputText(Sender);
end;

function TSearchCustomerForm.GetRecordCount(IBSQL: TIBSQL; MaxCount: integer): integer;
begin
 Result := 0;
 try
  if (IBSQL = nil) then exit;
  while not IBSQL.Eof do
   begin
    inc(Result);
    if (Result >= MaxCount) then break;
    IBSQL.Next;
   end;
 except
 on E: Exception do
  begin
   DataMod.PostException('Fail get records count: '+Trim(E.Message));
   DisplayErrMsg('Fail get records count.');
  end; 
 end;
end;

procedure TSearchCustomerForm.imgChkBeginWithClick(Sender: TObject);
begin
 if not (Sender is TImage) then exit;
 if TImage(Sender).Tag = 0 then TImage(Sender).Tag := 1
  else TImage(Sender).Tag := 0;
 ImageList.GetIcon(TImage(Sender).Tag, TImage(Sender).Picture.Icon);
end;

procedure TSearchCustomerForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
 VK_ESCAPE : btnCancel.Click;
 VK_RETURN : btnSearch.Click;
 end;
end;


end.
