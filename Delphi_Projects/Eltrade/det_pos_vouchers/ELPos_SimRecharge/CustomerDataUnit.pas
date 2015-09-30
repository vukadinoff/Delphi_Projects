unit CustomerDataUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxCore, dxSpeedButton, IBSQL, DBUtilsUnit,
  IBDataBase, ConstUnit, ExtCtrls, StdCtrls;

type
  TCustomerDataForm = class(TForm)
    pCustomer: TPanel;
    lbTitle: TLabel;
    lbBulstat: TLabel;
    lbTaxNumb: TLabel;
    imgSel: TImage;
    lbName: TLabel;
    eBulstat: TEdit;
    eTaxNumb: TEdit;
    eFirmName: TEdit;
    lbTown: TLabel;
    lbAddress: TLabel;
    lbMOL: TLabel;
    lbReceive: TLabel;
    eAddress: TEdit;
    eTown: TEdit;
    eMOL: TEdit;
    eReceiver: TEdit;
    btnCancel: TdxSpeedButton;
    btnOK: TdxSpeedButton;
    Bevel1: TBevel;
    procedure btnCancelClick(Sender: TObject);
    procedure eBulstatEnter(Sender: TObject);
    procedure eBulstatClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function DataValidity: boolean;
    function InsertCustomer: boolean;
    { Private declarations }
  public
    DestObject : TObject; // записва там данните за избрания клиент
    procedure FitComponents;
    procedure ClearCtrls;
    { Public declarations }
  end;

implementation
uses MainUnit, DataUnit, ResStrUnit, CheckDigitsUnit, MyMessageUnit, KBDUnit, CustomersUnit;

{$R *.dfm}

procedure TCustomerDataForm.FitComponents;
begin
 WindowState := wsMaximized;
 Application.ProcessMessages;

 pCustomer.Top  := (Self.ClientHeight-pCustomer.Height) div 2 - 20;
 pCustomer.Left := (Self.ClientWidth-pCustomer.Width) div 2;
 pCustomer.Visible := True;
 Application.ProcessMessages;
end;

procedure TCustomerDataForm.btnCancelClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TCustomerDataForm.eBulstatEnter(Sender: TObject);
begin
 imgSel.Top := TEdit(Sender).Top;
end;

procedure TCustomerDataForm.ClearCtrls;
var I: integer;
begin
 for I := 0 to ComponentCount-1 do
  begin
   if (Components[I] is TEdit) then
     TEdit(Components[I]).Text := '';
  end;
end;

function TCustomerDataForm.DataValidity: boolean;
begin
 Result := False;
 if (eBulstat.Text = '') then
  begin
   MyMessageDlg(S_Required+':'+QuotedStr(Copy(lbBulstat.Caption,1,(length(lbBulstat.Caption)-1)))+
                '.', mtWarning, [mbOK], 0);
   eBulstat.SetFocus;
   Exit;
  end;

 if not CheckBulstat(eBulstat.Text) then
  begin
   if MyMessageDlg(S_Invalid+':'+QuotedStr(Copy(lbBulstat.Caption,1,(length(lbBulstat.Caption)-1)))+'.'+sLineBreak+
                   S_ConfirmReq, mtInformation, [mbYes, mbNo], 0) = mrNo then
    begin
     eBulstat.SetFocus;
     Exit;
    end;
  end;

 if (eFirmName.Text = '') then
  begin
   MyMessageDlg(S_Required+':'+QuotedStr(Copy(lbName.Caption,1,(length(lbName.Caption)-1)))+
                '.', mtWarning, [mbOK], 0);
   eFirmName.SetFocus;
   exit;
  end;

 Result := True;
end;

function TCustomerDataForm.InsertCustomer: boolean;
var IBSQL: TIBSQL;
    IBT  : TIBTransaction;
begin
 Result := False;
 IBSQL  := nil;
 IBT    := nil;
 try
  try
   DB_CreateIBSQL(IBSQL, IBT, DataMod.aIBDatabase);
   if not DB_ExecuteSQL('INSERT INTO CUSTOMERS_DATA'+
                        ' (CUST_BULSTAT, CUST_TAXNUMB, CUST_FIRMNAME, CUST_TOWN,'+
                        ' CUST_FIRMADDRESS, CUST_MOL, CUST_RECEIVER) VALUES ('+
                        StrToSQL_(eBulstat.Text) +','+
                        StrToSQL_(eTaxNumb.Text) +','+
                        StrToSQL_(eFirmName.Text)+','+
                        StrToSQL_(eTown.Text)    +','+
                        StrToSQL_(eAddress.Text) +','+
                        StrToSQL_(eMOL.Text)     +','+
                        StrToSQL_(eReceiver.Text)+')', IBSQL, True, 'InsCustomer') then exit;
   DataMod.PostEvent(5, S_ContragentInserted+':'+eFirmName.Text+' ('+eBulstat.Text+').');
   Result := True;
  except
   on E: Exception do
    begin
     DataMod.PostException('Fail insert contragent: '+E.Message);
     if (Pos('PRIMARY', E.Message) > 1) then
      begin
       MyMessageDlg(S_DuplicateBulstat, mtError, [mbOK], 0);
       DisplayErrMsg(S_DuplicateBulstat);
       eBulstat.SetFocus;
      end
     else
      begin
       MyMessageDlg(S_ErrInsContragent, mtError, [mbOK], 0);
       DisplayErrMsg(S_ErrInsContragent);
      end;
    end;
  end;
 finally
  DB_DestroyIBSQL(IBSQL, IBT);
 end;
end;

procedure TCustomerDataForm.eBulstatClick(Sender: TObject);
begin
 InputText(Sender);
 //KBDForm.InputText(Sender);
end;

procedure TCustomerDataForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree; 
end;

procedure TCustomerDataForm.btnOKClick(Sender: TObject);
begin
 if (DataValidity) and (InsertCustomer) then
  begin
   DataMod.SetDestObject(DestObject, eFirmName.Text, eBulstat.Text);
   Close;
  end;
end;

procedure TCustomerDataForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);

 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
end;

end.
