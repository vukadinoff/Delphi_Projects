unit RegistrationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DB, IBCustomDataSet, IBQuery, IBDatabase,
  ExtCtrls, ConstUnit, dxCore, dxSpeedButton;

type
  TRegistrationForm = class(TForm)
    regTransaction: TIBTransaction;
    regQuery: TIBQuery;
    pSerial: TPanel;
    lSerial: TLabel;
    SerialEdit: TEdit;
    PanelBtm: TPanel;
    btnCancel: TdxSpeedButton;
    btnOK: TdxSpeedButton;
    pKey: TPanel;
    lKey: TLabel;
    KeyEdit: TEdit;
    Panel1: TPanel;
    Label1: TLabel;
    cbStorage: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function CheckStatus(CheckSerial: Boolean): boolean;
    { Public declarations }
  end;

implementation

uses ResStrUnit, DataUnit, ProtectUnit, MyMessageUnit;

{$R *.dfm}

function TRegistrationForm.CheckStatus(CheckSerial: Boolean): boolean;
var sn, path, key: string;
begin
 Result := False;
 Protection.ModuleCode := C_CurrentModuleCode;
 sn   := Protection.GenerateSerialNumber;
 path := Application.ExeName;
 key  := '';
 KeyEdit.Visible := CheckSerial;
 pKey.Visible    := CheckSerial;

 with regQuery do
  begin
   if Transaction.InTransaction then Transaction.Commit;
   if not Transaction.InTransaction then Transaction.StartTransaction;
   if Active then Close;
   SQL.Clear;
   SQL.Add('SELECT * FROM NN_MODULES');
   SQL.Add('WHERE (MODULE_SERIAL = ' + QuotedStr(sn) + ')');
   SQL.Add('AND (MODULE_DESCR = '+ QuotedStr(GetHostName) +')');
   SQL.Add('AND (MODULE_PATH = '+ QuotedStr(path) +')');
   Open;

   if (not FieldByName('MODULE_ID').IsNull) then
    begin
     CurrentModuleID   := FieldByName('MODULE_ID').AsInteger;
     CurrentWorkStorage:= FieldByName('MODULE_WORK_STORAGE').AsInteger;
     key               := FieldByName('MODULE_KEY').AsString;
     if CheckSerial then Result := (Protection.ValidateKey(sn, key)) and (not Protection.TimedOut(key))
      else Result := true;
    end;

   if Active then Close;
   if Transaction.InTransaction then Transaction.Commit;
  end;

 SerialEdit.Text := sn;
 KeyEdit.Text    := key;
end;

procedure TRegistrationForm.FormShow(Sender: TObject);
begin
  Protection.ModuleCode := C_CurrentModuleCode;
  if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
  if KeyEdit.Visible then KeyEdit.SetFocus;

  if CurrentWorkStorage <= 0 then CurrentWorkStorage := DataMod.GetFirstStorageNumb;

  cbStorage.Clear;
  with regQuery do
   begin
    if Active then Close;
    if Transaction.InTransaction then Transaction.Commit;
    SQL.Clear;
    SQL.Add('SELECT STORAGE_NUMB, STORAGE_NAME FROM N_STORAGES ORDER BY STORAGE_NUMB ASC');
    try
     Open;
     while not Eof do
      begin
       if CurrentWorkStorage = FieldByName('STORAGE_NUMB').AsInteger then
        cbStorage.ItemIndex := cbStorage.Items.AddObject(FieldByName('STORAGE_NAME').AsString, TObject(FieldByName('STORAGE_NUMB').AsInteger))
       else
        cbStorage.Items.AddObject(FieldByName('STORAGE_NAME').AsString, TObject(FieldByName('STORAGE_NUMB').AsInteger));
       Next;
      end;
     if Active then Close;
     if Transaction.InTransaction then Transaction.Commit;
    except
     on E: Exception do
      begin
       if Active then Close;
       if Transaction.InTransaction then Transaction.Commit;
       DataMod.PostException('Error loading storages list: '+E.Message);
      end;
    end;
   end;
end;

procedure TRegistrationForm.FormClose(Sender: TObject; var Action: TCloseAction);
var sn   : String;
    Path : String;
    HName: String;
begin
 if (ModalResult <> mrOK) then exit;

 if (SerialEdit.Text = '') or ((KeyEdit.Text = '')and(KeyEdit.Visible)) then
  begin
   MyMessageDlg(S_InvalidRegKey, mtWarning, [mbOK], 0);
   if KeyEdit.Visible then KeyEdit.SetFocus;
   Action := caNone;
   Exit;
  end;
 if cbStorage.ItemIndex < 0 then
  begin
   MyMessageDlg(S_InvalidStorageNumb, mtError, [mbOK], 0);
   Action := caNone;
   Exit;
  end;

 if Integer(cbStorage.Items.Objects[cbStorage.ItemIndex]) = CurrentWorkStorage then
  begin
    if MyMessageDlg(S_StorageStr1 + IntToStr(Integer(cbStorage.Items.Objects[cbStorage.ItemIndex]))+#13+
                    '"'+DataMod.GetStorageName(Integer(cbStorage.Items.Objects[cbStorage.ItemIndex]))+'"', mtWarning, [mbYes,mbNo], 0) = mrNo then
     begin
      Exit;
     end;
  end
 else
  begin
    if MyMessageDlg(S_StorageStr2+#13+#10+
                  S_StorageStr3+IntToStr(CurrentWorkStorage)+' '+CurrentStorageName+#13+#10+
                  S_StorageStr4+IntToStr(Integer(cbStorage.Items.Objects[cbStorage.ItemIndex]))+' '+
                  '"'+DataMod.GetStorageName(Integer(cbStorage.Items.Objects[cbStorage.ItemIndex]))+'"', mtWarning, [mbYes,mbNo], 0) = mrNo then
     begin
      Exit;
     end;
    CurrentWorkStorage := Integer(cbStorage.Items.Objects[cbStorage.ItemIndex]);
  end;



 sn   := Protection.GenerateSerialNumber;
 Path := Application.ExeName;
 HName:= GetHostName;

 if KeyEdit.Visible then
  if (not Protection.ValidateKey(sn, KeyEdit.Text)) or Protection.TimedOut(KeyEdit.Text) then
   begin
    MyMessageDlg(S_InvalidRegKey, mtWarning, [mbOK], 0);
    if KeyEdit.Visible then KeyEdit.SetFocus;
    Action := caNone;
    exit
   end;

 CurrentModuleID := -1;
 with regQuery do
  begin
   if Transaction.InTransaction then Transaction.Rollback;
   if not Transaction.InTransaction then Transaction.StartTransaction;
   if Active then Close;
   SQL.Clear;
   SQL.Add('SELECT * FROM NN_MODULES');
   SQL.Add('WHERE (MODULE_SERIAL = ' + QuotedStr(sn) + ')');
   SQL.Add('AND (MODULE_DESCR = '+ QuotedStr(HName) +')');
   SQL.Add('AND (MODULE_PATH = '+ QuotedStr(Path) +')');
   Open;
   if (not FieldByName('MODULE_ID').IsNull) then CurrentModuleID := FieldByName('MODULE_ID').AsInteger;
  end;

 // write to database the new registration
 with regQuery do
  try
   if Active then Close;
   if CurrentModuleID < 0 then
    begin
     SQL.Clear;
     SQL.Add('INSERT INTO NN_MODULES');
     SQL.Add('(MODULE_NUMB, MODULE_ACCESSNUMB, MODUL_NAME, MODULE_DESCR,');
     SQL.Add('MODULE_PATH, MODULE_SERIAL, MODULE_KEY, MODULE_WORK_STORAGE)');
     SQL.Add('VALUES (');
     SQL.Add(IntToStr(C_CurrentModuleNumb)+ ','); // MODULE_NUMB
     SQL.Add(InttoStr(OperatorAccessNumb) + ','); // MODULE_ACCESSNUMB
     SQL.Add(QuotedStr(CurrentModuleName) + ','); // MODUL_NAME
     SQL.Add(QuotedStr(HName)             + ','); // MODULE_DESCR
     SQL.Add(QuotedStr(Path)              + ','); // MODULE_PATH
     SQL.Add(QuotedStr(sn)                + ','); // MODULE_SERIAL
     SQL.Add(QuotedStr(KeyEdit.Text)      + ','); // MODULE_KEY,
     SQL.Add(IntToStr(CurrentWorkStorage) + ')'); // MODULE_WORK_STORAGE
    end
   else
    begin
     SQL.Clear;
     SQL.Add('UPDATE NN_MODULES SET');
     SQL.Add('MODULE_KEY = ' + QuotedStr(KeyEdit.Text) +', ');
     SQL.Add('MODULE_WORK_STORAGE = ' + IntToStr(CurrentWorkStorage));
     SQL.Add('WHERE (MODULE_ID = ' + IntToStr(CurrentModuleID) + ')');
    end;

   Prepare;
   ExecSQL;
   if Transaction.InTransaction then Transaction.Commit;
  except
   on E: Exception do
    begin
     if Transaction.InTransaction then Transaction.Rollback;
     CurrentModuleID := -1;
     CurrentWorkStorage := -1;
     DataMod.PostException('Fail to register module: '+Trim(E.Message));
     MyMessageDlg('Fail to register module.', mtError, [mbOK], 0);
     exit;
    end;
  end;

 if CurrentModuleID < 0 then
  begin
   with regQuery do
    try
     if not Transaction.InTransaction then Transaction.StartTransaction;
     if Active then Close;
     SQL.Clear;
     SQL.Add('SELECT * FROM NN_MODULES');
     SQL.Add('WHERE (MODULE_SERIAL = ' + QuotedStr(sn) + ')');
     SQL.Add('AND (MODULE_DESCR = '+ QuotedStr(HName) +')');
     SQL.Add('AND (MODULE_PATH = '+ QuotedStr(Path) +')');
     Open;

     if not FieldByName('MODULE_ID').IsNull then
      begin
       CurrentModuleID := FieldByName('MODULE_ID').AsInteger;
       CurrentWorkStorage:= FieldByName('MODULE_WORK_STORAGE').AsInteger;       
      end;
     if Transaction.InTransaction then Transaction.Commit;
    except
     on E: Exception do
      begin
       if Transaction.InTransaction then Transaction.Rollback;
       CurrentModuleID := -1;
       CurrentWorkStorage := -1;
       DataMod.PostException('Fail to read module registration data: ' + Trim(E.Message));
       MyMessageDlg('Fail to read module registration data.', mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TRegistrationForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);

 CurrentModuleID := -1;
end;

end.
