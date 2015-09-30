unit ServiceAppConnUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, RXCtrls, ToolWin, Menus, ActnList, ExtCtrls,
  ImgList, Placemnt, ShFolder, IniFiles, ConstUnit, Buttons, RxCombos, ShellApi;

type
  TDBObject = class(TObject)
   private
    FAppPath  : String;
    FServer   : String;
    FProtocol : Integer;
    FDBPath   : String;
    FLastConn : TDateTime;
    function FGetConnString: String;
   public
    property AppPath: String read FAppPath write FAppPath;
    property Server: String read FServer write FServer;
    property Protocol: Integer read FProtocol write FProtocol;
    property DBPath: String read FDBPath write FDBPath;
    property LastConnTime: TDateTime read FLastConn write FLastConn;
    property ConnString: String read FGetConnString;
  end;

  TfrmServiceAppConn = class(TForm)
    radioNotActive: TRadioButton;
    radioActive: TRadioButton;
    comboSAppDB: TComboBox;
    lCurrentDB: TLabel;
    pBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label2: TLabel;
    cbFont: TFontComboBox;
    Label3: TLabel;
    cbCharset: TComboBox;
    Bevel3: TBevel;
    chkUseSiteTypeN: TCheckBox;
    btnFindSvcApp: TSpeedButton;
    chkGenSvidetelstvo: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure radioActiveClick(Sender: TObject);
    procedure comboSAppDBChange(Sender: TObject);
    procedure radioNotActiveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnFindSvcAppClick(Sender: TObject);
  private
    function GetSpecialFolderPath(Folder: integer): string;
    procedure GetServiceAppDBs;
    procedure ClearListDBs;
    function SetServiceAppDB: Boolean;
    function ValidateData: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmServiceAppConn: TfrmServiceAppConn;

implementation
uses ServiceAppDataUnit, FiscMainUnit;
{$R *.dfm}

function TDBObject.FGetConnString: String;
begin
 Result := '';
 if DBPath <> '' then
  begin
   case Protocol of
   0: Result := Server+':'+DBPath;
   1: Result := '\\'+Server+'\'+DBPath;
   2: Result := Server+'@'+DBPath;
   end;
  end;
end;

procedure TfrmServiceAppConn.FormCreate(Sender: TObject);
begin
// FormPlacement.IniFileName := ChangeFileExt(Application.ExeName, '.ini');
// FormPlacement.IniSection  := Self.ClassName;
end;

procedure TfrmServiceAppConn.FormShow(Sender: TObject);
var I : Integer;
    S : String;
begin
 GetServiceAppDBs;

 cbCharset.Clear;
 with cbCharset.Items do // Fnt Charset
  begin
   for I := 1 to 255 do
    begin
     CharsetToIdent(I, S);
     if IndexOf(S) < 0 then Add(S);
    end;
  end;
 CharsetToIdent(Set_Charset, S);
 cbCharset.ItemIndex := cbCharset.Items.IndexOf(S);
 cbFont.ItemIndex    := cbFont.Items.IndexOf(Set_FontName);

 chkUseSiteTypeN.Checked    := Set_UseSiteTypeNumb;
 chkGenSvidetelstvo.Checked := Set_GenSvidetelstvo;
end;


procedure TfrmServiceAppConn.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if ModalResult = mrOK then
  begin
   if not ValidateData then
    begin
     Action := caNone;
     Exit;
    end;

   Set_UseSiteTypeNumb := chkUseSiteTypeN.Checked;
   Set_GenSvidetelstvo := chkGenSvidetelstvo.Checked;


   if (not SetServiceAppDB)or(not SaveAppSettings) then
    begin
     Action := caNone;
     Exit;
    end;
  end;
 ClearListDBs;
end;

function TfrmServiceAppConn.ValidateData: Boolean;
begin
 Result := false;

 if (radioActive.Checked)and(comboSAppDB.ItemIndex < 0) then
  begin
   MessageDlg('Не сте избрали Сервизна база.', mtWarning, [mbOK], 0);
   comboSAppDB.SetFocus;
   Exit;
  end;
 if cbFont.ItemIndex < 0 then
  begin
   MessageDlg('Невалиден шрифт', mtError, [mbOK], 0);
   cbFont.SetFocus;
   Exit;
  end;
 if cbCharset.ItemIndex < 0 then
  begin
   MessageDlg('Невалидна кодова таблица', mtError, [mbOK], 0);
   cbFont.SetFocus;
   Exit;
  end;

 Result := true;
end;

function TfrmServiceAppConn.GetSpecialFolderPath(Folder: integer): string;
const SHGFP_TYPE_CURRENT = 0;
var Path: array [0..MAX_PATH] of Char;
begin
 if Succeeded(SHGetFolderPath(0, folder, 0, SHGFP_TYPE_CURRENT, @path[0])) then
   Result := IncludeTrailingPathDelimiter(Path)
 else
   Result := '';
end;

procedure TfrmServiceAppConn.GetServiceAppDBs;
var IFile  : TIniFile;
    sl     : TStrings;
    ConnStr: String;
    I      : Integer;
    DBObj  : TDBObject;
begin
  try
   // get DB info && load them into list
   ClearListDBs;
   lCurrentDB.Caption := '';
   ConnStr := GetSpecialFolderPath(CSIDL_COMMON_APPDATA)+'Eltrade\ServiceApplication\EBODatabase.ini';
   if not FileExists(ConnStr) then Exit;


   IFile := TIniFile.Create(ConnStr);
   sl    := TStringList.Create;
   try
     IFile.ReadSections(sl);
     for I := 0 to sl.Count-1 do
      begin
       DBObj := TDBObject.Create;
       DBObj.AppPath  := sl.Strings[I];
       DBObj.DBPath   := IFile.ReadString(sl.Strings[I],   'DBPath', '');
       DBObj.Server   := IFile.ReadString(sl.Strings[I],   'ServerName', 'localhost');
       DBObj.Protocol := IFile.ReadInteger(sl.Strings[I],  'Protocol', 0);
       DBObj.LastConnTime := IFile.ReadDateTime(sl.Strings[I], 'LastDBConnDTime', 0);

       if IFile.ReadInteger(sl.Strings[I],  'Connection', 1) = 0 then
        begin
         DBObj.Server   := 'localhost';
         DBObj.Protocol := 0;
        end;
       if (DBObj.Server = '')or(not (DBObj.Protocol in [0..3])) then
        begin
         DBObj.Server   := 'localhost';
         DBObj.Protocol := 0;
        end;

       if (AnsiLowerCase(DBObj.FServer) = 'localhost')or(DBObj.FServer = '127.0.0.1') then
        comboSAppDB.AddItem(ExtractFilePath(DBObj.AppPath), DBObj)
       else
        comboSAppDB.AddItem('['+DBObj.FServer+'] '+ExtractFilePath(DBObj.AppPath), DBObj);
      end;
   finally
    IFile.Free;
    sl.Free;
   end;

   IFile   := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
   try
    ConnStr := IFile.ReadString(C_DBIniSectionSvceApp, C_DBIniValConn, '');
   finally
    IFile.Free;
   end;

   comboSAppDB.ItemIndex := -1;
   for I := 0 to comboSAppDB.Items.Count-1 do
    begin
     if (comboSAppDB.Items.Objects[I] is TDBObject) then
      begin
       if (ConnStr = TDBObject(comboSAppDB.Items.Objects[I]).ConnString) then
        begin
         comboSAppDB.ItemIndex := I;
         Break;
        end;
      end;
    end;

   radioActive.Checked := comboSAppDB.ItemIndex >= 0;
   comboSAppDBChange(comboSAppDB);
  except
  end;
end;

procedure TfrmServiceAppConn.ClearListDBs;
var I: integer;
begin
 for I := 0 to comboSAppDB.Items.Count -1 do
  begin
   if Assigned(comboSAppDB.Items.Objects[I]) then comboSAppDB.Items.Objects[I].Free;
  end;
 comboSAppDB.Clear;
end;

function tfrmServiceAppConn.SetServiceAppDB: Boolean;
var CnnString : String;
begin
 Result    := true;
 CnnString := '';
 try
   if radioActive.Checked then
    begin
     if (comboSAppDB.ItemIndex >= 0)and
        (comboSAppDB.Items.Objects[comboSAppDB.ItemIndex] is TDBObject) then
      begin
       CnnString := TDBObject(comboSAppDB.Items.Objects[comboSAppDB.ItemIndex]).ConnString;
      end;
    end;

   with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
   try
    WriteString(C_DBIniSectionSvceApp, C_DBIniValConn, CnnString);
   finally
    Free;
   end;
 except
  Result := false;
 end;
end;


procedure TfrmServiceAppConn.radioActiveClick(Sender: TObject);
begin
 comboSAppDB.Enabled := radioActive.Checked;
end;

procedure TfrmServiceAppConn.comboSAppDBChange(Sender: TObject);
var Obj: TObject;
begin
 lCurrentDB.Caption := '';
 btnFindSvcApp.Enabled := false;
 if radioActive.Checked then
  begin
   if comboSAppDB.ItemIndex >= 0 then
    begin
     Obj := comboSAppDB.Items.Objects[comboSAppDB.ItemIndex];
     if Obj is TDBObject then
      begin
       lCurrentDB.Caption := 'Прог.: '+ExtractFilePath(TDBObject(Obj).AppPath)+sLineBreak+
                             'База : '+TDBObject(Obj).ConnString;
       btnFindSvcApp.Enabled := true;
       btnFindSvcApp.Hint    := ExtractFilePath(TDBObject(Obj).AppPath);
      end;
    end;
  end;
end;

procedure TfrmServiceAppConn.radioNotActiveClick(Sender: TObject);
begin
 comboSAppDB.Enabled := not radioNotActive.Checked;
 if radioNotActive.Checked then
  begin
   comboSAppDB.ItemIndex := -1;
   lCurrentDB.Caption := '';
  end; 
end;

procedure TfrmServiceAppConn.btnFindSvcAppClick(Sender: TObject);
begin
 ShellExecute(0, 'open', PChar(btnFindSvcApp.Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
