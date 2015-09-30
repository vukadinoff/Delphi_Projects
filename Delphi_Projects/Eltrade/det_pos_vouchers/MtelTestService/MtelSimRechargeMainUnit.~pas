unit MtelSimRechargeMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MtelSimRechargeUnit, IniFiles, StdCtrls, Placemnt;

type
  TMtelMainForm = class(TForm)
    FormStorage1: TFormStorage;
    { Controls }
    eUserName    : TEdit;
    ePassword    : TEdit;
    eLocation    : TEdit;
    eMSISDN      : TEdit;
    cbAmmount    : TComboBox;
    btnCheckPrima: TButton;
    btnPayPrima  : TButton;
    Memo1        : TMemo;
    { Control's labels }
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    grp1: TGroupBox;
    { Form create & destroy }
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    { Buttons action }
    procedure btnCheckPrimaClick(Sender: TObject);
    procedure btnPayPrimaClick(Sender: TObject);
  private
    { Private declarations }
    SRM: TSimRechargeMtel;

    LocalPath       : string;
    IniFilePathName : string;

    URL             : string;
    UserName        : string;
    Password        : string;
    SMSid           : string;

    procedure LoadIniSettings(IniFileName: string);
    procedure LayoutControls();
    function  FormatPhoneNumber(phone: string): string;
    function  CalcSMSid(): string;
    { Show error message }
    procedure PostError(const S: string);
  end;

const
  { New line }
  sLineBreak = AnsiString(#13#10);
  { Required length of sms id }
  SMS_ID_LENGTH = 26;
var
  MtelMainForm: TMtelMainForm;

implementation

{$R *.dfm}

function TMtelMainForm.CalcSMSid(): string;
var
  i: Integer;
begin
  Randomize;
  for i := 1 to SMS_ID_LENGTH do
  begin
    Result := Result + IntToStr(Random(10));
  end;
end;

procedure TMtelMainForm.LoadIniSettings(IniFileName: String);
var
  IniSection : String;
begin
  IniSection := 'SETTINGS';

  with TIniFile.Create(IniFileName) do
  try
    if (not ValueExists(IniSection, 'URL')) then
       WriteString(IniSection, 'URL', 'http://www.mtel.bg');
    URL := ReadString(IniSection, 'URL', '');

    if (not ValueExists(IniSection, 'UserName')) then
       WriteString(IniSection, 'UserName', 'eltrade');
    UserName := ReadString(IniSection, 'UserName', '');

    if (not ValueExists(IniSection, 'Password')) then
       WriteString(IniSection, 'Password', 'gdelchev');
    Password :=  ReadString(IniSection, 'Password', '');
  finally
    Free;
  end;

  SMSid := CalcSMSid();
end;

procedure TMtelMainForm.FormCreate(Sender: TObject);
begin
  FormStorage1.IniFileName := ChangeFileExt(Application.ExeName, '.ini');
  SRM := nil;

  LocalPath        := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  IniFilePathName  := LocalPath + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');

  LoadIniSettings(IniFilePathName);
end;

procedure TMtelMainForm.LayoutControls();
begin
  eMSISDN.Text := '';
  Memo1.Clear;
end;

procedure TMtelMainForm.FormActivate(Sender: TObject);
begin
  LayoutControls();
end;

procedure TMtelMainForm.FormDestroy(Sender: TObject);
begin
  if (SRM <> nil) then FreeAndNil(SRM);
end;

procedure TMtelMainForm.PostError(const S: string);
begin
  Memo1.Lines.Add('On error: ' + S + sLineBreak);
end;

function TMtelMainForm.FormatPhoneNumber(phone: string): string;
begin
  Result := '0' + Copy(phone, Length(phone) - 8, 9);
end;

procedure TMtelMainForm.btnCheckPrimaClick(Sender: TObject);
var ResultCode: Integer;
begin
  if (SRM <> nil) then FreeAndNil(SRM);

  SRM := TSimRechargeMtel.Create(URL, eLocation.Text);
  SRM.OnError := PostError;
  SRM.ReadTimeout := 5000;
  Memo1.Lines.Add('Mtel SIM recharge object created successful.');
  Memo1.Lines.Add(sLineBreak + 'Check Prima...');
  if (SRM.CheckPrima(FormatPhoneNumber(eMSISDN.Text), ResultCode) = resOK) then
    Memo1.Lines.Add('Check Prima is successful.')
  else
    Memo1.Lines.Add('Check Prima failed.' + SRM.LastError);
end;

procedure TMtelMainForm.btnPayPrimaClick(Sender: TObject);
var Res: Integer;
begin
  if (SRM = nil) then Exit;

  Memo1.Lines.Add(sLineBreak + 'Trying to pay Prima...');
  if (SRM.PayPrima(FormatPhoneNumber(eMSISDN.Text), StrToFloat(cbAmmount.Text), Res) = resOK) then
  begin
    Memo1.Lines.Add('Paying Prima is successful!');
  end
  else
  begin
    Memo1.Lines.Add('Trying to pay Prima is failed. ' + SRM.LastError);
  end;
end;

end.
