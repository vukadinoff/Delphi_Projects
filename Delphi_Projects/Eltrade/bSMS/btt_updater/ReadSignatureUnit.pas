unit ReadSignatureUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ESKHandler;

type
  TfrmReadSignature = class(TForm)
    lMsg: TLabel;
    pBottom: TPanel;
    btnCancel: TButton;
    ESKTimer: TTimer;
    ProgressBar: TProgressBar;
    procedure FormActivate(Sender: TObject);
    procedure ESKTimerTimer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FESKReader  : Boolean;
    FESKData    : TEskData;
    FErrorMsg   : String;
    { Private declarations }
  public
    { Public declarations }
  end;


function ESK_ReadSignature(): Boolean;
procedure ShowWaitScreen(Msg_: String);
procedure ShowWaitScreenProgress(Percentage: Integer);
procedure HideWaitScreen;
procedure ShowWaitMessage(Msg_: String);

implementation
uses ESKReadUnit, ConstUnit;
{$R *.dfm}

var
  frmReadSignature: TfrmReadSignature;

function ESK_ReadSignature(): Boolean;
begin
 frmReadSignature := TfrmReadSignature.Create(Application);
 try
  frmReadSignature.FESKReader := true;
  frmReadSignature.ShowModal;
  CurrentESK.ESKData  := frmReadSignature.FESKData;
  Result := (frmReadSignature.FESKData.Serial <> '')and
            (frmReadSignature.FESKData.Code <> '')and
            (frmReadSignature.FESKData.Key <> '');
 finally
  FreeAndNil(frmReadSignature);
 end;
end;

procedure ShowWaitMessage(Msg_: String);
begin
 HideWaitScreen;
 frmReadSignature := TfrmReadSignature.Create(Application);
 if Application.MainForm <> nil then
  frmReadSignature.Caption      := Application.MainForm.Caption
 else
  frmReadSignature.Caption      := Application.Title;
 frmReadSignature.FESKReader    := false;
 frmReadSignature.lMsg.Caption  := Msg_;
 frmReadSignature.btnCancel.Caption := 'Затвори';
 frmReadSignature.btnCancel.Enabled := true;
 frmReadSignature.ShowModal;
end;

procedure ShowWaitScreen(Msg_: String);
begin
 if frmReadSignature = nil then frmReadSignature := TfrmReadSignature.Create(Application);
 frmReadSignature.Caption             := 'Моля изчакайте...';
 frmReadSignature.FESKReader          := false;
 frmReadSignature.lMsg.Caption        := Msg_;
 frmReadSignature.ProgressBar.Visible := false;
 if not frmReadSignature.Visible then frmReadSignature.Show;
 frmReadSignature.Update;
end;

procedure ShowWaitScreenProgress(Percentage: Integer);
begin
 if frmReadSignature <> nil then
  if frmReadSignature.Visible then
   begin
    frmReadSignature.ProgressBar.Position := Percentage;
    frmReadSignature.ProgressBar.Visible  := (Percentage in [0..100]);
   end;
end;

procedure HideWaitScreen;
begin
 if frmReadSignature <> nil then
  begin
   if frmReadSignature.Visible then frmReadSignature.Hide;
   FreeAndNil(frmReadSignature);
  end;
end;

procedure TfrmReadSignature.FormActivate(Sender: TObject);
begin
 if FESKReader then
  begin
   Self.Caption      := 'Проверка електронен ключ...';
   lMsg.Caption      := 'Проверка електронен ключ...';
   ESKTimer.Enabled  := true;
  end;
end;

procedure TfrmReadSignature.ESKTimerTimer(Sender: TObject);
var FESKModules: TStrings;
begin
 ESKTimer.Enabled  := false;
 btnCancel.Enabled := true;

 FESKModules := TStringList.Create;
 try
   case ESK_ReadEskDataInternal(1, 'ESC.', '', FESKData.Serial, FESKData.Version, FESKData.Code, FESKData.Key, FErrorMsg, FESKModules) of
   0: begin
       FESKData.Modules := FESKModules.Text;
       Close;
       Exit;
      end;
   cerr_NoDevicesFound:
      begin
       lMsg.Font.Style := [fsBold];
       lMsg.Caption    := FErrorMsg;
       ESKTimer.Interval := 200;
      end;
   cerr_NoValidDeviceFound, cerr_DeviceBussy:
      begin
       lMsg.Font.Style := [];
       lMsg.Caption    := FErrorMsg;
       ESKTimer.Interval := 3000;
      end;
   else
      begin
       lMsg.Font.Style := [];
       lMsg.Caption    := FErrorMsg;
       MessageDlg('Системна грешка: '+sLineBreak+
                  ''+sLineBreak+
                  FErrorMsg, mtError, [mbOK], 0);
       Close;
       Exit;
      end;
   end;
 finally
  FESKModules.Free;
  ESKTimer.Enabled := true;
 end;
end;

procedure TfrmReadSignature.btnCancelClick(Sender: TObject);
begin
 Close;
end;


end.
