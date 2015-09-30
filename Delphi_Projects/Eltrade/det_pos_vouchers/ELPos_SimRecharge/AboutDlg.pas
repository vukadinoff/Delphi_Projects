unit AboutDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Dialogs,
     RXCtrls, IniFiles;

type
  TAboutBox = class(TForm)
    PanelCenter: TPanel;
    ProgramIcon: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lbVersion: TLabel;
    lbTitle: TRxLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation
uses ResStrUnit, ConstUnit;
{$R *.DFM}

procedure TAboutBox.FormShow(Sender: TObject);
var IniFile : TIniFile;
    Res     : Integer;
begin
  lbVersion.Caption := S_Version + ' ' + GetVersion;

  IniFile := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+C_POSIniFileName);
  try
   Res := IniFile.ReadInteger('MAIN_WINDOW', 'WorkResolution',  0);
  finally
   IniFile.Free;
  end;

  Self.Left := 0;
  Self.Top  := 0;

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

  case Res of
  0  : Self.Width := 800;
  1,3: Self.Width := 1024;
  2  : Self.Width := 1152;
  4,7: Self.Width := 1280;
  5  : Self.Width := 1365;
  6  : Self.Width := 1536;
  else Self.Width := Screen.Width;
  end;

  case Res of
  0,3: Self.Height := 600;
  1,5: Self.Height := 768;
  2,6: Self.Height := 864;
  4  : Self.Height := 720;
  7  : Self.Height := 800;
  else Self.Height := Screen.Height;
  end;

 PanelCenter.Left := (Self.ClientWidth - PanelCenter.Width) div 2;
 PanelCenter.Top  := (Self.ClientHeight - PanelCenter.Height) div 2;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);
end;

end.

