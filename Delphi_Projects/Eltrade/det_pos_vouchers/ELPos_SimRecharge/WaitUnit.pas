unit WaitUnit;

interface

uses
  Windows, Controls, Forms, ResStrUnit, ConstUnit, StdCtrls, Classes;

type
  TWaitForm = class(TForm)
    lHeader: TLabel;
    lMessage: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure FitComponents;
    { Public declarations }
  end;

//var
//  WaitForm: TWaitForm;

implementation

uses MainUnit;

{$R *.dfm}

procedure TWaitForm.FitComponents;
begin
 Self.WindowState := wsMaximized;
 Application.ProcessMessages;

// lMsg.Left := (Self.Width - lMsg.Width) div 2;
 lHeader.Visible  := true;
 lMessage.Visible := true;
 Application.ProcessMessages;
end;

procedure TWaitForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TWaitForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);
 if Set_StatusColor > 0 then Self.Color := Set_StatusColor;
 MainForm.ScaleControls(Self); 
end;

end.
