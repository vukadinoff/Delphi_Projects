unit ErrorsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxSpeedButton, StdCtrls, ConstUnit, dxCore, ExtCtrls;

type
  TErrorsForm = class(TForm)
    MemoErr: TMemo;
    pBottom: TPanel;
    btnClose: TdxSpeedButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure LoadData;
    { Private declarations }
  public
    { Public declarations }
    procedure FitComponents;
  end;

implementation
uses ResStrUnit, MainUnit, SetNumberUnit;

{$R *.dfm}

procedure TErrorsForm.LoadData;
begin
 MemoErr.Lines.Clear;
 if not FileExists(ExceptPath+C_ErrFName) then exit;
 MemoErr.Lines.LoadFromFile(ExceptPath+C_ErrFName);
 MemoErr.Lines.Insert(0, '');
 MemoErr.Lines.Insert(0, '.\Exceptions\'+C_ErrFName);
 MemoErr.Lines.Insert(0, '');
end;

procedure TErrorsForm.btnCloseClick(Sender: TObject);
begin
 Close;
end;

procedure TErrorsForm.FormCreate(Sender: TObject);
begin
 if not LoadControlsStrings(LanguagePath + LngFName, Self) then LoadControlsStrings(LocalPath + LngFName, Self);
end;

procedure TErrorsForm.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
var SMesg: TWMVScroll;
begin
 SMesg.Pos := 0;

 case Key of
 VK_ESCAPE: btnClose.Click;
 VK_RETURN: btnClose.Click;
 VK_LEFT:   begin
             SMesg.Msg := WM_HSCROLL;
             SMesg.ScrollCode:=SB_LINELEFT;
             MemoErr.Dispatch(SMesg); MemoErr.Dispatch(SMesg);
             Key := 0;
            end;
  VK_RIGHT: begin
             SMesg.Msg := WM_HSCROLL;
             SMesg.ScrollCode:=SB_LINERIGHT;
             MemoErr.Dispatch(SMesg); MemoErr.Dispatch(SMesg);
             Key := 0;
            end;
  VK_UP   : begin
             SMesg.Msg := WM_VSCROLL;
             SMesg.ScrollCode:=SB_LINEUP;
             MemoErr.Dispatch(SMesg); MemoErr.Dispatch(SMesg);
             Key := 0;
            end;
  VK_DOWN :begin
             SMesg.Msg := WM_VSCROLL;
             SMesg.ScrollCode:=SB_LINEDOWN;
             MemoErr.Dispatch(SMesg); MemoErr.Dispatch(SMesg);
             Key := 0;
           end;
 end;
end;

procedure TErrorsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TErrorsForm.FormShow(Sender: TObject);
begin
 Self.Color := Application.MainForm.Color;
 LoadData;
 MemoErr.Height := (Height - btnClose.Height - 20);
end;

procedure TErrorsForm.FitComponents;
begin
 Self.WindowState := wsMaximized;
 Application.ProcessMessages;

 btnClose.Left := Self.ClientWidth - btnClose.Width - 10;
 Application.ProcessMessages;
end;

end.
