unit MyMessageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, dxBitButton, RXCtrls, ImgList,
  dxCore, dxSpeedButton;

type
  TMyMessageForm = class(TForm)
    BtnPanel: TPanel;
    Image: TImage;
    MsgLabel: TLabel;
    ImageList: TImageList;
  private
    procedure PlaceButtons;
    { Private declarations }
  public
    procedure InsertButton(Kind: TBitBtnKind; Name, Caption: String);
    procedure SetImage(Ind: Integer);
    { Public declarations }
  end;

var
  MyMessageForm: TMyMessageForm;

function MyMessageDlg(const Msg: string; DlgType: TMsgDlgType;
        Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

implementation
uses ResStrUnit, Main;

function MyMessageDlg(const Msg: string; DlgType: TMsgDlgType;
        Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var Ws, Hs : Integer;
begin
 Ws := Screen.Width;
 Hs := Screen.Height;
 MyMessageForm := TMyMessageForm.Create(Application);
 MyMessageForm.Name := 'MyMessageForm';
 if MainForm <> nil then
  begin
   MyMessageForm.Font.Assign(MainForm.Font);
   MyMessageForm.MsgLabel.Font.Assign(MainForm.Font);
   MyMessageForm.Color := MainForm.Color;
   Ws := MainForm.Width;
   Hs := MainForm.Height;
  end;
 MyMessageForm.MsgLabel.Font.Size  := 16;
 MyMessageForm.MsgLabel.Font.Style := [fsBold];

 case DlgType of
  mtWarning      : MyMessageForm.SetImage(0);
  mtError        : MyMessageForm.SetImage(1);
  mtInformation  : MyMessageForm.SetImage(2);
  mtConfirmation : MyMessageForm.SetImage(3);
 end;

 if mbYes    in Buttons then MyMessageForm.InsertButton(bkYes,'btnYes', S_BtnYes);
 if mbNo     in Buttons then MyMessageForm.InsertButton(bkNo,'btnNo', S_BtnNo);
 if mbOK     in Buttons then MyMessageForm.InsertButton(bkOK,'btnOk',S_BtnOK);
 if mbCancel in Buttons then MyMessageForm.InsertButton(bkCancel,'btnCancel',S_BtnCancel);
 if mbAbort  in Buttons then MyMessageForm.InsertButton(bkAbort,'btnAbort',S_BtnAbort);
 if mbRetry  in Buttons then MyMessageForm.InsertButton(bkRetry,'btnRetry',S_BtnRetry);
 if mbIgnore in Buttons then MyMessageForm.InsertButton(bkIgnore,'btnIgnore',S_BtnIgnore);

// if POSStyle <> nil then POSStyle.FormatControlsAndColors(MyMessageForm, '');

 with MyMessageForm do
  begin
   MsgLabel.Caption := Msg;
   Caption          := S_ApplicationTitle;

   ClientHeight := MsgLabel.Height + BtnPanel.Height + 40;
   ClientWidth  := MsgLabel.Width + Image.Width + 60;

   MsgLabel.Top  := (ClientHeight - BtnPanel.Height - MsgLabel.Height) div 2;
   MsgLabel.Left := Image.Width + 25;

   Left   := (Ws - Width) div 2;
   Top    := (Hs - Height) div 2;

   PlaceButtons;
  end;

 MyMessageForm.ShowModal;
 Result := MyMessageForm.ModalResult;
 MyMessageForm.Free;
end;

{$R *.dfm}

procedure TMyMessageForm.SetImage(Ind : Integer);
var Bmp : TBitmap;
begin
 Bmp := TBitmap.Create;
 try
   ImageList.GetBitmap(Ind,Bmp);
   Bmp.PixelFormat := pf24bit;
   Image.Picture.Bitmap := Bmp;
 finally
  Bmp.Free;
 end;
end;

procedure TMyMessageForm.PlaceButtons;
var I : Integer;
begin
 for I := 0 to BtnPanel.ControlCount - 1 do
  if BtnPanel.Controls[I] is TdxBitButton then
   begin
    with TdxBitButton(BtnPanel.Controls[I]) do
     begin
      Top    := 4;
      Left   := BtnPanel.Width - 2 - ((Width + 1) * (I +1));
     end;
   end;
end;

procedure TMyMessageForm.InsertButton(Kind : TBitBtnKind; Name, Caption : String);
var NewCtrl : TdxBitButton;
begin
  NewCtrl := TdxBitButton.Create(Self);

  if MainForm <> nil then
   begin
    NewCtrl.Colors.BackgroundFrom  := MainForm.btnUp.Colors.BackgroundFrom;
    NewCtrl.Colors.BackgroundTo    := MainForm.btnUp.Colors.BackgroundTo;
   end;
  NewCtrl.Kind    := Kind;
  NewCtrl.Parent  := BtnPanel;
  NewCtrl.Name    := Name;
  NewCtrl.Caption := Caption;
  NewCtrl.Quality := bqHigh;

  NewCtrl.Width  := 105;
  NewCtrl.Height := BtnPanel.ClientHeight - 8;

end;

end.
