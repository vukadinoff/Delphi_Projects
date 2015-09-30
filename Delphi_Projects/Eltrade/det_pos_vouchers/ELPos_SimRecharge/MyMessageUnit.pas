unit MyMessageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, dxBitButton, RXCtrls, ImgList,
  dxCore, ConstUnit, dxSpeedButton;

const
  C_ImageSize  = 128;
  C_ImageTheme = 4;
  C_FontSize   = 12;

type
  TMyMessageForm = class(TForm)
    BtnPanel: TPanel;
    Image: TImage;
    MsgLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    Scale : Real;
    procedure PlaceButtons;
    { Private declarations }
  public
    procedure InsertButton(Kind: TBitBtnKind; Name, Caption: String);
    procedure SetImage(DlgType: TMsgDlgType; Size: Integer=48; Style: Integer=0);
    { Public declarations }
  end;

var
  MyMessageForm: TMyMessageForm;

function MyMessageDlg(const Msg: string; DlgType: TMsgDlgType;
        Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

implementation
uses MainUnit, ResStrUnit;

function MyMessageDlg(const Msg: string; DlgType: TMsgDlgType;
        Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var Ws, Hs : Integer;
begin
 Ws := Screen.Width;
 Hs := Screen.Height;
 MyMessageForm := TMyMessageForm.Create(Application);
 try
   if MainForm <> nil then
    begin
     MyMessageForm.Font.Assign(MainForm.Font);
     MyMessageForm.MsgLabel.Font.Assign(MainForm.Font);
     MyMessageForm.Color := MainForm.Color;
     Ws := MainForm.Width;
     Hs := MainForm.Height;

     if (ScaleFactor > 0) then MyMessageForm.Scale := ScaleFactor;
    end;

   with MyMessageForm do
    begin
     Constraints.MinHeight := Constraints.MinHeight + Round(BtnPanel.Height * Scale) - BtnPanel.Height;
//     Constraints.MinWidth  := Round(Constraints.MinWidth * Scale);
     BtnPanel.Height := Round(BtnPanel.Height * Scale);

     MsgLabel.Font.Size  := Round(C_FontSize * Scale);
     MsgLabel.Font.Style := [fsBold];
     SetImage(DlgType, C_ImageSize, Set_MsgIconsTheme);
    end;

   if mbYes    in Buttons then MyMessageForm.InsertButton(bkYes,    'btnYes',    S_BtnYes);
   if mbNo     in Buttons then MyMessageForm.InsertButton(bkNo,     'btnNo',     S_BtnNo);
   if mbOK     in Buttons then MyMessageForm.InsertButton(bkOK,     'btnOk',     S_BtnOK);
   if mbCancel in Buttons then MyMessageForm.InsertButton(bkCancel, 'btnCancel', S_BtnCancel);
   if mbAbort  in Buttons then MyMessageForm.InsertButton(bkAbort,  'btnAbort',  S_BtnAbort);
   if mbRetry  in Buttons then MyMessageForm.InsertButton(bkRetry,  'btnRetry',  S_BtnRetry);
   if mbIgnore in Buttons then MyMessageForm.InsertButton(bkIgnore, 'btnIgnore', S_BtnIgnore);

   with MyMessageForm do
    begin
     MsgLabel.Caption := Msg;

     if C_AppTitle <> '' then Caption := C_AppTitle
      else Caption := Application.Title;

     ClientHeight := (MsgLabel.Height + BtnPanel.Height + Round(40 * Scale));
     ClientWidth  := (MsgLabel.Width + Image.Width + Round(60 * Scale));

     MsgLabel.Top  := (ClientHeight - BtnPanel.Height - MsgLabel.Height) div 2;
     MsgLabel.Left := Image.Left + Image.Width +  Round(25 * Scale);

     Left   := (Ws - Width) div 2;
     Top    := (Hs - Height) div 2;

     PlaceButtons;
    end;

   MyMessageForm.ShowModal;
   Result := MyMessageForm.ModalResult;
 finally
  MyMessageForm.Free;
 end;
end;

{$R *.dfm}

procedure TMyMessageForm.SetImage(DlgType: TMsgDlgType; Size: Integer=48; Style: Integer=0);
var Ind : Integer;
    Hnd : THandle;
begin
 try
   Hnd := LoadLibraryEx('EBOResources.dll', 0, LOAD_LIBRARY_AS_DATAFILE);
   if Hnd = 0 then Abort;
   try
    //    1     0      0
    //[тип][стил][диалог]
    case DlgType of
    mtWarning      : Ind := 101;
    mtError        : Ind := 102;
    mtInformation  : Ind := 103;
    mtConfirmation : Ind := 104;
    else             Ind := 105;
    end;

    Ind := Ind + Style*10;

    if FindResource(Hnd, MakeIntResource(Ind), RT_GROUP_ICON) = 0 then Abort;

    Image.Picture.Icon.ReleaseHandle;
    Image.Picture.Icon.Handle := LoadImage(Hnd, MakeIntResource(Ind), IMAGE_ICON, Size, Size, LR_DEFAULTCOLOR);

    if Image.Picture.Icon.Handle = 0 then Abort;
   finally
    FreeLibrary(Hnd);
   end;

 except
   Size := 32;
   case DlgType of
   mtWarning      : Image.Picture.Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
   mtError        : Image.Picture.Icon.Handle := LoadIcon(0, IDI_HAND);
   mtInformation  : Image.Picture.Icon.Handle := LoadIcon(0, IDI_ASTERISK);
   mtConfirmation : Image.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
   end;
 end;
 Image.Height := Size;
 Image.Width  := Size;
 Image.Top    := (Self.ClientHeight - BtnPanel.Height - Size) div 2;
 Image.Left   := Image.Top;
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
  if Scale < 1 then Scale := 1;
  NewCtrl := TdxBitButton.Create(Self);

  if MainForm <> nil then
   begin
    NewCtrl.Colors.BackgroundFrom  := MainForm.BtnClose.Colors.BackgroundFrom;
    NewCtrl.Colors.BackgroundTo    := MainForm.BtnClose.Colors.BackgroundTo;
   end;
  NewCtrl.Kind    := Kind;
  NewCtrl.Parent  := BtnPanel;
  NewCtrl.Name    := Name;
  NewCtrl.Caption := Caption;
  NewCtrl.Quality := bqHigh;

  NewCtrl.Font.Style := [fsBold];
  NewCtrl.Font.Size  := Round(NewCtrl.Font.Size * ScaleFactor);

  NewCtrl.Width  := Round(115 * Scale);
  NewCtrl.Height := BtnPanel.ClientHeight - 8;
end;

procedure TMyMessageForm.FormCreate(Sender: TObject);
begin
 Scale := 1;
end;

end.
