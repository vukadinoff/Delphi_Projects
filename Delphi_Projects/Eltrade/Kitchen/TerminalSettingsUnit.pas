unit TerminalSettingsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DB, IBDatabase, IBCustomDataSet, IBQuery,
  StdCtrls, Buttons, Menus, RXCtrls, Grids, Placemnt, DBGrids, RXDBCtrl,
  RxMemDS, ImgList;

type
  TTerminalSettingsForm = class(TForm)
    Panel2: TPanel;
    FormStorage: TFormStorage;
    DBGrid: TRxDBGrid;
    CloseBtn: TBitBtn;
    PopupMenu: TPopupMenu;
    NGridSettings: TMenuItem;
    StatusMemo: TMemo;
    NClockSet: TMenuItem;
    NAddMsg: TMenuItem;
    RefreshBtn: TBitBtn;
    MemData: TRxMemoryData;
    MemDataPOS_NAME: TStringField;
    MemDataPC_NAME: TStringField;
    MemDataPC_TCP: TStringField;
    MemDataERRORS: TBooleanField;
    DS: TDataSource;
    MemDataPOS_TYPE: TStringField;
    ImageList: TImageList;
    MemDataPOS_DB: TStringField;
    MemDataPOS_VERSION: TStringField;
    MemDataPOS_NUMBER: TStringField;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure NGridSettingsClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure DBGridTitleBtnClick(Sender: TObject; ACol: Integer;
      Field: TField);
    procedure DBGridGetBtnParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
      IsDown: Boolean);
    procedure RefreshBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LoadPOSTable;
    
  private
    SortField : String;
    SortAsc   : Boolean;
    FirstActivate: Boolean;
    procedure SetControls(Enabled_: Boolean);
    procedure UDPBroadcastAnswer(const FromIP_, FromHost_, Data_: String);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TerminalSettingsForm: TTerminalSettingsForm;

implementation
uses Main, DateUtils;
{$R *.dfm}

procedure TTerminalSettingsForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
 if MemData.Active then
  begin
   MemData.Close;
   MemData.EmptyTable;
  end;
 Action := caFree;
end;


procedure TTerminalSettingsForm.FormCreate(Sender: TObject);
begin
 LoadPOSTable;
end;

procedure TTerminalSettingsForm.SetControls(Enabled_: Boolean);
begin
 RefreshBtn.Enabled  := Enabled_;
 CloseBtn.Enabled    := Enabled_;
end;

procedure TTerminalSettingsForm.UDPBroadcastAnswer(const FromIP_, FromHost_, Data_: String);
begin
 if not MemData.Active then Exit;
 if MemData.Locate('PC_TCP', FromIP_, []) then
  begin
   MemData.Edit;
  end
 else
  begin
   MemData.Append;
   MemDataPC_TCP.AsString := FromIP_;
  end;
 with TStringList.Create do
 try
  Text := Data_;
  if IndexOfName('PosNumber') >= 0 then MemDataPOS_NUMBER.AsString  := Values['PosNumber'];
  if IndexOfName('PosName') >= 0 then   MemDataPOS_NAME.AsString    := Values['PosName'];
  if IndexOfName('LocalHost') >= 0 then MemDataPC_NAME.AsString     := Values['LocalHost'];
  if IndexOfName('Err') >= 0 then       MemDataERRORS.AsBoolean     := (Values['Err'] = 'YES');
  if IndexOfName('PosType') >= 0 then   MemDataPOS_TYPE.AsString    := Values['PosType'];
  if IndexOfName('DBServer') >= 0 then  MemDataPOS_DB.AsString      := Values['DBServer'];
  if IndexOfName('Version') >= 0 then   MemDataPOS_VERSION.AsString := Values['Version'];
 finally
  Free;
 end;
 MemData.Post;
end;

procedure TTerminalSettingsForm.LoadPOSTable;
var DataList : TStringList;
    I        : Integer;
    ST       : TDateTime;
begin
 SetControls(false);
 StatusMemo.Clear;

 if MemData.Active then
  begin
   MemData.Close;
   MemData.EmptyTable;
  end;
 MemData.Open;

 DataList := TStringList.Create;
 try
   DataList.Add('PosName=');
   DataList.Add('PosNumber=');
   DataList.Add('LocalHost=');
   DataList.Add('DBServer=');
   DataList.Add('Err=');
   DataList.Add('Version=');
   DataList.Add('PosType=');

   MainForm.UDP.OnBroadcastAnswer := UDPBroadcastAnswer;
   for I := 1 to 3 do
    begin
      if not MainForm.UDP.BroadcastCommand(6678, 'GetPosData', DataList.Text) then Break;

      // wait for answer
      ST := Now;
      repeat
       Application.ProcessMessages;
      until MilliSecondsBetween(ST, Now) >= 100;
    end;

 finally
  DataList.Free;
  MainForm.UDP.OnBroadcastAnswer := nil;

 end;

 MemData.SortOnFields('PC_TCP', true, false);
 MemData.First;
 SetControls(true);
end;

procedure TTerminalSettingsForm.NGridSettingsClick(Sender: TObject);
begin

 if MemData.IsEmpty then Exit;
 if MemDataPC_TCP.IsNull then Exit;

 Exit;
 end;

procedure TTerminalSettingsForm.CloseBtnClick(Sender: TObject);
begin
 Hide;
end;

procedure TTerminalSettingsForm.DBGridTitleBtnClick(Sender: TObject;
  ACol: Integer; Field: TField);
begin
 if Field.FieldName = SortField then
  SortAsc := not SortAsc
 else
  SortAsc := true;
 SortField := Field.FieldName;
 MemData.SortOnFields(SortField, true, not SortAsc);
end;

procedure TTerminalSettingsForm.DBGridGetBtnParams(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor;
  var SortMarker: TSortMarker; IsDown: Boolean);
begin
 if Field.FieldName = SortField then
  begin
   if SortAsc then SortMarker := smUp
    else SortMarker := smDown;
  end
 else
  SortMarker := smNone;
end;

procedure TTerminalSettingsForm.RefreshBtnClick(Sender: TObject);
begin
 LoadPOSTable;
end;

procedure TTerminalSettingsForm.FormActivate(Sender: TObject);
begin
if FirstActivate then LoadPOSTable;
FirstActivate := false;
end;

end.
