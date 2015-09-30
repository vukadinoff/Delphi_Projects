unit KGUnit;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Mask, ToolEdit, SysUtils;

type
  TKGForm = class(TForm)
    editSerial: TEdit;
    editKey: TEdit;
    Label1: TLabel;
    dateExp: TDateEdit;
    Label2: TLabel;
    Label3: TLabel;
    btnGenerate: TButton;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var KGForm: TKGForm;

implementation

uses ProtectUnit;

{$R *.dfm}

procedure TKGForm.FormCreate(Sender: TObject);
begin
 Protection.ModuleCode:= 'SimRechargeTSC';
 editSerial.Text      := Protection.GenerateSerialNumber;
 editKey.Text         := '';
 dateExp.Date         := Date;
end;

procedure TKGForm.btnGenerateClick(Sender: TObject);
begin
 editKey.Text := Protection.GenerateKey(editSerial.Text, dateExp.Date);
end;

procedure TKGForm.btnCloseClick(Sender: TObject);
begin
 Close;
end;

end.
