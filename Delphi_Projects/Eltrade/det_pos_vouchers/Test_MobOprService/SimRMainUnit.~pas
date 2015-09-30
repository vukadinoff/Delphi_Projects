unit SimRMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SimRchVivacomUnit, StdCtrls, Placemnt;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    eURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eLocatoion: TEdit;
    FormStorage1: TFormStorage;
    Button3: TButton;
    Button4: TButton;
    Label3: TLabel;
    eMsisDn: TEdit;
    Memo1: TMemo;
    Label4: TLabel;
    eAmmount: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
   SRV: TSimRechargeVivacom;

   procedure PostError(const S: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 FormStorage1.IniFileName := ChangeFileExt(Application.ExeName, '.ini');
 SRV := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if SRV <> nil then FreeAndNil(SRV);
end;

procedure TForm1.PostError(const S: string);
begin
 Memo1.Lines.Add('ON ERROR: '+S);
 Memo1.Lines.Add('');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 if SRV <> nil then FreeAndNil(SRV);
 SRV := TSimRechargeVivacom.Create(eURL.Text, eLocatoion.Text);
 SRV.OnError := PostError;
 SRV.ReadTimeout := 5000;
 Memo1.Lines.Add('Comm object created!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 if SRV <> nil then FreeAndNil(SRV);
 Memo1.Lines.Add('Comm object destroyed!');
end;

procedure TForm1.Button3Click(Sender: TObject);
var Res: Integer;
begin
 if SRV = nil then Exit;

 Memo1.Lines.Add('');
 Memo1.Lines.Add('--- CheckPrePaid ---');

 if SRV.CheckPrePaid(eMsisDn.Text, Res) = resOK then
  begin
   Memo1.Lines.Add('   Œ  ');
   Memo1.Lines.Add('   Code: '+IntToStr(Res));
   Memo1.Lines.Add('   Text: '+SRV.TranslateResultCodeCheck(Res));
  end
 else
  begin
   Memo1.Lines.Add('FAIL: CheckPrePaid '+SRV.LastError);
  end;
end;


procedure TForm1.Button4Click(Sender: TObject);
var Res: Integer;
begin
 if SRV = nil then Exit;

 Memo1.Lines.Add('');
 Memo1.Lines.Add('--- PayPrePaid ---');

 if SRV.PayPrePaid(eMsisDn.Text, StrToFloat(eAmmount.Text), Res) = resOK then
  begin
   Memo1.Lines.Add('   Œ  ');
   Memo1.Lines.Add('   Code: '+IntToStr(Res));
   Memo1.Lines.Add('   Text: '+SRV.TranslateResultCodePay(Res));
  end
 else
  begin
   Memo1.Lines.Add('FAIL: PayPrePaid '+SRV.LastError);
  end;
end;

end.
