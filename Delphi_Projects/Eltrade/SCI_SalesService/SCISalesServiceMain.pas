unit SCISalesServiceMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms, Dialogs,
  StdCtrls, IdHTTP, uLkJSON, HTTPApp, IdTCPConnection, IdSSLOpenSSL;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1  : TMemo;
    edtClient: TEdit;
    edtCard: TEdit;
    lblClient: TLabel;
    lblCard: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  http: TIdHTTP;
  stringStream: TStringStream;
  jsonToSend: TlkJSONObject;
  stringToSend: string;
  LHandler: TIdSSLIOHandlerSocket;
begin
  http := TIdHttp.Create(nil);
  http.HandleRedirects := True;
  http.ReadTimeout := 5000;
  http.Request.ContentType := 'application/json';
  http.Request.AcceptCharSet := 'utf-8';

  LHandler := TIdSSLIOHandlerSocket.Create(nil);
  http.IOHandler := LHandler;

  jsonToSend := TlkJSONObject.Create;
  jsonToSend.Add('responseLocale', 'bg-BG');
  jsonToSend.Add('merchantUsername', 'user01');
  jsonToSend.Add('merchantPassword', 'passwd');
  jsonToSend.Add('cardNo', '0025452548545');

  stringToSend := TlkJSON.GenerateText(jsonToSend);
  stringStream := TStringStream.Create(stringToSend);

  Memo1.Lines.Text := http.Post('https://node01.remotecall.ws/sales/verifyClient/v0/', stringStream);

  jsonToSend.free;
  LHandler.Free;
  http.free;
end;

end.

