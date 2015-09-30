unit EltradeKitchen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, fraItems;

type
  TfrmKitchen = class(TForm)
    scrItems: TScrollBox;
    Tm: TTimer;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKitchen: TfrmKitchen;

implementation

{$R *.dfm}

procedure TfrmKitchen.Button1Click(Sender: TObject);
var fr:TfrItems;
begin

  try
    fr := TfrItems.Create(scrItems);
    fr.Tag := scrItems.ComponentCount + 1;
    fr.lblTitle.Caption := IntToStr(fr.Tag);

  except
    on E : Exception do
    begin
      ShowMessage('Exception class name = '+E.ClassName);
      ShowMessage('Exception message = '+E.Message);
    end;
  end;

end;

end.
