unit FilesWork;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, fraItems, Contnrs, ItemList, IniFiles, Settings;

  type
  TFilesWork = class(TObject)

  private

  public
    SpoolerPath: String;

    procedure ReadSettings();
    procedure SaveSettings();
  end;


implementation

procedure TFilesWork.ReadSettings();
 var IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));

  try
    self.SpoolerPath := IniFile.ReadString('Paths', 'Spooler', ExtractFilePath(Application.ExeName) + 'Spool');
  finally
    IniFile.Free;
  end;

end;

procedure TFilesWork.SaveSettings();
 var IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));

  try
    IniFile.WriteString('Paths', 'Spooler', self.SpoolerPath);
  finally
    IniFile.Free;
  end;
end;

end.
