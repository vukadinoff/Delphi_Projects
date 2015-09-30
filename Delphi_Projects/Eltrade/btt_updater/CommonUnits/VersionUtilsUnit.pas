unit VersionUtilsUnit;

interface

uses SysUtils, Windows;

function GetAppVersion(FileName: String): String;
function CompareVersions(Ver1, Ver2: String): Integer; // -1 = по-малка; 0 = съвпадат; 1 = по-голямя
function GetMajorVersion(Ver: String): String;

implementation

function GetNum(var SSrc: String): String;
var Icnt: Integer;
begin
 Result := '';
 // чете число в началото на стринга
 for Icnt := 1 to Length(SSrc) do
  begin
   if SSrc[Icnt] in ['0'..'9'] then Result := Result + SSrc[Icnt]
    else Break;
  end;
 if Length(Result) > 0 then Delete(SSrc, 1, Length(Result));
 // премахва символите различни от число
 while (Length(SSrc) > 0)and(not(SSrc[1] in ['0'..'9'])) do Delete(SSrc, 1, 1);
end;

function GetMajorVersion(Ver: String): String;
begin
 Result := GetNum(Ver);
end;

function CompareVersions(Ver1, Ver2: String): Integer; // (Result < 0)(-) => Ver1 > Ver2
                                                       // (Result = 0)(0) => Ver1 = Ver2;
                                                       // (Result > 0)(+) => Ver1 < Ver2;
var I  : Integer;
    V1 : Integer;
    V2 : Integer;
begin
 try
  for I := 1 to 4 do
   begin
    V1 := StrToIntDef(GetNum(Ver1), 0);
    V2 := StrToIntDef(GetNum(Ver2), 0);
    Result := V2 - V1;
    if Result <> 0 then Break;
   end;
 except
  on E: Exception do
   begin
    Result := -1;
   end;
 end;
end;

function GetAppVersion(FileName: String): String;
const
  InfoNum = 10;
  InfoStr: array[1..InfoNum] of string = ('CompanyName', 'FileDescription',
                   'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
                   'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var N, Len : DWORD;
    Buf    : PChar;
    Value  : PChar;
    LangCharset : string;
    PCharset    : PLongInt;
    InfoLength  : UINT;
begin
 try
  Result := '';
  N := GetFileVersionInfoSize(PChar(FileName), N);
  if N > 0 then
   begin
    Buf := AllocMem(N);
    try
     GetFileVersionInfo(PChar(FileName), 0, N, Buf);
     if VerQueryValue(Buf, '\VarFileInfo\Translation', Pointer(PCharset), InfoLength) then
      LangCharset := Format('%.4x%.4x',[LoWord (PCharset^), HiWord (PCharset^)])
     else
      LangCharset := '040904E4';

     if VerQueryValue(Buf, PChar('StringFileInfo\'+LangCharset+'\' + InfoStr[3]), Pointer(Value), Len) then
       Result := StrPas(Value);
    finally
     FreeMem(Buf, N);
    end;
   end;
 except
 end;
end;

end.
