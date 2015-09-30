program StringListExtDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  StringListExtUnit;

const
  gcMAX_STRING_COUNT = 10;
  gcARRAY_OF_STRINGS: array[1..gcMAX_STRING_COUNT] of string = ('aaaaaaaaaaaaaaaaa', '1bbbbbbbbbbbbbbbb',
                                                                '2cccccccccccccccc', '3dddddddddddddddd',
                                                                '4eeeeeeeeeeeeeeee', 'fffffffffffffffff',
                                                                '5gggggggggggggggg', '6hhhhhhhhhhhhhhhh',
                                                                'iiiiiiiiiiiiiiiii', 'ijjjjjjjjjjjjjjjj');

var
  SL1: TStringListExt;

procedure AddString(const sString: string);
begin
  SL1.Add(sString);
  Writeln(sString + ' Added... Count of Special strings = ' + IntToStr(SL1.SpecialStrCnt));
end;

procedure DeleteString(const sString: string);
begin
  SL1.Delete(SL1.IndexOf(sString));
  Writeln(sString + ' Deleted... Count of Special strings = ' + IntToStr(SL1.SpecialStrCnt));
end;

procedure InsertString(iIndex: Integer; const sString: string);
begin
  SL1.Insert(iIndex, sString);
  Writeln(sString + ' Inserted... Count of Special strings = ' + IntToStr(SL1.SpecialStrCnt));
end;

procedure AddStrings;
var
  iter: Integer;
begin
  Writeln('Press Enter to ADD strings to StringList...');
  Readln;

  for iter := 1 to Length(gcARRAY_OF_STRINGS) do
  begin
    AddString(gcARRAY_OF_STRINGS[iter]);
  end;

  Writeln;
end;

procedure DeleteStrings;
var
  iter: Integer;
begin
  Writeln('Press Enter to DELETE strings from StringList...');
  Readln;

  for iter := 1 to Length(gcARRAY_OF_STRINGS) do
  begin
    DeleteString(gcARRAY_OF_STRINGS[iter]);
  end;

  Writeln;
end;

procedure InsertStrings;
var
  iter: Integer;
begin
  Writeln('Press Enter to INSERT strings from StringList...');
  Readln;

  for iter := 1 to Length(gcARRAY_OF_STRINGS) do
  begin
    InsertString((iter - 1), gcARRAY_OF_STRINGS[iter]);
  end;

  Writeln;
end;

procedure ClearStringList;
begin
  Writeln('Press Enter to CLEAR all strings from StringList...');
  Readln;
  SL1.Clear;
  Writeln('String List Cleared... Count of Special strings = ' + IntToStr(SL1.SpecialStrCnt));
  Writeln;
end;

begin
  SL1 := TStringListExt.Create;
  Writeln('Special strings count: ' + IntToStr(SL1.SpecialStrCnt));

  AddStrings;
  DeleteStrings;
  InsertStrings;
  ClearStringList;

  Writeln('Press Enter...');
  Readln;

  SL1.Free;
end.

