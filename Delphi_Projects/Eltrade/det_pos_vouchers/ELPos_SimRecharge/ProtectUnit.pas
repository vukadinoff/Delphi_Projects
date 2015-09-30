unit ProtectUnit;

interface

uses Windows, SysUtils, UDESCryp;

type
  TProtection = object
    { registration }
    ModuleCode : string;
    function GenerateSerialNumber : string;
    function GenerateKey(ser_n : string; a_date : TDateTime) : string;
    function ValidateKey(sn, key : string) : boolean;
    function TimedOut(key : string) : boolean;
    { authentication }
    function EncryptPassword(key : string) : string;
    function ValidatePassword(key, pass : string) : boolean;
  private
    function SerialOK (sn : string) : boolean;
    function KeyOK (key : string) : boolean;
    function ExtractSerialNumber(key : string) : string;
    function ExtractDate(key : string) : string;
    function InsertDate(key, a_date : string) : string;
    function crypt(s : string; d : integer) : string;
    function mangle(s : string) : string;
    function unmangle(s : string) : string;
  end;

var
  Protection : TProtection;

implementation


const
  KEY_LEN = 10;
  KEY_MAX = 16; // with date

function TProtection.GenerateSerialNumber : string;
var hw_key : String;
    SN : DWord;
    DW : DWord;
    SF : DWord;
  i, len : integer;
  d, d2 : byte;
begin
 // get HW key (# of HDD) -> modify -> this is the SN
 hw_key := '';
 try
  if GetVolumeInformation(nil, nil, 0, @SN, DW, SF, nil, 0) then
   hw_key := IntToStr(SN);
 except
  hw_key := '123123';
 end;


 // modify hw_key here (make use of ModuleCode)
 if (Length(hw_key) > Length(ModuleCode)) then
   len := length(ModuleCode)
 else
   len := length(hw_key);

 for i := 1 to len do
  begin
    // get a digit
    d := ord(ModuleCode[i]) mod 10;
    // what ascii do we have here ?
    d2 := ord(hw_key[i]);
    // add and bring back in 0-9 marigin
    d2 := d2 + d;
    if (d2 > ord('9')) then d2 := d2 - 9;
    hw_key[i] := chr(d2);
  end;

 Result := hw_key;
end;

function TProtection.GenerateKey(ser_n : string; a_date : TDateTime) : string;
var
  dd, mm, yy : string;
  date_str : string;
  c_date, c_sn, c_key : string;
begin
  // convert date to ddmmyy string
  date_str := DateToStr(a_date);
  dd := Copy(date_str, 1, 2);
  mm := Copy(date_str, 4, 2);
  yy := Copy(date_str, 9, 2);
  date_str := dd+mm+yy;
  // encode sn, mangle date
  c_sn := crypt(ser_n, 0);
  c_date := mangle(date_str);
  // mix sn && date to produce key
  c_key := InsertDate(c_sn, c_date);

  GenerateKey := c_key;
end;

function TProtection.ValidateKey(sn, key: string): boolean;
var
  i : integer;
  s, s1 : string;
  invalid : boolean;
begin
 Result := False;
 if (SerialOK(sn) and KeyOK(key)) then
  begin
   invalid := False;  
   while Length(sn) < KEY_LEN do sn := sn + '0';
   s1 := ExtractSerialNumber(key);

   for i := 5 to KEY_LEN do
    begin
     SetLength(s, 1);
     s[1] := s1[i];
     if (StrToInt(crypt(sn, i)) <> StrToInt(s)) then invalid := True;
    end;  

   s := Copy(key, 1, 1);
   if (StrToInt(crypt(sn, 1)) <> StrToInt(s)) then invalid := True;

   for i := 2 to 7 do
    begin
     SetLength(s, 1);
     s[1] := s1[i];
     if (StrToInt(crypt(sn, i)) <> StrToInt(s)) then invalid := True;     
    end;
   Result := not invalid;
  end;
end;


function TProtection.TimedOut(key : string) : boolean;
var
  dd,mm,yy : string;
  date_str : string;
  curr_timestamp, timestamp : TTimeStamp;
begin
  // gather date digits
  date_str := ExtractDate(key);
  // decode date digits
  date_str := unmangle(date_str);
  // construct date (dd.mm.yy)
  dd := Copy(date_str, 1, 2);
  mm := Copy(date_str, 3, 2);
  yy := Copy(date_str, 5, 2);
  date_str := dd + '.' + mm + '.' + yy;
  try
    timestamp := DateTimeToTimeStamp(StrToDate(date_str));
  except
    Result := true;
    Exit;
  end;
  curr_timestamp := DateTimeToTimeStamp(Date);
  // check if expired
  if (timestamp.Date >= curr_timestamp.Date) then
    TimedOut := false
  else
    TimedOut := true;
end;

function TProtection.EncryptPassword(key : string) : string;
var
  cr : TDESCrypt;
  salt : string[2];
  res : string;
begin
  cr := TDESCrypt.Create(nil);
  cr.Input := key;
  // random select 2 chars salt
//  salt[1] := pass[1];
//  salt[2] := pass[2];
  SetLength(salt, 2);
  cr.Salt := salt;


  if (cr.Execute) then
    res := cr.Output
  else
    res := '';

  cr.Free;
  EncryptPassword := res;
end;

function TProtection.ValidatePassword(key, pass : string) : boolean;
var
  cr : TDESCrypt;
  salt : string[2];
  newpass : string;
  res : boolean;
begin
  if (length(pass) > 1) then
   begin
    cr := TDESCrypt.Create(nil);
    // load key
    cr.Input := key;
    // get salt
    salt[1] := pass[1];
    salt[2] := pass[2];
    SetLength(salt, 2);
    cr.Salt := salt;

    if (cr.Execute) then
      newpass := cr.Output
    else
      newpass := '';

    // compare
    if (CompareStr(newpass, pass) = 0) then
      res := true
    else
      res := false;
    cr.Free;
   end
  else
    if ((length(key) = 0) and (length(pass) = 0)) then
      res := true
    else
      res := false;

  ValidatePassword := res;

end;


// ====================== private methods ================

function TProtection.SerialOK (sn : string) : boolean;
var valid : boolean;
    i     : integer;
begin
  valid := true;

  for i := 1 to length(sn) do
    if (not (sn[i] in ['0'..'9'])) then
      valid := false;

  if (length(sn) = 0) then
    valid := false;

  SerialOK := valid;
end;

function TProtection.KeyOK (key : string) : boolean;
var valid : boolean;
    i     : integer;
begin
  valid := true;

  for i := 1 to length(key) do
    if (not (key[i] in ['0'..'9'])) then
      valid := false;

  if (length(key) < KEY_MAX) then
    valid := false;

  if (length(key) = 0) then
    valid := false;

  KeyOK := valid;
end;

function TProtection.ExtractSerialNumber(key : string) : string;
var e_sn : string;
begin
  // extract all but date bytes from key at
  // positions (byte str): 10,5,12,15,2,8 (ddmmyy - encoded)
  e_sn := Copy(key, 1, 1) +
        Copy(key, 3, 2) +
        Copy(key, 6, 2) +
        Copy(key, 9, 1) +
        Copy(key, 11, 1) +
        Copy(key, 13, 2) +
        Copy(key, 16, length(key)-15);
  ExtractSerialNumber := e_sn;
end;

function TProtection.ExtractDate(key : string) : string;
var
  e_date : string;
begin
  // extract date bytes from key from
  // positions (byte str): 10,5,12,15,2,8 (ddmmyy - encoded)
  SetLength(e_date, 6);
  e_date[1] := key[10];
  e_date[2] := key[5];
  e_date[3] := key[12];
  e_date[4] := key[15];
  e_date[5] := key[2];
  e_date[6] := key[8];
  ExtractDate := e_date;
end;

function TProtection.InsertDate(key, a_date : string) : string;
var
  s : string;
begin
  // insert date bytes into key at
  // positions (byte str): 10,5,12,15,2,8 (ddmmyy - encoded)
  s := Copy(key, 1, 1) +
        Copy(a_date, 5, 1) +
        Copy(key, 2, 2) +
        Copy(a_date, 2, 1) +
        Copy(key, 4, 2) +
        Copy(a_date, 6, 1) +
        Copy(key, 6, 1) +
        Copy(a_date, 1, 1) +
        Copy(key, 7, 1) +
        Copy(a_date, 3, 1) +
        Copy(key, 8, 2) +
        Copy(a_date, 4, 1) +
        Copy(key, 10, length(key)-9);
  InsertDate := s;
end;

function TProtection.crypt(s : string; d : integer) : string;
var
  out_n : integer;
  i, i1, i2, l : integer;
  j, k, p : real;
  a, code, out_s : string;
begin
  // crypt, use ModuleCode: irreversible
  // d -> digit. =0 -> all

  // some sanity checks:
  if (length(ModuleCode) >= KEY_LEN) then
    code := Copy(ModuleCode, 1, KEY_LEN)
  else
   begin
    code := ModuleCode;
    while (length(code) < KEY_LEN) do code := code + 'ÿ';
   end;
  while Length(S) < KEY_LEN do S := S + '0';

  if (d > KEY_LEN) then d := KEY_LEN;

  if (d = 0) then begin
    SetLength(out_s, KEY_LEN);
    for i := 1 to KEY_LEN do begin
      i1 := i + 1;
      i2 := i1 + 1;
      if (i2 > KEY_LEN) then
        if (i1 > KEY_LEN) then begin
          i1 := 1;
          i2 := 2;
        end
        else
          i2 := 1;
      j := (ord(code[i]) + ord(s[i2]))/2;
      k := 42.42;
      p := (j - k)/k;
      p := p + k * ord(s[i1]) - ord(code[i2]);
      j := (j + p) * 42 + ord(s[i]);
      l := trunc(abs((j + ord(code[i1])) * 2));
      if (l < 9) then l := trunc(j*2);
      while (l > 9) do begin
        out_n := l mod 10;
        l := l div 10;
        l := l + out_n;
      end;
      out_n := round(l);
      a := IntToStr(out_n);
      out_s[i] := a[1];
    end;
  end
  else begin
    i := d;
    i1 := i + 1;
    i2 := i1 + 1;
    if (i2 > KEY_LEN) then
      if (i1 > KEY_LEN) then begin
        i1 := 1;
        i2 := 2;
      end
      else
        i2 := 1;
    j := (ord(code[i]) + ord(s[i2]))/2;
    k := 42.42;
    p := (j - k)/k;
    p := p + k * ord(s[i1]) - ord(code[i2]);
    j := (j + p) * 42 + ord(s[i]);
    l := trunc(abs((j + ord(code[i1])) * 2));
    if (l < 9) then l := trunc(j*2);
    while (l > 9) do begin
      out_n := l mod 10;
      l := l div 10;
      l := l + out_n;
    end;
    out_n := round(l);
    out_s := IntToStr(out_n);
  end;
  crypt := out_s;
end;

function TProtection.mangle(s : string) : string;
var
  n : integer;
  out_s : string;
begin
  // reversible
  n := StrToInt(s);
  n := n - 42;
  out_s := IntToStr(n);
  while (length(out_s) < 6) do out_s := '0' + out_s;
  mangle := out_s;
end;

function TProtection.unmangle(s : string) : string;
var
  n : integer;
  out_s : string;
begin
  n := StrToInt(s);
  n := n + 42;
  out_s := IntToStr(n);
  while (length(out_s) < 6) do out_s := '0' + out_s;
  unmangle := out_s;
end;

end.
