unit CheckDigitsUnit;

interface

uses SysUtils;

function CheckTaxNumb(TNum: String): Boolean;
function CheckBulstat(Bulst: String): Boolean;

implementation

function CheckTaxNumb(TNum: String): boolean;
var Summ, M: Integer;
begin
 if Length(TNum) <> 10 then
  begin
   Result := False;
  end
 else
  begin
   if TNum = '9999999999' then
    begin
     Result := True;
    end
   else
    begin
     try
      Summ := 4 * StrToInt(TNum[1]) + 3 * StrToInt(TNum[2]) + 2 * StrToInt(TNum[3]) +
              7 * StrToInt(TNum[4]) + 6 * StrToInt(TNum[5]) + 5 * StrToInt(TNum[6]) +
              4 * StrToInt(TNum[7]) + 3 * StrToInt(TNum[8]) + 2 * StrToInt(TNum[9]);
      M := Summ mod 11;
      if M = 1 then
       begin
        Result := False;
        exit;
       end;
      if (M = 0)and(TNum[10] = '0') then
       begin
        Result := True;
        exit;
       end;
      if (StrToInt(TNum[10])) <> (11-M) then
       begin
        Result := False;
        exit;
       end;
      Result := True;
     except
      Result := False;
     end;
    end;
  end;
end;

function CheckBulstat(Bulst: String): boolean;
var Summ, M: Integer;
    FirstOK: Boolean;
    OK     : Boolean;
begin
 if Length(Bulst) = 11 then
  begin // Служебен номер генериран от фискалното устройство
   Result := True;
   exit;
  end;

 if Length(Bulst) = 10 then // Check EGN
  begin
   try
    Summ := 2 * StrToInt(Bulst[1]) + 4  * StrToInt(Bulst[2]) + 8 * StrToInt(Bulst[3]) +
            5 * StrToInt(Bulst[4]) + 10 * StrToInt(Bulst[5]) + 9 * StrToInt(Bulst[6]) +
            7 * StrToInt(Bulst[7]) + 3  * StrToInt(Bulst[8]) + 6 * StrToInt(Bulst[9]);

    M := Summ mod 11;
    FirstOK := False;
    if (M = 10)and(Bulst[10] = '0') then
     begin
      FirstOK := True;
     end
    else
     begin
      if (StrToInt(Bulst[10])) = (M) then
       begin
        FirstOK := True;
       end;
     end;
   except
    FirstOK := False;
   end;
   Result := FirstOK;
   exit;
  end;

 if (Length(Bulst) < 9) or (Length(Bulst) > 13) then
  begin
   Result := False;
  end
 else
  begin
   // Check first 9 digits
   try
    Summ := StrToInt(Bulst[1]) + 2 * StrToInt(Bulst[2]) + 3 * StrToInt(Bulst[3]) +
            4 * StrToInt(Bulst[4]) + 5 * StrToInt(Bulst[5]) + 6 * StrToInt(Bulst[6]) +
            7 * StrToInt(Bulst[7]) + 8 * StrToInt(Bulst[8]) ;

    M := Summ mod 11;
    FirstOK := False;
    if (M = 10) then
     begin
      Summ := 3 * StrToInt(Bulst[1]) + 4 * StrToInt(Bulst[2]) + 5 * StrToInt(Bulst[3]) +
              6 * StrToInt(Bulst[4]) + 7 * StrToInt(Bulst[5]) + 8 * StrToInt(Bulst[6]) +
              9 * StrToInt(Bulst[7]) + 10 * StrToInt(Bulst[8]) ;
      M := Summ mod 11;
      if (M = 10)and(Bulst[9] = '0') then
        FirstOK := True
      else
      if StrToInt(Bulst[9]) = M then
        FirstOK := True;
     end
    else
     begin
      FirstOK := (StrToInt(Bulst[9]) = M);
     end;
   except
    FirstOK := False;
   end;

  if length(Bulst) = 9 then
   begin
    Result := FirstOK;
   end
  else
   begin
    if length(Bulst) = 13 then
     begin
      // Check first other digits
      try
       Summ := 2 * StrToInt(Bulst[9])  + 7 * StrToInt(Bulst[10]) +
               3 * StrToInt(Bulst[11]) + 5 * StrToInt(Bulst[12]);

       M := Summ mod 11;
       OK := False;
       if (M = 10) then
        begin
         Summ := 4 * StrToInt(Bulst[9])  + 9 * StrToInt(Bulst[10]) +
                 5 * StrToInt(Bulst[11]) + 7 * StrToInt(Bulst[12]);
         M := Summ mod 11;
         if (M = 10)and(Bulst[13] = '0') then
           OK := True
         else
         if StrToInt(Bulst[13]) = M then
           OK := True;
        end
       else
        begin
         OK := (StrToInt(Bulst[13]) = M);
        end;
      except
       OK := False;
      end;

      Result := (OK and FirstOK);
     end
    else
     begin
      result := False;
     end;
   end;
 end;
end;

end.
