unit EIKHandler;

interface

uses SysUtils;

function CheckEIK(EIKType, EIKValue: String): Byte;

implementation

function CheckEIK(EIKType, EIKValue: String): Byte;
var I  : Integer;
    Sum: Integer;
begin
// 1: Невалидни  EIKType
// 2: Невалиден  EIKValue
// 3: Грешна контролна сума на EIKValue
 try
   // check for numbers
   for I := 1 to Length(EIKValue) do
    begin
     if not (EIKValue[I] in ['0'..'9']) then
      begin
       Result := 2;
       Exit;
      end;
    end;

   if EIKType = '0' then // - Булстат
    begin
     if (Length(EIKValue) in [9, 13]) then
      begin
       // Check first 9 digits
       Sum := (1 * Strtoint(EIKValue[1]) + 2 * Strtoint(EIKValue[2]) + 3 * Strtoint(EIKValue[3]) +
               4 * Strtoint(EIKValue[4]) + 5 * Strtoint(EIKValue[5]) + 6 * Strtoint(EIKValue[6]) +
               7 * Strtoint(EIKValue[7]) + 8 * Strtoint(EIKValue[8])) mod 11 ;

       if (Sum = 10) then
        begin
         Sum := (3 * Strtoint(EIKValue[1]) + 4  * Strtoint(EIKValue[2]) + 5 * Strtoint(EIKValue[3]) +
                 6 * Strtoint(EIKValue[4]) + 7  * Strtoint(EIKValue[5]) + 8 * Strtoint(EIKValue[6]) +
                 9 * Strtoint(EIKValue[7]) + 10 * Strtoint(EIKValue[8])) mod 11;

         if ((Sum = 10)and(EIKValue[9] = '0'))or
            (Sum = StrToInt(EIKValue[9])) then
          Result := 0
         else
          Result := 3;
        end
       else
        begin
         if StrToInt(EIKValue[9]) = Sum then Result := 0
          else Result := 3;
        end;

       // Check 9-13 digits
       if (Result = 0)and(Length(EIKValue) = 13) then
        begin
         Sum := (2 * Strtoint(EIKValue[9])  + 7 * Strtoint(EIKValue[10]) +
                 3 * Strtoint(EIKValue[11]) + 5 * Strtoint(EIKValue[12])) mod 11;

         if (Sum = 10) then
          begin
           Sum := (4 * Strtoint(EIKValue[9])  + 9 * Strtoint(EIKValue[10]) +
                   5 * Strtoint(EIKValue[11]) + 7 * Strtoint(EIKValue[12])) mod 11;
           if ((Sum = 10)and(EIKValue[13] = '0'))or
              (Sum = StrToInt(EIKValue[13])) then
            Result := 0
           else
            Result := 3;
          end
         else
          begin
           if StrToInt(EIKValue[13]) = Sum then Result := 0
            else Result := 3;
          end;
        end;
      end
     else
      Result := 2;
    end
   else
   if EIKType = '1' then // - ЕГН
    begin
     if Length(EIKValue) = 10 then
      begin
       Sum := (2 * Strtoint(EIKValue[1]) + 4  * Strtoint(EIKValue[2]) + 8 * Strtoint(EIKValue[3]) +
               5 * Strtoint(EIKValue[4]) + 10 * Strtoint(EIKValue[5]) + 9 * Strtoint(EIKValue[6]) +
               7 * Strtoint(EIKValue[7]) + 3  * Strtoint(EIKValue[8]) + 6 * Strtoint(EIKValue[9])) mod 11;

       if ((Sum = 10)and(EIKValue[10] = '0'))or
          (Sum = StrToInt(EIKValue[10])) then
        Result := 0
       else
        Result := 3;
      end
     else
      Result := 2;
    end
   else
   if EIKType = '2' then // - ЛНЧ
    begin
     if (Length(EIKValue) in [9..13]) then
      Result := 0
     else
      Result := 2;  end
   else
   if EIKType = '3' then // - Служебен Номер
    begin
     if (Length(EIKValue) in [9..13]) then
      Result := 0
     else
      Result := 2;
    end
   else
    Result := 1;
 except
  Result := 2;
 end;
end;

end.
