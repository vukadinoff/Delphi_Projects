unit CreateFilesUnit;

interface

uses Classes, SysUtils, ConstUnit, StrUtils, ResStrUnit;

function Create_EndBon_File(BType: Integer; InvNumb, SellID: Int64; CustData: TObject;
                            DestPath: String; ExceptProc: TStringProcedure): Boolean;
function Create_Confirm_File(CustData: TObject; DestPath: String; ExceptProc: TStringProcedure): Boolean;
             
implementation

uses MyMessageUnit, Dialogs, MainUnit;

function Check_FString(S: String): String;
var I: Integer;
begin
 Result := '';
 for I := 1 to Length(S) do
  begin
   if ((S[I] > #31) and (S[I] < #127)) or ((S[I] > #191)) then
    begin
     if S[I] = '#' then Result := Result + ' '
      else Result := Result + S[I];
    end; 
  end;
end;

function Create_EndBon_File(BType: Integer; InvNumb, SellID: Int64; CustData: TObject;
 DestPath: String; ExceptProc: TStringProcedure): Boolean;
var I: Integer;
    StrL: TStrings;
    sDivider, S: string;
begin
 Result := False;                                 
 if (BType > 4) or (BType < 1) then
  begin
   ExceptProc('Create "End Bon File" error: Unexpected Bon Type: ' + IntToStr(BType));
   exit;
  end;

 StrL := nil; 
 try
  try
   sDivider := '';
   for I := 1 to C_PrnLineLength do sDivider := sDivider + '-';

   StrL := TStringList.Create;
   StrL.Add('##' + DestPath + '#');
   StrL.Add('#*'+IntToStr(BType)+'#'+IntToStr(CurrentOperID)+'#'+Check_FString(CurrentOperFullName)+'#'+IntToStr(InvNumb)+'#');
   if (BType in [3,4]) and Assigned(CustData) and (CustData is TCustomerObject) then
    begin // tax && not tax invoices
     StrL.Add('#!'+Check_FString(TCustomerObject(CustData).FFirmName)+'#'+
              Check_FString(TCustomerObject(CustData).FTaxNumb)+'#'+
              Check_FString(TCustomerObject(CustData).FBulstat)+'#'+
              Check_FString(TCustomerObject(CustData).FTown)+'#'+
              Check_FString(TCustomerObject(CustData).FAddress)+'#'+
              Check_FString(TCustomerObject(CustData).FMOL)+'#'+
              Check_FString(TCustomerObject(CustData).FReceiver)+'###');
    end;
   StrL.Add('#:# #');

   if SellID > 0 then
    begin
     StrL.Add('#:#Сист.No '+IntToStr(SellID)+'#');
     StrL.Add('#:# #');
    end;

   StrL.Add('#:#Зареждане на тел. No#');
   case SimRChObj.FMobOper of
    1: StrL.Add('#:#'+AnsiUpperCase(C_MTelName)+'#');
    2: StrL.Add('#:#'+AnsiUpperCase(C_GlobulName)+'#');
    3: StrL.Add('#:#'+AnsiUpperCase(C_VivacomName)+'#');
   end;
   StrL.Add('#:##');
   StrL.Add('#:#'+SimRChObj.FPhoneNumb+'#');
   StrL.Add('#:##');

   S := Check_FString(IntToStr(SimRChObj.FPluNumb))+'#';
   S := S + Trim(Copy(Check_FString(SimRChObj.FPluName),1,C_PrnLineLength-10))+'#';
   S := S + FormatFloat('0.00',SimRChObj.FSum)+'#';
   S := S + '1.000#';
   S := S + IntToStr(SimRChObj.FTaxGroup)+'#';
   StrL.Add('#^' + AnsiUpperCase(S));
   StrL.Add('#$'+IntToStr(SimRChObj.FPayPrnNumb)+'#'+Check_FString(SimRChObj.FPayPrnName)+'#'+
            FormatFloat('0.00',SimRChObj.FPayCource)+'#'+FormatFloat('0.00',SimRChObj.FSum)+'#');

   StrL.SaveToFile(TempPath+'SIMRCH'+IntToStr(Set_CurrentTermN)+ FormatDateTime('_DDMMYY-HHNNSS',Now)+'.prn');
   Result := True;
  except
  on E: Exception do
    ExceptProc('Create "End Bon File" error: '+Trim(E.Message));
  end;
 finally
  FreeAndNil(StrL);
 end; 
end;

function Create_Confirm_File(CustData: TObject; DestPath: String; ExceptProc: TStringProcedure): Boolean;
var I: Integer;
    StrL: TStrings;
    sDivider: String;
begin
 Result := False;                                 
 StrL   := nil;
 try
  try
   sDivider := '';
   for I := 1 to C_PrnLineLength do sDivider := sDivider + '-';

   StrL := TStringList.Create;
   StrL.Add('##' + DestPath + '#');
   StrL.Add('#*1#'+IntToStr(CurrentOperID)+'#'+Check_FString(CurrentOperFullName)+'#0#');
   StrL.Add('#:# #');

   StrL.Add('#:#Проверка на тел. No#');
   case SimRChObj.FMobOper of
    1: StrL.Add('#:#'+AnsiUpperCase(C_MTelName)+'#');
    2: StrL.Add('#:#'+AnsiUpperCase(C_GlobulName)+'#');
    3: StrL.Add('#:#'+AnsiUpperCase(C_VivacomName)+'#');
   end;
   StrL.Add('#:##');
   StrL.Add('#:#'+SimRChObj.FPhoneNumb+'#');
   StrL.Add('#:##');

   if (SimRChObj.FCustBulst <> '') and Assigned(CustData)and(CustData is TCustomerObject) then
    begin
     StrL.Add('#:#'+Check_FString(TCustomerObject(CustData).FFirmName)+'#');
     StrL.Add('#:#ЕИК: '+Check_FString(TCustomerObject(CustData).FBulstat)+'#');
     StrL.Add('#:#ЗДДС No: '+Check_FString(TCustomerObject(CustData).FTaxNumb)+'#');
     StrL.Add('#:# #');
    end;

   StrL.SaveToFile(TempPath+'SIMRCH'+IntToStr(Set_CurrentTermN)+FormatDateTime('_DDMMYY-HHNNSS',Now)+'.prn');
   Result := True;
  except
  on E: Exception do
    ExceptProc('Create "End Bon File" error: '+Trim(E.Message));
  end;
 finally
  FreeAndNil(StrL);
 end;
end;

end.
