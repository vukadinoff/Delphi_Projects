unit ESKReadUnit;

interface

uses Classes, SysUtils, DateUtils, ESKTypesUnit, ESKDeviceUnit, CryptoHandler, Math;

const
 cerr_NoDevicesFound       = 100;
 cerr_NoValidDeviceFound   = 101;
 cerr_DeviceBussy          = 103;

 cerr_DeviceApiNotReady    = 111;
 cerr_FailSearchForDevices = 112;

function ESK_ReadSignatureInternal(ErrLang: Integer; // на какъв език да връща грешките
                                   ProjectFilter, ModuleFilter: String; // за кой проект и модул да прави проверка (празно - не прави проверка за проект)
                                   var ESKUser, ESKCode, ESKVersion, ErrorMsg: String; // резултат
                                   ESKModules: TStrings = nil // връща проектите и модулите в тапата
                                   ): Integer; // връща код на грешка; 0 - всичко е ОК
function ESK_ReadEskDataInternal(ErrLang: Integer; // на какъв език да връща грешките
                                 ProjectFilter, ModuleFilter: String; // за кой проект и модул да прави проверка (празно - не прави проверка за проект)
                                 var ESKSerial, ESKVersion, ESKCode, ESKKey, ErrorMsg: String; // резултат
                                 ESKModules: TStrings = nil; // връща проектите и модулите в тапата
                                 TimeoutSec: Integer=40 // таймаут за четене - ако тапата е отворена от друг
                                 ): Integer; // връща код на грешка; 0 - всичко е ОК

implementation

type
  ESKException = class(EAbort)
  private
    FErrorCode: Integer;
  public
    constructor Create(ErrCode, Language: Integer; Msg: String = '');
    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

//******************************************************************************
//  ESKException
//******************************************************************************
constructor ESKException.Create(ErrCode, Language: Integer; Msg: String = '');
begin
 case ErrCode of
 cerr_DeviceApiNotReady:
     case Language of
     0: Message := 'Internal system Error!'+sLineBreak+
                   'HID device API is not loaded.';
     1: Message := 'Системна грешка!'+sLineBreak+
                   'Неуспешна инициализация на HID интерфейса.';
     end;
 cerr_FailSearchForDevices:
     case Language of
     0: Message := 'Internal system Error!'+sLineBreak+
                   '"Security key" search fail.';
     1: Message := 'Системна грешка!'+sLineBreak+
                   'Неуспешно претърсване за USB "електронен ключ".';
     end;
 cerr_NoDevicesFound:
     case Language of
     0: Message := '"Security key" not found.'+sLineBreak+
                   'Please insert valid security key (USB)';
     1: Message := 'НЕ е намерен "електронен ключ" (USB)!'+sLineBreak+
                   'Моля поставете валиден ключ за да'+sLineBreak+
                   'получите достъп до системата.';
     end;
 cerr_NoValidDeviceFound:
     case Language of
     0: Message := 'No valid "Security key" found.'+sLineBreak+
                   'Please insert valid security key (USB)';
     1: Message := 'НЕ е намерен валиден "електронен ключ" (USB)!'+sLineBreak+
                   'Валидния електронен ключ съдържа информация'+sLineBreak+
                   'за достъп до системата. Моля поставете'+sLineBreak+
                   'валиден ключ и опитайте отново.';
     end;
 cerr_DeviceBussy:
     case Language of
     0: Message := '"Security key" is bussu.'+sLineBreak+
                   'Please try again later or reboot system.';
     1: Message := 'Достъпа до електронния ключ е временно блокиран.'+sLineBreak+
                   'Моля опитайте по-късно или рестартирайте системата.';
     end;
 else
  begin
     case Language of
     0: Message := 'Unknown error!';
     1: Message := 'Неизвестна грешка!';
     end;
  end;
 end;

 FErrorCode := ErrCode;
 Message    := Message + sLineBreak + Msg;
end;

function EncryptCode(ESKCode_, ESKKey_: String): String;
var Cnt : Integer;
    D2k : TDateTime;
    CH  : TCrypto_TEA;
begin
 Randomize;
 Cnt := RandomRange(6, $F);
 D2k := EncodeDate(2000, 1, 1);
 if (Cnt < 6)or(Cnt > $F) then Cnt := 6;
 Result := IntToHex(Cnt, 1) + IntToHex(DaysBetween(D2k, Date) , 4);
 while Length(Result) < Cnt do Result := Result + IntToHex(RandomRange(1, $F), 1);

 CH := TCrypto_TEA.Create;
 try
  Result := CH.EncryptStringEx(Result + ESKCode_, ESKKey_);
 finally
  CH.Free;
 end;
end;

function ESK_ReadEskDataInternal(ErrLang: Integer; ProjectFilter, ModuleFilter: String;
                                 var ESKSerial, ESKVersion, ESKCode, ESKKey, ErrorMsg: String;
                                 ESKModules: TStrings = nil; TimeoutSec: Integer=40): Integer;
var ESKComm    : TESKCommClass;
    DevSerials : TStrings;
    DeviceCnt  : Byte;
    S          : String;
    I, J, K    : Integer;
    StartTime  : TDateTime;
    DeviceData : TESK_Data;
    MemStrm    : TmemoryStream;
    MemSize1   : Cardinal;
    MemSize2   : Cardinal;
    LoopCount  : Word;

    procedure ClearResult;
    begin
     ErrorMsg   := '';
     ESKSerial  := '';
     ESKVersion := '';
     ESKCode    := '';
     ESKKey     := '';
    end;
begin
 if not (ErrLang in [0, 1]) then ErrLang := 0;

 ClearResult;
 DevSerials := TStringList.Create;
 ESKComm    := TESKCommClass.Create();
 try
  // Check HID Api
  if not ESKComm.DeviceApiIsReady then
    raise ESKException.Create(cerr_DeviceApiNotReady, ErrLang);

  if not ESKComm.SearchForDevices(DeviceCnt, DevSerials) then
    raise ESKException.Create(cerr_FailSearchForDevices, ErrLang, ESKComm.ErrorString);

  if DeviceCnt = 0 then
    raise ESKException.Create(cerr_NoDevicesFound, ErrLang);

  StartTime  := Now;
  LoopCount  := 0;
  I := 0;
  repeat
   if DevSerials.Count = 0 then raise ESKException.Create(cerr_NoValidDeviceFound, ErrLang);
   if I >= DevSerials.Count then I := 0;

   case ESKComm.OpenDevice(DevSerials.Strings[I]) of
   eskApiError:    DevSerials.Delete(I); // remove wrong device
   eskWrongAnswer: DevSerials.Delete(I); // remove wrong device
   eskNotFound:    Inc(I);
   eskOK:
     begin
        DeviceData := TESK_Data.Create;
        MemStrm    := TMemoryStream.Create;
        try
          try
           DeviceData.Manufacturer := ESKComm.DevManufacturer;
           DeviceData.ProductName  := ESKComm.DevProductName;
           DeviceData.ProductID    := ESKComm.DevProductID;
           // read device data
           if not ESKComm.ReadESK_SerialNumber(S) then raise Exception.Create('Fail read device serail number:'+ESKComm.ErrorString);
           DeviceData.ESK_SerialNumber := S;
           if not ESKComm.ReadESK_ID(S) then raise Exception.Create('Fail read device ID:'+ESKComm.ErrorString);
           DeviceData.ESK_ID := S;
           if not ESKComm.ReadESK_Name(S) then raise Exception.Create('Fail read device name:'+ESKComm.ErrorString);
           DeviceData.ESK_Name := S;
           if not ESKComm.ReadESK_FirmwareDate(S) then raise Exception.Create('Fail read device firmware date:'+ESKComm.ErrorString);
           DeviceData.ESK_FirmwareDate := S;
           if not ESKComm.ReadESK_FirmwareTime(S) then raise Exception.Create('Fail read device firmware time:'+ESKComm.ErrorString);
           DeviceData.ESK_FirmwareTime := S;
           if not ESKComm.ReadESK_Revision(S) then raise Exception.Create('Fail read device revision:'+ESKComm.ErrorString);
           DeviceData.ESK_Revision := S;
           if Copy(DeviceData.ESK_SerialNumber, 1, 12) = 'FFFFFFFFFFFF' then raise Exception.Create('Device serial number not set');

           // Check esk
           if not ESKComm.ExecuteCheck('TESKSignature' + TimeToStr(Now)) then raise Exception.Create('Fail during device check'+ESKComm.ErrorString);
           // read memory
           // Първо се чете размера на заеманата памет
           // След това се чете самата памет
           MemSize1   := DeviceData.GetStartMemPacketSize;
           MemStrm.Clear; MemStrm.Position := 0;
           if not ESKComm.ReadMemory(MemStrm, 0, MemSize1, 0) then raise Exception.Create('Fail read device memory');
           MemStrm.Position := 0;
           if not DeviceData.CheckStartMemPacket(MemStrm, MemSize2) then raise Exception.Create('Device memory is not formatted: '+DeviceData.LastError);
           if MemSize2 = 0 then raise Exception.Create('Device memory is empty');
           if MemSize2 > $7000 then raise Exception.Create('Device format error.Self Not enough memory');
           MemStrm.Clear; MemStrm.Position := 0;
           if not ESKComm.ReadMemory(MemStrm, MemSize1, MemSize2, 0) then raise Exception.Create('Fail read device memory');
           MemStrm.Position := 0;
           if not DeviceData.LoadFromStream(MemStrm) then raise Exception.Create('Wrong data format: '+DeviceData.LastError);

           with DeviceData do
            begin
             if (ProjectFilter = '')or(Projects.ProjectByCode[ProjectFilter] <> nil) then
              begin
               if (ModuleFilter = '')or(Projects.ProjectByCode[ProjectFilter].ModuleByCode[ModuleFilter] <> nil) then
                begin
                 if (CustomData.DataByParam['Signature'] = 'EltradeService')and
                    (CustomData.DataByParam['SignatureCode'] <> '')and
                    (CustomData.DataByParam['SignatureKey'] <> '') then
                  begin
                   ESKSerial  := ESK_SerialNumber;
                   ESKVersion := ESK_Revision;
                   ESKCode    := CustomData.DataByParam['SignatureCode'];
                   ESKKey     := CustomData.DataByParam['SignatureKey'];

                   if ESKModules <> nil then
                    begin
                     ESKModules.Clear;
                     for J := 0 to Projects.Count - 1 do
                      begin
                       ESKModules.Add(Projects.Project[J].Code);
                       for K := 0 to Projects.Project[J].Count - 1 do
                        begin
                         ESKModules.Add(Projects.Project[J].Code+'|'+Projects.Project[J].Module[K].Code);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          except
           on E: Exception do
            begin
            end;
          end;
          StartTime := Now;
        finally
         MemStrm.Free;
         DeviceData.Free;
        end;
      DevSerials.Delete(I); // remove this device from list
     end;
   end; // case FESKComm.OpenDevice(I) of
   ESKComm.CloseDevice;

   Inc(LoopCount);
   // Опит за разбъркване на времената за четене
   case SecondsBetween(StartTime, Now) of
   0..2:    begin
              if (Frac(LoopCount / 2) = 0) then Sleep(450)
               else Sleep(50);
            end;
   3..5:    begin
              if (Frac(LoopCount / 2) = 0) then Sleep(550)
               else Sleep(900);
            end;
   6..12:   begin
              if (Frac(LoopCount / 2) = 0) then Sleep(1800)
               else Sleep(850);
            end;
   13..30:  begin
              if (Frac(LoopCount / 2) = 0) then Sleep(3500)
               else Sleep(5550);
            end;
   31..120: begin
              if (Frac(LoopCount / 2) = 0) then Sleep(4600)
               else Sleep(6000);
             end;
   end;


   if SecondsBetween(StartTime, Now) > TimeoutSec then
    raise ESKException.Create(cerr_DeviceBussy, ErrLang);

   // check for device existance
   if (Frac(LoopCount / 5) = 0) then
    begin
     if not ESKComm.SearchForDevices(DeviceCnt, DevSerials) then
       raise ESKException.Create(cerr_FailSearchForDevices, ErrLang, ESKComm.ErrorString);
     if DeviceCnt = 0 then
      raise ESKException.Create(cerr_NoDevicesFound, ErrLang);
    end;

  until (ESKCode <> '')and(ESKSerial <> '')and(ESKKey <> '');
  Result := 0;
 except
  on E: ESKException do
   begin
    ClearResult;
    Result   := E.ErrorCode;
    ErrorMsg := E.Message;
   end;
  on E: Exception do
   begin
    ClearResult;
    Result   := -1;
    ErrorMsg := 'Unexpected error!'+sLineBreak+E.Message;
   end;
 end;
 DevSerials.Free;
 ESKComm.Free;
end;

function ESK_ReadSignatureInternal(ErrLang: Integer; ProjectFilter, ModuleFilter: String;
                                   var ESKUser, ESKCode, ESKVersion, ErrorMsg: String;
                                   ESKModules: TStrings = nil): Integer;
var SC, SK : String;
begin
 Result := ESK_ReadEskDataInternal(ErrLang, ProjectFilter, ModuleFilter, ESKUser, ESKVersion, SC, SK, ErrorMsg);
 if Result = 0 then ESKCode := EncryptCode(SC, SK);
end;

end.
