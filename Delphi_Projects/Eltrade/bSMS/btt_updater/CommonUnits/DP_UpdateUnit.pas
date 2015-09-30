unit DP_UpdateUnit;

interface

uses SysUtils, Classes, Forms, DeviceUnit;

type
 TDP_Update = class(TDataProcessorCCDevice)
 private

 public
  function InDoc_Load(Source: String): Boolean; override;
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;


implementation
uses BillingConstUnit;

function TDP_Update.InDoc_Load(Source: String): Boolean;
begin
 InCommandText    := cmd_DevUpd;
 InCommandVersion := Device.DeviceInfo.Dev_Version;
 InCommandAppname := C_InCommandAppname_CC;
 Result := inherited InDoc_Load(Source);
end;

function TDP_Update.ProcessData(var InfoMsg: String): Boolean;
begin
 try

  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    InfoMsg   := E.Message;
    Result    := false;
   end;
 end;
end;

end.
