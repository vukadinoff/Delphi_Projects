unit DP_NRAUnit;

interface

uses SysUtils, Classes, Forms, DeviceUnit;

type
 TDP_NRARegisterSim = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

implementation
uses BillingConstUnit, NRAHandler;
//******************************************************************************
//       TDP_NRARegisterSim
//******************************************************************************
function TDP_NRARegisterSim.ProcessData(var InfoMsg: String): Boolean;
var RegSimCmd : TCmd_NRARegSimServer;
    UserError : String;
    Res       : Integer;
begin
 Result     := true;
 RegSimCmd := TCmd_NRARegSimServer.Create(XmlInDocument, Device);
 RegSimCmd.TestRequest := TestMode;
 try
  if not RegSimCmd.GetRequestFromDocument then raise EAbort.Create(RegSimCmd.LastError);
  if not RegSimCmd.Execute(Res, UserError) then raise EAbort.Create(RegSimCmd.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    RegSimCmd.XmlDocument := XmlOutDocument;
    if not RegSimCmd.AddAnswerToDocument then raise EAbort.Create(RegSimCmd.LastError);

    if not RegSimCmd.PostEventUser(0, C_EvType_NRA, 'A', RegSimCmd.UserName,
                         C_DeviceType_SIM, '',
                         'Reg SIM to NRA:'+RegSimCmd.SimStatusText+sLineBreak+
                         'Sim list: '+RegSimCmd.SimRequestList.Text+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Register SIM to NRA server:'+RegSimCmd.SimStatusText;
   end
  else
   begin
    if not RegSimCmd.PostEventUser(Res, C_EvType_NRA, 'A', RegSimCmd.UserName,
                         C_DeviceType_SIM, '',
                         'FAIL Reg SIM to NRA:'+RegSimCmd.SimStatusText+sLineBreak+
                         'Sim list: '+RegSimCmd.SimRequestList.Text+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Register SIM to NRA fail. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 RegSimCmd.Free;
end;

end.
