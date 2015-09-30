unit DP_FiscUnit;

interface

uses SysUtils, Classes, Forms, DeviceUnit, DP_SystemUnit;

type
 TDP_FiscRequest = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_FiscCommit = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_FiscValidate = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_FiscReadRequest = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_SimStatus = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_SimPayment = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_SimChange = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;



implementation
uses BillingConstUnit, FiscalizeHandler, SimHandler;

function TDP_FiscRequest.ProcessData(var InfoMsg: String): Boolean;
var CmdFRequest: TCmd_FiscRequestServer;
    UserError  : String;
    Res        : Integer;
begin
 Result      := true;
 CmdFRequest := TCmd_FiscRequestServer.Create(XmlInDocument, Device);
 try
  if not CmdFRequest.GetRequestFromDocument then raise EAbort.Create(CmdFRequest.LastError);
  if not CmdFRequest.Execute(Res, UserError) then raise EAbort.Create(CmdFRequest.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdFRequest.XmlDocument := XmlOutDocument;
    if not CmdFRequest.AddAnswerToDocument then raise EAbort.Create(CmdFRequest.LastError);

    if not CmdFRequest.PostEventUser(0, C_EvType_FiscR, 'D', CmdFRequest.ESKLoginData.EskSerial,
                         CmdFRequest.DeviceType, CmdFRequest.DeviceSerial,
                         'Request granted['+IntToStr(CmdFRequest.RequestType)+']:'+IntToStr(CmdFRequest.FiscRequestID)+CmdFRequest.TestModeAsText+sLineBreak+
                         'Device:'+CmdFRequest.DeviceRegData.DeviceInfo+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Request granted['+IntToStr(CmdFRequest.RequestType)+']:'+IntToStr(CmdFRequest.FiscRequestID)+CmdFRequest.TestModeAsText+sLineBreak+
               'Device: '+CmdFRequest.DeviceRegData.DeviceInfo;
   end
  else
   begin
    if not CmdFRequest.PostEventUser(Res, C_EvType_FiscR, 'D', CmdFRequest.ESKLoginData.EskSerial,
                         CmdFRequest.DeviceType, CmdFRequest.DeviceSerial,
                         'FAIL Request['+IntToStr(CmdFRequest.RequestType)+']'+CmdFRequest.TestModeAsText+sLineBreak+
                         'Device: '+CmdFRequest.DeviceRegData.DeviceInfo+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL request['+IntToStr(CmdFRequest.RequestType)+']'+CmdFRequest.TestModeAsText+sLineBreak+
               'Device: '+CmdFRequest.DeviceRegData.DeviceInfo+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdFRequest.Free;
end;

function TDP_FiscCommit.ProcessData(var InfoMsg: String): Boolean;
var CmdFCommit : TCmd_FiscCommitServer;
    UserError  : String;
    Res        : Integer;
begin
 Result    := true;
 CmdFCommit := TCmd_FiscCommitServer.Create(XmlInDocument, Device);
 try     
  if not CmdFCommit.GetRequestFromDocument then raise EAbort.Create(CmdFCommit.LastError);
  if not CmdFCommit.Execute(Res, UserError) then raise EAbort.Create(CmdFCommit.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdFCommit.XmlDocument := XmlOutDocument;
    if not CmdFCommit.AddAnswerToDocument then raise EAbort.Create(CmdFCommit.LastError);

    if CmdFCommit.ErrorCode <> errcode_ExecuteSucceed then
     begin
      if not CmdFCommit.PostEventUser(CmdFCommit.ErrorCode, C_EvType_FiscC, 'D', CmdFCommit.EskSerial,
                           C_DeviceType_FD, CmdFCommit.DeviceSerial,
                           'Commit ERROR ['+IntToStr(CmdFCommit.FiscRequestID)+']'+CmdFCommit.TestModeAsText+sLineBreak+
                           CmdFCommit.OperationName+' Dev:'+CmdFCommit.DeviceSerial+sLineBreak+
                           'Error: ['+IntToStr(CmdFCommit.ErrorCode)+'] '+CmdFCommit.ErrorMessage+sLineBreak+
                           ''+sLineBreak+
                           '['+InCommandAppname+'/'+InCommandVersion+']'+sLineBreak+
                           '['+Device.DeviceInfoAsText+']') then
       raise EAbort.Create(Self.LastError);

      InfoMsg := 'Commit ERROR: ['+IntToStr(CmdFCommit.FiscRequestID)+']'+CmdFCommit.TestModeAsText+sLineBreak+
                 'Device: '+CmdFCommit.DeviceSerial+sLineBreak+
                 'ERROR:'+IntToStr(CmdFCommit.ErrorCode)+' '+CmdFCommit.ErrorMessage;
     end
    else
     begin
      if not CmdFCommit.PostEventUser(0, C_EvType_FiscC, 'D', CmdFCommit.EskSerial,
                           C_DeviceType_FD, CmdFCommit.DeviceSerial,
                           'Commit OK ['+IntToStr(CmdFCommit.FiscRequestID)+']'+CmdFCommit.TestModeAsText+sLineBreak+
                           CmdFCommit.OperationName+' Dev:'+CmdFCommit.DeviceSerial+sLineBreak+
                           ''+sLineBreak+
                           '['+InCommandAppname+'/'+InCommandVersion+']'+sLineBreak+
                           '['+Device.DeviceInfoAsText+']') then
       raise EAbort.Create(Self.LastError);

      InfoMsg := 'Commit OK: ['+IntToStr(CmdFCommit.FiscRequestID)+']'+CmdFCommit.TestModeAsText;
     end;
   end
  else
   begin
    if not CmdFCommit.PostEventUser(Res, C_EvType_FiscC, 'D', CmdFCommit.EskSerial,
                         C_DeviceType_FD, CmdFCommit.DeviceSerial,
                         'FAIL commit: ['+IntToStr(CmdFCommit.FiscRequestID)+']'+CmdFCommit.TestModeAsText+sLineBreak+
                         'Device: '+CmdFCommit.DeviceSerial+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']'+sLineBreak+
                         '['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FIAL Commit: ['+IntToStr(CmdFCommit.FiscRequestID)+']'+CmdFCommit.TestModeAsText+sLineBreak+
               'Device: '+CmdFCommit.DeviceSerial+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdFCommit.Free;
end;

function TDP_FiscValidate.ProcessData(var InfoMsg: String): Boolean;
var CmdValidate: TCmd_FiscValidateServer;
    UserError  : String;
    Res        : Integer;
begin
 Result    := true;
 CmdValidate := TCmd_FiscValidateServer.Create(XmlInDocument, Device);
 try
  if not CmdValidate.GetRequestFromDocument then raise EAbort.Create(CmdValidate.LastError);
  if not CmdValidate.Execute(Res, UserError) then raise EAbort.Create(CmdValidate.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdValidate.XmlDocument := XmlOutDocument;
    if not CmdValidate.AddAnswerToDocument then raise EAbort.Create(CmdValidate.LastError);

    if not CmdValidate.PostEventUser(0, C_EvType_FiscV, 'D', CmdValidate.ESKLoginData.EskSerial,
                         CmdValidate.DeviceType, CmdValidate.DeviceSerial,
                         'Validation OK ['+IntToStr(CmdValidate.RequestType)+']'+CmdValidate.TestModeAsText+sLineBreak+
                         'Device: '+CmdValidate.DeviceRegData.DeviceInfo+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Validation OK ['+IntToStr(CmdValidate.RequestType)+']'+CmdValidate.TestModeAsText+sLineBreak+
               'Device: '+CmdValidate.DeviceRegData.DeviceInfo;
   end
  else
   begin
    if not CmdValidate.PostEventUser(Res, C_EvType_FiscV, 'D', CmdValidate.ESKLoginData.EskSerial,
                         CmdValidate.DeviceType, CmdValidate.DeviceSerial,
                         'FAIL validation ['+IntToStr(CmdValidate.RequestType)+']'+CmdValidate.TestModeAsText+sLineBreak+
                         'Device: '+CmdValidate.DeviceRegData.DeviceInfo+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL validation ['+IntToStr(CmdValidate.RequestType)+']'+CmdValidate.TestModeAsText+sLineBreak+
               'Device: '+CmdValidate.DeviceRegData.DeviceInfo+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdValidate.Free;
end;

function TDP_FiscReadRequest.ProcessData(var InfoMsg: String): Boolean;
var CmdRRequesr: TCmd_ReadRequestServer;
    UserError  : String;
    Res        : Integer;
begin
 Result      := true;
 CmdRRequesr := TCmd_ReadRequestServer.Create(XmlInDocument, Device);
 try
  if not CmdRRequesr.GetRequestFromDocument then raise EAbort.Create(CmdRRequesr.LastError);
  if not CmdRRequesr.Execute(Res, UserError) then raise EAbort.Create(CmdRRequesr.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdRRequesr.XmlDocument := XmlOutDocument;
    if not CmdRRequesr.AddAnswerToDocument then raise EAbort.Create(CmdRRequesr.LastError);

    if not CmdRRequesr.PostEventUser(0, C_EvType_ReadReq, 'D', CmdRRequesr.ESKLoginData.EskSerial,
                         C_DeviceType_FD, CmdRRequesr.DeviceSerial,
                         'Serve fisc.request data ['+IntToStr(CmdRRequesr.FiscRequestID)+']'+sLineBreak+
                         'Device: '+CmdRRequesr.DeviceRegData.DeviceInfo+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Serve fisc.req. data ['+IntToStr(CmdRRequesr.FiscRequestID)+']'+sLineBreak+
               'Device: '+CmdRRequesr.DeviceRegData.DeviceInfo;
   end
  else
   begin
    if not CmdRRequesr.PostEventUser(Res, C_EvType_ReadReq, 'D', CmdRRequesr.ESKLoginData.EskSerial,
                         C_DeviceType_FD, CmdRRequesr.DeviceSerial,
                         'FAIL serve fisc.req. data ['+IntToStr(CmdRRequesr.FiscRequestID)+']'+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL serve fisc.req. data ['+IntToStr(CmdRRequesr.FiscRequestID)+']'+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdRRequesr.Free;
end;

function TDP_SimStatus.ProcessData(var InfoMsg: String): Boolean;
var CmdSimStat : TCmd_SimStatusServer;
    UserError  : String;
    Res        : Integer;
begin
 Result     := true;
 CmdSimStat := TCmd_SimStatusServer.Create(XmlInDocument, Device);
 try
  if not CmdSimStat.GetRequestFromDocument then raise EAbort.Create(CmdSimStat.LastError);
  if not CmdSimStat.Execute(Res, UserError) then raise EAbort.Create(CmdSimStat.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdSimStat.XmlDocument := XmlOutDocument;
    if not CmdSimStat.AddAnswerToDocument then raise EAbort.Create(CmdSimStat.LastError);

    UserError := 'Check SIM status ['+CmdSimStat.Imsi+']'+sLineBreak;
    if CmdSimStat.IsExternal then   UserError := UserError + 'EXTERNAL SIM !!!'+sLineBreak;
    if CmdSimStat.PayedTo <> 0 then UserError := UserError + 'PayedTo: '+DateToStr(CmdSimStat.PayedTo)+sLineBreak
     else UserError := UserError + 'PayedTo: NO PAYMENT';

    if not CmdSimStat.PostEventUser(0, C_EvType_SIM, 'D', CmdSimStat.ESKLoginData.EskSerial,
                         C_DeviceType_SIM, CmdSimStat.Imsi,
                         UserError+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'SIM status checked ['+CmdSimStat.Imsi+']';
   end
  else
   begin
    if not CmdSimStat.PostEventUser(Res, C_EvType_SIM, 'D', CmdSimStat.ESKLoginData.EskSerial,
                         C_DeviceType_SIM, CmdSimStat.Imsi,
                         'FAIL get SIM status ['+CmdSimStat.Imsi+']'+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL get SIM status ['+CmdSimStat.Imsi+']'+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdSimStat.Free;
end;


function TDP_SimPayment.ProcessData(var InfoMsg: String): Boolean;
var CmdSimPay  : TCmd_SimPaymentServer;
    UserError  : String;
    Res        : Integer;
begin
 Result     := true;
 CmdSimPay := TCmd_SimPaymentServer.Create(XmlInDocument, Device);
 try
  if not CmdSimPay.GetRequestFromDocument then raise EAbort.Create(CmdSimPay.LastError);
  if not CmdSimPay.Execute(Res, UserError) then raise EAbort.Create(CmdSimPay.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdSimPay.XmlDocument := XmlOutDocument;
    if not CmdSimPay.AddAnswerToDocument then raise EAbort.Create(CmdSimPay.LastError);

    if not CmdSimPay.PostEventUser(0, C_EvType_SIMPay, 'D', CmdSimPay.ESKLoginData.EskSerial,
                         C_DeviceType_SIM, CmdSimPay.Imsi,
                         'SIM payment executed ['+CmdSimPay.Imsi+']'+sLineBreak+
                         CmdSimPay.PaymentInfo+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'SIM payment done. ['+CmdSimPay.Imsi+']';
   end
  else
   begin
    if not CmdSimPay.PostEventUser(Res, C_EvType_SIMPay, 'D', CmdSimPay.ESKLoginData.EskSerial,
                         C_DeviceType_SIM, CmdSimPay.Imsi,
                         'FAIL SIM payment ['+CmdSimPay.Imsi+']'+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL SIM payment ['+CmdSimPay.Imsi+']'+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdSimPay.Free;
end;

function TDP_SimChange.ProcessData(var InfoMsg: String): Boolean;
var CmdSimChg  : TCmd_SimChangeServer;
    UserError  : String;
    Res        : Integer;
begin
 Result    := true;
 CmdSimChg := TCmd_SimChangeServer.Create(XmlInDocument, Device);
 try
  if not CmdSimChg.GetRequestFromDocument then raise EAbort.Create(CmdSimChg.LastError);
  if not CmdSimChg.Execute(Res, UserError) then raise EAbort.Create(CmdSimChg.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CmdSimChg.XmlDocument := XmlOutDocument;
    if not CmdSimChg.AddAnswerToDocument then raise EAbort.Create(CmdSimChg.LastError);

    if not CmdSimChg.PostEventUser(0, C_EvType_SIM, 'D', CmdSimChg.ESKLoginData.EskSerial,
                         C_DeviceType_FD, CmdSimChg.FiscDevSerial,
                         'SIM change executed'+sLineBreak+
                         'Old SIM: '+CmdSimChg.OldSimIMSI+sLineBreak+
                         'New SIM: '+CmdSimChg.NewSimIMSI+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'SIM change done. ['+CmdSimChg. NewSimIMSI+']';
   end
  else
   begin
    if not CmdSimChg.PostEventUser(Res, C_EvType_SIM, 'D', CmdSimChg.ESKLoginData.EskSerial,
                         C_DeviceType_FD, CmdSimChg.FiscDevSerial,
                         'FAIL SIM change'+sLineBreak+
                         'Error: ['+IntToStr(Res)+'] '+UserError+sLineBreak+
                         ''+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL SIM change ['+CmdSimChg.OldSimIMSI+']'+sLineBreak+
               'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CmdSimChg.Free;
end;

end.
