unit DP_ESKUnit;

interface

uses SysUtils, Classes, Forms, DeviceUnit;

type
 TDP_ESKCreate = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_ESKRead = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_ESKChangePin = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

implementation
uses BillingConstUnit, LoginHandler, ESKHandler;
//******************************************************************************
//       TDP_ESKCreate
//******************************************************************************
function TDP_ESKCreate.ProcessData(var InfoMsg: String): Boolean;
var ESKCreate : TCmd_ESKCreateServer;
    UserError : String;
    Res       : Integer;
begin
 Result     := true;
 ESKCreate := TCmd_ESKCreateServer.Create(XmlInDocument, Device);
 try
  if not ESKCreate.GetRequestFromDocument then raise EAbort.Create(ESKCreate.LastError);
  if not ESKCreate.Execute(Res, UserError) then raise EAbort.Create(ESKCreate.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    ESKCreate.XmlDocument := XmlOutDocument;
    if not ESKCreate.AddAnswerToDocument then raise EAbort.Create(ESKCreate.LastError);

    if not ESKCreate.PostEventUser(0, C_EvType_ESK, 'A', ESKCreate.UserName,
                         C_DeviceType_ESK, ESKCreate.EskSerial,
                         'Create ESK signature. ESK: '+ESKCreate.EskSerial+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'ESK signature created. ESK:'+ESKCreate.EskSerial;
   end
  else
   begin
    if not ESKCreate.PostEventUser(Res, C_EvType_ESK, 'A', ESKCreate.UserName,
                         C_DeviceType_ESK, ESKCreate.EskSerial,
                         'FAIL Create ESK signature. ESK:'+ESKCreate.EskSerial+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'ESK signature create fail. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 ESKCreate.Free;
end;

//******************************************************************************
//       TDP_ESKTest
//******************************************************************************
function TDP_ESKRead.ProcessData(var InfoMsg: String): Boolean;
var ESKRead   : TCmd_ESKReadServer;
    UserError : String;
    Res       : Integer;
begin
 ESKRead := TCmd_ESKReadServer.Create(XmlInDocument, Device);
 try
  if not ESKRead.GetRequestFromDocument then raise EAbort.Create(ESKRead.LastError);
  if not ESKRead.Execute(Res, UserError) then raise EAbort.Create(ESKRead.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    ESKRead.XmlDocument := XmlOutDocument;
    if not ESKRead.AddAnswerToDocument then raise EAbort.Create(ESKRead.LastError);

    if not ESKRead.PostEventUser(0, C_EvType_ESK, 'D', ESKRead.EskSerial,
                         C_DeviceType_ESK, ESKRead.EskSerial,
                         'ESK Test OK. '+ESKRead.EskSerial+' ('+ESKRead.EskVersion+')'+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'ESK test succeed. '+ESKRead.EskSerial;
   end
  else
   begin
    if not ESKRead.PostEventUser(Res, C_EvType_ESK, 'D', ESKRead.EskSerial,
                         C_DeviceType_ESK, ESKRead.EskSerial,
                         'FAIL ESK Test. '+ESKRead.EskSerial+' ('+ESKRead.EskVersion+')'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'ESK Test fail. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);

  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 ESKRead.Free;
end;

//******************************************************************************
//       TDP_ESKChangePin
//******************************************************************************
function TDP_ESKChangePin.ProcessData(var InfoMsg: String): Boolean;
var ESKChgPin : TCmd_ESKChangePinServer;
    UserError : String;
    Res       : Integer;
begin
 ESKChgPin := TCmd_ESKChangePinServer.Create(XmlInDocument, Device);
 try
  if not ESKChgPin.GetRequestFromDocument then raise EAbort.Create(ESKChgPin.LastError);
  if not ESKChgPin.Execute(Res, UserError) then raise EAbort.Create(ESKChgPin.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    ESKChgPin.XmlDocument := XmlOutDocument;
    if not ESKChgPin.AddAnswerToDocument then raise EAbort.Create(ESKChgPin.LastError);

    if not ESKChgPin.PostEventUser(0, C_EvType_ChgPin, 'D', ESKChgPin.EskSerial,
                         C_DeviceType_ESK, ESKChgPin.ESKSerial,
                         'ESK PIN changed. '+ESKChgPin.EskSerial+' ('+ESKChgPin.EskVersion+')'+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'ESK PIN changed. '+ESKChgPin.EskSerial;
   end
  else
   begin
    if not ESKChgPin.PostEventUser(Res, C_EvType_ChgPin, 'D', ESKChgPin.EskSerial,
                         C_DeviceType_ESK, ESKChgPin.ESKSerial,
                         'FAIL ESK PIN change. '+ESKChgPin.EskSerial+' ('+ESKChgPin.EskVersion+')'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'ESK PIN change FAIL. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);

  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 ESKChgPin.Free;
end;

end.
