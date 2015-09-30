unit DP_PingUnit;

interface

uses SysUtils, Classes, Forms, DeviceUnit;

type
 TDP_Ping = class(TDataProcessorCCDevice)
 public
//  function InDoc_Load(Source: String): Boolean; override;
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_Report = class(TDataProcessorCCDevice)
 public
//  function InDoc_Load(Source: String): Boolean; override;
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

implementation
uses BillingConstUnit, DateUtils, CCDeviceHandler, DBInterfaceUnit;

//**************************************************************************************************
//    TDP_Ping
//**************************************************************************************************
{function TDP_Ping.InDoc_Load(Source: String): Boolean;
begin
 InCommandText    := cmd_DevPing;
 InCommandVersion := Device.DeviceInfo.Dev_Version;
 InCommandAppname := C_InCommandAppname_CC;

 Result := inherited InDoc_Load(Source);
end;}

function TDP_Ping.ProcessData(var InfoMsg: String): Boolean;
var CCDevice  : THndr_CCDevice;
    CCTask    : THndr_CCTask;
    CCEvent   : THndr_CCEvent;
    UserError : String;
    Res       : Integer;
begin
 CCDevice := THndr_CCDevice.Create(XmlInDocument, Device);
 CCTask   := THndr_CCTask.Create(XmlInDocument, Device);
 CCEvent  := THndr_CCEvent.Create(XmlInDocument, Device);
 try
  if not CCDevice.GetRequestFromDocument then raise EAbort.Create(CCDevice.LastError);
  if not CCTask.GetRequestFromDocument then   raise EAbort.Create(CCTask.LastError);
  if not CCEvent.GetRequestFromDocument then  raise EAbort.Create(CCEvent.LastError);

  if not CCDevice.Execute(Res, UserError) then raise EAbort.Create(CCDevice.LastError);
  if Res = errcode_ExecuteSucceed then
   begin
    if not CCTask.Execute(Res, UserError) then raise EAbort.Create(CCTask.LastError);
   end;
  if Res = errcode_ExecuteSucceed then
   begin
    CCEvent.TaskSentList := CCTask.SentTasksIdList;
    if not CCEvent.Execute(Res, UserError) then raise EAbort.Create(CCEvent.LastError);
   end;

  OutDoc_SetResponceCode(Res, UserError);
  if Res = errcode_ExecuteSucceed then
   begin
    CCDevice.XmlDocument := XmlOutDocument;
    CCTask.XmlDocument   := XmlOutDocument;
    CCEvent.XmlDocument  := XmlOutDocument;

    if not CCDevice.AddAnswerToDocument then raise EAbort.Create(CCDevice.LastError);
    if not CCTask.AddAnswerToDocument then   raise EAbort.Create(CCTask.LastError);
    if not CCEvent.AddAnswerToDocument then  raise EAbort.Create(CCEvent.LastError);
   end;

  PostLogToDB(Device.DeviceInfo.Dev_Serial, XmlInDocument.xml, XmlOutDocument.xml, Res);
  Result  := true;
 except
  on E: Exception do
   begin
    PostLogToDB(Device.DeviceInfo.Dev_Serial, XmlInDocument.xml,
                XmlOutDocument.xml+sLineBreak+sLineBreak+'ERROR:'+sLineBreak+E.Message, errcode_ExecuteFail_Internal);
    InfoMsg   := E.Message;
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CCDevice.Free;
 CCTask.Free;
 CCEvent.Free;
end;

//**************************************************************************************************
//    TDP_Ping
//**************************************************************************************************
{function TDP_Report.InDoc_Load(Source: String): Boolean;
begin
 InCommandText    := cmd_DevReport;
 InCommandVersion := Device.DeviceInfo.Dev_Version;
 InCommandAppname := C_InCommandAppname_CC;

 Result := inherited InDoc_Load(Source);
end;}

function TDP_Report.ProcessData(var InfoMsg: String): Boolean;
var CCReport  : THndr_CCReport;
    UserError : String;
    Res       : Integer;
begin
 CCReport := THndr_CCReport.Create(XmlInDocument, Device);
 try
  if not CCReport.GetRequestFromDocument then raise EAbort.Create(CCReport.LastError);
  if not CCReport.Execute(Res, UserError) then raise EAbort.Create(CCReport.LastError);

  OutDoc_SetResponceCode(Res, UserError);

  if Res = errcode_ExecuteSucceed then
   begin
    CCReport.XmlDocument := XmlOutDocument;

    if not CCReport.AddAnswerToDocument then raise EAbort.Create(CCReport.LastError);
   end;

  PostLogToDB(Device.DeviceInfo.Dev_Serial, XmlInDocument.xml, XmlOutDocument.xml, Res);
  Result  := true;
 except
  on E: Exception do
   begin
    PostLogToDB(Device.DeviceInfo.Dev_Serial, XmlInDocument.xml,
                XmlOutDocument.xml+sLineBreak+sLineBreak+'ERROR:'+sLineBreak+E.Message, errcode_ExecuteFail_Internal);
    InfoMsg   := E.Message;
    LastError := E.Message;
    Result    := false;
   end;
 end;
 CCReport.Free;
end;

end.
