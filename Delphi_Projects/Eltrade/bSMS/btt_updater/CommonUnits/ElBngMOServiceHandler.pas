unit ElBngMOServiceHandler;

interface

uses SysUtils, Classes, TCPCommUnit, Forms;

type
  TMobSimStatus = (msActive, msSuspended, msTerminated, msUnknown);

  THndr_ElBngMOService = class(TObject)
  private
   FLastError : String;
   FTcpComm   : TTCPCommClient;
   FUser      : String;
   FPass      : String;
   FSource    : String;

   procedure FAddCommonData(Data_: TStrings);
  public
    constructor Create(ServerHost: string='');
    destructor Destroy; override;

    function GetSimStatus(SimIMSI: String; var SimStatus: TMobSimStatus): Boolean; // връща състоянието на картata; обновява базата данни "SIM_STATUSOPERATOR"
    function SetSimStatus(SimIMSI: String; SimStatus: TMobSimStatus): Boolean; // променя състоянието на картata; обновява базата данни "SIM_STATUSOPERATOR"
    function ProcessDBRequests: Boolean; // изпълнява заявките за пускане и спиране в базата

    property LastError: String read FLastError write FLastError;
    property User: String read FUser write FUser;
    property Pass: String read FPass write FPass;
    property Source: String read FSource write FSource;
  end;

implementation

constructor THndr_ElBngMOService.Create(ServerHost: string='');
begin
 inherited Create;
 if ServerHost = '' then ServerHost := 'localhost';

 FTcpComm := TTCPCommClient.Create(2998, ServerHost);
 FUser    := '';
 FPass    := '';
 FSource  := ExtractFileName(Application.ExeName);
end;

destructor THndr_ElBngMOService.Destroy;
begin
 FTcpComm.Destroy;
 inherited Destroy;
end;

procedure THndr_ElBngMOService.FAddCommonData(Data_: TStrings);
begin
 if Data_ = nil then Exit;
 Data_.Values['USER']   := FUser;
 Data_.Values['PASS']   := FPass;
 Data_.Values['SOURCE'] := FSource;
end;

function THndr_ElBngMOService.GetSimStatus(SimIMSI: String; var SimStatus: TMobSimStatus): Boolean; // връща състоянието на картata; обновява базата данни "SIM_STATUSOPERATOR"
var Cmd      : String;
    Data     : TStrings;
    Responce : TStrings;
begin
 Result   := false;
 Cmd      := 'QuerySIM';
 Data     := TStringList.Create;
 Responce := TStringList.Create;
 try
   FAddCommonData(Data);
   Data.Values['IMSI'] := SimIMSI;

   SimStatus := msUnknown;
   if FTcpComm.SendTCPCommand(Cmd, Data, Responce) then
    begin
     if SameText(Responce.Values['Status'], 'ACTIVE') then SimStatus := msActive
     else
     if SameText(Responce.Values['Status'], 'SUSPEND') then SimStatus := msSuspended
     else
     if SameText(Responce.Values['Status'], 'TERMINATE') then SimStatus := msTerminated
     else
      FLastError := Responce.Text;

     Result := (SimStatus <> msUnknown);
    end
   else
    FLastError := 'Комуникационния сървис е недостъпен.'+sLineBreak+
                  'Моля свържете се с администратора'+sLineBreak+
                  FTcpComm.LastError;

 finally
  Data.Free;
  Responce.Free;
 end;
end;

function THndr_ElBngMOService.SetSimStatus(SimIMSI: String; SimStatus: TMobSimStatus): Boolean; // променя състоянието на картata; обновява базата данни "SIM_STATUSOPERATOR"
var Cmd      : String;
    Data     : TStrings;
    Responce : TStrings;
begin
 Result   := false;
 Cmd      := 'UpdateSIM';
 Data     := TStringList.Create;
 Responce := TStringList.Create;
 try
   FAddCommonData(Data);
   Data.Values['IMSI'] := SimIMSI;

   case SimStatus of
   msActive    : Data.Values['Status'] := 'ACTIVE';
   msSuspended : Data.Values['Status'] := 'SUSPEND';
   msTerminated: Data.Values['Status'] := 'TERMINATE';
   msUnknown   : Data.Values['Status'] := 'UNKNOWN';
   end;

   if FTcpComm.SendTCPCommand(Cmd, Data, Responce) then
    begin
     Result := SameText(Responce.Values['Result'], 'OK');

     if not Result then FLastError := Responce.Text;
    end
   else
    FLastError := 'Комуникационния сървис е недостъпен.'+sLineBreak+
                  'Моля свържете се с администратора'+sLineBreak+
                  FTcpComm.LastError;

 finally
  Data.Free;
  Responce.Free;
 end;
end;

function THndr_ElBngMOService.ProcessDBRequests: Boolean; // изпълнява заявките за пускане и спиране в базата
var Cmd      : String;
    Data     : TStrings;
    Responce : TStrings;
begin
 Result   := false;
 Cmd      := 'ProcessDBRequest';
 Data     := TStringList.Create;
 Responce := TStringList.Create;
 try
   FAddCommonData(Data);

   if FTcpComm.SendTCPCommand(Cmd, Data, Responce) then
    begin
     Result := SameText(Responce.Values['Result'], 'OK');

     if not Result then FLastError := Responce.Text;
    end
   else
    FLastError := 'Комуникационния сървис е недостъпен.'+sLineBreak+
                  'Моля свържете се с администратора'+sLineBreak+
                  FTcpComm.LastError;

 finally
  Data.Free;
  Responce.Free;
 end;
end;

end.
