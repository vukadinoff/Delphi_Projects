unit UDPThreadUnit;
{ UDP комуникация за сървър
  При изпращане на команда се чака отговор от клиента
  При изпращане на данни не се чака отговор от клиента

  Локален порт на AdminClient: 6677
  Локален порт на POS : 6678
  Локален порт на SyncClient : 6680
  Локален порт на SyncServer : 6679
  Клиента и сървъра работят през различни портове защото:
  1: Ако клиента изпрати нещо, другите клиенти не трябва да го слушат
  2: Ако клиента и сървъра работят на една машина...

  Протокол
  заявка - CMD:Command:Data:
  отговор - REP:Command:Host:Data:
  съобщение - MSG:message text::   }
  
interface

uses Windows, IdUDPServer, IdSocketHandle, Forms, Classes, SysUtils, DB, Sockets;

const  C_MsgTypeCMD : String = 'CMD'; // Incoming Command
       C_MsgTypeMSG : String = 'MSG'; // Incomming Message;
       C_MsgTypeREP : String = 'REP'; // Incoming RESULT on command
       C_MsgTypeERR : String = 'ERR'; // Incoming error message
       C_MsgTypeDAT : String = 'UD';  // Incoming data

type
 TUDPReceiveEvent = procedure(const FromIP_, FromHost_, Data_: String) of object;
 TUDPReceiveCmd = procedure(const FromIP_, FromHost_, Data_: String; var Answer_: String) of object;

 TMyUDPcomm = class(TObject)
  private
   UDP          : TIdUDPServer;
   LastError    : String;
   FLocalHost   : String;
   FCmdResult   : String;

   procedure FUDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
   procedure FUDPCommandRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
   procedure FHandleIncommingData(UdpData, FromIp: String; FromPort: Integer);
   function FGetSubString(var Src_: String; const Sep_: String): String;
//   function FGetNetBiosName(IPAddres_: String): String;
   function FGetLocalHostName: string;
  protected
   FOnInWorkEvent: TUDPReceiveEvent;
   FOnInMessage  : TUDPReceiveEvent;
   FOnInCommand  : TUDPReceiveCmd;
   FOnBroadcastAnswer : TUDPReceiveEvent;
   FOnError : TUDPReceiveEvent;
  public
    Terminated: Boolean;
    function SendCommand(ToHost : String; R_Port : Integer; Command: String; var Data: String) : Boolean;
    function BroadcastCommand(R_Port : Integer; Command: String; Data: String) : Boolean;

    function SendMessage(RHost : String; R_Port : Integer; Data : String) : Boolean;
    function GetLastError : String;
    constructor Create( L_Port : Integer);
    destructor DestroyUDP;

    property LocalHostName: String read FLocalHost;
    property OnInWorkEvent: TUDPReceiveEvent read FOnInWorkEvent write FOnInWorkEvent;
    property OnInMessage  : TUDPReceiveEvent read FOnInMessage write FOnInMessage;
    property OnInCommand  : TUDPReceiveCmd read FOnInCommand write FOnInCommand;
    property OnBroadcastAnswer : TUDPReceiveEvent read FOnBroadcastAnswer write FOnBroadcastAnswer;
  end;

implementation
uses TNetAdapterUnit, DateUtils, IdStack;

constructor TMyUDPcomm.Create(L_Port:  Integer);
begin
  inherited Create;
  FlocalHost := FGetLocalHostName;

  UDP := TIdUDPServer.Create(Application);
  UDP.ThreadedEvent := false;
  UDP.Bindings.Clear;
  with UDP.Bindings.Add do
   begin
    Port := L_Port;
    IP   := '';
   end;
  UDP.OnUDPRead  := FUDPRead;
  try
   UDP.Active := true;
  except
  end;
end;

destructor TMyUDPcomm.DestroyUDP;
begin
 UDP.Active := false;
 UDP.Destroy;
 inherited Destroy;
end;

procedure TMyUDPcomm.FUDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
var S : String;
begin
 if AData.Size > 0 then
  begin
   SetLength(S, AData.Size);
   AData.Read(S[1], AData.Size);
   FHandleIncommingData(S, ABinding.PeerIP, ABinding.PeerPort);
  end;
end;

procedure TMyUDPcomm.FUDPCommandRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
begin
 if AData.Size > 0 then
  begin
   SetLength(FCmdResult, AData.Size);
   AData.Read(FCmdResult[1], AData.Size);
  end;
end;

function TMyUDPcomm.FGetSubString(var Src_: String; const Sep_: String): String;
var Ii_ : Integer;
begin
  Ii_ := Pos(Sep_, Src_);
  if Ii_ > 0 then
   begin
    Result := Copy(Src_, 1, Ii_-1);
    Delete(Src_, 1, Ii_);
   end
  else
   begin
    Result := Src_;
    Src_   := '';
   end;
end;

{function TMyUDPcomm.FGetNetBiosName(IPAddres_: String): String;
begin
 Result := '';
 with TIpSocket.Create(Application) do
  begin
   try
    Result := LookupHostName(IPAddres_);
   finally
    Free;
   end;
  end;
end;}

function TMyUDPcomm.FGetLocalHostName: string;
var buff_ : PChar;
    len_  : DWORD;
begin
  Result := '';
  len_   := MAX_COMPUTERNAME_LENGTH + 1;
  GetMem(buff_, len_);
  try
    if GetComputerName(buff_, len_) then Result := StrPas(buff_);
  finally
    FreeMem(buff_);
  end;
end;

procedure TMyUDPcomm.FHandleIncommingData(UdpData, FromIp: String; FromPort: Integer);
var Mtype: String;
    Cmd  : String;
    Data : String;
    Hst  : String;
    Answ : String;
begin
 Mtype := FGetSubString(UdpData, ':');
 if Mtype = C_MsgTypeCMD then // Incoming Command
  begin
   Cmd  := FGetSubString(UdpData, ':');
   Hst  := FGetSubString(UdpData, ':');
   Data := FGetSubString(UdpData, ':;');

   if (Cmd <> '')and(Hst <> '')and(Assigned(FOnInCommand)) then
    begin
     FOnInCommand(FromIP, Hst, Data, Answ);
     if Answ <> '' then
      Answ := C_MsgTypeREP+':'+Cmd+':'+FLocalHost+':'+Answ+':;'
     else
      Answ := C_MsgTypeERR+':'+Cmd+':'+FLocalHost+':Unknown command:;';
    end
   else
    begin
     Answ := C_MsgTypeERR+':'+Cmd+':'+FLocalHost+':No command handler available:;';
    end;
   // send answer
   UDP.Send(FromIp, FromPort, Answ);
  end
 else
 if Mtype = C_MsgTypeREP then // Incoming RESULT
  begin
   Cmd := FGetSubString(UdpData, ':');
   Hst := FGetSubString(UdpData, ':');
   Data:= FGetSubString(UdpData, ';:');
   if (Data <> '')and(Assigned(FOnBroadcastAnswer)) then
    FOnBroadcastAnswer(FromIp, Hst, Data);
  end
 else
 if Mtype = C_MsgTypeMSG then // Incoming Message
  begin
   Hst  := FGetSubString(UdpData, ':');
   Data := FGetSubString(UdpData, ':;');
   if (Data <> '')and(Assigned(FOnInMessage)) then
    FOnInMessage(FromIp, Hst, Data);
  end
 else
 if Mtype = C_MsgTypeDAT then // Incoming data
  begin
   Hst  := FGetSubString(UdpData, ':');
   Data := FGetSubString(UdpData, ':;');
   if (Data <> '')and(Assigned(FOnInWorkEvent)) then
    FOnInWorkEvent(FromIp, Hst, Data);
  end
 else
 if Mtype = 'ERR' then // Incoming RESULT
  begin
   Cmd := FGetSubString(UdpData, ':');
   Hst := FGetSubString(UdpData, ':');
   Data:= FGetSubString(UdpData, ';:');
   LastError := Data;
   if (Assigned(FOnError)) then FOnError(FromIp, Hst, Data);
  end;
end;

function TMyUDPcomm.BroadcastCommand(R_Port : Integer; Command: String; Data: String) : Boolean;
var NetAdapter : TNetAdapter;
    I          : Integer;
    S          : String;
begin
 S:='CMD:'+Command+':'+FLocalHost+':'+Data+':;';
 Result := False;

 NetAdapter := TNetAdapter.Create;
 try
  if NetAdapter.ActiveIPList.Count = 0 then NetAdapter.ActiveIPList.Add('127.0.0.1', '255.255.255.255');
  try
    for I := 0 to NetAdapter.ActiveIPList.Count - 1 do
     begin
      UDP.Send(NetAdapter.ActiveIPList.Address[I].BroadcastAddress, R_Port, S);
     end;
    Result := true;
  except
  end;
 finally
  NetAdapter.Free;
 end;
end;

function TMyUDPcomm.SendCommand(ToHost : String; R_Port : Integer; Command: String; var Data : String) : Boolean;
var S        : String;
    I        : Integer;
    St       : TDateTime;
    Cmd      : String;
    Host     : String;
//    FromIp   : String;
//    FromPort : Integer;
begin
  Result := False;
  UDP.OnUDPRead := FUDPCommandRead;
  S:='CMD:'+Command+':'+FLocalHost+':'+Data+':;';
  Data := '';

  try
   for I := 1 to 3 do
    begin
     FCmdResult := '';
     ST         := Now;
     UDP.Send(ToHost, R_Port, S);
     while FCmdResult = '' do
      begin
       Application.ProcessMessages;
       if MilliSecondsBetween(ST, Now) > 100 then Break;
      end;
//     S := UDP.ReceiveString(FromIp, FromPort, 200);    // ne raboti po tozi na4in - da ne se polzwa taka
     S := FGetSubString(FCmdResult, ':');
     if S = C_MsgTypeREP then
      begin
       Cmd  := FGetSubString(FCmdResult, ':');
       Host := FGetSubString(FCmdResult, ':');
       Data := FGetSubString(FCmdResult, ';:');
       if (Cmd = Command)and(Host <> '') then
        begin
         Result := True;
         Break;
        end;
      end
     else
     if S = C_MsgTypeERR then
      begin
       Cmd  := FGetSubString(FCmdResult, ':');
       Host := FGetSubString(FCmdResult, ':');
       Data := FGetSubString(FCmdResult, ';:');
       LastError := Data;
      end;
    end;
  except
  end;
  UDP.OnUDPRead := FUDPRead;
end;

function TMyUDPcomm.SendMessage(RHost : String; R_Port : Integer; Data : String) : Boolean;
var S : String;
begin
  S := C_MsgTypeMSG+':'+FLocalHost+':'+Data+':;';
  try
   UDP.Send(RHost, R_Port, S);
   result       := true;
  except
   Result:=false;
  end;
end;

function TMyUDPcomm.GetLastError : String;
begin
 result := LastError;
end;

end.
