unit TCPCommUnit;

interface

uses Classes, SysUtils, DateUtils, Forms,
     IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdTCPServer,
     IdSocketHandle, IdAntiFreeze;

const
  C_END      = $03;    //* indicates end of packet */
  C_ESC      = $04;    //* indicates byte stuffing */
  C_ESC_END  = $05;    //* ESC ESC_END means END data byte */
  C_ESC_ESC  = $06;    //* ESC ESC_ESC means ESC data byte */

  Cmd_Start    : Byte = $14;
  Cmd_Command  : Byte = $15;


type
  TTCPCommandEvent     = procedure(Command: String; Data, Responce: TStrings) of object;
  TTCPExceptProcedure  = procedure(ExceptMsg: String) of object;

  TTCPCommBasics = class(TObject)
  private
   FLastError       : String;
   FExceptProcedure : TTCPExceptProcedure;


//   FOnDataListRead  : TTCPDataListEvent;
//   FOnFileRead      : TTCPFileEvent;
   procedure CapsulatePacket(InStream:TStream; OutStream:TStream);
//   procedure DeCapsulatePacket(InStream:TStream; OutStream:TStream);
   procedure WriteStringToStream(Str: String; Stream: TStream);
   procedure ReadStringFromStream(Stream: TStream; var Str: String);
  public
   property ExceptProcedure: TTCPExceptProcedure read FExceptProcedure write FExceptProcedure;
   property LastError: String read FlastError write FlastError;

//   property OnDataListRead: TTCPDataListEvent read FOnDataListRead write FOnDataListRead;
//   property OnFileRead: TTCPFileEvent read FOnFileRead write FOnFileRead;
  end;

  TTCPCommServer = class(TTCPCommBasics)
  private
   FTCPServer: TIdTCPServer;
   FOnCommandRead   : TTCPCommandEvent;
   function FGetActive: Boolean;
   procedure FTCPServerExecute(AThread: TIdPeerThread);
  public
   constructor Create(Port: Integer; BindInterface: String);
   destructor Destroy; override;

   property Active: Boolean read FGetActive;
   property OnCommandRead: TTCPCommandEvent read FOnCommandRead write FOnCommandRead;
  end;

  TTCPCommClient = class(TTCPCommBasics)
  private
   FTCPClient   : TIdTCPClient;
//   FIdAntiFreeze: TIdAntiFreeze;
   FNoConnEvent : Boolean;

   function FConnectTCPClient: Boolean;
   function FReadResult(TimeoutSec: Byte; var Data, Responce: TStrings): Boolean;
   function FGetTcpConnected: Boolean;
  public
   constructor Create(Port: Integer; ServerHost: String);
   destructor Destroy; override;

   property LogNoConnectionEvent: Boolean read FNoConnEvent write FNoConnEvent;
   property Connected: Boolean read FGetTcpConnected;

   function SendTCPCommand(Command: String; var Data, Responce: TStrings): Boolean;
  end;

implementation

uses
  Dialogs, IdIOHandlerSocket;

//*************************************************************************
//                            TTCPCommBasics
//*************************************************************************

procedure TTCPCommBasics.CapsulatePacket(InStream:TStream; OutStream:TStream);
var K : array[0..1] of Byte;
begin
 // encapsulate data
 InStream.Position  := 0;
 OutStream.Position := 0;
 while InStream.Position < InStream.Size do
  begin
   InStream.Read(K[0],1);
   case K[0] of
   C_END: // Replace END character
    begin
     K[0]:= C_ESC; K[1]:= C_ESC_END;
     OutStream.Write(K[0],2);
    end;
   C_ESC: // Replace ESC character
    begin
     K[0]:= C_ESC; K[1]:= C_ESC_ESC;
     OutStream.Write(K[0],2);
    end;
   else
    OutStream.Write(K[0],1);
   end;
  end;
 K[0]:= C_END; // Send END character
 OutStream.Write(K[0],1);
 OutStream.Position := 0;
end;
{
procedure TTCPCommBasics.DeCapsulatePacket(InStream:TStream; OutStream:TStream);
var K : array[0..1] of Byte;
begin
 InStream.Position  := 0;
 OutStream.Position := 0;
 while InStream.Position < InStream.Size do
  begin
   InStream.Read(K[0],1);
   case K[0] of
   C_END:
    begin
     Break;
    end;
   C_ESC:
    begin
     InStream.Read(K[0],1);
     case K[0]of
     C_ESC_END: K[0] := C_END;
     C_ESC_ESC: K[0] := C_ESC;
     end;
    end;
   end;
   OutStream.Write(K[0],1);
  end;
 OutStream.Position := 0;
end;
}

procedure TTCPCommBasics.WriteStringToStream(Str: String; Stream: TStream);
var StrSize : Longint;
    PValue  : PChar;
begin
 if Stream = nil then Exit;
 StrSize := Length(Str);
 Stream.Write(StrSize, SizeOf(Longint));
 if StrSize > 0 then
  begin
    PValue := Pchar(Str);
    Stream.Write(PValue^, StrSize);
  end;
end;

procedure TTCPCommBasics.ReadStringFromStream(Stream: TStream; var Str: String);
var  StrSize : Longint;
     PValue  : PChar;
begin
  Stream.Read(StrSize, SizeOf(Longint));
  if StrSize > 0 then
   begin
    GetMem(PValue, StrSize);
    try
     Stream.Read(PValue^, StrSize);
     Str := Copy(StrPas(PValue), 1, StrSize);
    finally
     FreeMem(PValue);
    end;
   end
  else
   Str:='';
end;

//*************************************************************************
//                            TTCPCommServer
//*************************************************************************

constructor TTCPCommServer.Create(Port: Integer; BindInterface: String);
var Binding : TIdSocketHandle;
begin
 inherited Create;
 FTCPServer := TIdTCPServer.Create(nil);
 try
  FTCPServer.Bindings.Clear;
  Binding       := FTCPServer.Bindings.Add;
  Binding.IP    := BindInterface;
  Binding.Port  := Port;
  FTCPServer.OnConnect := FTCPServerExecute;
  FTCPServer.Active := True;
 except
  on E: Exception do
   begin
    E.Message  := '[TCPserver] Fail to activate TCP server:'+E.Message;
    FLastError := E.Message;
    if Assigned(FExceptProcedure) then FExceptProcedure(E.Message);
   end;
 end;
end;

destructor TTCPCommServer.Destroy;
begin
 if FTCPServer.Active then FTCPServer.Active := false;
 FTCPServer.Free;
 inherited Destroy;
end;

function TTCPCommServer.FGetActive: Boolean;
begin
 Result := FTCPServer.Active;
end;

procedure TTCPCommServer.FTCPServerExecute(AThread: TIdPeerThread);
var InStrm   : TMemoryStream;
    OutStrm  : TMemoryStream;
    K        : array[0..1] of Byte;
    EndTime  : TDateTime;
    Data     : TStrings;
    Responce : TStrings;
    S1,S2,S3 : String;
begin
 if not AThread.Terminated and AThread.Connection.Connected then
  begin
   InStrm  := TMemoryStream.Create;
   OutStrm := TMemoryStream.Create;
   try
     EndTime := IncSecond(Now, 10);
     repeat
      if not AThread.Connection.Connected then Break;
      AThread.Connection.ReadFromStack(False, 300, False);
      if AThread.Connection.InputBuffer.Size > 0 then
       begin
        AThread.Connection.InputBuffer.Position := 0;
        while AThread.Connection.InputBuffer.Position < AThread.Connection.InputBuffer.Size do
         begin
          AThread.Connection.InputBuffer.Read(K[0],1);
          case K[0] of
          C_ESC:
           begin
            AThread.Connection.InputBuffer.Read(K[0],1);
            case K[0]of
            C_ESC_END: K[0] := C_END;
            C_ESC_ESC: K[0] := C_ESC;
            end;
           end;
          C_END:
           begin
            // handle answer
            if InStrm.Size > 2 then
             begin
              InStrm.Position := 0;
              InStrm.Read(K[0],1);
              if K[0] = Cmd_Start then
               begin
                InStrm.Read(K[0],1);
                if K[0] = Cmd_Command then
                 begin
                  ReadStringFromStream(InStrm, S1); // Command
                  Data     := TStringList.Create;
                  Responce := TStringList.Create;
                  try
                   ReadStringFromStream(InStrm, S2); // Data
                   Data.Text := S2;
                   Data.Values['IP'] := AThread.Connection.Socket.Binding.PeerIP;
                   if Assigned(FOnCommandRead) then FOnCommandRead(S1, Data, Responce);
                   S2 := Data.Text;
                   S3 := Responce.Text;
                  finally
                   Data.Free;
                   Responce.Free;
                  end;
                  // send back responce
                  InStrm.Clear;
                  InStrm.Write(Cmd_Start, SizeOf(Byte));
                  InStrm.Write(Cmd_Command, SizeOf(Byte));
                  WriteStringToStream(S1, InStrm);
                  WriteStringToStream(S2, InStrm);
                  WriteStringToStream(S3, InStrm);
                 end
                else
                 raise Exception.Create('Error in incoming packet format. (second byte)');

                AThread.Connection.OpenWriteBuffer;
                CapsulatePacket(InStrm, OutStrm);
                AThread.Connection.WriteStream(OutStrm, True);
                AThread.Connection.CloseWriteBuffer;
               end
              else
               raise Exception.Create('Error in incoming packet format. (first byte)');
             end;
            AThread.Connection.InputBuffer.Clear;
            AThread.Connection.Disconnect;
            InStrm.Clear; OutStrm.Clear;
            FreeAndNil(InStrm);
            FreeAndNil(OutStrm);
            Exit;
           end;
          end;
          InStrm.Write(K[0],1);
         end;
        AThread.Connection.InputBuffer.Clear;
       end;
     until(EndTime < Now);

     if InStrm.Size > 0 then
      begin
       Raise Exception.Create('Incomplete packet has been retrieved.');
      end;

   except
    on E: Exception do
     begin
      if Pos('DISCONNECT', UpperCase(E.Message)) = 0  then
       begin
        E.Message := '[TCPserver] Fail execute tcp server: '+E.Message;
        FLastError := E.Message;
        if Assigned(FExceptProcedure) then FExceptProcedure(E.Message);
       end;
     end;
   end;
   InStrm.Clear; OutStrm.Clear;
   FreeAndNil(InStrm);
   FreeAndNil(OutStrm);
   AThread.Connection.Disconnect;
  end;
end;

//*************************************************************************
//
//                            TTCPCommClient
//
//*************************************************************************

constructor TTCPCommClient.Create(Port: Integer; ServerHost: String);
begin
 inherited Create;
// FIdAntiFreeze   := TIdAntiFreeze.Create(nil);
// FIdAntiFreeze.Active      := true;
// FIdAntiFreeze.IdleTimeOut := 20;
 FTCPClient      := TIdTCPClient.Create(nil);
 FTCPClient.Port := Port;
 FTCPClient.Host := ServerHost;
end;

destructor TTCPCommClient.Destroy;
begin
// FIdAntiFreeze.Free;
 if FTCPClient.Connected then FTCPClient.Disconnect;
 FTCPClient.Free;

 inherited Destroy;
end;

function TTCPCommClient.FGetTcpConnected: Boolean;
begin
 Result := FTCPClient.Connected;
end;

function TTCPCommClient.FConnectTCPClient: Boolean;
begin
 try
   if not FTCPClient.Connected then FTCPClient.Connect(400);
   if not FTCPClient.Connected then FTCPClient.Connect(400);
 except
  on E: Exception do
   begin
//    E.Message  := '[TCPClient] Fail to connect "'+FTCPClient.Host+':'+IntToStr(FTCPClient.Port)+'": '+E.Message;
    E.Message  := '[TCPClient] Fail to connect: '+E.Message;
    FLastError := E.Message;
    if (FNoConnEvent)and(Assigned(FExceptProcedure)) then FExceptProcedure(E.Message);
   end;
 end;
 Result := FTCPClient.Connected;
end;

function TTCPCommClient.SendTCPCommand(Command: String; var Data, Responce: TStrings): Boolean;
var InStream  : TMemoryStream;
    OutStream : TMemoryStream;
begin
 Result := false;
 if (Data = nil)or(Responce = nil) then Exit;
 if (Command<>'')and(FConnectTCPClient) then
  begin
   InStream  := TMemoryStream.Create;
   OutStream := TMemoryStream.Create;
   try
    InStream.Write(Cmd_Start, SizeOf(Byte));
    InStream.Write(Cmd_Command, SizeOf(Byte));
    WriteStringToStream(Command, InStream);
    WriteStringToStream(Data.Text, InStream);
    CapsulatePacket(InStream, OutStream);
    FTCPClient.WriteStream(OutStream);
    Result := FReadResult(40, Data, Responce);
   except
    on E: Exception do
     begin
      E.Message  := '[TCPClient] Fail to send command: '+E.Message;
      FLastError := E.Message;
      if Assigned(FExceptProcedure) then FExceptProcedure(E.Message);
     end;
   end;
   InStream.Free;
   OutStream.Free;
   if FTCPClient.Connected then FTCPClient.Disconnect;
  end;
end;

function TTCPCommClient.FReadResult(TimeoutSec: Byte; var Data, Responce: TStrings): Boolean;
var K         : Array[0..1] of byte;
    EndTime   : TDateTime;
    SocStrm   : TMemoryStream;
//    StrList   : TStrings;
    S1,S2,S3  : String;
begin
 Result := false;
 Responce.Clear;
 if not FTCPClient.Connected then Exit;

 SocStrm := TMemoryStream.Create;
 try
   EndTime := IncSecond(Now, TimeoutSec);
   repeat
    if not FTCPClient.Connected then Break;
    FTCPClient.ReadFromStack(False, 300, False);
    if FTCPClient.InputBuffer.Size > 0 then
     begin
      FTCPClient.InputBuffer.Position := 0;
      while FTCPClient.InputBuffer.Position < FTCPClient.InputBuffer.Size do
       begin
        FTCPClient.InputBuffer.Read(K[0],1);
        case K[0] of
        C_ESC:
         begin
          FTCPClient.InputBuffer.Read(K[0],1);
          case K[0]of
          C_ESC_END: K[0] := C_END;
          C_ESC_ESC: K[0] := C_ESC;
          end;
         end;
        C_END:
         begin
          // Proceed packet
          SocStrm.Position := 0;
          SocStrm.Read(K[0],1);
          if K[0] = Cmd_Start then
           begin
            SocStrm.Read(K[0],1);
            if K[0] = Cmd_Command then
             begin
              ReadStringFromStream(SocStrm, S1);
              ReadStringFromStream(SocStrm, S2);
              ReadStringFromStream(SocStrm, S3);
              FTCPClient.InputBuffer.Clear;
              Data.Text     := S2;
              Responce.Text := S3;
              Result := true;
             end
            else
             raise Exception.Create('Error in incoming packet format. (command byte)');
           end
          else
           raise Exception.Create('Error in incoming packet format. (first byte)');
          FreeAndNil(SocStrm);
          Exit;
         end;
        end;
        SocStrm.Write(K[0],1);
       end;
      FTCPClient.InputBuffer.Clear;
     end;
   until(EndTime < Now);

   if SocStrm.Size > 0 then  raise Exception.Create('Incomplete packet has been retrieved.');
 except
  on E: Exception do
   begin
    E.Message  := '[TCPClient] Fail to read TCP data: '+E.Message;
    FLastError := E.Message;
    if Assigned(FExceptProcedure) then FExceptProcedure(E.Message);
   end;
 end;
 FreeAndNil(SocStrm);
end;

end.
