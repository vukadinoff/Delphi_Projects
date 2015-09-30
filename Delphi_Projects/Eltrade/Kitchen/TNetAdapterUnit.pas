unit TNetAdapterUnit;

interface

uses Classes, SysUtils, Contnrs, Windows;



type
 TAddressData = class(TObject)
 private
  FIP   : String;
  FMAsk : String;
  function FGetBroadcastAddress: String;
  function FGetNetworkAddress: String;
  function FGetSubstrFromStr(var Src: String): String;
 public
  property IP: String read FIP write FIP;
  property Mask: String read FMask write FMask;
  property BroadcastAddress: String read FGetBroadcastAddress;
  property NetworkAddress: String read FGetNetworkAddress;

  function IsIPFromNetwork(Ip_: String): Boolean;
 end;

 TAddressList = class(TObject)
 private
  FAddressList : TobjectList;
  function FGetAddress(Index: Integer): TAddressData;
  function FGetCount: Integer;
  function FGetFirstIP: String;
  function FGetFirstMask: String;
 public
  constructor Create;
  destructor Destroy; override;

  procedure Clear;
  function Add(IP_, Mask_: String): Integer;
  function IndexOf(IP_: String): Integer;

  property Address[Index: Integer]: TAddressData read FGetAddress;
  property Count: Integer read FGetCount;
  property FirstIP: String read FGetFirstIP;
  property FirstMask: String read FGetFirstMask;
 end;

 TAdapterInfo = class(TObject)
 private
  FName         : String;
  FDescription  : String;
  FMacAddress   : String;
  FIpAddressList: TAddressList;
  FGatewayList  : TAddressList;
  FDhcpServer   : TAddressList;
  FWinsList     : TAddressList;
  FDhcpEnabled  : Boolean;
  FHaveWINS     : Boolean;
 public
  constructor Create;
  destructor Destroy; override;

  property Name         : String read FName write FName;
  property Description  : String read FDescription write FDescription;
  property MacAddress   : String read FMacAddress write FMacAddress;
  property IpAddressList: TAddressList read FIpAddressList write FIpAddressList;
  property GatewayList  : TAddressList read FGatewayList write FGatewayList;
  property DhcpServer   : TAddressList read FDhcpServer write FDhcpServer;
  property WinsList     : TAddressList read FWinsList write FWinsList;
  property DhcpEnabled  : Boolean read FDhcpEnabled write FDhcpEnabled;
  property HaveWINS     : Boolean read FHaveWINS write FHaveWINS;
 end;

 TNetAdapter = class(TObject)
 private
  FAdapterList : TobjectList;
  FActiveIPList: TAddressList;
  FLastError   : String;

  procedure FLoadAdaptersInfo;
  function FGetAdapter(Index: Integer): TAdapterInfo;
  function FGetAdapterByIP(IP: String): TAdapterInfo;
  function FGetAdaptersCount: Integer;
 public
  constructor Create;
  destructor Destroy; override;

  property Adapters[Index: Integer]: TAdapterInfo read FGetAdapter;
  property AdapterByIP[IP: String]: TAdapterInfo read FGetAdapterByIP;
  property AdaptersCount: Integer read FGetAdaptersCount;
  property ActiveIPList: TAddressList read FActiveIPList;
  property LastError: String read FLastError;

  procedure GetActiveIPAddresses(IPList: TStrings);
  procedure GetActiveBroadcastAddresses(IPList: TStrings);
 end;



implementation

//******************************************************************************
//                           TAddressData
//******************************************************************************
function TAddressData.FGetSubstrFromStr(var Src: String): String;
var I : Integer;
begin
 I := Pos('.', Src);
 if I > 0 then
  begin
   Result := copy(Src, 1, I-1);
   Delete(Src, 1, I);
  end
 else
  begin
   Result := Src;
   Src    := '';
  end;
end;

function TAddressData.FGetBroadcastAddress: String;
var IParr   : array[1..4] of Byte;
    Maskarr : array[1..4] of Byte;
    I : Integer;
    S : String;
begin
 Result := '';
 try
   S := FIP;
   for I := 1 to 4 do IParr[I]   := StrToInt(FGetSubstrFromStr(S));
   S := FMAsk;
   for I := 1 to 4 do Maskarr[I] := StrToInt(FGetSubstrFromStr(S));
   // invert mask
   for I := 1 to 4 do Maskarr[I] := 255 - Maskarr[I];
   // calculate broadcast
   for I := 1 to 4 do Result := Result + '.' + IntToStr(IParr[I] or Maskarr[I]);
   Delete(Result,1,1);
 except
  Result := '';
 end;
end;

function TAddressData.FGetNetworkAddress: String;
var IParr   : array[1..4] of Byte;
    Maskarr : array[1..4] of Byte;
    I : Integer;
    S : String;
begin
 Result := '';
 try
   S := FIP;
   for I := 1 to 4 do IParr[I]   := StrToInt(FGetSubstrFromStr(S));
   S := FMAsk;
   for I := 1 to 4 do Maskarr[I] := StrToInt(FGetSubstrFromStr(S));
   for I := 1 to 4 do Result := Result + '.' + IntToStr(IParr[I] and Maskarr[I]);
   Delete(Result,1,1);
 except
  Result := '';
 end;
end;

function TAddressData.IsIPFromNetwork(Ip_: String): Boolean;
var IParr   : array[1..4] of Byte;
    Netarr  : array[1..4] of Byte;
    Maskarr : array[1..4] of Byte;
    I : Integer;
    S : String;
begin
 Result := false;
 try
   S := FMAsk;
   for I := 1 to 4 do Maskarr[I] := StrToInt(FGetSubstrFromStr(S));
   S := Ip_;
   for I := 1 to 4 do IParr[I]  := StrToInt(FGetSubstrFromStr(S));
   S := IP;
   for I := 1 to 4 do Netarr[I]  := StrToInt(FGetSubstrFromStr(S));
   for I := 1 to 4 do Netarr[I]  := Netarr[I] and Maskarr[I];

   for I := 1 to 4 do
    if (IParr[I] and Maskarr[I]) <> (Netarr[I] and Maskarr[I]) then Exit;
   Result := true;
  except
   Result := false;
  end;
end;

//******************************************************************************
//                           TAddressList
//******************************************************************************
constructor TAddressList.Create;
begin
 inherited Create;
 FAddressList := TObjectList.Create;
 FAddressList.OwnsObjects := true;
end;

destructor TAddressList.Destroy;
begin
 FAddressList.Clear;
 FAddressList.Free;
 inherited Destroy;
end;

function TAddressList.FGetAddress(Index: Integer): TAddressData;
begin
 Result := TAddressData(FAddressList.Items[Index]);
end;

function TAddressList.FGetCount: Integer;
begin
 Result := FAddressList.Count;
end;

function TAddressList.FGetFirstIP: String;
begin
 Result := '';
 if FAddressList.Count > 0 then Result := TAddressData(FAddressList.Items[0]).IP;
end;

function TAddressList.FGetFirstMask: String;
begin
 Result := '';
 if FAddressList.Count > 0 then Result := TAddressData(FAddressList.Items[0]).Mask;
end;

procedure TAddressList.Clear;
begin
 FAddressList.Clear;
end;

function TAddressList.Add(IP_, Mask_: String): Integer;
var Adta : TAddressData;
begin
 Adta      := TAddressData.Create;
 Adta.IP   := IP_;
 Adta.Mask := Mask_;
 Result := FAddressList.Add(Adta);
end;

function TAddressList.IndexOf(IP_: String): Integer;
var I : Integer;
begin
 Result := -1;
 for I := 0 to FAddressList.Count - 1 do
  begin
   if TAddressData(FAddressList.Items[I]).IP = IP_ then
    begin
     Result := I;
     Break;
    end;
  end;
end;

//******************************************************************************
//                           GetAdaptersInfo
//******************************************************************************
constructor TAdapterInfo.Create;
begin
 inherited Create;
 FIpAddressList:= TAddressList.Create;
 FGatewayList  := TAddressList.Create;
 FDhcpServer   := TAddressList.Create;
 FWinsList     := TAddressList.Create;
end;

destructor TAdapterInfo.Destroy;
begin
 FIpAddressList.Free;
 FGatewayList.Free;
 FDhcpServer.Free;
 FWinsList.Free;
 inherited Destroy;
end;

//******************************************************************************
//                           GetAdaptersInfo
//******************************************************************************
const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  {$EXTERNALSYM MAX_ADAPTER_DESCRIPTION_LENGTH}
  MAX_ADAPTER_NAME_LENGTH        = 256; // arb.
  {$EXTERNALSYM MAX_ADAPTER_NAME_LENGTH}
  MAX_ADAPTER_ADDRESS_LENGTH     = 8; // arb.
  {$EXTERNALSYM MAX_ADAPTER_ADDRESS_LENGTH}
  DEFAULT_MINIMUM_ENTITIES       = 32; // arb.
  {$EXTERNALSYM DEFAULT_MINIMUM_ENTITIES}
  MAX_HOSTNAME_LEN               = 128; // arb.
  {$EXTERNALSYM MAX_HOSTNAME_LEN}
  MAX_DOMAIN_NAME_LEN            = 128; // arb.
  {$EXTERNALSYM MAX_DOMAIN_NAME_LEN}
  MAX_SCOPE_ID_LEN               = 256; // arb.
  {$EXTERNALSYM MAX_SCOPE_ID_LEN}

type
  PIP_MASK_STRING = ^IP_MASK_STRING;
  {$EXTERNALSYM PIP_MASK_STRING}
  IP_ADDRESS_STRING = record
    S: array [0..15] of Char;
  end;
  {$EXTERNALSYM IP_ADDRESS_STRING}
  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  {$EXTERNALSYM PIP_ADDRESS_STRING}
  IP_MASK_STRING = IP_ADDRESS_STRING;
  {$EXTERNALSYM IP_MASK_STRING}
  TIpAddressString = IP_ADDRESS_STRING;
  PIpAddressString = PIP_MASK_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  {$EXTERNALSYM PIP_ADDR_STRING}
  _IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;
  {$EXTERNALSYM _IP_ADDR_STRING}
  IP_ADDR_STRING = _IP_ADDR_STRING;
  {$EXTERNALSYM IP_ADDR_STRING}
  TIpAddrString = IP_ADDR_STRING;
  PIpAddrString = PIP_ADDR_STRING;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  {$EXTERNALSYM PIP_ADAPTER_INFO}
  _IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
    Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
    AddressLength: UINT;
    Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    Index: DWORD;
    Type_: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: Longint;
    LeaseExpires: Longint;
  end;
  {$EXTERNALSYM _IP_ADAPTER_INFO}
  IP_ADAPTER_INFO = _IP_ADAPTER_INFO;
  {$EXTERNALSYM IP_ADAPTER_INFO}
  TIpAdapterInfo = IP_ADAPTER_INFO;
  PIpAdapterInfo = PIP_ADAPTER_INFO;


function GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall;
external 'iphlpapi.dll' name 'GetAdaptersInfo';

//******************************************************************************
//                       TNetAdapter
//******************************************************************************
constructor TNetAdapter.Create;
begin
 inherited Create;
 FAdapterList := TObjectList.Create;
 FAdapterList.OwnsObjects := true;
 FActiveIPList := TAddressList.Create;
 try
  FLoadAdaptersInfo;
 except
 end;
end;

destructor TNetAdapter.Destroy;
begin
 FAdapterList.Clear;
 FAdapterList.Free;
 FActiveIPList.Free;
 inherited Destroy;
end;

procedure TNetAdapter.FLoadAdaptersInfo;
var pAdapterInfo : PIP_ADAPTER_INFO;
    IpAddrString : PIP_ADDR_STRING;
    BufSize      : DWORD;
    Status       : DWORD;
    I,J          : Integer;
    Buf          : String;
    Adapter      : TAdapterInfo;
    S1,S2        : String;

    function IpListToStr(Src_: IP_ADDR_STRING): String;
    begin
     Result := StrPas(Src_.IpAddress.S);
    end;
begin
  FAdapterList.Clear;
  Status := GetAdaptersInfo(nil, BufSize);
  if Status <> ERROR_BUFFER_OVERFLOW then Exit;
  pAdapterInfo := AllocMem(BufSize);

  try
   Status := GetAdaptersInfo(pAdapterInfo, BufSize);
   if Status <> ERROR_SUCCESS then
    begin
     case Status of
     ERROR_NOT_SUPPORTED : FLastError := 'GetAdaptersInfo is not supported by the operating ' +
                                         'system running on the local computer.';
     ERROR_NO_DATA : FLastError := 'No network adapter on the local computer.';
     else FLastError := 'GetAdaptersInfo failed with error #' + IntToStr(Status);
     end;
     Exit;
    end;

   while pAdapterInfo <> nil do
    begin
     Adapter             := TAdapterInfo.Create;
     Adapter.Name        := pChar(@pAdapterInfo^.AdapterName);
     Adapter.Description := pChar(@pAdapterInfo^.Description);

     Buf := '';
     for I := 0 to pAdapterInfo^.AddressLength - 1 do Buf := Buf + '-' + IntToHex(pAdapterInfo^.Address[I], 2);
     Delete(Buf, 1, 1);
     Adapter.MacAddress  := Buf;

     IpAddrString := @pAdapterInfo^.IpAddressList;
     while IpAddrString <> nil do
      begin
       S1 := StrPas(pAdapterInfo^.IpAddressList.IpAddress.S);
       S2 := StrPas(pAdapterInfo^.IpAddressList.IpMask.S);
       if (S1 <> '')and(S2 <> '') then Adapter.IpAddressList.Add(S1, S2)
        else Break;
       if Adapter.IpAddressList.Count > 100 then Break;
       IpAddrString := pAdapterInfo^.IpAddressList.Next;
      end;

     IpAddrString := @pAdapterInfo^.GatewayList;
     while IpAddrString <> nil do
      begin
       S1 := StrPas(pAdapterInfo^.GatewayList.IpAddress.S);
       S2 := StrPas(pAdapterInfo^.GatewayList.IpMask.S);
       if (S1 <> '')and(S2 <> '') then Adapter.GatewayList.Add(S1, S2)
        else Break;
       if Adapter.GatewayList.Count > 100 then Break;
       IpAddrString := pAdapterInfo^.GatewayList.Next;
      end;

     IpAddrString := @pAdapterInfo^.DhcpServer;
     while IpAddrString <> nil do
      begin
       S1 := StrPas(pAdapterInfo^.DhcpServer.IpAddress.S);
       S2 := StrPas(pAdapterInfo^.DhcpServer.IpMask.S);
       if (S1 <> '')and(S2 <> '') then Adapter.DhcpServer.Add(S1, S2)
        else Break;
       if Adapter.DhcpServer.Count > 100 then Break;
       IpAddrString := pAdapterInfo^.DhcpServer.Next;
      end;

     IpAddrString := @pAdapterInfo^.PrimaryWinsServer;
     while IpAddrString <> nil do
      begin
       S1 := StrPas(pAdapterInfo^.PrimaryWinsServer.IpAddress.S);
       S2 := StrPas(pAdapterInfo^.PrimaryWinsServer.IpMask.S);
       if (S1 <> '')and(S2 <> '') then Adapter.WinsList.Add(S1, S2)
        else Break;
       if Adapter.WinsList.Count > 100 then Break;
       IpAddrString := pAdapterInfo^.PrimaryWinsServer.Next;
      end;

     IpAddrString := @pAdapterInfo^.SecondaryWinsServer;
     while IpAddrString <> nil do
      begin
       S1 := StrPas(pAdapterInfo^.SecondaryWinsServer.IpAddress.S);
       S2 := StrPas(pAdapterInfo^.SecondaryWinsServer.IpMask.S);
       if (S1 <> '')and(S2 <> '') then Adapter.WinsList.Add(S1, S2)
        else Break;
       if Adapter.WinsList.Count > 200 then Break;
       IpAddrString := pAdapterInfo^.SecondaryWinsServer.Next;
      end;

     Adapter.HaveWINS    := pAdapterInfo^.HaveWins;
     Adapter.DHCPenabled := (pAdapterInfo^.DhcpEnabled <> 0);

     FAdapterList.Add(Adapter);
     pAdapterInfo := pAdapterInfo^.Next;
     if FAdapterList.Count > 100 then Break;
    end;
  finally
   FreeMem(pAdapterInfo);
  end;

 // load active IP interfaces
 FActiveIPList.Clear;
 for I := 0 to AdaptersCount - 1 do
  begin
   for J := 0 to Adapters[I].IpAddressList.Count - 1 do
    with Adapters[I].IpAddressList.Address[J] do
     begin
      if (IP <> '')and(IP <> '0.0.0.0')and(IP <> '255.255.255.255')and(FActiveIPList.IndexOf(IP) < 0) then
       FActiveIPList.Add(IP, Mask);
     end;
  end;  
end;


function TNetAdapter.FGetAdapter(Index: Integer): TAdapterInfo;
begin
 Result := TAdapterInfo(FAdapterList.Items[Index]);
end;

function TNetAdapter.FGetAdapterByIP(IP: String): TAdapterInfo;
var I,J : Integer;
begin
 Result := nil;
 for I := 0 to AdaptersCount - 1 do
  begin
   for J := 0 to TAdapterInfo(FAdapterList.Items[I]).IpAddressList.Count - 1 do
    begin
     if TAdapterInfo(FAdapterList.Items[I]).IpAddressList.Address[J].IP = IP then
      begin
       Result := TAdapterInfo(FAdapterList.Items[I]);
       Break;
      end;
    end;
  end;
end;

function TNetAdapter.FGetAdaptersCount: Integer;
begin
 Result := FAdapterList.Count;
end;

procedure TNetAdapter.GetActiveIPAddresses(IPList: TStrings);
var I,J : Integer;
begin
 if IPList = nil then Exit;
 IPList.Clear;
 for I := 0 to AdaptersCount - 1 do
  begin
   for J := 0 to Adapters[I].IpAddressList.Count - 1 do
    with Adapters[I].IpAddressList.Address[J] do
     begin
      if (IP <> '')and(IP <> '0.0.0.0')and(IP <> '255.255.255.255')and(IPList.IndexOf(IP) < 0) then
       IPList.Add(IP);
     end;
  end;
end;

procedure TNetAdapter.GetActiveBroadcastAddresses(IPList: TStrings);
var I,J : Integer;
begin
 if IPList = nil then Exit;
 IPList.Clear;
 for I := 0 to AdaptersCount - 1 do
  begin
   for J := 0 to Adapters[I].IpAddressList.Count - 1 do
    with Adapters[I].IpAddressList.Address[J] do
     begin
      if (BroadcastAddress <> '')and(BroadcastAddress <> '0.0.0.0')and(BroadcastAddress <> '255.255.255.255')and(IPList.IndexOf(BroadcastAddress) < 0) then
       IPList.Add(BroadcastAddress);
     end;
  end;
end;

end.
