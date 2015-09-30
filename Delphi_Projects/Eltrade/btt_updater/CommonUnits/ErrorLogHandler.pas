unit ErrorLogHandler;

interface
uses SysUtils, Classes, BaseHandler, DeviceUnit;

type
 THndr_ClientErrLogServer = class(TStringList)
 private
  FDevice    : TRemoteDevice;
  FEskSerial : String;
  FDevType   : String;
  FDevSerial : String;
  FErrorCode : Integer;
 public
  constructor Create(RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function Execute(var ErrorMessage: String): Boolean;

  property Device: TRemoteDevice read FDevice write FDevice;
  property EskSerial: String read FEskSerial write FEskSerial;
  property DevType: String read FDevType write FDevType;
  property DevSerial: String read FDevSerial write FDevSerial;
  property ErrorCode: Integer read FErrorCode write FErrorCode;
 end;

implementation


constructor THndr_ClientErrLogServer.Create(RemoteDevice: TRemoteDevice);
begin
 FDevice := RemoteDevice;
 FEskSerial := '';
 FDevType   := '';
 FDevSerial := '';
 FErrorCode := 0;
end;

destructor THndr_ClientErrLogServer.Destroy;
begin
 inherited Destroy;
end;

function THndr_ClientErrLogServer.Execute(var ErrorMessage: String): Boolean;
begin
 // за момемта не правим нищо
 Result := true;
end;

end.
