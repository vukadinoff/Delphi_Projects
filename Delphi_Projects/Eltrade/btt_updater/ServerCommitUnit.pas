unit ServerCommitUnit;

interface

uses
  Classes, SysUtils, SyncObjs, BaseHandler, BillingConstUnit, BillingClientUnit;

type
  TSrvCommitThread = class(TThread)
  private
   FHttpClient    : TBillingXMLClient;
   FLastError     : String;
   FSectionId     : String;
   FOnCommitError : TNotifyEvent;
   FOnCommitDone  : TNotifyEvent;
   FServerByssy   : Boolean;

   function FGetCommand: TCommandClient;
   procedure DoOnCommitError;
   procedure DoOnCommitDone;
   procedure FGetServerByssy;
   procedure FSetServerFree;
  protected
   procedure Execute; override;
  public
   constructor Create(TestMode: Boolean; ESKSerial, AppVersion, Section: String);
   destructor Destroy; override;

   procedure Start;
   function AddCommand(CommandClass: TCommandClientClass): TCommandClient;

   property Command: TCommandClient read FGetCommand;
   property OnCommitError: TNotifyEvent read FOnCommitError write FOnCommitError;
   property OnCommitDone: TNotifyEvent read FOnCommitDone write FOnCommitDone;
   property LastError: String read FLastError write FLastError;
   property SectionId: String read FSectionId write FSectionId;
  end;

implementation
uses IniFiles, DateUtils, ConstUnit;

constructor TSrvCommitThread.Create(TestMode: Boolean; ESKSerial, AppVersion, Section: String);
var ServerUrl: String;
begin
 inherited Create(true);
 FreeOnTerminate := true;

 if TestMode then ServerUrl := Set_ServerHostTest
  else ServerUrl := Set_ServerHostWork;

 FHttpClient := TBillingXMLClient.Create(ServerUrl, dev_ESK, ESKSerial, AppVersion);
 FSectionId  := Section;
end;

destructor TSrvCommitThread.Destroy;
begin
 FHttpClient.Free;
 inherited Destroy;
end;

procedure TSrvCommitThread.Start;
begin
 Self.Resume;
end;

procedure TSrvCommitThread.DoOnCommitError;
begin
 if Assigned(FOnCommitError) then FOnCommitError(Self);
end;

procedure TSrvCommitThread.DoOnCommitDone;
begin
 if Assigned(FOnCommitDone) then FOnCommitDone(Self);
end;

function TSrvCommitThread.AddCommand(CommandClass: TCommandClientClass): TCommandClient;
begin
 Result := FHttpClient.AddCommand(CommandClass);
end;

function TSrvCommitThread.FGetCommand: TCommandClient;
begin
 Result := nil;
 if FHttpClient.CommandsCount > 0 then Result := FHttpClient.Commands[0];
end;

procedure TSrvCommitThread.FGetServerByssy;
begin
 FServerByssy := CommunicationActive;
 // много е важно веднага след проверка да го вкараме в бизию
 // иначе всички нишки прочитат че не е преди да го вкарат и се омазват нещата
 if not CommunicationActive then CommunicationActive := true;
end;

procedure TSrvCommitThread.FSetServerFree;
begin
 CommunicationActive := false;
end;

procedure TSrvCommitThread.Execute;
var FDelay: Cardinal;
    FErrC : Integer;
    ST    : TDateTime;
begin
   FDelay := 1000;
   FErrC  := 0;
   ST     := Now;
   try
    if FHttpClient.Host = '' then raise EAbort.Create('Internal error. Comm server url is not specified.');
    if FGetCommand = nil then raise EAbort.Create('Internal error. Server command is not specified.');

    repeat
      // дали в момента има активна сесия към сървъра
      Synchronize(FGetServerByssy);
      if (not FServerByssy) {or(MinutesBetween(ST, Now) > 5)} then
       begin
        case FHttpClient.SendCommand(0, hcmPost, true) of
        errcode_ExecuteFail_Internal:
           begin
            FLastError := 'Грешка при комуникация!'+sLineBreak+
                          FHttpClient.LastError+sLineBreak+
                          FHttpClient.LastCommandErrorMessage;
            Synchronize(FSetServerFree);
            Synchronize(DoOnCommitError);
           end;
        errcode_ExecuteSucceed:
           begin
            Synchronize(FSetServerFree);
            Synchronize(DoOnCommitDone);
            Break;
           end;
        else
           begin
            FDelay := 5000;
            Inc(FErrC);
            FLastError := 'Операцията отказана от сървъра:'+sLineBreak+
                          '['+IntToStr(FHttpClient.LastCommandResultCode)+'] '+FHttpClient.LastCommandErrorMessage;
            Synchronize(FSetServerFree);
            Synchronize(DoOnCommitError);
           end;
        end;
       end
      else // if (not FServerByssy)or(MinutesBetween(ST, Now) > 5) then
       begin
        FDelay := 300;
       end;


      with TEvent.Create(nil, True, False, 'TSrvCommitThread') do
      try
       if WaitFor(FDelay) <> wrTimeOut then Break;
      finally
       Free;
      end;

      if FErrC >= 3 then Break;
       begin
        // Сървъра явно не приема заявката
        // Казваме че е изпълнена за да не се праща отново
        Synchronize(DoOnCommitDone);
        Break;
       end;
      if MinutesBetween(ST, Now) >= 5 then
       begin
        Break;
       end;

    until (Terminated);
   except
    on E: Exception do
     begin
      FLastError := E.Message;
      Synchronize(FSetServerFree);
      Synchronize(DoOnCommitError);
     end;
   end;
  Terminate;
end;

initialization
 CommunicationActive := false;

end.
