(************************************************************************)
(*  Copyright(c) Eltrade Ltd.                                           *)
(*    All Rights Reserved.                                              *)
(*                                                                      *)
(*  Author: Stiliyan Vukadinov                                          *)
(*  Revision History:                                                   *)
(*    << 1.0 08.09.2013 >>                                              *)
(************************************************************************)
(*  File History:                                                       *)
(*    bSMSComm.pas  Version 1.0                                         *)
(*    Checked in    08/09/2013 at 12:39:03                              *)
(*    Retrieved     08/09/2013 at 18:16:48                              *)
(************************************************************************)
(*  Purpose:                                                            *)
(*    This file contains the code to send SMS via TCM - 1 server.       *)
(*                                                                      *)
(*                                                                      *)
(************************************************************************)
(*  Propertis:                                                          *)
(*    LastError - Contains last error message from send                 *)
(*                and check status opertions                            *)
(************************************************************************)
(*  Public Methods:                                                     *)
(*    SendSMS -                                                         *)
(*    CheckStatus -                                                     *)
(************************************************************************)

unit bSMSComm;

interface

uses
  SysUtils, Classes, IniFiles, md5, HTTPApp, IdHTTP, IdTCPConnection;

type
  TbSMS = class(TObject)
  private
    { Private declarations }
    URL     : string;
    UserName: string;
    Password: string;
    SMSid   : string;

    fLastError    : string; { Private string using from property LastError only for read }
    urlQueryString: string; { String construct for sending to server }
   { Procedures & Functions }
    procedure  LoadIniSettings(IniFileName, IniFileSection: string);
    function   ConvertToMD5(const Input: string): string; { Converting pasword to MD5 format }
    function   FormatPhoneNumber(phone: string): string;
    function   CalcSMSid(): string;
    procedure  QueryToServer(url: string); { Sending query to server }
  public
    constructor Create(IniFileName, IniFileSection: string);
    { Properties }
    property LastError: string read fLastError; { Contains last error message from send and check status opertions }
    { Procedures & Functions }
    function SendSMS(phone, phone_msg: string; id: string = ''): boolean;
  end;

implementation

const
  { Maximum length of sms id }
  MAX_SMS_ID_VALUE = '1000000';
  { Maximum permit length of SMS from mobile operators }
  MAX_SMS_LENGTH = 160;
  { Byte Order Mark Code }
  BOM = '﻿';

function TbSMS.FormatPhoneNumber(phone: string): string;
var
  phoneToInt: Integer;
begin
  Result := '';
  { Remove phone number prefix }
  if (Copy(phone, 1, 5) = '00359') then
    phone := '0' + Copy(phone, 6, Length(phone) - 5);

  if (Copy(phone, 1, 4) = '+359') then
    phone := '0' + Copy(phone, 5, Length(phone) - 4);

  if (Copy(phone, 1, 3) = '359') then
    phone := '0' + Copy(phone, 4, Length(phone) - 3);

  { If phone number contains only digits && it length is 10 && start with prefix '08' then return formatted number }
  if ((Copy(phone, 1, 2) = '08') and (Length(phone) = 10) and (TryStrToInt(phone, phoneToInt))) then
    Result := phone;
end;

function TbSMS.CalcSMSid(): string;
begin
  Randomize;
  Result := SysUtils.Format('%.*d', [Length(MAX_SMS_ID_VALUE), Random(StrToInt(MAX_SMS_ID_VALUE))]);
end;

procedure TbSMS.LoadIniSettings(IniFileName, IniFileSection: string);
begin
  with TIniFile.Create(IniFileName) do
  try
    if (not ValueExists(IniFileSection, 'URL')) then
       WriteString(IniFileSection, 'URL', 'http://www.bsms.bg/api/api_single.php');
    URL := ReadString(IniFileSection, 'URL', '');

    if (not ValueExists(IniFileSection, 'UserName')) then
       WriteString(IniFileSection, 'UserName', 'eltrade');
    UserName := ReadString(IniFileSection, 'UserName', '');

    if (not ValueExists(IniFileSection, 'Password')) then
       WriteString(IniFileSection, 'Password', 'gdelchev');
    Password :=  ReadString(IniFileSection, 'Password', '');
  finally
    Free;
  end;

  SMSid := CalcSMSid();
end;

constructor TbSMS.Create(IniFileName, IniFileSection: string);
begin
  LoadIniSettings(IniFileName, IniFileSection);
  fLastError := '';
  urlQueryString := '';
end;

{ Hash Cryptograph algorithm to conversion text password in MD5 format }
function TbSMS.ConvertToMD5(const Input: string): string;
begin
  Result := MD5Print(MD5String(Input));
end;

{ Sending request to server }
procedure TbSMS.QueryToServer(url: string);
var
  http          : TIdHttp; { High level protocol }
  stringSteam   : TStringStream; { Read text from another storage medium }
begin
  http := TIdHttp.Create(nil);  { Create indy }
  { BOM still exist once again in this format of constructor parameter }
  stringSteam := TStringStream.Create('');
  try
    http.Get(url, StringSteam); { Send Request To Site }
    fLastError := StringReplace(StringSteam.DataString, BOM, '', []); { Remove BOM if exist and get last message }
  except
    on E: EIdHTTPProtocolException do
      fLastError := E.Message; { Get Error Message }
  end;
  stringSteam.Free;
  http.Free;
end;

function TbSMS.SendSMS(phone, phone_msg: string; id: string = ''): boolean;
var
  phone_number: string;
begin
  { When id parameter is missing then generate random value }
  if (id = '') then id := CalcSMSid();

  { Chech phone number format }
  phone_number := FormatPhoneNumber(phone);
  if (phone_number = '') then
    fLastError := 'Phone number format is invalid.'
  else if ((Length(phone_msg) = 0) or (Length(phone_msg) > MAX_SMS_LENGTH)) then
    fLastError := 'SMS message is empty or is longer than 160 characters.' { Check SMS message length }
  else
  begin
    { Construct server request string }
    urlQueryString := URL +
                      '?user=' + HTTPEncode(UserName) +
                      '&pass=' + HTTPEncode(ConvertToMD5(Password)) +
                      '&phone=' + HTTPEncode(phone_number) +
                      '&phone_msg=' + HTTPEncode(phone_msg) +
                      '&phone_api_id=' + HTTPEncode(SMSid);
    QueryToServer(urlQueryString);
  end;

  Result := (fLastError = 'OK');
end;

end.
