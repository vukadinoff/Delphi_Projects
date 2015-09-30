unit FiscalizeHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit, XMLHandler, ESKHandler,
     FiscalizationTypes, CryptoHandler, NRAHandler;

type
//******************************************************************************
//           Fiscalization - Base data
//******************************************************************************
 TCmd_FiscDataClient = class(TCommandClient)
 private
  FESKLoginHndr : THndr_ESKLoginClient;
  FDevRegData   : TRegistrationData;
  FReqType      : Integer; // 1 - Регистрация на ФУ/ИАСУТД;   2 - Промяна на обстоятелства;  3 - Отрегистрация
  FTestMode     : Boolean; // предава се от сървъра към клиента. Индицира дали работещия сървър е тестов
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
  property DeviceRegData: TRegistrationData read FDevRegData write FDevRegData;
  property RequestType: Integer read FReqType write FReqType;
  property TestMode: Boolean read FTestMode write FTestMode;
 end;

 TCmd_FiscDataServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr : THndr_ESKLoginServer;
  FDevRegData   : TRegistrationData;
  FReqType      : Integer; // 1 - Регистрация на ФУ/ИАСУТД;   2 - Промяна на обстоятелства;  3 - Отрегистрация
  FTestMode     : Boolean; // предава се от клиента към сървъра. Индицира дали искаме заявка към тестов сървър
  function FGetTestModeText: String;
  function FGetDeviceType: String;
  function FGetDeviceSerial: String;
 published
  // Проверки и валидации в базата - при системна грешка райзват ексепшън
  function FRecordExist(Table: String; KeyField, KeyValue: array of String): Boolean;
  function FCheckInputData(ReqType: Integer; var ErrCode: Integer; var ErrMsg: String): Boolean;  // проверяваме входящите данни за валидност
  function FCheckDelaerStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;  // проверяваме финансовия лимит на дилъра
  function FCheckFDStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;     // проверяваме дали устройството вече не е фискализирано или бракувано
  function FCheckModemStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;  // проверяваме версията на модема
  function FCheckSimStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;     // дали картата съществува в базата
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property DeviceRegData: TRegistrationData read FDevRegData write FDevRegData;
  property RequestType: Integer read FReqType write FReqType;
  property TestMode: Boolean read FTestMode write FTestMode;
  property TestModeAsText: String read FGetTestModeText;
  property DeviceType: String read FGetDeviceType;
  property DeviceSerial: String read FGetDeviceSerial;
 end;

//******************************************************************************
//           Fiscalization - VALIDATE
//******************************************************************************
 TCmd_FiscValidateClient = class(TCmd_FiscDataClient)
 private

 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;
 end;

 TCmd_FiscValidateServer = class(TCmd_FiscDataServer)
 private

 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;
 end;

//******************************************************************************
//           Fiscalization - REQUEST
//******************************************************************************
 TCmd_FiscRequestClient = class(TCmd_FiscDataClient)
 private
  FRequestID : Integer;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property FiscRequestID: Integer read FRequestID write FRequestID;
 end;

 TCmd_FiscRequestServer = class(TCmd_FiscDataServer)
 private
  FRequestID : Integer;

  function FRecordExist(Table: String; Field, Value: array of String): Boolean;
  procedure FUpdateCustomer;
  procedure FUpdateSite(var SiteID: Integer);
  procedure FUpdateDeviceGPRS(SiteID: Integer);
  procedure FUpdateDeviceFU(SiteID: Integer);
//  procedure FUpdatePairHistory(SiteID: Integer);
  procedure FInsertDeviceAction(SiteID: Integer; ActType: Integer; ActComment, ActData: String);
  procedure FUpdateSIMData(SiteID: Integer);
  procedure FRegisterSimToNRA(SIMAction: TSimStatusType; SiteID: Integer);
//  procedure FUpdateSIMPayment(SiteID: Integer);
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property FiscRequestID: Integer read FRequestID write FRequestID;
 end;

//******************************************************************************
//           Fiscalization - COMMIT
//******************************************************************************
 TCmd_FiscCommitClient = class(TCommandClient)
 private
  FCrcHandler     : TCRC32;
  FDeviceSerial   : String;
  FFiscRequestID  : Integer;
  FRegistrationID : Integer;
  FErrorCode      : Integer;
  FErrorMessage   : String;
  FEskSerial      : String;
  FTimeElapsedMsec: Integer;
  FTestMode       : Boolean;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property DeviceSerial: String read FDeviceSerial write FDeviceSerial;
  property FiscRequestID: Integer read FFiscRequestID write FFiscRequestID;
  property RegistrationID: Integer read FRegistrationID write FRegistrationID;
  property ErrorCode: Integer read FErrorCode write FErrorCode;
  property ErrorMessage: String read FErrorMessage write FErrorMessage;
  property EskSerial: String read FEskSerial write FEskSerial;
  property TimeElapsedMsec: Integer read FTimeElapsedMsec write FTimeElapsedMsec;
  property TestMode: Boolean read FTestMode write FTestMode;
 end;

 TCmd_FiscCommitServer = class(THandlerServerUserEvent)
 private
  FCrcHandler     : TCRC32;
  FDeviceSerial   : String;
  FFiscRequestID  : Integer;
  FRegistrationID : Integer;
  FErrorCode      : Integer;
  FErrorMessage   : String;
  FEskSerial      : String;
  FTimeElapsedMsec: Integer;
  FTestMode       : Boolean;
  // попълват се след execute
  FOprType        : Integer;
  FOprName        : String;
  function CalculateCRC(Source: String): String;
  function FGetTestModeText: String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property DeviceSerial: String read FDeviceSerial write FDeviceSerial;
  property FiscRequestID: Integer read FFiscRequestID write FFiscRequestID;
  property RegistrationID: Integer read FRegistrationID write FRegistrationID;
  property ErrorCode: Integer read FErrorCode write FErrorCode;
  property ErrorMessage: String read FErrorMessage write FErrorMessage;
  property EskSerial: String read FEskSerial write FEskSerial;
  property TimeElapsedMsec: Integer read FTimeElapsedMsec write FTimeElapsedMsec;
  property TestMode: Boolean read FTestMode write FTestMode;
  property TestModeAsText: String read FGetTestModeText;
  property OperationType: Integer read FOprType write FOprType;
  property OperationName: String read FOprName write FOprName;
 end;

//******************************************************************************
//           Read fiscalization request - ReadRequest
//******************************************************************************
 TCmd_ReadRequestClient = class(TCommandClient)
 private
  FESKLoginHndr : THndr_ESKLoginClient;
  FDevRegData   : TRegistrationData;
  FFiscRequestID: Integer;
  FDeviceSerial : String;
  FReqDate      : TDateTime;
  FReqComment      : String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property DeviceSerial: String read FDeviceSerial write FDeviceSerial;
  property FiscRequestID: Integer read FFiscRequestID write FFiscRequestID;
  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
  property DeviceRegData: TRegistrationData read FDevRegData write FDevRegData;
  property RequestDate: TDateTime read FReqDate write FReqDate;
  property RequestComment: String read FReqComment write FReqComment;
 end;

 TCmd_ReadRequestServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr : THndr_ESKLoginServer;
  FDevRegData   : TRegistrationData;
  FFiscRequestID: Integer;
  FDeviceSerial : String;
  FReqDate      : TDateTime;
  FReqComment   : String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property DeviceSerial: String read FDeviceSerial write FDeviceSerial;
  property FiscRequestID: Integer read FFiscRequestID write FFiscRequestID;
  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property DeviceRegData: TRegistrationData read FDevRegData write FDevRegData;
  property RequestDate: TDateTime read FReqDate write FReqDate;
  property RequestComment: String read FReqComment write FReqComment;
 end;

implementation
uses BillingConstUnit, DBInterfaceUnit, EIKHandler, DB, DateUtils, VersionUtilsUnit, Dialogs,
     XMLHandlerMS, SimHandler;

//******************************************************************************
//           TCmd_FiscDataClient
//******************************************************************************
constructor TCmd_FiscDataClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
 FDevRegData   := TRegistrationData.Create;
 FTestMode     := false;
end;

destructor TCmd_FiscDataClient.Destroy;
begin
 FESKLoginHndr.Free;
 FDevRegData.Free;
 inherited Destroy;
end;

function TCmd_FiscDataClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FDevRegData.TextForCRC + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqType)).Text := IntToStr(FReqType);
  if FTestMode then iNode.AppendChild(XmlDocument.CreateElement(xml_Node_TestMode)).Text := 'YES';
  if not FDevRegData.SaveToXML(XmlDocument, iNode) then raise EAbort.Create(FDevRegData.LastError);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscDataClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_FiscData);
  FReqType  := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FiscReqType), 0);
  FTestMode := (XML_GetNodeText(iNode, xml_Node_TestMode, false, false) = 'YES');
  if not FDevRegData.LoadFromXML(iNode) then raise EAbort.Create(FDevRegData.LastError);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(FDevRegData.TextForCRC + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//           TCmd_FiscDataServer
//******************************************************************************
constructor TCmd_FiscDataServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
 FDevRegData   := TRegistrationData.Create;
end;

destructor TCmd_FiscDataServer.Destroy;
begin
 FESKLoginHndr.Free;
 FDevRegData.Free;
 inherited Destroy;
end;

function TCmd_FiscDataServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_FiscData);
  FReqType  := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FiscReqType), 0);
  FTestMode := (XML_GetNodeText(iNode, xml_Node_TestMode, false, false) = 'YES');
  if not FDevRegData.LoadFromXML(iNode) then raise EAbort.Create(FDevRegData.LastError);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(FDevRegData.TextForCRC + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscDataServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FDevRegData.TextForCRC + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqType)).Text := IntToStr(FReqType);
  if Device.TestMode then iNode.AppendChild(XmlDocument.CreateElement(xml_Node_TestMode)).Text := 'YES';
  if not FDevRegData.SaveToXML(XmlDocument, iNode) then raise EAbort.Create(FDevRegData.LastError);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscDataServer.FRecordExist(Table: String; KeyField, KeyValue: array of String): Boolean;
var SQL : String;
    I   : Integer;
begin
 Result := false;
 Assert(length(KeyField) = Length(KeyValue));
 with Device.DbInterface do
 try
  SQL := 'SELECT COUNT(*) "RCNT" FROM '+Table+' WHERE ';
  for I := 0 to Length(KeyField)-1 do
   begin
    if I > 0 then SQL := SQL + 'AND';
    SQL := SQL + '('+KeyField[I]+' = '+StrToSQL(KeyValue[I])+')';
   end;
  if not FillDataSet(SQL) then raise EAbort.Create('Check record exist fail! Table: '+Table);
  Result := (DataSet.FieldByName('RCNT').AsInteger > 0);
 finally
  CloseDataSet;
 end;
end;

function TCmd_FiscDataServer.FCheckInputData(ReqType: Integer; var ErrCode: Integer; var ErrMsg: String): Boolean;
var S  : String;

    procedure CheckDataExist(Data_: TObject; ClassName_, DataName_: String);
    begin
     if Data_ = nil then
      raise EHandledException.Create(errcode_Fisc_MissingData,
                                     'Неуспешна валидация на данните от сървъра!'+sLineBreak+
                                     'Подадена е непълна информация към системата.'+sLineBreak+
                                     'Проверете версията на софтуера за фискализация!'+sLineBreak+
                                     ''+sLineBreak+
                                     'Липсва информация за: '+DataName_,
                                     'Incomplete request data. "'+ClassName_+'" is NULL.');
    end;

begin
 Result  := true;
 ErrCode := errcode_ExecuteSucceed;
 ErrMsg  := '';
 try
   if not(RequestType in [1..3]) then
    raise EHandledException.Create(errcode_Fisc_InvalidRequest, 'Неавалиден тип на заявката',
                                                                'Invalid request type: '+IntToStr(RequestType));

   with DeviceRegData do
    begin
     CheckDataExist(FiscDev,   'FiscDev',   'Фискалното устройство');
     CheckDataExist(FiscDevEx, 'FiscDevEx', 'ФУ допълн. данни');
     CheckDataExist(Modem,     'Modem',     'GPRS модема');
     CheckDataExist(Sim,       'Sim',       'SIM картата');
     CheckDataExist(Owner,     'Owner',     'Собственика на ФУ');
     CheckDataExist(Dealer,    'Dealer',    'Сервизна организация ФУ');
     CheckDataExist(Site,      'Site',      'Обект инсталация ФУ');
     CheckDataExist(Services,  'Services',  'Договора сервиз ФУ');
    end;
   if FTestMode <> Device.TestMode then
    begin
     if FTestMode then
      begin
       raise EHandledException.Create(errcode_Fisc_TestModeMismatch, 'НЕВАЛЛИДНА КОНФИГУРАЦИЯ НА СИСТЕМАТА!'+sLineBreak+
                                                                     'Изпратена е тестова заявка към продуктивен сървър!'+sLineBreak+
                                                                     'Моля проверете настройките за URL на сървъра.');
      end
     else
      begin
       raise EHandledException.Create(errcode_Fisc_TestModeMismatch, 'НЕВАЛЛИДНА КОНФИГУРАЦИЯ НА СИСТЕМАТА!'+sLineBreak+
                                                                     'Изпратена е реална заявка към тестов сървър!'+sLineBreak+
                                                                     'Моля проверете настройките за URL на сървъра.');
      end;
    end;
   with DeviceRegData.FiscDev do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните за фискалното устройство!'+sLineBreak+
          ''+sLineBreak;
     if (Length(Serial) <> 8) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден номер на ФУ "EDxxxxxx"',
                                                               'Invalid FD serial: '+Serial);
     if (Copy(Serial, 1, 2) <> 'ED')and(Copy(Serial, 1, 2) <> 'RE')and
        (Copy(Serial, 1, 2) <> 'OP')and(Copy(Serial, 1, 2) <> 'DS')and
        (Copy(Serial, 1, 2) <> 'IZ') then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден номер на ФУ "EDxxxxxx"',
                                                               'Invalid FD serial: '+Serial);
     if (Length(MFM) <> 8) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден номер на МФП "44xxxxxx"',
                                                               'Invalid FD MFM'+MFM);
     if(Copy(MFM, 1, 2) <> '44')and(Copy(MFM, 1, 2) <> '45')and
       (Copy(MFM, 1, 2) <> '55')and(Copy(MFM, 1, 2) <> '35')and
       (Copy(MFM, 1, 2) <> '61') then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден номер на МФП "44xxxxxx"',
                                                               'Invalid FD MFM'+MFM);
     if (Model = '') then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Липсва модел на ФУ',
                                                               'Missing FD model');
     if NRAType = 0 then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден тип на ФУ',
                                                               'Invalid FD NRAType: 0');
     if not FRecordExist('SYSN_FUTYPES', ['FUT_ID'], [IntToStr(NRAType)]) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден тип на ФУ',
                                                               'Unknown ECR NRAType: '+IntToStr(NRAType));
     ModelId := 0; // ID на модела в базата на сървъра
    end;
   with DeviceRegData.FiscDevEx do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в доп.данни за фискалното устройство!'+sLineBreak+
          ''+sLineBreak;
     if LineLength = 0 then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалидна дължина на ред',
                                                               'Invalid FD LineLength: 0');
     if PayTypesCount = 0 then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден брой плащания',
                                                               'Invalid FD Pya Count: 0');
     // валидацията и попълване на нов модел за прави от друга функция
{     with Device.DbInterface do
     try
      if not FillDataSet('SELECT DFM_ID, DFM_SVIDNUMBER, DFM_SVIDDATE, DFM_SVIDMODEL, DFM_NRATYPE '+
                         'FROM DEVICES_FU_MODELS '+
                         'WHERE DFM_MODELNAME = '+StrToSQL(DeviceRegData.FiscDev.Model)) then
       raise EAbort.Create('Select FD model fail: '+Device.DbInterface.LastError);
      if DataSet.IsEmpty then
       begin
        CertifNumber := '';
        CertifDate   := '';
        CertifModel  := '';
        ModelId      := 0;
        CloseDataSet;
        if not ExecuteSQLStatement('INSERT INTO DEVICES_FU_MODELS (DFM_MODELNAME) '+
                                   'VALUES('+StrToSQL(DeviceRegData.FiscDev.Model)+')', RA) then
         raise EAbort.Create('Insert FD model fail: '+Device.DbInterface.LastError);
       end
      else
       begin
        CertifNumber := DataSet.FieldByName('DFM_SVIDNUMBER').AsString;
        CertifDate   := FormatDateTime('DD.MM.YYYY', DataSet.FieldByName('DFM_SVIDDATE').AsDateTime);
        CertifModel  := DataSet.FieldByName('DFM_SVIDMODEL').AsString;
        ModelId      := DataSet.FieldByName('DFM_ID').AsInteger;
        // Ако имаме изчично зададен тип го изпращаме към системата
        if DataSet.FieldByName('DFM_NRATYPE').AsInteger > 0 then DeviceRegData.FiscDev.NRAType := DataSet.FieldByName('DFM_NRATYPE').AsInteger;
       end;
      if (CertifNumber = '')or(CertifDate = '')or(CertifModel = '') then
       begin
        if (Device.TestMode)and(Self.TestMode) then
         begin
          // в тестов режим се ползва някакво свидетелство по подразбиране
          CertifNumber := '500';
          CertifDate   := '01.01.2011';
          CertifModel  := DeviceRegData.FiscDev.Model;
         end
        else
         begin
           // за свидетелство се проверява само в реален режим
           raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Непознат модел ФУ за системата!'+sLineBreak+
                                                                    'Моля свържете се със сервиза на Елтрейд',
                                                                    'Unknown FD model: "'+DeviceRegData.FiscDev.Model+'"'+sLineBreak+
                                                                    'Please enter registration data into DEVICES_FU_MODELS');
         end;
       end;
     finally
      CloseDataSet;
     end;}
    end;
   with DeviceRegData.Modem do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните за данъчния модем!'+sLineBreak+
          ''+sLineBreak;
     if Serial  = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден номер на модем',
                                                               'Invalid modem serial: empty');
     if Model   = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден модел на модем',
                                                               'Invalid modem model: empty');
     if Version = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалидна версия на модем',
                                                               'Invalid modem version: empty');
     ModelId      := 0; // ID на модела в базата на сървъра
     LastVersion  := '';
     FirmwareFile := '';

     // валидацията и попълване на нов модел за прави от друга функция
{     if not FRecordExist('DEVICES_GPRS_MODELS', ['DCM_MODEL'], [Model]) then
      begin
       with Device.DbInterface do
       try
        if not ExecuteSQLStatement('INSERT INTO DEVICES_GPRS_MODELS (DCM_MODEL, DCM_MODELCOMMENT, DCM_FIRMWARE) '+
                                   'VALUES ('+StrToSQL(Model)+','+StrToSQL('Модем Rev: '+Model)+',1)', RA) then
         raise EAbort.Create('Insert GPRS model fail: '+Device.DbInterface.LastError);
       finally
        CloseDataSet;
       end;
      end;}

    end;
   with DeviceRegData.Sim do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните SIM картата!'+sLineBreak+
          ''+sLineBreak;
     if IMSI = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден IMSI номер на SIM',
                                                               'Invalid SIM IMSI: empty');
     // останалите данни се валидират и попълват от функцията за проверка на СИМ
     //MSISDN : String;
     //ICC    : String;
     //Operat : Integer;
     //PayedTo: TDateTime;
    end;
   with DeviceRegData.Owner do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните за собственика на ФУ!'+sLineBreak+
          ''+sLineBreak;
     case CheckEIK(IntToStr(EIKType), EIK) of
     1: raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден ЕИК тип',
                                                                 'Invalid owner EIK type: '+IntToStr(EIKType)+' / '+EIK);
     2: raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден ЕИК',
                                                                 'Invalid owner EIK: '+IntToStr(EIKType)+' / '+EIK);
     end;
     if Name = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Липсва име на собственика',
                                                               'Invalid owner name: empty');
     if (ReqType < 3)and(ShortAdr = '') then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Липсва адрес на собственика',
                                                               'Invalid owner short address: empty'); //- липсва при дерегистрация
     //TaxEik  : String;
    end;
   with DeviceRegData.Dealer do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните за сервизната организация!'+sLineBreak+
          ''+sLineBreak;

     EIKType := 0;
     EIK     := FESKLoginHndr.DealerData.CompanyEIK;
     Name    := FESKLoginHndr.DealerData.CompanyName;

     case CheckEIK(IntToStr(EIKType), EIK) of
     1: raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден ЕИК тип',
                                                                 'Invalid dealer EIK type: '+IntToStr(EIKType)+' / '+EIK);
     2: raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден ЕИК',
                                                                 'Invalid dealer EIK: '+IntToStr(EIKType)+' / '+EIK);
     end;
     if Name = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Липсва име на фирмата',
                                                               'Invalid dealer name: empty');
    end;
   with DeviceRegData.Site do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните за обекта/локацията на ФУ!'+sLineBreak+
          ''+sLineBreak;
     if SiteType.Code       = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден код на обект',
                                                               'Invalid SiteType Code: empty');
     if SiteName            = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалидно име на обект',
                                                               'Invalid SiteName: empty');
     if Number              = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден номер на обект',
                                                               'Invalid site number: empty');
     if Address.Town.Code   = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден код на град',
                                                               'Invalid site town code: empty');
     if Address.Street.Code = '' then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден код на улица',
                                                               'Invalid site street code: empty');
     if not FRecordExist('CUSTOMER_SITETYPES', ['SITET_ID'], [SiteType.Code]) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден код на обект: '+SiteType.Code,
                                                               'SiteType code not found in CUSTOMER_SITETYPES: '+SiteType.Code);
     if not FRecordExist('ADDR_TOWN', ['T_CODE'], [Address.Town.Code]) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден код на град: '+Address.Town.Code,
                                                               'SiteTown code not found in ADDR_TOWN: '+Address.Town.Code+' / '+Address.Town.Name);
     if (Address.Area.Code <> '')and(not FRecordExist('ADDR_AREA', ['A_CODES', 'A_TOWNCODE'], [Address.Area.Code, Address.Town.Code])) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден код на раьон: '+Address.Area.Code,
                                                               'SiteArea code not found in ADDR_AREA: AREA='+Address.Area.Code+' TOWN='+Address.Town.Code);
     if (Address.Street.Code <> C_StreetCodeUnknown)and(not FRecordExist('ADDR_STREET', ['S_CODE', 'S_TOWNCODE'], [Address.Street.Code, Address.Town.Code])) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Несъществуващ код на улица: '+Address.Street.Code,
                                                               'SiteStreet code no found in ADDR_STREET: STREET='+Address.Street.Code+' TOWN='+Address.Town.Code);
    end;
   with DeviceRegData.Services do
    begin
     S := 'Неуспешна валидация на данните от сървъра!'+sLineBreak+
          'Подадена е невалидна информация към системата.'+sLineBreak+
          'Грешка в данните за абонаментен договор!'+sLineBreak+
          ''+sLineBreak;
     if StartDate = 0 then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалидна дата начало експлоат.',
                                                               'Invalid service start date: 0');
     //SvcContrNo   : String;
     if (ReqType < 3)and(SvcContrFrom = 0) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден период на договор за сервиз',
                                                               'Invalid service ContractFrom: 0');
     if SvcContrTo   = 0 then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден период на договор за сервиз',
                                                               'Invalid service ContractTo: 0');
     if (ReqType < 3)and(SvcContrFrom > SvcContrTo) then
      raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден период на договор за сервиз',
                                                               'Invalid service Contractperiod: From='+DateToStr(SvcContrFrom)+' To='+DateToStr(SvcContrTo));
{     if (ReqType < 2) then
      begin
       if SimPeriodId  = 0 then
        raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден период таксуване SIM',
                                                                 'Invalid SIM period ID: 0');
       if not FRecordExist('SIM_CHARGEPERIODS', ['SCP_ID'], [IntToStr(SimPeriodId)]) then
        raise EHandledException.Create(errcode_Fisc_InvalidData, S+'Невалиден период таксуване SIM',
                                                                 'Sim period not found in SIM_CHARGEPERIODS: ID='+IntToStr(SimPeriodId));
      end;}
    end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/CheckInputData');
     end;
    ErrCode := E.ErrorCode;
    ErrMsg  := E.Message;
    Result  := false;
   end;
  on E: Exception do
   begin
    ErrCode := errcode_ExecuteFail_Internal;
    ErrMsg  := 'Unhandled exception!'+sLineBreak+
               '['+Self.ClassName+']:'+sLineBreak+
               E.Message;
    Result  := false;
    Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
   end;
 end;
end;

function TCmd_FiscDataServer.FCheckDelaerStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;
var SQL : String;
    Due : Real;
begin
 Result  := true;
 ErrCode := errcode_ExecuteSucceed;
 ErrMsg  := '';
 try
   with Device.DbInterface do
   try
    SQL := 'SELECT SUM(P.DSP_AMOUNT * C.CUR_RATE) "DUEAMMOUNT" '+
           'FROM DEALERS_SIMPAYMENTS P '+
           'LEFT JOIN CURRENCIES C ON P.DSP_CURRENCY = C.CUR_ID '+
           'WHERE (DSP_DEALEREIK = '+StrToSQL(ESKLoginData.DealerData.CompanyEIK)+')AND'+
           '(DSP_NAVPAYMENTN IS NULL)';
    if not FillDataSet(SQL) then raise EAbort.Create('Select dealer payments fail');
    Due := DataSet.FieldByName('DUEAMMOUNT').AsFloat;
    CloseDataSet;

    SQL := 'SELECT DU.DU_ACTIVE, DU.DU_EXPIREDATE, D.D_ACTIVE, D.D_EXPIREDATE, '+
           'D.D_LIMITAMMOUNT '+
           'FROM DEALERS_USERS DU '+
           'LEFT JOIN DEALERS_USERSRIGHTS DR ON DR.DUR_USERID = DU.DU_ID '+
           'LEFT JOIN DEALERS D ON DU.DU_DEALEREIK = D.D_EIK '+
           'WHERE (DU.DU_USERNAME = '+StrToSQL(ESKLoginData.EskSerial)+')AND'+
           '(DU.DU_DEALEREIK = '+StrToSQL(ESKLoginData.DealerData.CompanyEIK)+')AND'+
           '(DR.DUR_RIGHTID = '+StrToSQL(C_DlrRight_AccFiscApp)+')';
    if not FillDataSet(SQL) then raise EAbort.Create('Select dealer data fail');

    if DataSet.RecordCount = 0 then
     raise EHandledException.Create(errcode_Fisc_DealerAccessDenied, 'Забранен достъп до фискализиращ модул.',
                                                                     'Dealer user not found in DEALERS_USERS, DEALERS and DEALERS_USERSRIGHTS'+sLineBreak+
                                                                     'Access not granted!');

    if DataSet.FieldByName('DU_ACTIVE').AsInteger <> 1 then
     raise EHandledException.Create(errcode_Fisc_DealerUserDisabled, 'Деактивиран потребител.',
                                                                     'Dealer ESK user not active!');

    if DataSet.FieldByName('D_ACTIVE').AsInteger <> 1 then
     raise EHandledException.Create(errcode_Fisc_DealerFirmDisabled, 'Деактивиран потребител/фирма.',
                                                                     'Dealer company not active!');

    if (not DataSet.FieldByName('DU_EXPIREDATE').IsNull)and
       (DataSet.FieldByName('DU_EXPIREDATE').AsDateTime < Date) then
     raise EHandledException.Create(errcode_Fisc_DealerUserExpired, 'Изтекла валидност потребител.',
                                                                    'Dealer ESK user expired: '+DataSet.FieldByName('DU_EXPIREDATE').AsString);

    if (not DataSet.FieldByName('D_EXPIREDATE').IsNull)and
       (DataSet.FieldByName('D_EXPIREDATE').AsDateTime < Date) then
     raise EHandledException.Create(errcode_Fisc_DealerFirmExpired, 'Изтекла валидност потребител/фирма.',
                                                                    'Dealer comapny expired: '+DataSet.FieldByName('D_EXPIREDATE').AsString);

    if (DataSet.FieldByName('D_LIMITAMMOUNT').AsFloat > 0)and
       (Due > DataSet.FieldByName('D_LIMITAMMOUNT').AsFloat) then
     raise EHandledException.Create(errcode_Fisc_DealerLimitExceeded, 'Превишен кредитен лимит!',
                                                                      'Dealer payment limit exceeded: LIMIT='+DataSet.FieldByName('D_LIMITAMMOUNT').AsString+' DUE='+FloatToSql(Due));
   finally
    CloseDataSet;
   end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/CheckDelaerStatus');
     end;
    ErrCode := E.ErrorCode;
    ErrMsg  := 'Достъпа до системата отказан!'+sLineBreak+
               ''+sLineBreak+
               E.Message+sLineBreak+
               ''+sLineBreak+
               'За повече информация се свържете'+sLineBreak+
               'с обслужващия ви търговец!';
    Result  := false;
   end;
  on E: Exception do
   begin
    ErrCode := errcode_ExecuteFail_Internal;
    ErrMsg  := 'Unhandled exception!'+sLineBreak+
               '['+Self.ClassName+']'+sLineBreak+
               E.Message;
    Result  := false;
    Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
   end;
 end;
end;

function TCmd_FiscDataServer.FCheckModemStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;
var RA  : Integer;
    SQL : String;
begin
 Result  := true;
 ErrCode := errcode_ExecuteSucceed;
 ErrMsg  := '';
 try
  if DeviceRegData.Modem = nil then
   raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за Модем',
                                                            'Invalid DeviceRegData. Modem: empty');
  // get device ModelID
  with Device.DbInterface do
  try
    repeat
     SQL := 'SELECT M.DCM_ID, M.DCM_MODEL, F.DCF_LASTVERSION, F.DCF_FILENAME '+
            'FROM DEVICES_GPRS_MODELS M '+
            'LEFT JOIN DEVICES_GPRS_FIRMWARE F ON M.DCM_FIRMWARE = F.DCF_ID '+
            'WHERE M.DCM_MODEL = '+ StrToSQL(DeviceRegData.Modem.Model);

     if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_GPRS_MODELS fail: '+Device.DbInterface.LastError);
     if DataSet.IsEmpty then
      begin
        DeviceRegData.Modem.ModelId      := 0;
        DeviceRegData.Modem.LastVersion  := '';
        DeviceRegData.Modem.FirmwareFile := '';
        Device.DbInterface.CloseDataSet;

        if not ExecuteSQLStatement('INSERT INTO DEVICES_GPRS_MODELS (DCM_MODEL, DCM_MODELCOMMENT, DCM_FIRMWARE) VALUES ('+
                                   StrToSQL(DeviceRegData.Modem.Model)+','+
                                   StrToSQL('Модем Rev: '+DeviceRegData.Modem.Model)+','+
                                   '1)', RA) then
         raise EAbort.Create('Insert GPRS model fail: '+Device.DbInterface.LastError);

      end
     else
      begin
        DeviceRegData.Modem.ModelId      := DataSet.FieldByName('DCM_ID').AsInteger;
        DeviceRegData.Modem.LastVersion  := DataSet.FieldByName('DCF_LASTVERSION').AsString;
        DeviceRegData.Modem.FirmwareFile := DataSet.FieldByName('DCF_FILENAME').AsString;
        CloseDataSet;
       end;
    until DeviceRegData.Modem.ModelId > 0;
  finally
   CloseDataSet;
  end;

  // проверка версията на фирмуера
//  if (Device.DeviceInfo.Dev_Type = 'ESK')and
//     (CompareVersions('1.1.0.0', Device.DeviceInfo.Dev_Version) >= 0) then
//   begin // От тази версия на фискализатора нагоре се прави проверка за ъпдейт на модем

    // ако нямаме изрично зададена версия за този тип модем включваме аварийна проверка
    // аварийната проверка е по първата цифра на версията на устройството
    // това е заради бъг в системата - имаме външни и вътрешни модеми произзведени с еднакъв модел
    if (DeviceRegData.Modem.LastVersion = '')or(DeviceRegData.Modem.FirmwareFile = '') then
     begin
      with Device.DbInterface do
      try
       SQL := 'SELECT DCF_LASTVERSION, DCF_FILENAME FROM DEVICES_GPRS_FIRMWARE '+
              'WHERE (DCF_LASTVERSION IS NOT NULL)and(DCF_LASTVERSION <> '+StrToSQL('')+')and(DCF_FILENAME <> '+StrToSQL('')+')';
       if not FillDataSet(SQL) then raise EAbort.Create('Read DEVICES_GPRS_FIRMWARE fail: '+LastError);
       while not DataSet.Eof do
        begin
         if AnsiSameText(GetMajorVersion(DataSet.FieldByName('DCF_LASTVERSION').AsString),
                         GetMajorVersion(DeviceRegData.Modem.Version)) then
          begin
           DeviceRegData.Modem.LastVersion  := DataSet.FieldByName('DCF_LASTVERSION').AsString;
           DeviceRegData.Modem.FirmwareFile := DataSet.FieldByName('DCF_FILENAME').AsString
          end;
         DataSet.Next;
        end;
      finally
       CloseDataSet;
      end;
     end;

    if (DeviceRegData.Modem.LastVersion <> '')and(DeviceRegData.Modem.FirmwareFile <> '') then
     begin
      if CompareVersions(DeviceRegData.Modem.Version, DeviceRegData.Modem.LastVersion) > 0 then
       begin
        raise EHandledException.Create(errcode_Fisc_ModemNeedUpdate,
                                       'Фирмуера на модема има нужда от обновяване!'+sLineBreak+
                                       'Вашата версия: '+ DeviceRegData.Modem.Version+sLineBreak+
                                       'Последна версия: '+ DeviceRegData.Modem.LastVersion+sLineBreak+
                                       ''+sLineBreak+
                                       'Моля обновете версията на модема за да'+sLineBreak+
                                       'продължите с избраната операция.', '');
       end;
     end;
//   end; // if (Device.DeviceInfo.Dev_Type = 'ESK')and

 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/CheckModemStatus');
     end;
    ErrCode := E.ErrorCode;
    ErrMsg  := E.Message;
    Result  := false;
   end;
  on E: Exception do
   begin
    ErrCode := errcode_ExecuteFail_Internal;
    ErrMsg  := 'Unhandled exception!'+sLineBreak+
               '['+Self.ClassName+']'+sLineBreak+
               E.Message;
    Result  := false;
    Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
   end;
 end;
end;

function TCmd_FiscDataServer.FCheckFDStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;
var SQL: String;
    RA : Integer;
begin
 Result  := true;
 ErrCode := errcode_ExecuteSucceed;
 ErrMsg  := '';
 try
   with Device.DbInterface do
   try
    if DeviceRegData.FiscDev = nil then
     raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за ФУ',
                                                              'Invalid DeviceRegData.FiscDev: empty');
    if DeviceRegData.FiscDevEx = nil then
     raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват доп.данни за ФУ',
                                                              'Invalid DeviceRegData.FiscDevEx: empty');
    // get device ModelID
    repeat
      SQL := 'SELECT DFM_ID, DFM_SVIDNUMBER, DFM_SVIDDATE, DFM_SVIDMODEL, DFM_NRATYPE '+
             'FROM DEVICES_FU_MODELS '+
             'WHERE DFM_MODELNAME = '+StrToSQL(DeviceRegData.FiscDev.Model);
      if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU_MODELS fail: '+Device.DbInterface.LastError);
      if DataSet.IsEmpty then
       begin
        DeviceRegData.FiscDev.ModelId        := 0;
        DeviceRegData.FiscDevEx.CertifNumber := '';
        DeviceRegData.FiscDevEx.CertifDate   := '';
        DeviceRegData.FiscDevEx.CertifModel  := '';
        CloseDataSet;
        if not ExecuteSQLStatement('INSERT INTO DEVICES_FU_MODELS (DFM_MODELNAME) '+
                                   'VALUES('+StrToSQL(DeviceRegData.FiscDev.Model)+')', RA) then
         raise EAbort.Create('Insert FD model fail: '+Device.DbInterface.LastError);
       end
      else
       begin
        DeviceRegData.FiscDev.ModelId        := DataSet.FieldByName('DFM_ID').AsInteger;
        DeviceRegData.FiscDevEx.CertifNumber := DataSet.FieldByName('DFM_SVIDNUMBER').AsString;
        DeviceRegData.FiscDevEx.CertifDate   := FormatDateTime('DD.MM.YYYY', DataSet.FieldByName('DFM_SVIDDATE').AsDateTime);
        DeviceRegData.FiscDevEx.CertifModel  := DataSet.FieldByName('DFM_SVIDMODEL').AsString;
        // Ако имаме изчично зададен тип го изпращаме към системата
        if DataSet.FieldByName('DFM_NRATYPE').AsInteger > 0 then DeviceRegData.FiscDev.NRAType := DataSet.FieldByName('DFM_NRATYPE').AsInteger;
        CloseDataSet;
       end;
    until DeviceRegData.FiscDev.ModelId > 0;

    if (DeviceRegData.FiscDevEx.CertifNumber = '')or
       (DeviceRegData.FiscDevEx.CertifDate = '')or
       (DeviceRegData.FiscDevEx.CertifModel = '') then
     begin
      if (Device.TestMode)and(Self.TestMode) then
       begin
        // в тестов режим се ползва някакво свидетелство по подразбиране
        DeviceRegData.FiscDevEx.CertifDate   := '01.01.2011';
        DeviceRegData.FiscDevEx.CertifModel  := DeviceRegData.FiscDev.Model;
        if DeviceRegData.FiscDev.NRAType = 3 then DeviceRegData.FiscDevEx.CertifNumber := '900FS'
         else DeviceRegData.FiscDevEx.CertifNumber := '500';
       end
      else
       begin
         // за свидетелство се проверява само в реален режим
         raise EHandledException.Create(errcode_Fisc_InvalidData,
                                        'Неуспешна валидация на данните от сървъра!'+sLineBreak+
                                        'Грешка в данните за фискалното устройство!'+sLineBreak+
                                        ''+sLineBreak+
                                        'Непознат модел ФУ за системата!'+sLineBreak+
                                        'Моля свържете се със сервиза на Елтрейд',
                                        'Unknown FD model: "'+DeviceRegData.FiscDev.Model+'"'+sLineBreak+
                                        'Please enter registration data into DEVICES_FU_MODELS');
       end;
     end;

    SQL := 'SELECT DF_MODEL, DF_CREATEDATE, DF_FISCALIZATIONDATE, DF_STATUS '+
           'FROM DEVICES_FU '+
           'WHERE DF_SERIAL = '+StrToSQL(DeviceRegData.FiscDev.Serial);
    if not FillDataSet(SQL) then raise EAbort.Create('Select device FU fail');
    if DataSet.IsEmpty then
     begin
      // 1 - Регистрация на ФУ/ИАСУТД;   2 - Промяна на обстоятелства;  3 - Отрегистрация
      if RequestType in [2..3] then
       begin
        Device.PostEventSystem(C_EvType_Error, 'FD not found in DEVICES_FU during RequestType='+IntToStr(RequestType), Self.ClassName);
//        raise EHandledException.Create(errcode_Fisc_DeviceNotExist,
//                                       'Устройството не съществува'+sLineBreak+
//                                       'в базата данни!'+sLineBreak+
//                                       'Не може да извършите избраната'+sLineBreak+
//                                       'операция при нерегистрирано устройство.',
//                                       'FD not found in DEVICES_FU during RequestType='+IntToStr(RequestType));
       end;
     end
    else
     begin
      case DataSet.FieldByName('DF_STATUS').AsInteger of
      0,1: //нова каса; подадена заявка за регистрация към НАП
       begin
//        if RequestType = 3 then
//          Device.PostEventSystem(C_EvType_Error, 'FD deregistration on unregistered device!'+sLineBreak+
//                                                 'Device: '+DeviceRegData.FiscDev.Serial+sLineBreak+
//                                                 'DF_STATUS=0,1; RequestType=3', Self.ClassName);
//         raise EHandledException.Create(errcode_Fisc_DeviceNotRegistered,
//                                        'Устройството не е регистрирано!'+sLineBreak+
//                                        'Не може да извършите избраната'+sLineBreak+
//                                        'операция при нерегистрирано устройство.',
//                                        'Invalid DF_STATUS from DEVICES_FU. DF_STATUS='+DataSet.FieldByName('DF_STATUS').AsString+' RequestType='+IntToStr(RequestType));
       end;
      2: //направена регистрация в НАП
       begin
//        Device.PostEventSystem(C_EvType_Error, '')
//        if RequestType = 1 then
//          Device.PostEventSystem(C_EvType_Error, 'FD registration on registered device!'+sLineBreak+
//                                                 'Device: '+DeviceRegData.FiscDev.Serial+sLineBreak+
//                                                 'DF_STATUS=2; RequestType=2', Self.ClassName);
//         raise EHandledException.Create(errcode_Fisc_DeviceAlrRegistered,
//                                        'Устройството вече е регистрирано!'+sLineBreak+
//                                        'Не може да извършите избраната'+sLineBreak+
//                                        'операция при регистрирано устройство.',
//                                        'Invalid DF_STATUS from DEVICES_FU. DF_STATUS='+DataSet.FieldByName('DF_STATUS').AsString+' RequestType='+IntToStr(RequestType));
       end;
      3: //Дерегистрирано устройство
       begin
//        if RequestType = 3 then
//          Device.PostEventSystem(C_EvType_Error, 'FD deregistration on deregistered device!'+sLineBreak+
//                                                 'Device: '+DeviceRegData.FiscDev.Serial+sLineBreak+
//                                                 'DF_STATUS=3; RequestType=3', Self.ClassName);
//         raise EHandledException.Create(errcode_Fisc_DeviceAlrDisabled,
//                                        'Устройството вече е дерегистрирано!'+sLineBreak+
//                                        'Не може да извършите избраната'+sLineBreak+
//                                        'операция при дерегистрирано устройство.',
//                                        'Invalid DF_STATUS from DEVICES_FU. DF_STATUS='+DataSet.FieldByName('DF_STATUS').AsString+' RequestType='+IntToStr(RequestType));
       end;
      end;
     end;
   finally
    CloseDataSet;
   end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/CheckFDStatus');
     end;
    ErrCode := E.ErrorCode;
    ErrMsg  := E.Message;
    Result  := false;
   end;
  on E: Exception do
   begin
    ErrCode := errcode_ExecuteFail_Internal;
    ErrMsg  := 'Unhandled exception!'+sLineBreak+
               '['+Self.ClassName+']'+sLineBreak+
               E.Message;
    Result  := false;
    Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
   end;
 end;
end;

function TCmd_FiscDataServer.FCheckSimStatus(var ErrCode: Integer; var ErrMsg: String): Boolean;
var SQL: String;

    function IncludeTrailingSlash(Src_: String): String;
    begin
     Result := Trim(Src_);
     if Result[Length(Result)] <> '/' then Result := Result + '/';
    end;
begin
 Result  := true;
 ErrCode := errcode_ExecuteSucceed;
 ErrMsg  := '';
 try
  if DeviceRegData.Sim = nil then
   raise EHandledException.Create(errcode_Fisc_MissingData, 'Липсват данни за SIM',
                                                            'Ivalid DeviceRegData.Sim: NULL');
  with Device.DbInterface do
  try
   SQL := 'SELECT S.SIM_MSISDN, S.SIM_ICC, S.SIM_OPERATORNAME, S.SIM_OPERATORCODE, '+
          'S.SIM_STATUSNAP, S.SIM_STATUSOPERATOR, S.SIM_PAYEDTODATE, S.SIM_ACTIVATIONDATE, '+
          'M.MOBO_APN2URL1, M.MOBO_APN2URL2 '+
          'FROM SIM S '+
          'LEFT JOIN SYSN_MOBILEOPERATORS M ON S.SIM_OPERATORCODE = M.MOBO_ID '+
          'WHERE S.SIM_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
   if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail:'+Device.DbInterface.LastError);
   if DataSet.IsEmpty then  // картата липсва в основните карти - търсим във външните
    begin
     CloseDataSet;
     SQL := 'SELECT S.SE_MSISDN, S.SE_ICC, S.SE_MOBILEOPERATOR, S.SE_STATUS, S.SE_APN2ENABLED, '+
            'M.MOBO_APN2URL1, M.MOBO_APN2URL2 '+
            'FROM SIM_EXTERNAL S '+
            'LEFT JOIN SYSN_MOBILEOPERATORS M ON S.SE_MOBILEOPERATOR = M.MOBO_ID '+
            'WHERE S.SE_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
     if not FillDataSet(SQL) then raise EAbort.Create('Select SIMext fail');
     if not DataSet.IsEmpty then
      begin
       DeviceRegData.Sim.MSISDN     := DataSet.FieldByName('SE_MSISDN').AsString;
       DeviceRegData.Sim.ICC        := DataSet.FieldByName('SE_ICC').AsString;
       DeviceRegData.Sim.Operat     := DataSet.FieldByName('SE_MOBILEOPERATOR').AsInteger;
       DeviceRegData.Sim.PayedTo    := 0;  // външна карта за системата
       DeviceRegData.Sim.StatusOper := 0;
       DeviceRegData.Sim.IsExternal := true;
       DeviceRegData.Sim.Activation := 0;
       DeviceRegData.Sim.Apn2Url    := '';

       if DataSet.FieldByName('SE_APN2ENABLED').AsInteger > 0 then
        begin
         // разделяне на устройствата към 2 порта
         if (DataSet.FieldByName('MOBO_APN2URL2').AsString <> '')and
            (DeviceRegData.FiscDev.Serial[Length(DeviceRegData.FiscDev.Serial)] in ['5'..'9']) then
          DeviceRegData.Sim.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL2').AsString) + cmd_DevPing
         else
         if (DataSet.FieldByName('MOBO_APN2URL1').AsString <> '') then
          DeviceRegData.Sim.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL1').AsString) + cmd_DevPing;
        end;

       if (DeviceRegData.Sim.ICC = '')or(DeviceRegData.Sim.MSISDN = '') then
        raise EHandledException.Create(errcode_Fisc_InvalidData, 'SIM картата е регистрирана с невалидни данни в системата!');

       case DataSet.FieldByName('SE_STATUS').AsInteger of
       0:begin // Чака одобрение;
          raise EHandledException.Create(errcode_Sim_WaitApproval, 'SIM картата не е регистрирана!'+sLineBreak+
                                                                   'Заявката за подмяна на SIM карта все още'+sLineBreak+
                                                                   'не е одобрена. Моля опитайте по-късно.',
                                                                   'External SIM card waiting for approval!'+sLineBreak+
                                                                   'SIM_EXTERNAL.SE_STATUS=0'+sLineBreak+
                                                                   'IMSI: '+DeviceRegData.Sim.IMSI);
         end;
       1:begin // Одобрена;
         end;
       2:begin // Фискализирано у-во;
         end;
       3:begin // Забранена
          raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е блокирана!',
                                                               'Forbidden external SIM card'+sLineBreak+
                                                               'SIM_EXTERNAL.SE_STATUS=3'+sLineBreak+
                                                               'IMSI: '+DeviceRegData.Sim.IMSI);
         end;
       end;
      end
     else
      raise EHandledException.Create(errcode_Sim_NotExist, 'SIM картата не е намерена в системата!'+sLineBreak+
                                                           'Моля подайте заявка за подмяна на SIM карта.'+sLineBreak+
                                                           'https://partners.eltrade.com/',
                                                           'Missing SIM card in SIM and SIM_EXTERNAL'+sLineBreak+
                                                           'This is external SIM without registration!'+sLineBreak+
                                                           'IMSI: '+DeviceRegData.Sim.IMSI);
    end
   else
    begin
     DeviceRegData.Sim.MSISDN     := DataSet.FieldByName('SIM_MSISDN').AsString;
     DeviceRegData.Sim.ICC        := DataSet.FieldByName('SIM_ICC').AsString;
     DeviceRegData.Sim.Operat     := DataSet.FieldByName('SIM_OPERATORCODE').AsInteger;
     DeviceRegData.Sim.PayedTo    := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;
     DeviceRegData.Sim.Activation := DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime;
     DeviceRegData.Sim.StatusOper := DataSet.FieldByName('SIM_STATUSOPERATOR').AsInteger;
     DeviceRegData.Sim.IsExternal := false;

     // разделяне на устройствата към 2 порта
     if (DataSet.FieldByName('MOBO_APN2URL2').AsString <> '')and
        (DeviceRegData.FiscDev.Serial[Length(DeviceRegData.FiscDev.Serial)] in ['5'..'9']) then
      DeviceRegData.Sim.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL2').AsString) + cmd_DevPing
     else
     if (DataSet.FieldByName('MOBO_APN2URL1').AsString <> '') then
      DeviceRegData.Sim.Apn2Url := IncludeTrailingSlash(DataSet.FieldByName('MOBO_APN2URL1').AsString) + cmd_DevPing
     else
      DeviceRegData.Sim.Apn2Url := '';

     if (DeviceRegData.Sim.ICC = '')or(DeviceRegData.Sim.MSISDN = '') then
      raise EHandledException.Create(errcode_Fisc_InvalidData, 'SIM картата е регистрирана с невалидни данни в системата!');


     case DataSet.FieldByName('SIM_STATUSOPERATOR').AsInteger of
     3: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е деактивирана (терминирана)!'+sLineBreak+
                                                             'Тази карта не може да бъде използвана повече.'+sLineBreak+
                                                             'Необходима е физическата и подмяна!'+sLineBreak+
                                                             'IMSI: '+DeviceRegData.Sim.IMSI,
                                                             '');
     4: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е в процес на спиране!'+sLineBreak+
                                                             'Нужна е активна картата за да изпъните операцията.'+sLineBreak+
                                                             'IMSI: '+DeviceRegData.Sim.IMSI,
                                                             '');
//     5: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е в процес на пускане!'+sLineBreak+
//                                                             'Чакаме мобилния оператор да потвърди пускането.'+sLineBreak+
//                                                             'Моля опитайте по късно. (на следващия ден...)'+sLineBreak+
//                                                             'IMSI: '+DeviceRegData.Sim.IMSI,
//                                                             '');
     6: raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е в процес на деактивиране!'+sLineBreak+
                                                             'Тази карта не може да бъде използвана повече.'+sLineBreak+
                                                             'Необходима е физическата и подмяна!'+sLineBreak+
                                                             'IMSI: '+DeviceRegData.Sim.IMSI,
                                                             '');
     end;

     // проверка на период СИМ
     if DeviceRegData.Services.SimPeriodLen = 0 then
      begin
       SQL := 'SELECT SCP_ID, SCP_LENGTH FROM SIM_CHARGEPERIODS ORDER BY SCP_LENGTH DESC';
       if not FillDataSet(SQL) then raise EAbort.Create('Select SIM_CHARGEPERIODS fail');
       if not DataSet.IsEmpty then
        begin
         DeviceRegData.Services.SimPeriodLen := DataSet.FieldByName('SCP_LENGTH').AsInteger;
         DeviceRegData.Services.SimPeriodId  := DataSet.FieldByName('SCP_ID').AsInteger;
        end;
      end;

{     // проверка за минимален период на таксата
     // при дерегистрация на се избира такъв период
     if (RequestType in [1, 2])and
        (DeviceRegData.Services.SimPeriodLen < DataSet.FieldByName('MOBO_MINCHARGEPERIOD').AsInteger)and
        ((DeviceRegData.Sim.PayedTo = 0)or(DeviceRegData.Sim.PayedTo < IncDay(Now, 5))) then
      begin
       raise EHandledException.Create(errcode_Sim_PeriodError, 'Невалиден срок за активиране на SIM!'+sLineBreak+
                                                               'Минималния период за активиране на карта'+sLineBreak+
                                                               'предоставена от: "'+DataSet.FieldByName('SIM_OPERATORNAME').AsString+
                                                               '" e '+DataSet.FieldByName('MOBO_MINCHARGEPERIOD').AsString+' месеца.'+sLineBreak+
                                                               'Моля коригирайте избрания период.',
                                                               'Invalid SIM activation period: '+IntToStr(DeviceRegData.Services.SimPeriodLen)+sLineBreak+
                                                               'Min period for operator: '+DataSet.FieldByName('MOBO_MINCHARGEPERIOD').AsString+sLineBreak+
                                                               'IMSI: '+DeviceRegData.Sim.IMSI);
      end;  }
    end;
  finally
   CloseDataSet;
  end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/CheckSimStatus');
     end;
    ErrCode := E.ErrorCode;
    ErrMsg  := E.Message;
    Result  := false;
   end;
  on E: Exception do
   begin
    ErrCode := errcode_ExecuteFail_Internal;
    ErrMsg  := 'Unhandled exception!'+sLineBreak+
               '['+Self.ClassName+']'+sLineBreak+
               E.Message;
    Result  := false;
    Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
   end;
 end;
end;

function TCmd_FiscDataServer.FGetTestModeText: String;
begin
 if FTestMode then Result := '[ТЕСТОВ СЪРВЪР]'
  else Result := '';
end;

function TCmd_FiscDataServer.FGetDeviceType: String;
begin
 Result := '';
 if FDevRegData.FiscDev <> nil then Result := C_DeviceType_FD;
end;

function TCmd_FiscDataServer.FGetDeviceSerial: String;
begin
 Result := '';
 if FDevRegData.FiscDev <> nil then Result := FDevRegData.FiscDev.Serial;
end;

//******************************************************************************
//           TCmd_FiscValidateClient
//******************************************************************************
constructor TCmd_FiscValidateClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
end;

destructor TCmd_FiscValidateClient.Destroy;
begin
 inherited Destroy;
end;

function TCmd_FiscValidateClient.GetCommandName: String;
begin
 Result := cmd_FiscValidate;
end;

function TCmd_FiscValidateClient.AddRequestToDocument: Boolean;
begin
 Result := inherited AddRequestToDocument;
end;

function TCmd_FiscValidateClient.GetAnswerFromDocument: Boolean;
begin
 Result := inherited GetAnswerFromDocument;
end;

//******************************************************************************
//           TCmd_FiscValidateServer
//******************************************************************************
constructor TCmd_FiscValidateServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_FiscValidateServer.Destroy;
begin
 inherited Destroy;
end;

function TCmd_FiscValidateServer.GetRequestFromDocument: Boolean;
begin
 Result := inherited GetRequestFromDocument;
end;

function TCmd_FiscValidateServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
begin
 Result := true;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    if (not FCheckInputData(FReqType, ErrCode, UserError))or
       (not FCheckDelaerStatus(ErrCode, UserError))or
       (not FCheckModemStatus(ErrCode, UserError))or    // първо се прави проверка на версията на модема а после тази на модела на фу
       (not FCheckFDStatus(ErrCode, UserError))or
       (not FCheckSimStatus(ErrCode, UserError)) then
     raise EHandledException.Create(ErrCode, UserError);


   end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_FiscValidateServer.AddAnswerToDocument: Boolean;
begin
 Result := inherited AddAnswerToDocument;
end;

//******************************************************************************
//           TCmd_FiscRequestClient
//******************************************************************************
constructor TCmd_FiscRequestClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
end;

destructor TCmd_FiscRequestClient.Destroy;
begin
 inherited Destroy;
end;

function TCmd_FiscRequestClient.GetCommandName: String;
begin
 Result := cmd_FiscRequest;
end;

function TCmd_FiscRequestClient.AddRequestToDocument: Boolean;
begin
 Result := inherited AddRequestToDocument;
end;

function TCmd_FiscRequestClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
begin
 Result := inherited GetAnswerFromDocument;
 if not Result then Exit;

 try
  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_FiscData);
  FRequestID := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FiscReqID), 0);

  if FRequestID = 0 then raise EAbort.Create('Invalid FiscalizationRequestID');

 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//           TCmd_FiscRequestServer
//******************************************************************************
constructor TCmd_FiscRequestServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_FiscRequestServer.Destroy;
begin
 inherited Destroy;
end;

function TCmd_FiscRequestServer.GetRequestFromDocument: Boolean;
begin
 Result := inherited GetRequestFromDocument;
end;

function TCmd_FiscRequestServer.FRecordExist(Table: String; Field, Value: array of String): Boolean;
var SQL : String;
    I   : Integer;
begin
 Result := false;
 Assert(Length(Field) = Length(Value));
 with Device.DbInterface do
 try
  SQL := 'SELECT COUNT(*) "RCNT" FROM '+Table+' WHERE ';
  for I := 0 to Length(Field) - 1 do
   begin
    if I > 0 then SQL := SQL + 'AND';
    SQL := SQL + '('+Field[I]+' = '+QuotedStr(Value[I])+')'
   end;
  if not FillDataSet(SQL) then raise EAbort.Create('Check for record existance fail. Table: '+Table);
  Result := (DataSet.FieldByName('RCNT').AsInteger > 0);
 finally
  CloseDataSet;
 end;
end;

procedure TCmd_FiscRequestServer.FUpdateCustomer;
var SQL : String;
    RA  : Integer;

    procedure AddStrDataToSQL(Field, Value: String; MaxLen: Integer);
    begin
     if Value <> '' then SQL := SQL + ',' + Field + ' = ' + Device.DbInterface.StrToSQL(Value, MaxLen);
    end;
    procedure AddIntDataToSQL(Field: String; Value: Integer);
    begin
     if Value <> 0 then SQL := SQL + ',' + Field + ' = ' + Device.DbInterface.IntToSql(Value);
    end;
begin
  with Device.DbInterface do
   begin
    if FRecordExist('CUSTOMERS', ['CUST_EIK'], [DeviceRegData.Owner.EIK]) then
     begin
      SQL := 'UPDATE CUSTOMERS SET '+
             'CUST_EIKTYPE = '+IntToSql(DeviceRegData.Owner.EIKType);
      AddStrDataToSQL('CUST_VATNUMBER',   DeviceRegData.Owner.TaxEik, 20);   //[CUST_VATNUMBER] varchar(20) NULL,
      AddStrDataToSQL('CUST_NAME',        DeviceRegData.Owner.Name, 200);    //[CUST_NAME] varchar(200) NULL,
      AddStrDataToSQL('CUST_NAME2',       DeviceRegData.Owner.Comment, 500); //[CUST_NAME2] varchar(500) NULL,
      AddStrDataToSQL('CUST_TOWN',        DeviceRegData.Owner.TownName, 100);//[CUST_TOWN] varchar(100) NULL,
      AddIntDataToSQL('CUST_TOWNCODE',    DeviceRegData.Owner.TownCode);     //[CUST_TOWNCODE] int NULL,
      AddStrDataToSQL('CUST_ADDRESS',     DeviceRegData.Owner.Address, 500); //[CUST_ADDRESS] varchar(500) NULL,
      AddStrDataToSQL('CUST_SHORTADDRESS',DeviceRegData.Owner.ShortAdr, 50); //[CUST_SHORTADDRESS] varchar(50) NULL,
      AddStrDataToSQL('CUST_MOL',         DeviceRegData.Owner.MOL, 100);     //[CUST_MOL] varchar(100) NULL,
      AddStrDataToSQL('CUST_PHONE_G1',    DeviceRegData.Owner.PhoneG1, 20);  //[CUST_PHONE_G1] varchar(20) NULL,
      AddStrDataToSQL('CUST_PHONE_G2',    DeviceRegData.Owner.PhoneG2, 20);  //[CUST_PHONE_G2] varchar(20) NULL,
      AddStrDataToSQL('CUST_PHONE_M1',    DeviceRegData.Owner.PhoneM1, 20);  //[CUST_PHONE_M1] varchar(20) NULL,
      AddStrDataToSQL('CUST_PHONE_M2',    DeviceRegData.Owner.PhoneM2, 20);  //[CUST_PHONE_M2] varchar(20) NULL,
      AddStrDataToSQL('CUST_CONTACTMAIN', DeviceRegData.Owner.Contact, 100); //[CUST_CONTACTMAIN] varchar(100) NULL,
      AddStrDataToSQL('CUST_EMAIL',       DeviceRegData.Owner.Email, 100);   //[CUST_EMAIL] varchar(100) NULL,
      AddStrDataToSQL('CUST_RESPDEALER',  DeviceRegData.Dealer.EIK, 13);     //[CUST_RESPDEALER] varchar(13) NULL,
      SQL := SQL + 'WHERE CUST_EIK = '+StrToSQL(DeviceRegData.Owner.EIK);
     end
    else
     begin
      SQL := 'INSERT INTO CUSTOMERS (CUST_EIK, CUST_EIKTYPE, CUST_VATNUMBER, CUST_NAME, '+
             'CUST_NAME2, CUST_TOWN, CUST_TOWNCODE, CUST_ADDRESS, CUST_SHORTADDRESS, CUST_MOL, '+
             'CUST_PHONE_G1, CUST_PHONE_G2, CUST_PHONE_M1, CUST_PHONE_M2, CUST_CONTACTMAIN, '+
             'CUST_EMAIL, CUST_RESPDEALER) VALUES('+
             StrToSQL(DeviceRegData.Owner.EIK)                 +','+ //[CUST_EIK] varchar(13)
             IntToSql(DeviceRegData.Owner.EIKType)             +','+ //[CUST_EIKTYPE] int NOT NULL,
             StrToSQL(DeviceRegData.Owner.TaxEik,    20, true) +','+ //[CUST_VATNUMBER] varchar(20)
             StrToSQL(DeviceRegData.Owner.Name,     200, true) +','+ //[CUST_NAME] varchar(200)
             StrToSQL(DeviceRegData.Owner.Comment,  500, true) +','+ //[CUST_NAME2] varchar(500)
             StrToSQL(DeviceRegData.Owner.TownName, 100, true) +','+ //[CUST_TOWN] varchar(100)
             IntToSql(DeviceRegData.Owner.TownCode)            +','+ //[CUST_TOWNCODE] int NULL
             StrToSQL(DeviceRegData.Owner.Address,  500, true) +','+ //[CUST_ADDRESS] varchar(500)
             StrToSQL(DeviceRegData.Owner.ShortAdr,  50, true) +','+ //[CUST_SHORTADDRESS] varchar(50)
             StrToSQL(DeviceRegData.Owner.MOL,      100, true) +','+ //[CUST_MOL] varchar(100)
             StrToSQL(DeviceRegData.Owner.PhoneG1,   20, true) +','+ //[CUST_PHONE_G1] varchar(20)
             StrToSQL(DeviceRegData.Owner.PhoneG2,   20, true) +','+ //[CUST_PHONE_G2] varchar(20)
             StrToSQL(DeviceRegData.Owner.PhoneM1,   20, true) +','+ //[CUST_PHONE_M1] varchar(20)
             StrToSQL(DeviceRegData.Owner.PhoneM2,   20, true) +','+ //[CUST_PHONE_M2] varchar(20)
             StrToSQL(DeviceRegData.Owner.Contact,  100, true) +','+ //[CUST_CONTACTMAIN] varchar(100)
             StrToSQL(DeviceRegData.Owner.Email,    100, true) +','+ //[CUST_EMAIL] varchar(100)
             StrToSQL(DeviceRegData.Dealer.EIK)                +')'; //[CUST_RESPDEALER] varchar(13)
     end;
    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Upd/Ins Customer: '+Device.DbInterface.LastError);
   end;
end;

procedure TCmd_FiscRequestServer.FUpdateSite(var SiteID: Integer);
var SQL : String;

    procedure AddStrToSql(FldName, FldValue: String);
    begin
     if FldValue = '' then SQL := SQL + 'AND('+FldName+' IS NULL)'
      else SQL := SQL + 'AND('+FldName+' = '+Device.DbInterface.StrToSQL(FldValue)+')';
    end;
    procedure AddIntToSql(FldName: String; FldValue: Integer);
    begin
     if FldValue = 0 then SQL := SQL + 'AND('+FldName+' IS NULL)'
      else SQL := SQL + 'AND('+FldName+' = '+IntToStr(FldValue)+')';
    end;
begin
 SiteID := 0;
 with Device.DbInterface do
 try
  // Process Customer's site
  SQL := 'SELECT CUSTS_ID FROM CUSTOMER_SITES WHERE '+
         '(CUSTS_CUSTEIK = '+    StrToSQL(DeviceRegData.Owner.EIK)                   +')AND'+
         '(CUSTS_NUMBER = '+     StrToSQL(DeviceRegData.Site.Number)                 +')AND'+
         '(CUSTS_TYPE = '+       IntToSql(StrToInt(DeviceRegData.Site.SiteType.Code))+')AND'+
         '(CUSTS_NAME = '+       StrToSQL(DeviceRegData.Site.SiteName)               +')';
  AddStrToSql('CUSTS_TOWNNAME',   DeviceRegData.Site.Address.Town.Name);
  AddIntToSql('CUSTS_TOWNCODE',   StrToIntDef(DeviceRegData.Site.Address.Town.Code, 0));
  AddStrToSql('CUSTS_AREANAME',   DeviceRegData.Site.Address.Area.Name);
  AddStrToSql('CUSTS_AREACODE',   DeviceRegData.Site.Address.Area.Code);
  AddStrToSql('CUSTS_STREETNAME', DeviceRegData.Site.Address.Street.Name);
  AddIntToSql('CUSTS_STREETCODE', StrToIntDef(DeviceRegData.Site.Address.Street.Code, 0));
  AddStrToSql('CUSTS_STREETNO',   DeviceRegData.Site.Address.StrNo);
  AddStrToSql('CUSTS_BLOCK',      DeviceRegData.Site.Address.Block);
  AddStrToSql('CUSTS_ENTRANCE',   DeviceRegData.Site.Address.Entr);
  AddStrToSql('CUSTS_FLOOR',      DeviceRegData.Site.Address.Floor);
  AddStrToSql('CUSTS_APARTMENT',  DeviceRegData.Site.Address.App);

  if not FillDataSet(SQL) then raise EAbort.Create('Check CUSTOMER_SITES: '+Device.DbInterface.LastError);
  SiteID := DataSet.FieldByName('CUSTS_ID').AsInteger;
  CloseDataSet;

  if SiteID <= 0 then
   begin
    SQL := 'INSERT INTO CUSTOMER_SITES(CUSTS_CUSTEIK, CUSTS_NUMBER, CUSTS_TYPE, '+
           'CUSTS_NAME, CUSTS_TOWNNAME, CUSTS_TOWNCODE, CUSTS_TOWNPREF, CUSTS_AREANAME, '+
           'CUSTS_AREACODE, CUSTS_AREAPREF, CUSTS_STREETNAME, CUSTS_STREETCODE, '+
           'CUSTS_STREETPREF, CUSTS_STREETNO, CUSTS_BLOCK, CUSTS_ENTRANCE, '+
           'CUSTS_FLOOR, CUSTS_APARTMENT, CUSTS_SHORTADDRESS) VALUES ('+
           StrToSQL(DeviceRegData.Owner.EIK)                           + ','+ // [CUSTS_CUSTEIK] varchar(13) NOT NULL,
           StrToSQL(DeviceRegData.Site.Number,               10, true) + ','+ // [CUSTS_NUMBER] varchar(10) NOT NULL,
           IntToSql(StrToInt(DeviceRegData.Site.SiteType.Code))        + ','+ // [CUSTS_TYPE] int NOT NULL,
           StrToSQL(DeviceRegData.Site.SiteName,            100, true) + ','+ // [CUSTS_NAME] varchar(100) NOT NULL,
           StrToSQL(DeviceRegData.Site.Address.Town.Name,   100, true) + ','+ // [CUSTS_TOWNNAME] varchar(100) NULL,
           IntToSql(StrToInt(DeviceRegData.Site.Address.Town.Code), true)  + ','+ // [CUSTS_TOWNCODE] int NULL,
           StrToSQL(DeviceRegData.Site.Address.Town.Pref,    10, true) + ','+ // [CUSTS_TOWNPREF] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.Address.Area.Name,   100, true) + ','+ // [CUSTS_AREANAME] varchar(100) NULL,
           StrToSQL(DeviceRegData.Site.Address.Area.Code,    20, true) + ','+ // [CUSTS_AREACODE] varchar(20) NULL,
           StrToSQL(DeviceRegData.Site.Address.Area.Pref,    10, true) + ','+ // [CUSTS_AREAPREF] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.Address.Street.Name, 200, true) + ','+ // [CUSTS_STREETNAME] varchar(200) NULL,
           IntToSql(StrToInt(DeviceRegData.Site.Address.Street.Code), true) + ','+ // [CUSTS_STREETCODE] int NULL,
           StrToSQL(DeviceRegData.Site.Address.Street.Pref,  20, true) + ','+ // [CUSTS_STREETPREF] varchar(20) NULL,
           StrToSQL(DeviceRegData.Site.Address.StrNo,        10, true) + ','+ // [CUSTS_STREETNO] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.Address.Block,        10, true) + ','+ // [CUSTS_BLOCK] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.Address.Entr,         10, true) + ','+ // [CUSTS_ENTRANCE] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.Address.Floor,        10, true) + ','+ // [CUSTS_FLOOR] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.Address.App,          10, true) + ','+ // [CUSTS_APARTMENT] varchar(10) NULL,
           StrToSQL(DeviceRegData.Site.ShortAddr,            50, true) + ');'+// [CUSTS_SHORTADDRESS] varchar(50) NULL
           'select SCOPE_IDENTITY() "CUSTS_ID";';

    if not FillDataSet(SQL) then raise EAbort.Create('Insert CUSTOMER_SITES: '+Device.DbInterface.LastError);
    SiteID := DataSet.FieldByName('CUSTS_ID').AsInteger;
   end;
  if SiteID <= 0 then raise EAbort.Create('Invalid site ID');
 finally
  CloseDataSet;
 end;
end;

procedure TCmd_FiscRequestServer.FUpdateDeviceGPRS(SiteID: Integer);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
   if FRecordExist('DEVICES_GPRS', ['DC_SERIAL'], [DeviceRegData.Modem.Serial]) then
    begin
     SQL := 'UPDATE DEVICES_GPRS SET '+
            'DC_FUDEVICE = '+ StrToSQL(DeviceRegData.FiscDev.Serial, 10) +', '+ // [DC_FUDEVICE] varchar(10) NULL,
            'DC_CUSTEIK = '+  StrToSQL(DeviceRegData.Owner.EIK, 13)      +', '+ // [DC_CUSTEIK] varchar(13) CNULL,
            'DC_CUSTSITE = '+ IntToSql(SiteID)                           +', '+ // [DC_CUSTSITE] int NULL,
            'DC_SIMIMSI = '+  StrToSQL(DeviceRegData.Sim.IMSI, 32)       +', '+ // [DC_SIMIMSI] varchar(32) NULL,
            'DC_MODEL = '+    StrToSQL(IntToStr(DeviceRegData.Modem.ModelId), 20)    +', '+ // [DC_MODEL] varchar(20) NULL,
            'DC_VERSION = '+  StrToSQL(DeviceRegData.Modem.Version, 20)  +', '+ // [DC_VERSION] varchar(20) NULL,
            'DC_DEALEREIK = '+StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) +', '+ // [DC_DEALEREIK] varchar(13) NOT NULL,
            'DC_DEALERBRANCH = '+IntToSQL(ESKLoginData.DealerData.BranchID)    +', '+ // [DC_DEALERBRANCH] int NOT NULL,
            'DC_TESTMODE = ' + BoolToSql(Device.TestMode)                      +' ' + // [DC_TESTMODE] int NULL,
            'WHERE DC_SERIAL = '+ StrToSQL(DeviceRegData.Modem.Serial);
    end
   else
    begin
     SQL := 'INSERT INTO DEVICES_GPRS (DC_SERIAL, DC_FUDEVICE, DC_CUSTEIK, DC_CUSTSITE, '+
            'DC_SIMIMSI, DC_MODEL, DC_VERSION, DC_DEALEREIK, DC_DEALERBRANCH, DC_TESTMODE) VALUES ('+
            StrToSQL(DeviceRegData.Modem.Serial, 10)        +', '+ // [DC_SERIAL] varchar(10) NOT NULL,
            StrToSQL(DeviceRegData.FiscDev.Serial, 10)      +', '+ // [DC_FUDEVICE] varchar(10) NULL,
            StrToSQL(DeviceRegData.Owner.EIK, 13)           +', '+ // [DC_CUSTEIK] varchar(13) CNULL,
            IntToSQL(SiteID)                                +', '+ // [DC_CUSTSITE] int NULL,
            StrToSQL(DeviceRegData.Sim.IMSI, 32)            +', '+ // [DC_SIMIMSI] varchar(32) NULL,
            StrToSQL(IntToStr(DeviceRegData.Modem.ModelId), 20)         +', '+ // [DC_MODEL] varchar(20) NULL,
            StrToSQL(DeviceRegData.Modem.Version, 20)       +', '+ // [DC_VERSION] varchar(20) NULL,
            StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13)+', '+ // [DC_DEALEREIK] varchar(13) NOT NULL,
            IntToSQL(ESKLoginData.DealerData.BranchID)      +', '+ // [DC_DEALERBRANCH] int NOT NULL,
            BoolToSql(Device.TestMode)                      +') '; // [DC_TESTMODE] int NULL,
    end;
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Upd/Ins DEVICES_GPRS: '+Device.DbInterface.LastError);

   // remove duplicate SIM
   SQL := 'UPDATE DEVICES_GPRS SET '+
          'DC_SIMIMSI = '+ StrToSQL('000000000000000', 32) +' '+ // [DC_SIMIMSI] varchar(32) NULL,
          'WHERE (DC_SIMIMSI = '+StrToSQL(DeviceRegData.Sim.IMSI, 32)+')'+
          'AND(DC_SERIAL <> '+StrToSQL(DeviceRegData.Modem.Serial, 10)+')';
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_GPRS.DC_SIMIMSI Fail: '+Device.DbInterface.LastError);

   SQL := 'UPDATE DEVICES_GPRS SET '+
          'DC_FUDEVICE = '+ StrToSQL('0000000000', 10) +' '+ // [DC_FUDEVICE] varchar(10) NULL,
          'WHERE (DC_FUDEVICE = '+StrToSQL(DeviceRegData.FiscDev.Serial, 10)+')'+
          'AND(DC_SERIAL <> '+StrToSQL(DeviceRegData.Modem.Serial, 10)+')';
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_GPRS.DC_FUDEVICE Fail: '+Device.DbInterface.LastError);
  end;
end;

procedure TCmd_FiscRequestServer.FUpdateDeviceFU(SiteID: Integer);
var SQL : String;
    RA  : Integer;

    procedure AddStrDataToSQL(Field, Value: String; MaxLen: Integer);
    begin
     if Value <> '' then SQL := SQL + ',' + Field + ' = ' + Device.DbInterface.StrToSQL(Value, MaxLen);
    end;
    procedure AddDateDataToSQL(Field: String; Value: TDateTime);
    begin
     if Value <> 0 then SQL := SQL + ',' + Field + ' = ' + Device.DbInterface.DateToSQL(Value);
    end;
begin
 with Device.DbInterface do
  begin
   // update data for fiscal device
   if FRecordExist('DEVICES_FU', ['DF_SERIAL'], [DeviceRegData.FiscDev.Serial]) then
    begin
     SQL := 'UPDATE DEVICES_FU SET '+
            'DF_DEVICETYPE = '+   IntToSQL(DeviceRegData.FiscDev.NRAType)         +', '+ // [DF_DEVICETYPE] int DEFAULT 0 NULL,
            'DF_MFM = '+          StrToSQL(DeviceRegData.FiscDev.MFM, 10)         +', '+ // [DF_MFM] varchar(10) NOT NULL,
            'DF_MODEL = '+        IntToSQL(DeviceRegData.FiscDev.ModelId)         +', '+ // [DF_MODEL] int NULL,
            'DF_GPRSDEVICE = '+   StrToSQL(DeviceRegData.Modem.Serial, 10)        +', '+ // [DF_GPRSDEVICE] varchar(10) NULL,
            'DF_DEALEREIK = '+    StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13)+', '+ // [DF_DEALEREIK] varchar(13) NOT NULL,
            'DF_DEALERBRANCH = '+ IntToSQL(ESKLoginData.DealerData.BranchID)      +', '+ // [DF_DEALERBRANCH] int NOT NULL,
            'DF_CUSTEIK = '+      StrToSQL(DeviceRegData.Owner.EIK, 13)           +', '+ // [DF_CUSTEIK] varchar(13) NULL,
            'DF_CUSTSITE = '+     IntToSql(SiteID)                                +', '+ // [DF_CUSTSITE] int NULL,
            'DF_SIMIMSI = '+      StrToSQL(DeviceRegData.Sim.IMSI, 32)            +', '+ // [DF_SIMIMSI] varchar(32) NULL,
            'DF_TESTMODE = '+     BoolToSql(Device.TestMode)                      +' ';  // [DF_TESTMODE] int NULL,

     AddStrDataToSQL ('DF_LSRVCONTRNUMB',     DeviceRegData.Services.SvcContrNo, 10);    // [DF_LSRVCONTRNUMB] varchar(10) NULL,
     AddDateDataToSQL('DF_LSRVCONTRDATEFROM', DeviceRegData.Services.SvcContrFrom);      // [DF_LSRVCONTRDATE] datetime NULL,
     AddDateDataToSQL('DF_LSRVCONTRDATETO',   DeviceRegData.Services.SvcContrTo);        // [DF_LSRVCONTRDATE] datetime NULL,
     AddStrDataToSQL ('DF_COMMENT',           DeviceRegData.FiscDevEx.Comment, 0);       // [DF_COMMENT] varchar(max) NULL,

     SQL := SQL + ' WHERE DF_SERIAL = '+StrToSQL(DeviceRegData.FiscDev.Serial);
     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU Fail: '+Device.DbInterface.LastError);
    end
   else
    begin
     SQL := 'INSERT INTO DEVICES_FU (DF_SERIAL, DF_DEVICETYPE, DF_MFM, DF_MODEL, '+
            'DF_GPRSDEVICE, DF_LSRVCONTRNUMB, DF_LSRVCONTRDATEFROM, DF_LSRVCONTRDATETO, '+
            'DF_DEALEREIK, DF_DEALERBRANCH, DF_STATUS, DF_CUSTEIK, DF_CUSTSITE, '+
            'DF_SIMIMSI, DF_TESTMODE, DF_COMMENT) VALUES ('+
            StrToSQL(DeviceRegData.FiscDev.Serial, 10)       +', '+ // [DF_SERIAL] varchar(10) NOT NULL,
            IntToSQL(DeviceRegData.FiscDev.NRAType)          +', '+ // [DF_DEVICETYPE] int NOT NULL,
            StrToSQL(DeviceRegData.FiscDev.MFM, 10)          +', '+ // [DF_MFM] varchar(10) NOT NULL,
            IntToSQL(DeviceRegData.FiscDev.ModelId)          +', '+ // [DF_MODEL] int NOT NULL,
            StrToSQL(DeviceRegData.Modem.Serial, 10)         +', '+ // [DF_GPRSDEVICE] varchar(10) NOT NULL,
            StrToSQL(DeviceRegData.Services.SvcContrNo, 10)  +', '+ // [DF_LSRVCONTRNUMB] varchar(10) NULL,
            DateToSQL(DeviceRegData.Services.SvcContrFrom)   +', '+ // [DF_LSRVCONTRDATEFROM] datetime NULL,
            DateToSQL(DeviceRegData.Services.SvcContrTo)     +', '+ // [DF_LSRVCONTRDATETO] datetime NULL,
            StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) +', '+ // [DF_DEALEREIK] varchar(13) NOT NULL,
            IntToSQL(ESKLoginData.DealerData.BranchID)       +', '+ // [DF_DEALERBRANCH] int NOT NULL,
            IntToSQL(0 {нова каса})                          +', '+ // [DF_STATUS] int NOT NULL,
            StrToSQL(DeviceRegData.Owner.EIK, 13)            +', '+ // [DF_CUSTEIK] varchar(13) NOT NULL,
            IntToSQL(SiteID)                                 +', '+ // [DF_CUSTSITE] int NOT NULL,
            StrToSQL(DeviceRegData.Sim.IMSI, 32)             +', '+ // [DF_SIMIMSI] varchar(32) NOT NULL,
            BoolToSql(Device.TestMode)                       +', '+ // [DF_TESTMODE] int NULL,
            StrToSQL(DeviceRegData.FiscDevEx.Comment)        +') '; // [DF_COMMENT] varchar(max) NULL,

     if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEVICES_FU Fail: '+Device.DbInterface.LastError);
     FInsertDeviceAction(SiteID, fdevact_InitialInsert,
                         'Заявка за фискализация на ново устройство', DeviceRegData.DeviceInfo);
    end;

   // remove duplicate SIM
   SQL := 'UPDATE DEVICES_FU SET '+
          'DF_SIMIMSI = '+ StrToSQL('000000000000000', 32) +' '+ // [DF_SIMIMSI] varchar(32) NULL,
          'WHERE (DF_SIMIMSI = '+StrToSQL(DeviceRegData.Sim.IMSI, 32)+')'+
          'AND(DF_SERIAL <> '+StrToSQL(DeviceRegData.FiscDev.Serial, 10)+')';
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU.DF_SIMIMSI Fail: '+Device.DbInterface.LastError);
   if RA > 0 then
    begin
     FInsertDeviceAction(SiteID, fdevact_MoveSimFromDevice, 'СИМ картата е взета от друго устройство', '');
     PostEventUser(0, C_EvType_SIM, 'D', ESKLoginData.EskSerial, C_DeviceType_FD, DeviceRegData.FiscDev.Serial,
                   'СИМ картата е взета от друго устройство'+sLineBreak+
                   'IMSI: '+DeviceRegData.Sim.IMSI);
    end;

   // remove duplicate GPRS device
   SQL := 'UPDATE DEVICES_FU SET '+
          'DF_GPRSDEVICE = '+ StrToSQL('0000000000', 10) +' '+ // [DF_GPRSDEVICE] varchar(10) NULL,
          'WHERE (DF_GPRSDEVICE = '+StrToSQL(DeviceRegData.Modem.Serial, 10)+')'+
          'AND(DF_SERIAL <> '+StrToSQL(DeviceRegData.FiscDev.Serial, 10)+')';
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU.DF_GPRSDEVICE Fail: '+Device.DbInterface.LastError);
   if RA > 0 then
    begin
     FInsertDeviceAction(SiteID, fdevact_MoveSimFromDevice, 'Модема е взет от друго устройство', '');
    end;
  end;
end;

{procedure TCmd_FiscRequestServer.FUpdatePairHistory(SiteID: Integer);
var SQL : String;
    ID  : Integer;
begin
 with Device.DbInterface do
 try
  SQL := 'SELECT HP_ID FROM HISTORYPAIR WHERE '+
         '(HP_FISCALDEVICE = '+ StrToSQL(DeviceRegData.FiscDev.Serial)+')AND'+
         '(HP_GPRSDEVICE = '+ StrToSQL(DeviceRegData.Modem.Serial)+')AND'+
         '(HP_SIMIMSI = '+ StrToSQL(DeviceRegData.Sim.IMSI)+')AND'+
         '(HP_CUSTEIK = '+ StrToSQL(DeviceRegData.Owner.EIK)+')AND'+
         '(HP_CUSTSITE = '+ IntToSql(SiteID)+')AND'+
         '(HP_DEALEREIK = '+ StrToSQL(ESKLoginData.DealerData.CompanyEIK)+')AND'+
         '(HP_DEALERBRANCH = '+ IntToSql(ESKLoginData.DealerData.BranchID)+')';
  if not FillDataSet(SQL) then raise EAbort.Create('Check HISTORYPAIR: '+Device.DbInterface.LastError);
  ID := DataSet.FieldByName('HP_ID').AsInteger;
  CloseDataSet;

  if ID <= 0 then
   begin
    SQL := 'INSERT INTO HISTORYPAIR (HP_FISCALDEVICE, HP_GPRSDEVICE, HP_SIMIMSI, '+
           'HP_CUSTEIK, HP_CUSTSITE, HP_DEALEREIK, HP_DEALERBRANCH) VALUES ('+
           StrToSQL(DeviceRegData.FiscDev.Serial, 10)       + ', '+ // [HP_FISCALDEVICE] varchar(10) NOT NULL,
           StrToSQL(DeviceRegData.Modem.Serial, 10)         + ', '+ // [HP_GPRSDEVICE] varchar(10) NOT NULL,
           StrToSQL(DeviceRegData.Sim.IMSI, 32)             + ', '+ // [HP_SIMIMSI] varchar(32) NOT NULL,
           StrToSQL(DeviceRegData.Owner.EIK, 13)            + ', '+ // [HP_CUSTEIK] varchar(13) NULL,
           IntToSQL(SiteID)                                 + ', '+ // [HP_CUSTSITE] int NULL,
           StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) + ', '+ // [HP_DEALEREIK] varchar(13) NULL,
           IntToSQL(ESKLoginData.DealerData.BranchID)       + ') '; // [HP_DEALERBRANCH] int NULL,
    if not ExecuteSQLStatement(SQL, ID) then raise EAbort.Create('Insert HISTORYPAIR: '+Device.DbInterface.LastError);
   end;
 finally
  CloseDataSet;
 end;
end;}

procedure TCmd_FiscRequestServer.FInsertDeviceAction(SiteID: Integer; ActType: Integer; ActComment, ActData: String);
var SQL : String;
    RA  : Integer;
begin
 with Device.DbInterface do
  begin
   SQL := 'INSERT INTO DEVICES_FU_ACTIONS (DFA_DEVFUSERIAL, DFA_DEVGPRSSERIAL, '+
          'DFA_SIMIMSI, DFA_ACTIONTYPE, DFA_COMMENT, DFA_DEALEREIK, DFA_DEALERUSER, DFA_CUSTEIK, '+
          'DFA_DEALERBRANCH, DFA_CUSTSITE, DFA_DATA) VALUES ('+
           StrToSQL(DeviceRegData.FiscDev.Serial, 10)       + ', '+ // [DFA_DEVFUSERIAL] varchar(10) NOT NULL,
           StrToSQL(DeviceRegData.Modem.Serial, 10)         + ', '+ // [DFA_DEVGPRSSERIAL] varchar(10) NOT NULL,
           StrToSQL(DeviceRegData.Sim.IMSI, 32)             + ', '+ // [DFA_SIMIMSI] varchar(32) NULL,
           IntToSql(ActType)                                + ', '+ // [DFA_ACTIONTYPE] int NULL,
           StrToSQL(ActComment, 100, true)                  + ', '+ // [DFA_COMMENT] varchar(100) NULL,
           StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) + ', '+ // [DFA_DEALEREIK] varchar(13) NULL,
           StrToSQL(ESKLoginData.EskSerial, 10)             + ', '+ // [DFA_DEALERUSER] varchar(10) NULL,
           StrToSQL(DeviceRegData.Owner.EIK, 13)            + ', '+ // [DFA_CUSTEIK] varchar(13) NULL,
           IntToSql(ESKLoginData.DealerData.BranchID)       + ', '+ // [DFA_DEALERBRANCH] int NULL,
           IntToSql(SiteID)                                 + ', '+ // [DFA_CUSTSITE] int NULL,
           StrToSQL(ActData, 0, true)                       + ') '; // [DFA_DATA] varchar(max) NULL,
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEVICES_FU_ACTIONS: '+Device.DbInterface.LastError);
  end;
end;

procedure TCmd_FiscRequestServer.FUpdateSIMData(SiteID: Integer);
var SQL        : String;
    RA         : Integer;
begin
 with Device.DbInterface do
 try
  if not DeviceRegData.Sim.IsExternal then
   begin
    // промяна на основните данни за картата
    SQL := 'UPDATE SIM SET '+
           'SIM_CUSTEIK = '+      StrToSQL(DeviceRegData.Owner.EIK, 13, true)      +', '+ // [SIM_CUSTEIK] varchar(13) NOT NULL,
           'SIM_CUSTSITEID = '+   IntToSql(SiteID, true)                           +', '+ // [SIM_CUSTSITEID] int NULL,
           'SIM_FISCALDEVICE = '+ StrToSQL(DeviceRegData.FiscDev.Serial, 10, true) +', '+ // [SIM_FISCALDEVICE] varchar(10) NULL,
           'SIM_GPRSDEVICE = '+   StrToSQL(DeviceRegData.Modem.Serial, 10, true)   +', '+ // [SIM_GPRSDEVICE] varchar(10) NULL,
           'SIM_TESTMODE = '+     BoolToSql(Device.TestMode)                       +'  '+ // [SIM_TESTMODE] int NULL,
           'WHERE SIM_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Chage SIM customer fail:'+Device.DbInterface.LastError);

   // remove duplicate FU
   SQL := 'UPDATE SIM SET '+
          'SIM_FISCALDEVICE = '+ StrToSQL('0000000000', 10) +' '+ // [SIM_FISCALDEVICE] varchar(10) NULL,
          'WHERE (SIM_FISCALDEVICE = '+StrToSQL(DeviceRegData.FiscDev.Serial, 10, true)+')'+
          'AND(SIM_IMSI <> '+StrToSQL(DeviceRegData.Sim.IMSI)+')';
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM.SIM_FISCALDEVICE Fail: '+Device.DbInterface.LastError);

   // remove duplicate Modem
   SQL := 'UPDATE SIM SET '+
          'SIM_GPRSDEVICE = '+ StrToSQL('0000000000', 10) +' '+ // [SIM_GPRSDEVICE] varchar(10) NULL,
          'WHERE (SIM_GPRSDEVICE = '+StrToSQL(DeviceRegData.Modem.Serial, 10, true)+')'+
          'AND(SIM_IMSI <> '+StrToSQL(DeviceRegData.Sim.IMSI)+')';
   if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM.SIM_FISCALDEVICE Fail: '+Device.DbInterface.LastError);


{    // регистрация на картата в НАП
    case OperType of
    1: // Регистрация на ФУ/ИАСУТД;
       begin
        FRegisterSimToNRA(simsActivate, SiteID);     // вътрешно изпълнява FInsertDeviceAction
        // регистриране на плащане
        FInsertSIMPayment(SiteID);

        case OperStatus of
        0: begin // - нова карта
            SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
                   StrToSQL(DeviceRegData.Sim.IMSI, 32) +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
                   IntToSql(simoperact_ActivateNewSim)  +','+ //  [SOA_TYPE] int NOT NULL,
                   StrToSQL('Активиран нов СИМ при регистрация ФУ: '+DeviceRegData.FiscDev.Serial, 100) +')'; //  [SOA_DATA] varchar(100) NULL,
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
           end;
        1: begin // - активирана
           end;
        2: begin // - спряна временно
            // активране на СИМ в сървъра на мобилния оператор
            SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
                   StrToSQL(DeviceRegData.Sim.IMSI, 32)     +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
                   IntToSql(simoperact_ActivateStoppedSim)  +','+ //  [SOA_TYPE] int NOT NULL,
                   StrToSQL('Реактивиран СИМ при регистрация ФУ: '+DeviceRegData.FiscDev.Serial, 100) +')'; //  [SOA_DATA] varchar(100) NULL,
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
           end;
        3: begin // - спряна за вечни времена
            raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е блокирана от мобилния оператор!');
           end;
        end;
       end;
    2: // Промяна на обстоятелства;
       begin
        FRegisterSimToNRA(simsActivate, SiteID);     // вътрешно изпълнява FInsertDeviceAction
        // регистриране на плащане
        FInsertSIMPayment(SiteID);

        case OperStatus of
        0: begin // - нова карта
            SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
                   StrToSQL(DeviceRegData.Sim.IMSI, 32) +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
                   IntToSql(simoperact_ActivateNewSim)  +','+ //  [SOA_TYPE] int NOT NULL,
                   StrToSQL('Активиран нов СИМ при промяна обстоятелства ФУ: '+DeviceRegData.FiscDev.Serial, 100) +')'; //  [SOA_DATA] varchar(100) NULL,
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
           end;
        1: begin // - активирана
           end;
        2: begin // - спряна временно
            // активране на СИМ в сървъра на мобилния оператор
            SQL := 'INSERT INTO SIM_OPERATORACTIONS(SOA_SIMIMSI, SOA_TYPE,  SOA_DATA) VALUES ('+
                   StrToSQL(DeviceRegData.Sim.IMSI, 32)     +','+ //  [SOA_SIMIMSI] varchar(32) NOT NULL,
                   IntToSql(simoperact_ActivateStoppedSim)  +','+ //  [SOA_TYPE] int NOT NULL,
                   StrToSQL('Реактивиран СИМ при промяна обстоятелства ФУ: '+DeviceRegData.FiscDev.Serial, 100) +')'; //  [SOA_DATA] varchar(100) NULL,
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_OPERATORACTIONS fail: '+Device.DbInterface.LastError);
           end;
        3: begin // - спряна за вечни времена
            raise EHandledException.Create(errcode_Sim_Disabled, 'SIM картата е блокирана от мобилния оператор!');
           end;
        end;
       end;
    3: // Де-регистрация
       begin
        FRegisterSimToNRA(simsDestroy, SiteID);     // вътрешно изпълнява FInsertDeviceAction
       end;
    end;}
   end
  else
   begin
    SQL := 'SELECT SE_MSISDN, SE_ICC, SE_DEALEREIK, SE_DEALERBRANCHID, SE_MOBILEOPERATOR, '+
           'SE_CREATEDDATE, SE_VALIDTO, SE_STATUS, SE_FISCALDEVICE, SE_GPRSDEVICE, SE_CUSTEIK, '+
           'SE_CUSTSITEID FROM SIM_EXTERNAL WHERE SE_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
    if not FillDataSet(SQL) then raise EAbort.Create('Load ExtSIM fail: '+Device.DbInterface.LastError);
    if DataSet.RecordCount > 0 then
     begin
      if (DataSet.FieldByName('SE_ICC').AsString = '')or(DataSet.FieldByName('SE_MSISDN').AsString = '') then
       raise EHandledException.Create(errcode_Fisc_InvalidData, 'SIM картата е регистрирана с невалидни данни в системата!');
      //проверка на  SE_DEALEREIK  SE_VALIDTO
      CloseDataSet;

      SQL := 'UPDATE SIM_EXTERNAL SET '+
             'SE_FISCALDEVICE = '+ StrToSQL(DeviceRegData.FiscDev.Serial, 10, true) +','+ // [SE_FISCALDEVICE] varchar(10) NULL,
             'SE_GPRSDEVICE = '+   StrToSQL(DeviceRegData.Modem.Serial, 10, true)   +','+ // [SE_GPRSDEVICE] varchar(10) NULL,
             'SE_CUSTEIK = '+      StrToSQL(DeviceRegData.Owner.EIK, 13, true)      +','+ // [SE_CUSTEIK] varchar(13) NULL,
             'SE_CUSTSITEID = '+   IntToSql(SiteID, true)                           +','+ // [SE_CUSTSITEID] int NULL,
             'SE_TESTMODE = '+     BoolToSql(Device.TestMode)                       +' '+ // [SE_TESTMODE] int NULL,
             'WHERE SE_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
      if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM_EXTERNAL fail: '+Device.DbInterface.LastError);

      SQL := 'INSERT INTO SIM_EXTERNALACTIONS(SEA_IMSI, SEA_ACTIONTYPE, SEA_DATA) VALUES ('+
             StrToSQL(DeviceRegData.Sim.IMSI, 32)   +','+ //  [SEA_IMSI] varchar(32) NOT NULL,
             IntToSQL(simextact_FDevRegistered)     +','+ //  [SEA_ACTIONTYPE] int NOT NULL,
             StrToSQL('ФУ: '+DeviceRegData.FiscDev.Serial+sLineBreak+
                      'Дилър:'+DeviceRegData.Dealer.Name+sLineBreak+
                      'Клиент:'+DeviceRegData.Owner.Name, 200)   +')'; //  [SEA_DATA] varchar(200) NULL,
      if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert SIM_EXTERNALACTIONS fail: '+Device.DbInterface.LastError);

      FInsertDeviceAction(SiteID, fdevact_SimActivation, 'Външна СИМ карта. Регистрацията се пропуска!', '');
     end
    else
     raise EHandledException.Create(errcode_Sim_NotExist, 'СИМ картата не съществува [2]');
   end;
 finally
  CloseDataSet;
 end;
end;

procedure TCmd_FiscRequestServer.FRegisterSimToNRA(SIMAction: TSimStatusType; SiteID: Integer);
var RegSimHndr : THndr_NRARegSimServer;
    ErrCode    : Integer;
    ErrMsg     : String;
    STest      : String;
begin
  RegSimHndr := THndr_NRARegSimServer.Create(nil, Device);
  try
   if Device.TestMode then STest := ' [ТЕСТОВ сървър] '
    else STest := '';

   if Device.TestMode then
    begin
     // регистрация на СИМ от името на Мобилен оператор
     RegSimHndr.SimRequestType := srtIMSI_List;
     RegSimHndr.TestRequest    := Device.TestMode;
     RegSimHndr.UserType       := 'D';
     RegSimHndr.UserName       := ESKLoginData.EskSerial;
     RegSimHndr.SimAccount     := satMobileOpr;
     RegSimHndr.SimStatus      := simsActivate;
     RegSimHndr.SimRequestList.Clear;
     RegSimHndr.SimRequestList.Add(DeviceRegData.Sim.IMSI);

     if not RegSimHndr.Execute(ErrCode, ErrMsg) then
      raise EAbort.Create('NRA activation fail: '+RegSimHndr.LastError);

     if ErrCode <> errcode_ExecuteSucceed then
      begin
       ErrMsg := 'Неуспешна инициализация на СИМ в НАП!'+STest+sLineBreak+
                 'Операция: '+ RegSimHndr.SimStatusText+sLineBreak+
                 'IMSI: '+DeviceRegData.Sim.IMSI+sLineBreak+
                 'MSISDN: '+DeviceRegData.Sim.MSISDN+sLineBreak+
                 'Клиент: '+DeviceRegData.Owner.Name + ' / '+DeviceRegData.Owner.EIK+sLineBreak+
                 ''+sLineBreak+
                 'Грешка ['+IntToStr(ErrCode)+']: '+ErrMsg;

       Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
       FInsertDeviceAction(SiteID, fdevact_SimActivation, 'Неуспешна регистрация на СИМ в НАП!', ErrMsg);
       raise EHandledException.Create(ErrCode, ErrMsg);
      end;
    end;

   // регистрация на СИМ от името на Елтрейд
   RegSimHndr.SimRequestType := srtIMSI_List;
   RegSimHndr.TestRequest    := Device.TestMode;
   RegSimHndr.UserType       := 'D';
   RegSimHndr.UserName       := ESKLoginData.EskSerial;
   RegSimHndr.SimAccount     := satEltrade;
   RegSimHndr.SimStatus      := SIMAction;
   RegSimHndr.SimRequestList.Clear;
   RegSimHndr.SimRequestList.Add(DeviceRegData.Sim.IMSI);
   if not RegSimHndr.Execute(ErrCode, ErrMsg) then
    raise EAbort.Create('NRA activation fail: '+RegSimHndr.LastError);

   if ErrCode <> errcode_ExecuteSucceed then
    begin
     ErrMsg := 'Неуспешна регистрация на СИМ в НАП!'+STest+sLineBreak+
               'Операция: '+ RegSimHndr.SimStatusText+sLineBreak+
               'IMSI: '+DeviceRegData.Sim.IMSI+sLineBreak+
               'MSISDN: '+DeviceRegData.Sim.MSISDN+sLineBreak+
               'Клиент: '+DeviceRegData.Owner.Name + ' / '+DeviceRegData.Owner.EIK+sLineBreak+
               ''+sLineBreak+
               'Грешка ['+IntToStr(ErrCode)+']: '+ErrMsg;

//     Device.PostEventSystem(C_EvType_Error, ErrMsg, Self.ClassName);
     FInsertDeviceAction(SiteID, fdevact_SimActivation, 'Неуспешна регистрация на СИМ в НАП!', ErrMsg);
     raise EHandledException.Create(ErrCode, ErrMsg);
    end;

   FInsertDeviceAction(SiteID, fdevact_SimActivation,
                       'Регистрация на SIM в НАП'+STest,
                       'Регистрация на SIM в НАП'+STest+sLineBreak+
                       'Операция: '+ RegSimHndr.SimStatusText+STest+sLineBreak+
                       'IMSI: '+DeviceRegData.Sim.IMSI+sLineBreak+
                       'MSISDN: '+DeviceRegData.Sim.MSISDN+sLineBreak+
                       'Клиент: '+DeviceRegData.Owner.Name + ' / '+DeviceRegData.Owner.EIK);
  finally
   RegSimHndr.Free;
  end;
end;

{procedure TCmd_FiscRequestServer.FUpdateSIMPayment(SiteID: Integer);
var SQL      : String;
    RA       : Integer;
    EndPeriod: TDateTime;
    PriceDlr : Real;
    PriceStd : Real;
    Ammount  : Real;
begin
 PriceDlr := -1;
 PriceStd := -1;

 with Device.DbInterface do
 try
  SQL := 'SELECT SIM_PAYEDTODATE FROM SIM WHERE SIM_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
  if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail');
  if not DataSet.IsEmpty then
   begin
    if DataSet.FieldByName('SIM_PAYEDTODATE').IsNull then DeviceRegData.Sim.PayedTo := Date    // ако картата е нова започваме от днес
     else DeviceRegData.Sim.PayedTo := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;
    CloseDataSet;

    if (DeviceRegData.Sim.PayedTo < IncDay(Now, 5)) then
     begin // генерираме плащане
       // ако картата е нова започваме от днес
       if DeviceRegData.Sim.PayedTo = 0 then DeviceRegData.Sim.PayedTo := Date;

       // Check SIM period !!! - при деригстрация и промяна не се задава от клиента
       SQL := 'SELECT SCP_ID, SCP_LENGTH FROM SIM_CHARGEPERIODS ORDER BY SCP_LENGTH ASC';
       if not FillDataSet(SQL) then raise EAbort.Create('Load SIM_CHARGEPERIODS fail: '+Device.DbInterface.LastError);
       if DataSet.IsEmpty then raise EHandledException.Create(errcode_Sim_PriceError, 'Не са дефинирани периоди за плащане!');
       if not DataSet.Locate('SCP_ID', DeviceRegData.Services.SimPeriodId, []) then
        begin
         DataSet.First;
         DeviceRegData.Services.SimPeriodId  := DataSet.FieldByName('SCP_ID').AsInteger;
         DeviceRegData.Services.SimPeriodLen := DataSet.FieldByName('SCP_LENGTH').AsInteger;
        end;
       CloseDataSet;

       // select price
       SQL := 'SELECT DP_DEALEREIK, DP_PRICE, DP_CURRENCY '+
              'FROM DEALERS_PRICES '+
              'WHERE ((DP_DEALEREIK IS NULL)OR(DP_DEALEREIK = '+StrToSQL(ESKLoginData.DealerData.CompanyEIK)+'))'+
              'AND(DP_CHARGEPERIOD = '+IntToSql(DeviceRegData.Services.SimPeriodId)+')';
       if not FillDataSet(SQL) then raise EAbort.Create('Load DEALERS_PRICES fail: '+Device.DbInterface.LastError);
       if DataSet.IsEmpty then raise EHandledException.Create(errcode_Sim_PriceError, 'Не е дефинирана цена за избрания период!');
       DataSet.First;
       while not DataSet.Eof do
        begin
         if DataSet.FieldByName('DP_DEALEREIK').IsNull then PriceStd := DataSet.FieldByName('DP_PRICE').AsFloat
          else PriceDlr := DataSet.FieldByName('DP_PRICE').AsFloat;
         DataSet.Next;
        end;
       CloseDataSet;

       if PriceStd < 0 then raise EHandledException.Create(errcode_Sim_PriceError, 'Не е дефинирана цена за избрания период!');
       if PriceDlr < 0 then PriceDlr := PriceStd;
       Ammount   := PriceDlr * DeviceRegData.Services.SimPeriodLen;
       EndPeriod := IncMonth(DeviceRegData.Sim.PayedTo, DeviceRegData.Services.SimPeriodLen);
       if Ammount < 0 then raise EHandledException.Create(errcode_Sim_PriceError, 'Не е дефинирана цена за избрания период!');

       SQL := 'INSERT INTO DEALERS_SIMPAYMENTS (DSP_DEALEREIK, DSP_DEALERBRANCH, DSP_DEALERUSERID, '+
              'DSP_DEALERUSERNAME, DSP_DEVICEFU, DSP_DEVICEGPRS, DSP_IMSI, DSP_CUSTEIK, DSP_CUSTSITE, '+
              'DSP_AMOUNT, DSP_PERIOD, DSP_PAYEDFROM, DSP_PAYEDTO, DSP_PRICEBASE, DSP_PRICEDEALER) VALUES ('+
              StrToSQL(ESKLoginData.DealerData.CompanyEIK, 13) +', '+ // [DSP_DEALEREIK] varchar(13) NOT NULL,
              IntToSQL(ESKLoginData.DealerData.BranchID)       +', '+ // [DSP_DEALERBRANCH] int NOT NULL,
              IntToSQL(ESKLoginData.DealerData.UserID)         +', '+ // [DSP_DEALERUSERID] int NOT NULL,
              StrToSQL(ESKLoginData.EskSerial, 10)             +', '+ // [DSP_DEALERUSERNAME] varchar(10) NOT NULL,
              StrToSQL(DeviceRegData.FiscDev.Serial, 10)       +', '+ // [DSP_DEVICEFU] varchar(10) NOT NULL,
              StrToSQL(DeviceRegData.Modem.Serial)             +', '+ // [DSP_DEVICEGPRS] varchar(10) NOT NULL,
              StrToSQL(DeviceRegData.Sim.IMSI)                 +', '+ // [DSP_IMSI] varchar(32) NOT NULL,
              StrToSQL(DeviceRegData.Owner.EIK)                +', '+ // [DSP_CUSTEIK] varchar(13) COLLATE Cyrillic_General_CI_AS NOT NULL,
              IntToSQL(SiteID)                                 +', '+ // [DSP_CUSTSITE] int NOT NULL,
              FloatToSql(Ammount)                              +', '+ // [DSP_AMOUNT] float NOT NULL,
              IntToSQL(DeviceRegData.Services.SimPeriodLen)    +', '+ // [DSP_PERIOD] int NOT NULL,
              DateToSQL(DeviceRegData.Sim.PayedTo)             +', '+ // [DSP_PAYEDFROM] date NOT NULL,
              DateToSQL(EndPeriod)                             +', '+ // [DSP_PAYEDTO] date NOT NULL,
              FloatToSql(PriceStd)                             +', '+ // [DSP_PRICEBASE] float NOT NULL,
              FloatToSql(PriceDlr)                             +')';  // [DSP_PRICEDEALER] float NOT NULL,
       if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEALERS_SIMPAYMENTS fail: '+Device.DbInterface.LastError);

       // update end date
       SQL := 'UPDATE SIM SET SIM_PAYEDTODATE = '+DateToSQL(EndPeriod)+' WHERE SIM_IMSI = '+StrToSQL(DeviceRegData.Sim.IMSI);
       if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM_PAYEDTODATE fail: '+Device.DbInterface.LastError);

       // update dealer ammount
       SQL := 'UPDATE DEALERS SET '+
              'D_SIMPAYMENT_TOTAL = D_SIMPAYMENT_TOTAL + '+FloatToSql(Ammount)+', '+
              'D_SIMPAYMENT_DUE = D_SIMPAYMENT_DUE + '+FloatToSql(Ammount)+' '+
              'WHERE D_EIK = '+StrToSQL(ESKLoginData.DealerData.CompanyEIK);
       if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEALERS fail: '+Device.DbInterface.LastError);

       FInsertDeviceAction(SiteID, fdevact_InsertPayment, 'Генерирано плащане! Период: '+DateToStr(DeviceRegData.Sim.PayedTo)+' - '+DateToStr(EndPeriod),
                           'Period length: '+IntToStr(DeviceRegData.Services.SimPeriodLen)+sLineBreak+
                           'Ammount: '+FloatToStr(Ammount));
     end
    else
      FInsertDeviceAction(SiteID, fdevact_InsertPayment, 'Не е генерирано плащане! Картата е платена до: '+DateToStr(DeviceRegData.Sim.PayedTo), '');
   end
  else
   raise EAbort.Create('Системна грешка. Не е открита СИМ карта [3]');
 finally
  CloseDataSet;
 end;
end;}

function TCmd_FiscRequestServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var Site    : Integer;
    SQL     : String;
    OprType : String;
    RA      : Integer;
begin
 Result := true;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    if (not FCheckInputData(FReqType, ErrCode, UserError))or
       (not FCheckDelaerStatus(ErrCode, UserError))or
       (not FCheckModemStatus(ErrCode, UserError))or  // първо се прави проверка на версията на модема а после тази на модела на фу
       (not FCheckFDStatus(ErrCode, UserError))or
       (not FCheckSimStatus(ErrCode, UserError)) then
     raise EHandledException.Create(ErrCode, UserError);


    FUpdateCustomer;                  // process customers data - insert or update
    FUpdateSite(Site);                // process site data - insert new site ot get existing site ID
    // Pocess Customers contacts
    FUpdateDeviceGPRS (Site);         // Process Device GPRS
    FUpdateDeviceFU   (Site);         // Process Device FU    вътрешно изпълнява FInsertDeviceAction
    FUpdateSimData    (Site);         // присвоява ФУ, клиент, обект към картата

    // Прави се от процедура в базата след упдейт на DEVICES_FU
    // Не е нужно да се прави от тук!!!
    //FUpdatePairHistory(Site);         // Process pair history

    case FReqType of
    1:  // Регистрация на ФУ/ИАСУТД;
      begin
        OprType := 'фискализация';
        FInsertDeviceAction(Site, fdevact_FiscalizeRequest, 'Заявка за '+OprType+' на устройство', DeviceRegData.DataAsXML(xml_Node_FiscalizeRoot));

        // само за наши карти
        if not DeviceRegData.Sim.IsExternal then
         begin
          FRegisterSimToNRA(simsActivate, Site);     // вътрешно изпълнява FInsertDeviceAction

          if (DeviceRegData.Sim.PayedTo <= IncDay(Date))or(DeviceRegData.Sim.StatusOper in [2, 3]) then
           begin
            FInsertDeviceAction(Site, fdevact_SimPaymentCheck, 'Проверка абонамент. НЕВАЛИДЕН!',
                                                               'Картата няма валиден абонамент!'+sLineBreak+
                                                               'Картата е платена до: '+DateToStr(DeviceRegData.Sim.PayedTo));
             // вътрешно активира картата
             with THndr_SimPayments.Create(Device) do
             try
              Imsi      := DeviceRegData.Sim.IMSI;
              PayPeriod := DeviceRegData.Services.SimPeriodLen;
              ChargeTo  := ESKLoginData.DealerData;
              ChargeEsk := ESKLoginData.EskSerial;
              Source    := 'OnDeviceFiscalize';
              if not Execute(ErrCode, UserError) then raise EAbort.Create(LastError);
              if ErrCode <> errcode_ExecuteSucceed then raise EHandledException.Create(ErrCode, UserError);

              // имаме промяна в данните за картата - трябва да ги отразиме
              DeviceRegData.Sim.PayedTo    := PayedTo;
              DeviceRegData.Sim.Activation := Activation;
              DeviceRegData.Sim.StatusOper := StatusMob;
             finally
              Free;
             end;
           end
          else
           FInsertDeviceAction(Site, fdevact_SimPaymentCheck, 'Проверка абонамент. ОК',
                                                              'Не е генерирано задължение!'+sLineBreak+
                                                              'Картата е платена до: '+DateToStr(DeviceRegData.Sim.PayedTo));
         end;

        with Device.DbInterface do
        try
         SQL := 'UPDATE DEVICES_FU SET '+
                'DF_STATUS = '            + IntToSql(1)              +' '+    {подадена заявка за регистрация към НАП}
                'WHERE DF_SERIAL = '      + StrToSQL(DeviceRegData.FiscDev.Serial);
         if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU: '+Device.DbInterface.LastError);

         SQL := 'SELECT MAX(DFA_ID) "DFA_ID" FROM DEVICES_FU_ACTIONS '+
                'WHERE (DFA_DEVFUSERIAL = '+StrToSQL(DeviceRegData.FiscDev.Serial)+')'+
                'AND(DFA_DEVGPRSSERIAL = '+StrToSQL(DeviceRegData.Modem.Serial)+')'+
                'AND(DFA_SIMIMSI = '+StrToSQL(DeviceRegData.Sim.IMSI)+')'+
                'AND(DFA_ACTIONTYPE = '+IntToSql(fdevact_FiscalizeRequest)+')';
         if not FillDataSet(SQL) then raise EAbort.Create('Select Fiscalization request ID fail: '+Device.DbInterface.LastError);
         if DataSet.FieldByName('DFA_ID').IsNull then
          raise EHandledException.Create(errcode_Fisc_InternalError, 'Системна грешка.'+sLineBreak+
                                                                     'Неидентифицирана заявка за фискализация!',
                                                                     'MAX(DFA_ID).DEVICES_FU_ACTIONS not found'+sLineBreak+
                                                                     'Unexpected error during: '+OprType);
         FRequestID := DataSet.FieldByName('DFA_ID').AsInteger;
        finally
         CloseDataSet;
        end;

        // Генериране н аключ за отключване на СИМ
        DeviceRegData.Sim.UnlockCode := '';
      end;
    2:  // Промяна на обстоятелства;
      begin
        OprType := 'промяна обстоятелства';
        FInsertDeviceAction(Site, fdevact_FiscChangeRequest, 'Заявка за '+OprType+' на устройство', DeviceRegData.DataAsXML(xml_Node_FiscalizeRoot));

        if not DeviceRegData.Sim.IsExternal then
         begin
          FRegisterSimToNRA(simsActivate, Site);     // вътрешно изпълнява FInsertDeviceAction

          if (DeviceRegData.Sim.PayedTo <= IncDay(Date))or(DeviceRegData.Sim.StatusOper in [2, 3]) then
           begin
            FInsertDeviceAction(Site, fdevact_SimPaymentCheck, 'Проверка абонамент. НЕВАЛИДЕН!',
                                                               'Картата няма валиден абонамент!'+sLineBreak+
                                                               'Картата е платена до: '+DateToStr(DeviceRegData.Sim.PayedTo));
             with THndr_SimPayments.Create(Device) do
             try
              Imsi      := DeviceRegData.Sim.IMSI;
              PayPeriod := DeviceRegData.Services.SimPeriodLen;
              ChargeTo  := ESKLoginData.DealerData;
              ChargeEsk := ESKLoginData.EskSerial;
              Source    := 'OnDeviceFiscUpdate';
              if not Execute(ErrCode, UserError) then raise EAbort.Create(LastError);
              if ErrCode <> errcode_ExecuteSucceed then raise EHandledException.Create(ErrCode, UserError);

              // имаме промяна в данните за картата - трябва да ги отразиме
              DeviceRegData.Sim.PayedTo    := PayedTo;
              DeviceRegData.Sim.Activation := Activation;
              DeviceRegData.Sim.StatusOper := StatusMob;
             finally
              Free;
             end;
           end
          else
           FInsertDeviceAction(Site, fdevact_SimPaymentCheck, 'Проверка абонамент. ОК',
                                                              'Не е генерирано задължение!'+sLineBreak+
                                                              'Картата е платена до: '+DateToStr(DeviceRegData.Sim.PayedTo));
         end;

        with Device.DbInterface do
        try
         SQL := 'SELECT MAX(DFA_ID) "DFA_ID" FROM DEVICES_FU_ACTIONS '+
                'WHERE (DFA_DEVFUSERIAL = '+StrToSQL(DeviceRegData.FiscDev.Serial)+')'+
                'AND(DFA_DEVGPRSSERIAL = '+StrToSQL(DeviceRegData.Modem.Serial)+')'+
                'AND(DFA_SIMIMSI = '+StrToSQL(DeviceRegData.Sim.IMSI)+')'+
                'AND(DFA_ACTIONTYPE = '+IntToSql(fdevact_FiscChangeRequest)+')';
         if not FillDataSet(SQL) then raise EAbort.Create('Select Fiscalization request ID fail: '+Device.DbInterface.LastError);
         if DataSet.FieldByName('DFA_ID').IsNull then
          raise EHandledException.Create(errcode_Fisc_InternalError, 'Системна грешка.'+sLineBreak+
                                                                     'Неидентифицирана заявка за фискализация!',
                                                                     'MAX(DFA_ID).DEVICES_FU_ACTIONS not found'+sLineBreak+
                                                                     'Unexpected error during: '+OprType);
         FRequestID := DataSet.FieldByName('DFA_ID').AsInteger;
        finally
         CloseDataSet;
        end;

        // Генериране н аключ за отключване на СИМ
        DeviceRegData.Sim.UnlockCode := '';
      end;
    3:  // Отрегистрация
      begin
        OprType := 'де-регистрация';
        FInsertDeviceAction(Site, fdevact_FiscDropRequest, 'Заявка за '+OprType+' на устройство', DeviceRegData.DataAsXML(xml_Node_FiscalizeRoot));

        if not DeviceRegData.Sim.IsExternal then
         begin
          FRegisterSimToNRA(simsDestroy, Site);     // вътрешно изпълнява FInsertDeviceAction
         end;

        with Device.DbInterface do
        try
         SQL := 'UPDATE DEVICES_FU SET '+
                'DF_STATUS = '            + IntToSql(3)              +' '+    {Подадена заявка за де-регистрация към НАП}
                'WHERE DF_SERIAL = '      + StrToSQL(DeviceRegData.FiscDev.Serial);
         if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU: '+Device.DbInterface.LastError);

         SQL := 'SELECT MAX(DFA_ID) "DFA_ID" FROM DEVICES_FU_ACTIONS '+
                'WHERE (DFA_DEVFUSERIAL = '+StrToSQL(DeviceRegData.FiscDev.Serial)+')'+
                'AND(DFA_DEVGPRSSERIAL = '+StrToSQL(DeviceRegData.Modem.Serial)+')'+
                'AND(DFA_SIMIMSI = '+StrToSQL(DeviceRegData.Sim.IMSI)+')'+
                'AND(DFA_ACTIONTYPE = '+IntToSql(fdevact_FiscDropRequest)+')';
         if not FillDataSet(SQL) then raise EAbort.Create('Select Fiscalization request ID fail: '+Device.DbInterface.LastError);
         if DataSet.FieldByName('DFA_ID').IsNull then
          raise EHandledException.Create(errcode_Fisc_InternalError, 'Системна грешка.'+sLineBreak+
                                                                     'Неидентифицирана заявка за фискализация!',
                                                                     'MAX(DFA_ID).DEVICES_FU_ACTIONS not found'+sLineBreak+
                                                                     'Unexpected error during: '+OprType);
         FRequestID := DataSet.FieldByName('DFA_ID').AsInteger;
        finally
         CloseDataSet;
        end;
      end;
    end;

    if Device.TestMode then DeviceRegData.FiscDevEx.NraUrl := C_NRA_Url_Test
     else DeviceRegData.FiscDevEx.NraUrl := C_NRA_Url_Work;
   end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    if Site > 0 then FInsertDeviceAction(Site, fdevact_RejectOperation, 'Отказана заявка за '+OprType, '['+IntToStr(E.ErrorCode)+']'+sLineBreak+E.Message);
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    if Site > 0 then FInsertDeviceAction(Site, fdevact_RejectOperation, 'Отказана заявка за '+OprType, LastError);
    Result    := false;
   end;
 end;
end;

function TCmd_FiscRequestServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
begin
 Result := inherited AddAnswerToDocument;
 if not Result then Exit;

 try
  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_FiscData);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqID)).Text := IntToStr(FRequestID);
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//           TCmd_FiscCommitClient
//******************************************************************************
constructor TCmd_FiscCommitClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FCrcHandler     := TCRC32.Create;
 FTimeElapsedMsec:= 0;
 FDeviceSerial   := '';
 FFiscRequestID  := 0;
 FRegistrationID := 0;
 FErrorCode      := errcode_ExecuteSucceed;
 FErrorMessage   := '';
 FEskSerial      := '';
 FTestMode       := false;
end;

destructor TCmd_FiscCommitClient.Destroy;
begin
 inherited Destroy;
 FCrcHandler.Free;
end;

function TCmd_FiscCommitClient.GetCommandName: String;
begin
 Result := cmd_FiscCommit;
end;

function TCmd_FiscCommitClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(FDeviceSerial+EskSerial+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscCommit));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DevSerial)).Text    := FDeviceSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqID)).Text    := IntToStr(FFiscRequestID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReggId)).Text   := IntToStr(FRegistrationID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscResult)).Text   := IntToStr(FErrorCode);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscError)).Text    := FErrorMessage;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text    := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_TimeElapsed)).Text  := IntToStr(FTimeElapsedMsec);
  if FTestMode then iNode.AppendChild(XmlDocument.CreateElement(xml_Node_TestMode)).Text := 'YES';
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text         := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text          := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscCommitClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_FiscCommit);

  FDeviceSerial   :=          XML_GetNodeText(iNode, xml_Node_DevSerial);
  FFiscRequestID  := StrToInt(XML_GetNodeText(iNode, xml_Node_FiscReqID));
  FRegistrationID := StrToInt(XML_GetNodeText(iNode, xml_Node_FiscReggId));
  FErrorCode      := StrToInt(XML_GetNodeText(iNode, xml_Node_FiscResult));
  FErrorMessage   :=          XML_GetNodeText(iNode, xml_Node_FiscError, true, false);
  FEskSerial      :=          XML_GetNodeText(iNode, xml_Node_ESKSerial);
  FTimeElapsedMsec:= StrToInt(XML_GetNodeText(iNode, xml_Node_TimeElapsed));
  FTestMode       :=         (XML_GetNodeText(iNode, xml_Node_TestMode, False, False) = 'YES');
  SDate           :=          XML_GetNodeText(iNode, xml_Node_Time);
  SCRC            :=          XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(FDeviceSerial+EskSerial+SDate) then
   raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscCommitClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

//******************************************************************************
//    TCmd_FiscCommitServer
//******************************************************************************
constructor TCmd_FiscCommitServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FCrcHandler     := TCRC32.Create;
 FTimeElapsedMsec:= 0;
 FDeviceSerial   := '';
 FFiscRequestID  := 0;
 FRegistrationID := 0;
 FErrorCode      := errcode_ExecuteSucceed;
 FErrorMessage   := '';
 FEskSerial      := '';
 FTestMode       := Device.TestMode;
 FOprType        := 0;
 FOprName        := '';
end;

destructor TCmd_FiscCommitServer.Destroy;
begin
 FCrcHandler.Free;
 inherited Destroy;
end;

function TCmd_FiscCommitServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  iNode   := XML_GetRootNode;
  iNode   := XML_FindNodeByName(iNode, xml_Node_FiscCommit);

  FDeviceSerial   :=          XML_GetNodeText(iNode, xml_Node_DevSerial);
  FFiscRequestID  := StrToInt(XML_GetNodeText(iNode, xml_Node_FiscReqID));
  FRegistrationID := StrToInt(XML_GetNodeText(iNode, xml_Node_FiscReggId));
  FErrorCode      := StrToInt(XML_GetNodeText(iNode, xml_Node_FiscResult));
  FErrorMessage   :=          XML_GetNodeText(iNode, xml_Node_FiscError, true, false);
  FEskSerial      :=          XML_GetNodeText(iNode, xml_Node_ESKSerial);
  FTimeElapsedMsec:= StrToInt(XML_GetNodeText(iNode, xml_Node_TimeElapsed));
  FTestMode       :=         (XML_GetNodeText(iNode, xml_Node_TestMode, False, False) = 'YES');
  SDate           :=          XML_GetNodeText(iNode, xml_Node_Time);
  SCRC            :=          XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(FDeviceSerial+EskSerial+SDate) then
   raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscCommitServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(FDeviceSerial+EskSerial+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscCommit));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DevSerial)).Text    := FDeviceSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqID)).Text    := IntToStr(FFiscRequestID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReggId)).Text   := IntToStr(FRegistrationID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscResult)).Text   := IntToStr(FErrorCode);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscError)).Text    := FErrorMessage;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text    := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_TimeElapsed)).Text  := IntToStr(FTimeElapsedMsec);
  if FTestMode then iNode.AppendChild(XmlDocument.CreateElement(xml_Node_TestMode)).Text := 'YES'; // появява се по-късно
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text         := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text          := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_FiscCommitServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL       : String;
    RA        : Integer;
    ModSerial : String;
    SimIMSI   : String;
    DlrEIK    : String;
    DlrUser   : String;
    DlrUserId : Integer;
    DlrBranch : Integer;
    CustEIK   : String;
    CustSite  : Integer;
    ActComment: String;
    ActData   : String;
    ActivDate : TDateTime;
    PayDateOld: TDateTime;
    PayDateNew: TDateTime;

    procedure InsertDeviceAction(ActType_: Integer; ActComment_, ActData_: String);
    begin
     if not Device.DbInterface.ExecuteSQLStatement(
            'INSERT INTO DEVICES_FU_ACTIONS (DFA_DEVFUSERIAL, DFA_DEVGPRSSERIAL, '+
            'DFA_SIMIMSI, DFA_ACTIONTYPE, DFA_COMMENT, DFA_DEALEREIK, DFA_DEALERUSER, DFA_CUSTEIK, '+
            'DFA_DEALERBRANCH, DFA_CUSTSITE, DFA_DATA) VALUES ('+
            Device.DbInterface.StrToSQL(FDeviceSerial, 10)       + ', '+ // [DFA_DEVFUSERIAL] varchar(10) NOT NULL,
            Device.DbInterface.StrToSQL(ModSerial, 10)           + ', '+ // [DFA_DEVGPRSSERIAL] varchar(10) NOT NULL,
            Device.DbInterface.StrToSQL(SimIMSI, 32)             + ', '+ // [DFA_SIMIMSI] varchar(32) NULL,
            Device.DbInterface.IntToSql(ActType_)                + ', '+ // [DFA_ACTIONTYPE] int NULL,
            Device.DbInterface.StrToSQL(ActComment_, 100, true)  + ', '+ // [DFA_COMMENT] varchar(100) NULL,
            Device.DbInterface.StrToSQL(DlrEIK, 13)              + ', '+ // [DFA_DEALEREIK] varchar(13) NULL,
            Device.DbInterface.StrToSQL(DlrUser, 10)             + ', '+ // [DFA_DEALERUSER] varchar(10) NULL,
            Device.DbInterface.StrToSQL(CustEIK, 13)             + ', '+ // [DFA_CUSTEIK] varchar(13) NULL,
            Device.DbInterface.IntToSql(DlrBranch)               + ', '+ // [DFA_DEALERBRANCH] int NULL,
            Device.DbInterface.IntToSql(CustSite)                + ', '+ // [DFA_CUSTSITE] int NULL,
            Device.DbInterface.StrToSQL(ActData_, 0, true)       + ') ', // [DFA_DATA] varchar(max) NULL,
     RA) then raise EAbort.Create('Insert DEVICES_FU_ACTIONS: '+Device.DbInterface.LastError);
    end;
begin
 Result := true;
 try
   with Device.DbInterface do
   try
     SQL := 'SELECT A.DFA_DEVFUSERIAL, A.DFA_DEVGPRSSERIAL, A.DFA_SIMIMSI, A.DFA_ACTIONTYPE, '+
            'AT.DFAT_NAME, A.DFA_DATE, A.DFA_COMMENT, A.DFA_DEALEREIK, A.DFA_DEALERUSER, A.DFA_CUSTEIK, '+
            'A.DFA_DEALERBRANCH, A.DFA_CUSTSITE, DU.DU_ID '+
            'FROM DEVICES_FU_ACTIONS A '+
            'LEFT JOIN DEVICES_FU_ACTIONTYPES AT ON A.DFA_ACTIONTYPE = AT.DFAT_ID '+
            'LEFT JOIN DEALERS_USERS DU ON A.DFA_DEALERUSER = DU.DU_USERNAME '+
            'WHERE DFA_ID = '+IntToSql(FFiscRequestID);
     if not FillDataSet(SQL) then raise EAbort.Create('Read DEVICES_FU_ACTIONS fail: '+Device.DbInterface.LastError);

     if DataSet.RecordCount = 0 then
      raise EHandledException.Create(errcode_Fisc_RequestNotFound, 'Заявка за фискализация '+IntToSql(FFiscRequestID)+' не е намерена!',
                                                                   'Request not found in DEVICES_FU_ACTIONS. Where DFA_ID='+IntToSql(FFiscRequestID));
     if (DataSet.FieldByName('DFA_DEVFUSERIAL').AsString <> FDeviceSerial) then
      raise EHandledException.Create(errcode_Fisc_RequestNotMatch, 'Заявка за фискализация '+IntToSql(FFiscRequestID)+' не е за устройство: '+FDeviceSerial,
                                                                   'FD serial donot match: '+DataSet.FieldByName('DFA_DEVFUSERIAL').AsString+'<>'+FDeviceSerial);
     ModSerial := DataSet.FieldByName('DFA_DEVGPRSSERIAL').AsString;
     SimIMSI   := DataSet.FieldByName('DFA_SIMIMSI').AsString;
     DlrEIK    := DataSet.FieldByName('DFA_DEALEREIK').AsString;
     DlrUser   := DataSet.FieldByName('DFA_DEALERUSER').AsString;
     DlrBranch := DataSet.FieldByName('DFA_DEALERBRANCH').AsInteger;
     DlrUserId := DataSet.FieldByName('DU_ID').AsInteger;
     CustEIK   := DataSet.FieldByName('DFA_CUSTEIK').AsString;
     CustSite  := DataSet.FieldByName('DFA_CUSTSITE').AsInteger;
     FOprType  := DataSet.FieldByName('DFA_ACTIONTYPE').AsInteger;
     FOprName  := DataSet.FieldByName('DFAT_NAME').AsString;
     CloseDataSet;

     case FOprType of
     fdevact_FiscalizeRequest:    //	Заявка за фискализация
        begin
          if FErrorCode = errcode_ExecuteSucceed then
           begin
            // процедура за отместване на такса СИМ напред във времето при неуспешна фискализация
            try
              SQL := 'SELECT DF_FISCALIZATIONDATE, DF_NRAREGID FROM DEVICES_FU '+
                     'WHERE DF_SERIAL = ' + StrToSQL(FDeviceSerial);
              if not FillDataSet(SQL) then raise EAbort.Create('Select DEVICES_FU fail.'+sLineBreak+Device.DbInterface.LastError);
              if (DataSet.FieldByName('DF_FISCALIZATIONDATE').IsNull)and
                 (DataSet.FieldByName('DF_NRAREGID').IsNull) then
               begin
                SQL := 'UPDATE DEVICES_FU SET '+
                       'DF_FIRSTFISCALIZATION = ' + DateTimeToSQL(Now)              + ', '+
                       'DF_FIRSTNRAREGID = '      + IntToSql(FRegistrationID, true) + '  '+
                       'WHERE DF_SERIAL = '       + StrToSQL(FDeviceSerial);
                 if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU: '+Device.DbInterface.LastError);

                //Имаме нова фискализация. Проверяваме кога е била активирана СИМ картата
                SQL := 'SELECT SIM_FISCALDEVICE, SIM_ACTIVATIONDATE, SIM_PAYEDTODATE '+
                       'FROM SIM WHERE SIM_IMSI = '+StrToSQL(SimIMSI);
                if not FillDataSet(SQL) then raise EAbort.Create('Select SIM fail.'+sLineBreak+Device.DbInterface.LastError);
                if (not DataSet.FieldByName('SIM_ACTIVATIONDATE').IsNull)and // активирана СИМ
                   (not DataSet.FieldByName('SIM_PAYEDTODATE').IsNull)and    // платена СИМ
                   (DateOf(DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime) < Date)and // СИМ е активирана преди фискализацията
                   (DaysBetween(DateOf(DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime), Date) <= 30)and  // СИМ е активирана преди по-малко от 30 дни
                   (DaysBetween(Date, DateOf(DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime)) > 30) then // остават повече от 30 дни до изтичането на картата
                 begin
                  // Сим картата е била активирана преди фискализацията (в рамките на 1 месец)
                  // изчисляваме новата крайна дата и подготваме другите данни
                  PayDateOld := DataSet.FieldByName('SIM_PAYEDTODATE').AsDateTime;
                  PayDateNew := IncDay(PayDateOld, DaysBetween(DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime, Date)+1);
                  ActivDate  := DataSet.FieldByName('SIM_ACTIVATIONDATE').AsDateTime;

                  // Проверяваме колко каси са ръчкани с тази карта
                  SQL := 'SELECT DISTINCT HP_FISCALDEVICE FROM HISTORYPAIR WHERE HP_SIMIMSI = '+StrToSQL(SimIMSI);
                  if not FillDataSet(SQL) then raise EAbort.Create('Select HISTORYPAIR fail.'+sLineBreak+Device.DbInterface.LastError);
                  DataSet.Last;
                  if DataSet.RecordCount < 2 then
                   begin
                    // с тази СИМ карта не са ръчкани няколко каси
                    // местим крайната дата напред във времето
                    ActComment := 'Удължен период СИМ. До:'+DateToStr(PayDateNew);
                    ActData    := 'Удължен период СИМ заради по-късна фискализация'+sLineBreak+
                                  'Таксата за периода е за сметка на Елтрейд'+sLineBreak+
                                  'СИМ активиран на: '+ DateToStr(ActivDate)+sLineBreak+
                                  'Фискализация на: '+  DateToStr(Date)+sLineBreak+
                                  'Стара валидност: '+  DateToStr(PayDateOld)+sLineBreak+
                                  'Нова валидност: '+   DateToStr(PayDateNew)+sLineBreak+
                                  'Удължен период дни: '+ IntToStr(DaysBetween(PayDateOld, PayDateNew))+sLineBreak+
                                  'SIM IMSI: '+SimIMSI+sLineBreak+
                                  'ФУ: '+FDeviceSerial;
                    InsertDeviceAction(fdevact_MoveSimPayDate, ActComment, ActData);

                    SQL := 'UPDATE SIM SET SIM_PAYEDTODATE = '+DateToSQL(PayDateNew)+' WHERE SIM_IMSI = '+StrToSQL(SimIMSI);
                    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update SIM Fail'+sLineBreak+Device.DbInterface.LastError);

                    SQL := 'INSERT INTO DEALERS_SIMPAYMENTS (DSP_DEALEREIK, DSP_DEALERBRANCH, DSP_DEALERUSERID, '+
                           'DSP_DEALERUSERNAME, DSP_DEVICEFU, DSP_DEVICEGPRS, DSP_IMSI, DSP_CUSTEIK, DSP_CUSTSITE, '+
                           'DSP_AMOUNT, DSP_PERIOD, DSP_PAYEDFROM, DSP_PAYEDTO, DSP_PRICEBASE, DSP_PRICEDEALER, '+
                           'DSP_IS_RENEW, DSP_COMMENT) VALUES ('+
                           StrToSQL(DlrEIK, 13)            +', '+ // [DSP_DEALEREIK] varchar(13) NOT NULL,
                           IntToSQL(DlrBranch)             +', '+ // [DSP_DEALERBRANCH] int NOT NULL,
                           IntToSQL(DlrUserId)             +', '+ // [DSP_DEALERUSERID] int NOT NULL,
                           StrToSQL(DlrUser, 10)           +', '+ // [DSP_DEALERUSERNAME] varchar(10) NOT NULL,
                           StrToSQL(FDeviceSerial, 10)     +', '+ // [DSP_DEVICEFU] varchar(10) NOT NULL,
                           StrToSQL(ModSerial, 10)         +', '+ // [DSP_DEVICEGPRS] varchar(10) NOT NULL,
                           StrToSQL(SimIMSI, 32)           +', '+ // [DSP_IMSI] varchar(32) NOT NULL,
                           StrToSQL(CustEIK, 13)           +', '+ // [DSP_CUSTEIK] varchar(13) COLLATE Cyrillic_General_CI_AS NOT NULL,
                           IntToSQL(CustSite)              +', '+ // [DSP_CUSTSITE] int NOT NULL,
                           FloatToSql(0)                   +', '+ // [DSP_AMOUNT] float NOT NULL,
                           IntToSQL(0)                     +', '+ // [DSP_PERIOD] int NOT NULL,
                           DateToSQL(PayDateOld)           +', '+ // [DSP_PAYEDFROM] date NOT NULL,
                           DateToSQL(PayDateNew)           +', '+ // [DSP_PAYEDTO] date NOT NULL,
                           FloatToSql(0)                   +', '+ // [DSP_PRICEBASE] float NOT NULL,
                           FloatToSql(0)                   +', '+ // [DSP_PRICEDEALER] float NOT NULL,
                           IntToSql(2)                     +', '+ // [DSP_IS_RENEW] smallint DEFAULT 0 NOT NULL    //0-нов абонамент; 1-подновяване; 2-местене на абонамент от друга карта и удължаване заради неуспешна фискализация
                           StrToSQL(ActData)               +')';  // [DSP_COMMENT] varchar(max)
                    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert DEALERS_SIMPAYMENTS: '+Device.DbInterface.LastError);

                    SQL := 'INSERT INTO EVENTS_USER (EU_DATATIME, EU_MODULE, EU_SUBMODULE, EU_TYPE, '+
                           'EU_REMOTEIP, EU_USERTYPE, EU_USERNAME, EU_DATA, EU_ERRORFLAG, EU_DEVTYPE, '+
                           'EU_DEVSERIAL) VALUES ('+
                            DateTimeToSQL(Now)                   +','+ // [EU_DATATIME] datetime NOT NULL,
                            StrToSQL(C_ModuleNameDev, 20)        +','+ // [EU_MODULE] varchar(20) NOT NULL,
                            StrToSQL(Device.ServerName, 50)      +','+ // [EU_SUBMODULE] varchar(50) NULL,
                            StrToSQL(C_EvType_SIM, 10)           +','+ // [EU_TYPE] varchar(10) NOT NULL,
                            StrToSQL(Device.ConnectionInfo.RemoteIP, 20)+','+ // [EU_REMOTEIP] varchar(20) NULL,
                            StrToSQL('D', 1)                     +','+ // [EU_USERTYPE] varchar(1) NOT NULL,
                            StrToSQL(DlrUser, 50, true)          +','+ // [EU_USERNAME] varchar(50) NULL,
                            StrToSQL(ActData)                    +','+ // [EU_DATA] varchar(max) NULL,
                            IntToSql(0)                          +','+ // [EU_ERRORFLAG] int DEFAULT 0 NOT NULL,
                            StrToSQL(C_DeviceType_FD, 1, true)   +','+ // [EU_DEVTYPE] varchar(1) NULL,
                            StrToSQL(FDeviceSerial, 32, true)    +')'; // [EU_DEVSERIAL] varchar(32) NULL
                    if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Insert EVENTS_USER: '+Device.DbInterface.LastError);
                   end
                  else
                   begin
                    // с тази СИМ карта са ръчкани няколко каси
                    // не пипаме крайната дата
                    InsertDeviceAction(fdevact_MoveSimPayDate, 'Запазен период СИМ.',
                                                               'Запазен период СИМ въпреки по-късна фискализация.'+sLineBreak+
                                                               'СИМ картата е поставяна в няколко устройства!');
                   end;
                  CloseDataSet;
                 end;
               end;
            except
             on E: Exception do
              begin
               CloseDataSet;
               Device.PostEventSystem(C_EvType_Error, 'FAIL procedure - move forward SIM charge!'+sLineBreak+E.Message);
              end;
            end; // край на процедура за отместване на такса СИМ напред във времето при неуспешна фискализация

            // обновяване данните за ФУ след потвърдена успешна фискализация
            SQL := 'UPDATE DEVICES_FU SET '+
                   'DF_FISCALIZATIONDATE = ' + DateToSQL(Date)          +', '+
                   'DF_FISCALIZATIONREQ = '  + IntToSql(FFiscRequestID) +', '+
                   'DF_STATUS = '            + IntToSql(2)              +', '+    {направена регистрация в НАП}
                   'DF_NRAREGID = '          + IntToSql(FRegistrationID)+'  '+
                   'WHERE DF_SERIAL = '      + StrToSQL(FDeviceSerial);
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU: '+Device.DbInterface.LastError);
           end;
        end;
     fdevact_FiscChangeRequest:   //	Заявка промяна на обстоятелства
        begin
          if FErrorCode = errcode_ExecuteSucceed then
           begin
            SQL := 'UPDATE DEVICES_FU SET '+
                   'DF_FISCALIZATIONREQ = '  + IntToSql(FFiscRequestID) +', '+
                   'DF_STATUS = '            + IntToSql(2)              +' '+    {направена регистрация в НАП}
                   'WHERE DF_SERIAL = '      + StrToSQL(FDeviceSerial);
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU: '+Device.DbInterface.LastError);
           end;
        end;
     fdevact_FiscDropRequest:     //	Заявка за дерегистрация
        begin
          if FErrorCode = errcode_ExecuteSucceed then
           begin
            SQL := 'UPDATE DEVICES_FU SET '+
                   'DF_STATUS = '            + IntToSql(4) +' '+    {Дерегистрирано устройство в НАП}
                   'WHERE DF_SERIAL = '      + StrToSQL(FDeviceSerial);
            if not ExecuteSQLStatement(SQL, RA) then raise EAbort.Create('Update DEVICES_FU: '+Device.DbInterface.LastError);
           end;
        end;
     fdevact_ReplaceSIM: // подмяна на СИМ
        begin
         if FErrorCode <> errcode_ExecuteSucceed then
          begin

          end;
        end;
     end;

     // успешна операция
     if FErrorCode = errcode_ExecuteSucceed then
      begin
       ActComment := 'Потвърдена "'+FOprName+'" No:'+IntToStr(FFiscRequestID);
       ActData    := 'Request ID: '+          IntToStr(FFiscRequestID)+sLineBreak+
                     'Reqistration ID: '+     IntToStr(FRegistrationID)+sLineBreak+
                     'Time elapsed (msec): '+ IntToStr(FTimeElapsedMsec);

       case FOprType of
       fdevact_FiscalizeRequest:    InsertDeviceAction(fdevact_FiscalizeCommit,  ActComment, ActData); //	Заявка за фискализация
       fdevact_FiscChangeRequest:   InsertDeviceAction(fdevact_FiscChangeCommit, ActComment, ActData); //	Заявка промяна на обстоятелства
       fdevact_FiscDropRequest:     InsertDeviceAction(fdevact_FiscDropCommit,   ActComment, ActData); //	Заявка за дерегистрация
       fdevact_ReplaceSIM:          InsertDeviceAction(fdevact_ReplaceSIM,       ActComment, ActData); // Заявка за подмяна на СИМ
       else                         InsertDeviceAction(fdevact_InitialInsert,    ActComment, ActData); // neizwestna operaciq
       end;
      end
     else
      begin
       ActComment := 'Неуспешна "'+FOprName+'" No:'+IntToStr(FFiscRequestID);
       ActData    := 'RequestID: '+   IntToStr(FFiscRequestID)+sLineBreak+
                     'ErrorCode: '+   IntToStr(FErrorCode)+sLineBreak+
                     '-----------------------'+sLineBreak+
                     FErrorMessage+sLineBreak+
                     '-----------------------'+sLineBreak+
                     'Time elapsed (msec): '+IntToStr(FTimeElapsedMsec);
       InsertDeviceAction(fdevact_RejectOperation, ActComment, ActData);
      end;

   finally
    CloseDataSet;
   end;

  ErrCode   := errcode_ExecuteSucceed;
  UserError := '';
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_FiscCommitServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

function TCmd_FiscCommitServer.FGetTestModeText: String;
begin
 if FTestMode then Result := 'ТЕСТОВ СЪРВЪР'
  else Result := '';
end;


//******************************************************************************
//           TCmd_ReadRequestClient
//******************************************************************************
constructor TCmd_ReadRequestClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
 FDevRegData   := nil;
 FFiscRequestID:= 0;
 FDeviceSerial := '';
 FReqDate      := 0;
 FReqComment   := '';
end;

destructor TCmd_ReadRequestClient.Destroy;
begin
 FESKLoginHndr.Free;
 if Assigned(FDevRegData) then FreeAndNil(FDevRegData);
 inherited Destroy;
end;

function TCmd_ReadRequestClient.GetCommandName: String;
begin
 Result := cmd_FiscGetReq;
end;

function TCmd_ReadRequestClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FDeviceSerial+IntToStr(FFiscRequestID)+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscRequest));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DevSerial)).Text := FDeviceSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqID)).Text := IntToStr(FFiscRequestID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ReadRequestClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 DecimalSeparator := '.';
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_FiscRequest);
  FDeviceSerial  := XML_GetNodeText(iNode, xml_Node_DevSerial);
  FFiscRequestID := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FiscReqID), 0);
  FReqDate       := StrToFloatDef(XML_GetNodeText(iNode, xml_Node_FiscReqDate, false, false), 0);
  FReqComment    := XML_GetNodeText(iNode, xml_Node_FiscReqCmnt, false, false);
  SDate          := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC           := XML_GetNodeText(iNode, xml_Node_CRC);

  iNode  := XML_FindNodeByName(iNode, xml_Node_FiscData);
  if iNode <> nil then
   begin
    FDevRegData := TRegistrationData.Create;
    if not FDevRegData.LoadFromXML(iNode) then raise EAbort.Create(FDevRegData.LastError);
   end;

  if SCRC <> FESKLoginHndr.CalculateCRC(FDeviceSerial+IntToStr(FFiscRequestID)+SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    if Assigned(FDevRegData) then FreeAndNil(FDevRegData);
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//    TCmd_ReadRequestServer
//******************************************************************************
constructor TCmd_ReadRequestServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
 FDevRegData   := nil;
 FFiscRequestID:= 0;
 FDeviceSerial := '';
 FReqDate      := 0;
 FReqComment   := '';
end;

destructor TCmd_ReadRequestServer.Destroy;
begin
 FESKLoginHndr.Free;
 if Assigned(FDevRegData) then FreeAndNil(FDevRegData);
 inherited Destroy;
end;

function TCmd_ReadRequestServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_FiscRequest);
  FDeviceSerial  := XML_GetNodeText(iNode, xml_Node_DevSerial);
  FFiscRequestID := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FiscReqID), 0);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(FDeviceSerial+IntToStr(FFiscRequestID)+SDate) then raise EAbort.Create('Invalid signature');

  // трябва да се направи след подписа (CRC)
  FDeviceSerial := UpperCase(FDeviceSerial);

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ReadRequestServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 DecimalSeparator := '.';
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(FDeviceSerial+IntToStr(FFiscRequestID)+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscRequest));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DevSerial)).Text   := FDeviceSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqID)).Text   := IntToStr(FFiscRequestID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqDate)).Text := FloatToStr(FReqDate);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscReqCmnt)).Text := FReqComment;

  if FDevRegData <> nil then
   begin
    if not FDevRegData.SaveToXML(XmlDocument, iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FiscData))) then
     raise EAbort.Create(FDevRegData.LastError);
   end;

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ReadRequestServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL    : String;
begin
 Result := true;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
     with Device.DbInterface do
     try
      if (FDeviceSerial = '') then
       raise EHandledException.Create(errcode_Fisc_RequestInvalidData, 'Невалидни входни данни.'+sLineBreak+
                                                                       'Невалиден номер на ФУ',
                                                                       'Invalid input data. Empty "FDeviceSerial"');
      if (FFiscRequestID = 0) then
       raise EHandledException.Create(errcode_Fisc_RequestInvalidData, 'Невалидни входни данни.'+sLineBreak+
                                                                       'Невалиден номер на заявка за фискализация.',
                                                                       'Invalid input data. "FFiscRequestID" =  0');

      SQL := 'SELECT DFA_DEVFUSERIAL, DFA_DATE, DFA_COMMENT, DFA_DATA '+
             'FROM DEVICES_FU_ACTIONS '+
             'WHERE (DFA_ID = '+IntToSql(FFiscRequestID)+')AND(DFA_ACTIONTYPE IN (2,6))';
      if not FillDataSet(SQL) then raise EAbort.Create('Read DEVICES_FU_ACTIONS fail: '+Device.DbInterface.LastError);

      if DataSet.RecordCount = 0 then
       raise EHandledException.Create(errcode_Fisc_RequestNotFound, 'Заявка за фискализация с номер '+IntToSql(FFiscRequestID)+' не е намерена!'+sLineBreak+
                                                                    ''+sLineBreak+
                                                                    'Да се има в предвид че номера на заявката е различен от FDRID на НАП.'+sLineBreak+
                                                                    'Това е номера на документа по който е фискализирано устройството'+sLineBreak+
                                                                    'в системата на Елтрейд. Номера на заявката може да видите от'+sLineBreak+
                                                                    'информационната система на адрес: http://partners.eltrade.com/', '');
      if (DataSet.FieldByName('DFA_DEVFUSERIAL').AsString <> FDeviceSerial) then
       raise EHandledException.Create(errcode_Fisc_RequestNotMatch, 'Заявка за фискализация '+IntToSql(FFiscRequestID)+' не е за устройство: '+FDeviceSerial+
                                                                    ''+sLineBreak+
                                                                    'Да се има в предвид че номера на заявката е различен от FDRID на НАП.'+sLineBreak+
                                                                    'Това е номера на документа по който е фискализирано устройството'+sLineBreak+
                                                                    'в системата на Елтрейд. Номера на заявката може да видите от'+sLineBreak+
                                                                    'информационната система на адрес: http://partners.eltrade.com/', '');

      FReqDate    := DataSet.FieldByName('DFA_DATE').AsDateTime;
      FReqComment := DataSet.FieldByName('DFA_COMMENT').AsString;
      FDevRegData := TRegistrationData.Create;
      if not FDevRegData.LoadFromXML(DataSet.FieldByName('DFA_DATA').AsString) then
       raise EHandledException.Create(errcode_Fisc_RequestInvalidData, 'Заявка за фискализация '+IntToSql(FFiscRequestID)+' съдържа невалидни данни!',
                                                                       'Loading request from XML fail: '+FDevRegData.LastError);
     finally
      CloseDataSet;
     end;
   end;
 except
  on E: EHandledException do
   begin
    if Assigned(FDevRegData) then FreeAndNil(FDevRegData);
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    ErrCode   := E.ErrorCode;
    UserError := E.Message;
    Result := true;
   end;
  on E: Exception do
   begin
    if Assigned(FDevRegData) then FreeAndNil(FDevRegData);
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

end.
