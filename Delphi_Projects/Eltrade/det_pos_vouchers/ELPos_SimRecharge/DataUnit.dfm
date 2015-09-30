object DataMod: TDataMod
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 362
  Top = 173
  Height = 279
  Width = 285
  object aIBDatabase: TIBDatabase
    DatabaseName = 'D:\Work\IB-software\Distrib\Database\ELTRADEBACKOFFICE.GDB'
    Params.Strings = (
      'user_name=eltrade'
      'password=gdelchev'
      'lc_ctype=WIN1251')
    LoginPrompt = False
    AfterConnect = aIBDatabaseAfterConnect
    Left = 32
    Top = 8
  end
  object IBSecurityService: TIBSecurityService
    TraceFlags = []
    Left = 32
    Top = 64
  end
end
