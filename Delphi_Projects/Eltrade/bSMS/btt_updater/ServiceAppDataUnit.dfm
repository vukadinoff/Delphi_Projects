object ServiceAppDataMod: TServiceAppDataMod
  OldCreateOrder = False
  Left = 469
  Top = 295
  Height = 228
  Width = 234
  object IBDatabase: TIBDatabase
    Params.Strings = (
      'user_name=eltrade'
      'password=gdelchev'
      'lc_ctype=WIN1251')
    LoginPrompt = False
    DefaultTransaction = IBTr
    Left = 24
    Top = 8
  end
  object IBTr: TIBTransaction
    DefaultDatabase = IBDatabase
    Left = 24
    Top = 56
  end
  object IBQry: TIBQuery
    Database = IBDatabase
    Transaction = IBTr
    AutoCalcFields = False
    ParamCheck = False
    Left = 24
    Top = 112
  end
end
