unit DBUtilsUnit;

interface

uses SysUtils, IBDatabase, IBSQL, ConstUnit;

procedure DB_CreateIBSQL(var IBSQL: TIBSQL; var IBT: TIBTransaction; DB: TIBDatabase);
procedure DB_DestroyIBSQL(var IBSQL: TIBSQL; var IBT: TIBTransaction);
function DB_FieldExist(TableName, FieldName: String; IBSQL: TIBSQL): Boolean;
function DB_TableExist(TableName: String; IBSQL: TIBSQL): Boolean;
function DB_ExecuteSQL(SQL_: String; IBSQL: TIBSQL; Commit_: boolean; ErrDesc: ShortString): Boolean;
function DB_GeneratorExist(GenName: String; IBSQL: TIBSQL): Boolean;
function DB_ExtractGenerator(GenName: String; Increment: Boolean; IBSQL: TIBSQL): Integer;
procedure DB_CommitIBSQL(IBSQL: TIBSQL);
procedure DB_RollbackIBSQL(IBSQL: TIBSQL);

implementation

uses DataUnit;

procedure DB_CreateIBSQL(var IBSQL: TIBSQL; var IBT: TIBTransaction; DB: TIBDatabase);
begin
 IBSQL := TIBSQL.Create(nil);
 IBT   := TIBTransaction.Create(nil);
 IBT.DefaultDatabase:= DB;
 IBSQL.Database     := DB;
 IBSQL.Transaction  := IBT;
 IBSQL.ParamCheck   := False;
 IBSQL.GoToFirstRecordOnExecute:= True;
end;

procedure DB_DestroyIBSQL(var IBSQL: TIBSQL; var IBT: TIBTransaction);
begin
 if IBSQL <> nil then
  begin
   if IBSQL.Open then IBSQL.Close;
   if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Rollback;
   IBSQL.SQL.Clear;
   FreeAndNil(IBT);
   FreeAndNil(IBSQL);
  end;
end;

function DB_FieldExist(TableName, FieldName: String; IBSQL: TIBSQL): Boolean;
begin
 Result := False;
 try
  try
   if IBSQL.Open then IBSQL.Close;
   IBSQL.SQL.Clear;
   IBSQL.SQL.Add('select count(rdb$relation_fields.rdb$field_name) "FLDCNT"');
   IBSQL.SQL.Add('from rdb$relation_fields');
   IBSQL.SQL.Add('where (rdb$relation_fields.rdb$system_flag = 0)');
   IBSQL.SQL.Add('and (upper(rdb$relation_fields.rdb$relation_name) = upper('+QuotedStr(TableName)+'))');
   IBSQL.SQL.Add('and (upper(rdb$relation_fields.rdb$field_name) = upper('+QuotedStr(FieldName)+'))');

   if not IBSQL.Transaction.InTransaction then IBSQL.Transaction.StartTransaction;
   IBSQL.Prepare; IBSQL.ExecQuery;
   Result := (IBSQL.FieldByName('FLDCNT').AsInteger > 0);
  except
  on E: Exception do
    DataMod.PostException('[FieldExist] '+Trim(E.Message));
  end;
 finally
  if IBSQL.Open then IBSQL.Close;
  if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Commit;
 end;
end;

function DB_TableExist(TableName: String; IBSQL: TIBSQL): Boolean;
begin
 Result := False;
 try
  try
   if IBSQL.Open then IBSQL.Close;
   IBSQL.SQL.Clear;
   IBSQL.SQL.Add('select count(rdb$relations.rdb$relation_name) "FLDCNT"');
   IBSQL.SQL.Add('from rdb$relations');
   IBSQL.SQL.Add('where (rdb$relations.rdb$system_flag = 0)');
   IBSQL.SQL.Add('and (upper(rdb$relations.rdb$relation_name) = upper('+QuotedStr(TableName)+'))');

   if not IBSQL.Transaction.InTransaction then IBSQL.Transaction.StartTransaction;
   IBSQL.Prepare; IBSQL.ExecQuery;
   Result := (IBSQL.FieldByName('FLDCNT').AsInteger > 0);
  except
  on E: Exception do
    DataMod.PostException('[TableExist] '+Trim(E.Message));
  end;
 finally
  if IBSQL.Open then IBSQL.Close;
  if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Commit;
 end;
end;

function DB_ExecuteSQL(SQL_: String; IBSQL: TIBSQL; Commit_: boolean; ErrDesc: ShortString): Boolean;
begin
 Result := False;
 try
  if (IBSQL = nil) then raise EAbort.Create('Query object is not defined.');

  if IBSQL.Open then IBSQL.Close;
  IBSQL.SQL.Text := SQL_;

  if not IBSQL.Transaction.InTransaction then IBSQL.Transaction.StartTransaction;

  IBSQL.Prepare; IBSQL.ExecQuery;

  if (Commit_) and (IBSQL.Transaction.InTransaction) then IBSQL.Transaction.Commit;

  Result := True;
 except
  on E: Exception do
   begin
    if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Rollback;
    if IBSQL.Open then IBSQL.Close;
    DataMod.PostException('[ExecSQL]['+ErrDesc+'] '+Trim(E.Message));
    while IBSQL.SQL.Count > 0 do
     begin
      DataMod.PostException('[SQL]: '+IBSQL.SQL.Strings[0]);
      IBSQL.SQL.Delete(0);
     end;
   end;
 end;
end;

function DB_GeneratorExist(GenName: String; IBSQL: TIBSQL): Boolean;
begin
 Result := False;
 try
  try
   if IBSQL.Open then IBSQL.Close;
   IBSQL.SQL.Clear;
   IBSQL.SQL.Add('SELECT COUNT(RDB$GENERATORS.RDB$GENERATOR_NAME) "FLDCNT"');
   IBSQL.SQL.Add('FROM RDB$GENERATORS');
   IBSQL.SQL.Add('WHERE (RDB$GENERATORS.RDB$SYSTEM_FLAG IS NULL)');
   IBSQL.SQL.Add('AND (UPPER(RDB$GENERATORS.RDB$GENERATOR_NAME) = UPPER('+QuotedStr(GenName)+'))');

   if not IBSQL.Transaction.InTransaction then IBSQL.Transaction.StartTransaction;
   IBSQL.Prepare; IBSQL.ExecQuery;
   Result := (IBSQL.FieldByName('FLDCNT').AsInteger > 0);
  except
  on E: Exception do
    DataMod.PostException('[GenExist] '+Trim(E.Message));
  end;
 finally
  if IBSQL.Open then IBSQL.Close;
  if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Commit;
 end;
end;

function DB_ExtractGenerator(GenName: String; Increment: Boolean; IBSQL: TIBSQL): integer;
begin
 Result := -1;
 if DB_ExecuteSQL('SELECT GEN_ID('+GenName+','+IntToStr(BoolToInt_(Increment))+') "GENVALUE" '+
                  'FROM RDB$DATABASE', IBSQL, True, 'ExtractGen') then
   Result := IBSQL.FieldByName('GENVALUE').Value;
end;

procedure DB_CommitIBSQL(IBSQL: TIBSQL);
begin
 if (IBSQL <> nil) then
  begin
   if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Commit;
  end;
end;

procedure DB_RollbackIBSQL(IBSQL: TIBSQL);
begin
 if (IBSQL <> nil) then
  begin
   if IBSQL.Transaction.InTransaction then IBSQL.Transaction.Rollback;
  end;
end;

end.
