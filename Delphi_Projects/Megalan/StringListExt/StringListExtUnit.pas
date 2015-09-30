unit StringListExtUnit;

interface

uses
  SysUtils,
  Classes;

type
  TStringListExt = class(TStringList)
  private
    FSpecialStrCnt            : Integer;
    function GetSpecialStrCnt : Integer;
    procedure SetSpecialStrCount(Value: Integer);
  public
    constructor Create;

    function Add(const sString: string): Integer; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const sString: string); override;
    procedure Clear; override;
  published
    property SpecialStrCnt: Integer read GetSpecialStrCnt write SetSpecialStrCount;
  end;

implementation

constructor TStringListExt.Create;
begin
  inherited Create;
  FSpecialStrCnt := 0;
end;

function TStringListExt.GetSpecialStrCnt: Integer;
begin
  Result := FSpecialStrCnt;
end;

procedure TStringListExt.SetSpecialStrCount(Value: Integer);
begin
  FSpecialStrCnt := Value;
end;

function IsStrSpecial(sString: string): Boolean;
begin
  Result := False;
  if (sString[1] in ['0'..'9']) then Result := True;
end;

function TStringListExt.Add(const sString: string): Integer;
begin
  if (IsStrSpecial(sString)) then
  begin
    SpecialStrCnt := SpecialStrCnt + 1;
  end;
  inherited Add(sString);
  Result := SpecialStrCnt;
end;

procedure TStringListExt.Delete(Index: Integer);
begin
  if (IsStrSpecial(Self.Get(Index))) then
  begin
    SpecialStrCnt := SpecialStrCnt - 1;
  end;
  inherited Delete(Index);
end;

procedure TStringListExt.Insert(Index: Integer; const sString: string);
begin
  if (IsStrSpecial(sString)) then
  begin
    SpecialStrCnt := SpecialStrCnt + 1;
  end;
  inherited Insert(Index, sString);
end;

procedure TStringListExt.Clear;
begin
  inherited Clear;
  SpecialStrCnt := 0;
end;

end.
