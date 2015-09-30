unit ItemList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Contnrs, fraItems;

  type
TItemsList = class(TObjectList)
  private
    function getItem(aindex: integer): TfrItems;
    procedure setItem(aindex: integer; const Value: TfrItems);
  public
    property items[aindex: integer] : TfrItems read getItem write setItem; default;
    function add(aitem:TfrItems): integer;
  end;

  
implementation

{ TItemsList }
function TItemsList.add(aitem: TfrItems): integer;
begin
  // This method not strictly necessary, but ensures that can only add TfrItems objects.
  Result := inherited Add(aitem);
end;

function TItemsList.getItem(aindex: integer): TfrItems;
begin
  result := inherited Items[aindex] as TfrItems;
end;

procedure TItemsList.setItem(aindex: integer; const Value: TfrItems);
begin
  inherited Items[aindex] := Value;
end;

end.
 