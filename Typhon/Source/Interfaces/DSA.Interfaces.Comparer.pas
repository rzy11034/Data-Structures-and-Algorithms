unit DSA.Interfaces.Comparer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  generic IDSA_Comparer<T> = interface
    ['{1887062D-EFA5-43F7-99E4-F26EF191F5A2}']
    function Compare(const left, right: T): integer;
  end;

  generic TComparer<T> = class(TInterfacedObject, specialize IDSA_Comparer<T>)
  public
    constructor Default;
    function Compare(const left, right: T): integer;
  end;

implementation

{ TComparer }

constructor TComparer.Default;
begin
  inherited Create;
end;

function TComparer.Compare(const Left, Right: T): integer;
var
  bool: integer;
begin
  if left > right then
    bool := 1
  else if left < right then
    bool := -1
  else
    bool := 0;

  Result := bool;
end;

end.
