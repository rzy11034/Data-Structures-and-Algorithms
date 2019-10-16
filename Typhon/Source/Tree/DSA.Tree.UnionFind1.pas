// 第1版Union-Find
unit DSA.Tree.UnionFind1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure;

type

  { TUnionFind1 }

  TUnionFind1 = class(TInterfacedObject, IUnionFind)
  private
    __id: specialize TArray<integer>;

    /// <summary> 查找元素p所对应的集合编号 </summary>
    function __find(p: integer): integer;

  public
    constructor Create(newSize: integer);
    function GetSize: integer;
    /// <summary> 查看元素p和元素q是否所属一个集合 </summary>
    function IsConnected(p, q: integer): boolean;
    /// <summary> 合并元素p和元素q所属的集合 </summary>
    procedure UnionElements(p, q: integer);
  end;

implementation

{ TUnionFind1 }

constructor TUnionFind1.Create(newSize: integer);
var
  i: integer;
begin
  SetLength(__id, newSize);

  for i := 0 to newSize - 1 do
    __id[i] := i;
end;

function TUnionFind1.GetSize: integer;
begin
  Result := Length(__id);
end;

function TUnionFind1.IsConnected(p, q: integer): boolean;
begin
  Result := __find(p) = __find(q);
end;

procedure TUnionFind1.UnionElements(p, q: integer);
var
  pid, qid, i: integer;
begin
  pid := __find(p);
  qid := __find(q);

  if pid = qid then
    Exit;

  for i := 0 to Length(__id) - 1 do
  begin
    if __id[i] = pid then
      __id[i] := qid;
  end;
end;

function TUnionFind1.__find(p: integer): integer;
begin
  if (p < 0) and (p >= Length(__id)) then
    raise Exception.Create('p is out of bound.');

  Result := __id[p];
end;

end.
