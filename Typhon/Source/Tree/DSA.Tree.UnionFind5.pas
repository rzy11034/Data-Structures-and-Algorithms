// 第5版Union-Find
unit DSA.Tree.UnionFind5;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  TUnionFind5 = class(TInterfacedObject, IUnionFind)
  private
    __parent: TArray_int;
    /// <summary> __rank[i]表示以i为根的集合所表示的树的层数 </summary>
    __rank: TArray_int;
    /// <summary> 查找元素p所对应的集合编号 </summary>
    function _Find(p: integer): integer;
  public
    constructor Create(size: integer);
    function GetSize: integer;
    /// <summary> 查看元素p和元素q是否所属一个集合 </summary>
    function IsConnected(p, q: integer): boolean;
    /// <summary> 合并元素p和元素q所属的集合 </summary>
    procedure UnionElements(p, q: integer);
  end;

implementation

{ TUnionFind2 }

constructor TUnionFind5.Create(size: integer);
var
  i: integer;
begin
  SetLength(__parent, size);
  SetLength(__rank, size);
  for i := 0 to size - 1 do
  begin
    __parent[i] := i;
    __rank[i] := 1;
  end;
end;

function TUnionFind5.GetSize: integer;
begin
  Result := Length(__parent);
end;

function TUnionFind5.IsConnected(p, q: integer): boolean;
begin
  Result := _Find(p) = _Find(q);
end;

procedure TUnionFind5.UnionElements(p, q: integer);
var
  pRoot, qRoot: integer;
begin
  pRoot := _Find(p);
  qRoot := _Find(q);

  if pRoot = qRoot then
    Exit;

  // 根据两个元素所在树的元素个数不同判断合并方向
  // 将rank低的集合合并到rank高的集合上
  if __rank[pRoot] < __rank[qRoot] then
  begin
    __parent[pRoot] := qRoot;
  end
  else if __rank[qRoot] < __rank[pRoot] then
  begin
    __parent[qRoot] := __parent[pRoot];
  end
  else // __rank[qRoot] = __rank[pRoot]
  begin
    __parent[qRoot] := pRoot;
    __rank[pRoot] := __rank[pRoot] + 1;
  end;
end;

function TUnionFind5._Find(p: integer): integer;
begin
  if (p < 0) and (p >= Length(__parent)) then
    raise Exception.Create('p is out of bound.');

  while p <> __parent[p] do
  begin
    __parent[p] := __parent[__parent[p]];
    p := __parent[p];
  end;

  Result := p;
end;

end.
