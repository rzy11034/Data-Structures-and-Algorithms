// 第2版Union-Find
unit DSA.Tree.UnionFind2;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type

  { TUnionFind2 }

  TUnionFind2 = class(TInterfacedObject, IUnionFind)
  private
    __parent: TArray_int;

    /// <summary> 查找元素p所对应的集合编号，
    /// O(h)复杂度, h为树的高度.
    /// </summary>
    function __find(p: integer): integer;

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

constructor TUnionFind2.Create(size: integer);
var
  i: integer;
begin
  SetLength(__parent, size);

  for i := 0 to size - 1 do
    __parent[i] := i;
end;

function TUnionFind2.GetSize: integer;
begin
  Result := Length(__parent);
end;

function TUnionFind2.IsConnected(p, q: integer): boolean;
begin
  Result := __find(p) = __find(q);
end;

procedure TUnionFind2.UnionElements(p, q: integer);
var
  pRoot, qRoot: integer;
begin
  pRoot := __find(p);
  qRoot := __find(q);

  if pRoot = qRoot then
    Exit;

  __parent[pRoot] := qRoot;
end;

function TUnionFind2.__find(p: integer): integer;
begin
  if (p < 0) and (p >= Length(__parent)) then
    raise Exception.Create('p is out of bound.');

  while p <> __parent[p] do
    p := __parent[p];

  Result := p;
end;

end.
