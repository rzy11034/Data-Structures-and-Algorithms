// 第3版Union-Find
unit DSA.Tree.UnionFind3;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure;

type
  TUnionFind3 = class(TInterfacedObject, IUnionFind)
  private
    __parent: TArray<Integer>;
    /// <summary> sz[i]表示以i为根的集合中元素个数 </summary>
    __sz: TArray<Integer>;

    /// <summary> 查找元素p所对应的集合编号 </summary>
    function __find(p: Integer): Integer;

  public
    constructor Create(size: Integer);
    function GetSize: Integer;
    /// <summary> 查看元素p和元素q是否所属一个集合 </summary>
    function IsConnected(p, q: Integer): Boolean;
    /// <summary> 合并元素p和元素q所属的集合 </summary>
    procedure UnionElements(p, q: Integer);
  end;

implementation

{ TUnionFind2 }

constructor TUnionFind3.Create(size: Integer);
var
  i: Integer;
begin
  SetLength(__parent, size);
  SetLength(__sz, size);

  for i := 0 to size - 1 do
  begin
    __parent[i] := i;
    __sz[i] := 1;
  end;
end;

function TUnionFind3.GetSize: Integer;
begin
  Result := Length(__parent);
end;

function TUnionFind3.IsConnected(p, q: Integer): Boolean;
begin
  Result := __find(p) = __find(q);
end;

procedure TUnionFind3.UnionElements(p, q: Integer);
var
  pRoot, qRoot: Integer;
begin
  pRoot := __find(p);
  qRoot := __find(q);

  if pRoot = qRoot then
    Exit;

  // 根据两个元素所在树的元素个数不同判断合并方向
  // 将元素个数少的集合合并到元素个数多的集合上
  if __sz[pRoot] < __sz[qRoot] then
  begin
    __parent[pRoot] := qRoot;
    __sz[qRoot] := __sz[qRoot] + __sz[pRoot];
  end
  else
  begin
    __parent[qRoot] := pRoot;
    __sz[pRoot] := __sz[pRoot] + __sz[qRoot];
  end;
end;

function TUnionFind3.__find(p: Integer): Integer;
begin
  if (p < 0) and (p >= Length(__parent)) then
    raise Exception.Create('p is out of bound.');

  while p <> __parent[p] do
    p := __parent[p];

  Result := p;
end;

end.
