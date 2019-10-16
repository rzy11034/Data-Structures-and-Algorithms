// 第6版Union-Find
unit DSA.Tree.UnionFind6;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  TUnionFind6 = class(TInterfacedObject, IUnionFind)
  private
    __parent: TArray_int;
    /// <summary> __rank[i]表示以i为根的集合所表示的树的层数 </summary>
    __rank: TArray_int;
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

constructor TUnionFind6.Create(size: Integer);
var
  i: Integer;
begin
  SetLength(__parent, size);
  SetLength(__rank, size);

  for i := 0 to size - 1 do
  begin
    __parent[i] := i;
    __rank[i] := 1;
  end;
end;

function TUnionFind6.GetSize: Integer;
begin
  Result := Length(__parent);
end;

function TUnionFind6.IsConnected(p, q: Integer): Boolean;
begin
  Result := __find(p) = __find(q);
end;

procedure TUnionFind6.UnionElements(p, q: Integer);
var
  pRoot, qRoot: Integer;
begin
  pRoot := __find(p);
  qRoot := __find(q);

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
  else // FRank[qRoot] = FRank[pRoot]
  begin
    __parent[qRoot] := pRoot;
    __rank[pRoot] := __rank[pRoot] + 1;
  end;
end;

function TUnionFind6.__find(p: Integer): Integer;
begin
  if (p < 0) and (p >= Length(__parent)) then
    raise Exception.Create('p is out of bound.');

  if p <> __parent[p] then
  begin
    __parent[p] := __find(__parent[p]);
  end;

  Result := __parent[p];
end;

end.
