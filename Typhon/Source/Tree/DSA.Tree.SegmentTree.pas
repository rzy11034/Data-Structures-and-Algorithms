unit DSA.Tree.SegmentTree;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  rtti,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type

  { TSegmentTree }

  generic TSegmentTree<T> = class(TObject)
  private
    __data: array of T;
    __tree: array of T;
    __merger: specialize IMerger<T>;

    /// <summary> 返回完全二叉树的数组表示中，
    /// 一个索引所表示的元素的左孩子节点的索引
    /// </summary>
    function __leftChild(index: integer): integer;
    /// <summary> 返回完全二叉树的数组表示中，
    /// 一个索引所表示的元素的右孩子节点的索引
    /// </summary>
    function __rightChild(index: integer): integer;
    /// <summary> 在treeIndex的位置创建表示区间[l..r]的线段树 </summary>
    procedure __buildSegmentTree(treeIndex: integer; l, r: integer);
    /// <summary> 在以treeIndex为根的线段树中[l..r]的范围里，
    /// 搜索区[queryL..queryR]的值
    /// </summary>
    function __query(treeIndex, l, r, queryL, queryR: integer): T;
    procedure __set(treeIndex, l, r, index: integer; e: T);

  public
    constructor Create(arr: array of T; merger: specialize IMerger<T>);
    function Get(index: integer): T;
    function GetSize: integer;
    /// <summary> 返回区间[queryL..queryR]的值 </summary>
    function Query(queryL, queryR: integer): T;
    /// <summary> 将index位置的值，更新为e </summary>
    procedure Set_(index: integer; e: T);
    function ToString(): string; override;
  end;

  { TMerger }

  TMerger = class(TInterfacedObject, specialize IMerger<integer>)
  public
    function Merge(a, b: integer): integer;
  end;

procedure Main;

implementation

type
  TsegmentTree_int = specialize TSegmentTree<integer>;

procedure Main;
var
  nums: TArray_int;
  SegmentTree: TsegmentTree_int;
begin
  nums := [-2, 0, 3, -5, 2, -1];

  SegmentTree := TsegmentTree_int.Create(nums, TMerger.Create);
  Writeln(SegmentTree.ToString);

  Writeln(SegmentTree.Query(0, 2));
  Writeln(SegmentTree.Query(2, 5));
  Writeln(SegmentTree.Query(0, 5));
end;

{ TMerger }

function TMerger.Merge(a, b: integer): integer;
begin
  Result := a + b;
end;

{ TSegmentTree }

constructor TSegmentTree.Create(arr: array of T; merger: specialize IMerger<T>);
var
  i: integer;
begin
  SetLength(__data, Length(arr));
  for i := 0 to Length(arr) - 1 do
    __data[i] := arr[i];

  Self.__merger := merger;

  SetLength(__tree, GetSize * 4);
  __buildSegmentTree(0, 0, GetSize - 1);
end;

function TSegmentTree.Get(index: integer): T;
begin
  if (index < 0) or (index >= GetSize) then
    raise Exception.Create('Index is illegal.');

  Result := __data[index];
end;

function TSegmentTree.GetSize: integer;
begin
  Result := Length(__data);
end;

function TSegmentTree.Query(queryL, queryR: integer): T;
begin
  if (queryL < 0) or (queryL >= GetSize) or (queryR < 0) or
    (queryR >= GetSize) or (queryL > queryR) then
    raise Exception.Create('Index is illegar.');

  Result := __query(0, 0, GetSize - 1, queryL, queryR);
end;

procedure TSegmentTree.Set_(index: integer; e: T);
begin
  if (index < 0) or (index >= GetSize) then
    raise Exception.Create('Index is illegar.');

  __data[index] := e;
  __set(0, 0, GetSize - 1, index, e);
end;

function TSegmentTree.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  Value: TValue;
begin
  res := TStringBuilder.Create;
  try
    res.Append('[');

    for i := 0 to Length(Self.__tree) - 1 do
    begin
      TValue.Make(@__tree[i], TypeInfo(T), Value);
      if Value.ToString <> '0' then
        res.Append(Value.ToString).Append(' ')
      else
        res.Append('N').Append(' ');
    end;

    if i >= Length(Self.__tree) - 1 then
      res.Append(']').AppendLine;

    Result := res.ToString;
  finally
    res.Free;
  end;
end;

procedure TSegmentTree.__buildSegmentTree(treeIndex, l, r: integer);
var
  mid, leftTreeIndex, rightTreeIndex: integer;
begin
  if l = r then
  begin
    __tree[treeIndex] := __data[l];
    Exit;
  end;

  leftTreeIndex := __leftChild(treeIndex);
  rightTreeIndex := __rightChild(treeIndex);
  mid := l + (r - l) div 2;
  __buildSegmentTree(leftTreeIndex, l, mid);
  __buildSegmentTree(rightTreeIndex, mid + 1, r);
  __tree[treeIndex] := __merger.Merge(__tree[leftTreeIndex],
    __tree[rightTreeIndex]);
end;

function TSegmentTree.__leftChild(index: integer): integer;
begin
  Result := index * 2 + 1;
end;

function TSegmentTree.__query(treeIndex, l, r, queryL, queryR: integer): T;
var
  mid, leftTreeIndex, rightTreeIndex: integer;
  leftRes, rightRes: T;
begin
  if (l = queryL) and (r = queryR) then
  begin
    Result := __tree[treeIndex];
    Exit;
  end;

  leftTreeIndex := __leftChild(treeIndex);
  rightTreeIndex := __rightChild(treeIndex);
  mid := l + (r - l) div 2;

  if queryL >= mid + 1 then
    Result := __query(rightTreeIndex, mid + 1, r, queryL, queryR)
  else if queryR <= mid then
    Result := __query(leftTreeIndex, l, mid, queryL, queryR)
  else
  begin
    leftRes := __query(leftTreeIndex, l, mid, queryL, mid);
    rightRes := __query(rightTreeIndex, mid + 1, r, mid + 1, queryR);
    Result := __merger.Merge(leftRes, rightRes);
  end;
end;

function TSegmentTree.__rightChild(index: integer): integer;
begin
  Result := index * 2 + 2;
end;

procedure TSegmentTree.__set(treeIndex, l, r, index: integer; e: T);
var
  mid, leftTreeIndex, rightTreeIndex: integer;
begin
  if l = r then
  begin
    __tree[treeIndex] := e;
    Exit;
  end;

  leftTreeIndex := __leftChild(treeIndex);
  rightTreeIndex := __rightChild(treeIndex);
  mid := l + (r - l) div 2;

  if index >= mid + 1 then
    __set(rightTreeIndex, mid + 1, r, index, e)
  else
    __set(leftTreeIndex, l, mid, index, e);

  __tree[treeIndex] := __merger.Merge(__tree[leftTreeIndex],
    __tree[rightTreeIndex]);
end;

end.
