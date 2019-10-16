unit DSA.Tree.BSTMap;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.List_Stack_Queue.ArrayList;

type
  TBSTMap<K, V> = class(TInterfacedObject, IMap<K, V>)
  private type
    TNode = class
    public
      key: K;
      Value: V;
      Left, Right: TNode;
      constructor Create(newkey: K; newValue: V);
    end;

    TArray_K = TArray<K>;
    TArrayList_K = TArrayList<K>;
    TComparer_K = TComparer<K>;
    TPtr_V = TPtr_V<V>;

  var
    __root: TNode;
    __size: Integer;
    __comparer_K: IComparer<K>;

    function __add(node: TNode; key: K; Value: V): TNode;
    function __getNode(node: TNode; key: K): TNode;
    function __removeMin(node: TNode): TNode;
    function __remove(node: TNode; key: K): TNode;
    function __minimum(node: TNode): TNode;
    procedure __inOrder(node: TNode; list: TArrayList_K);

  public
    constructor Create();
    function Contains(key: K): Boolean;
    function Get(key: K): TPtr_V;
    function GetSize: Integer;
    function IsEmpty: Boolean;
    function Remove(key: K): TPtr_V;
    procedure Add(key: K; Value: V);
    procedure Set_(key: K; newValue: V);
    function KeySets: TArrayList_K;
  end;

procedure Main;

implementation

type
  TBSTMap_str_int = TBSTMap<string, Integer>;

procedure Main();
var
  words: TArrayList_str;
  map: TBSTMap_str_int;
  i: Integer;
begin
  words := TArrayList_str.Create();
  if TDsaUtils.ReadFile(FILE_PATH + A_File_Name, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  map := TBSTMap_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if map.Contains(words[i]) then
      map.Set_(words[i], map.Get(words[i]).PValue^ + 1)
    else
      map.Add(words[i], 1);
  end;

  Writeln('Total different words: ', map.GetSize);
  TDsaUtils.DrawLine;
  Writeln('Frequency of pride: ', map.Get('pride').PValue^);
  Writeln('Frequency of prejudice: ', map.Get('prejudice').PValue^);
end;

{ TBSTMap<K, V>.TNode }

constructor TBSTMap<K, V>.TNode.Create(newkey: K; newValue: V);
begin
  Self.key := newkey;
  Self.Value := newValue;
  Left := nil;
  Right := nil;
end;

{ TBSTMap<K, V> }

procedure TBSTMap<K, V>.Add(key: K; Value: V);
begin
  __root := __add(__root, key, Value)
end;

function TBSTMap<K, V>.Contains(key: K): Boolean;
begin
  Result := __getNode(__root, key) <> nil;
end;

constructor TBSTMap<K, V>.Create;
begin
  inherited;
  __comparer_K := TComparer_K.Default;
end;

function TBSTMap<K, V>.Get(key: K): TPtr_V;
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
    Result.PValue := nil
  else
    Result.PValue := @node.Value;
end;

function TBSTMap<K, V>.GetSize: Integer;
begin
  Result := __size;
end;

function TBSTMap<K, V>.IsEmpty: Boolean;
begin
  Result := __size = 0;
end;

function TBSTMap<K, V>.KeySets: TArrayList_K;
var
  list: TArrayList_K;
  i: Integer;
  arr: TArray_K;
begin
  list := TArrayList_K.Create;
  __inOrder(__root, list);
  Result := list;
end;

function TBSTMap<K, V>.Remove(key: K): TPtr_V;
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
  begin
    Result.PValue := nil;
  end
  else
  begin
    __root := __remove(__root, key);
    Result.PValue := @node.Value;
  end;
end;

procedure TBSTMap<K, V>.Set_(key: K; newValue: V);
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
    raise Exception.Create(TValue.From<K>(key).ToString + ' doesn''t exist!');

  node.Value := newValue;
end;

function TBSTMap<K, V>.__add(node: TNode; key: K; Value: V): TNode;
begin
  if node = nil then
  begin
    Inc(__size);
    Result := TNode.Create(key, Value);
    Exit;
  end;

  if __comparer_K.Compare(key, node.key) < 0 then
    node.Left := __add(node.Left, key, Value)
  else if __comparer_K.Compare(key, node.key) > 0 then
    node.Right := __add(node.Right, key, Value)
  else
    node.Value := Value;

  Result := node;
end;

function TBSTMap<K, V>.__getNode(node: TNode; key: K): TNode;
begin
  if node = nil then
    Exit(nil);

  if __comparer_K.Compare(key, node.key) < 0 then
    Result := __getNode(node.Left, key)
  else if __comparer_K.Compare(key, node.key) > 0 then
    Result := __getNode(node.Right, key)
  else
    Result := node;
end;

procedure TBSTMap<K, V>.__inOrder(node: TNode; list: TArrayList_K);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node.key);
  __inOrder(node.Right, list);
end;

function TBSTMap<K, V>.__minimum(node: TNode): TNode;
begin
  if node.Left = nil then
  begin
    Result := node;
  end
  else
  begin
    Result := __minimum(node.Left)
  end;
end;

function TBSTMap<K, V>.__remove(node: TNode; key: K): TNode;
var
  leftNode, rightNode, succesorNode, minNode: TNode;
  bool: Integer;
begin
  leftNode := nil;
  rightNode := nil;
  succesorNode := nil;

  if node = nil then
    Exit(nil);

  bool := __comparer_K.Compare(key, node.key);
  if bool < 0 then
  begin
    node.Left := __remove(node.Left, key);
    Result := node;
  end
  else if bool > 0 then
  begin
    node.Right := __remove(node.Right, key);
    Result := node;
  end
  else // e = node.e
  begin
    if node.Left = nil then
    begin
      rightNode := node.Right;
      FreeAndNil(node);
      Dec(__size);
      Result := rightNode;
    end
    else if node.Right = nil then
    begin
      leftNode := node.Left;
      FreeAndNil(node);
      Dec(__size);
      Result := leftNode;
    end
    else
    begin
      // 待删除节点左右子树均不空的情况
      // 找到比待删除节点大的最小节点，即待删除节点右子树的最小节点
      // 用这个节点顶替待删除节点的位置
      minNode := __minimum(node.Right);
      succesorNode := TNode.Create(minNode.key, minNode.Value);
      succesorNode.Right := __removeMin(node.Right);
      succesorNode.Left := node.Left;
      FreeAndNil(node);
      Result := succesorNode;
    end;
  end;
end;

function TBSTMap<K, V>.__removeMin(node: TNode): TNode;
var
  rightNode: TNode;
begin
  if node.Left = nil then
  begin
    rightNode := node.Right;
    FreeAndNil(node);
    Dec(__size);
    Result := rightNode;
  end
  else
  begin
    node.Left := __removeMin(node.Left);
    Result := node;
  end;

end;

end.
