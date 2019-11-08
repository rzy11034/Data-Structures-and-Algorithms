unit DSA.Tree.BSTMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.Interfaces.Comparer,
  DSA.Interfaces.DataStructure,
  DSA.Utils,
  DSA.List_Stack_Queue.ArrayList;

type

  { TBSTMap }

  generic TBSTMap<K, V, TKeyComparer> = class(TInterfacedObject,
    specialize IMap<K, V>)
  private
    type

    { TNode }

    TNode = class
    public
      key: K;
      Value: V;
      Left, Right: TNode;
      constructor Create(newkey: K; newValue: V);
    end;

    TPtrV = specialize TPtr_V<V>;
    TArrayList_K = specialize TArrayList<K>;

  var
    __root: TNode;
    __size: integer;
    __comparer: specialize IDSA_Comparer<K>;

    function __add(node: TNode; key: K; Value: V): TNode;
    function __getNode(node: TNode; key: K): TNode;
    function __removeMin(node: TNode): TNode;
    function __remove(node: TNode; key: K): TNode;
    function __minimum(node: TNode): TNode;
    procedure __inOrder(node: TNode; list: TArrayList_K);

  public
    constructor Create();
    function Contains(key: K): boolean;
    function Get(key: K): TPtrV;
    function GetSize: integer;
    function IsEmpty: boolean;
    function Remove(key: K): TPtrV;
    procedure Add(key: K; Value: V);
    procedure Set_(key: K; newValue: V);
    function KeySets: TArrayList_K;

  end;

procedure Main;

implementation

type
  TBSTMap_str_int = specialize TBSTMap<string, integer, TComparer_str>;

procedure Main();
var
  words: TArrayList_str;
  map: TBSTMap_str_int;
  i: integer;
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

{ TBSTMap }

procedure TBSTMap.Add(key: K; Value: V);
begin
  __root := __add(__root, key, Value);
end;

function TBSTMap.Contains(key: K): boolean;
begin
  Result := __getNode(__root, key) <> nil;
end;

constructor TBSTMap.Create();
begin
  inherited;
  __comparer := TKeyComparer.Default;
end;

function TBSTMap.Get(key: K): TPtrV;
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
    Result.PValue := nil
  else
    Result.PValue := @node.Value;
end;

function TBSTMap.GetSize: integer;
begin
  Result := __size;
end;

function TBSTMap.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

function TBSTMap.Remove(key: K): TPtrV;
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

procedure TBSTMap.Set_(key: K; newValue: V);
var
  node: TNode;
  Value: TValue;
  s: string;
begin
  node := __getNode(__root, key);

  if node = nil then
  begin
    TValue.Make(@key, TypeInfo(K), Value);
    s := Value.ToString;
    raise Exception.Create(s + ' doesn''t exist!');
  end
  else
    node.Value := newValue;
end;

function TBSTMap.KeySets: TArrayList_K;
var
  list: TArrayList_K;
begin
  list := TArrayList_K.Create;
  __inOrder(__root, list);
  Result := list;
end;

function TBSTMap.__add(node: TNode; key: K; Value: V): TNode;
var
  bool: integer;
begin
  if node = nil then
  begin
    Inc(__size);
    Result := TNode.Create(key, Value);
    Exit;
  end;

  bool := __comparer.Compare(Key, node.Key);
  if bool < 0 then
    node.Left := __add(node.Left, key, Value)
  else if bool > 0 then
    node.Right := __add(node.Right, key, Value)
  else
    node.Value := Value;

  Result := node;
end;

function TBSTMap.__getNode(node: TNode; key: K): TNode;
var
  bool: integer;
begin
  if node = nil then
    Exit(nil);

  bool := __comparer.Compare(Key, node.Key);
  if bool < 0 then
    Result := __getNode(node.Left, key)
  else if bool > 0 then
    Result := __getNode(node.Right, key)
  else
    Result := node;
end;

function TBSTMap.__minimum(node: TNode): TNode;
begin
  if node.Left = nil then
  begin
    Result := node;
  end
  else
  begin
    Result := __minimum(node.Left);
  end;
end;

procedure TBSTMap.__inOrder(node: TNode; list: TArrayList_K);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node.key);
  __inOrder(node.Right, list);
end;

function TBSTMap.__remove(node: TNode; key: K): TNode;
var
  leftNode, rightNode, succesorNode, minNode: TNode;
  bool: integer;
begin
  leftNode := nil;
  rightNode := nil;
  succesorNode := nil;

  if node = nil then
    Exit(nil);

  bool := __comparer.Compare(Key, node.Key);
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

function TBSTMap.__removeMin(node: TNode): TNode;
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

{ TBSTMap.TNode }

constructor TBSTMap.TNode.Create(newkey: K; newValue: V);
begin
  Self.key := newkey;
  Self.Value := newValue;
  Left := nil;
  Right := nil;
end;

end.
