unit DSA.Tree.LinkedListMap;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.List_Stack_Queue.ArrayList;

type
  TLinkedListMap<K, V> = class(TInterfacedObject, IMap<K, V>)
  private type
    TNode = class
    public
      Key: K;
      Value: V;
      Next: TNode;
      constructor Create(newKey: K; newValue: V; newNext: TNode); overload;
      constructor Create(newKey: K); overload;
      constructor Create; overload;
      function ToString: string; override;
    end;

    TArrayList_K = TArrayList<K>;
    TComparer_K = TComparer<K>;
    TPtr_V = TPtr_V<V>;

  private
    __comparer_K: IComparer<K>;
    __dummyHead: TNode;
    __size: Integer;
    function __getNode(Key: K): TNode;
  public
    constructor Create;
    function Contains(Key: K): Boolean;
    function Get(Key: K): TPtr_V;
    function GetSize: Integer;
    function IsEmpty: Boolean;
    function Remove(Key: K): TPtr_V;
    procedure Add(Key: K; Value: V);
    procedure Set_(Key: K; Value: V);
    function KeySets: TArrayList_K;
  end;

procedure Main;

implementation

type
  TLinkedListMap_str_int = TLinkedListMap<string, Integer>;

procedure Main();
var
  words: TArrayList_str;
  map: TLinkedListMap_str_int;
  i: Integer;
begin
  words := TArrayList_str.Create();
  if TDsaUtils.ReadFile(FILE_PATH + A_FILE_NAME, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  map := TLinkedListMap_str_int.Create;

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

{ TLinkedListMap<K, V>.TNode }

constructor TLinkedListMap<K, V>.TNode.Create(newKey: K; newValue: V;
  newNext: TNode);
begin
  Self.Key := newKey;
  Self.Value := newValue;
  Self.Next := newNext;
end;

constructor TLinkedListMap<K, V>.TNode.Create(newKey: K);
begin
  Self.Create(newKey, default (V), nil);
end;

constructor TLinkedListMap<K, V>.TNode.Create;
begin
  Self.Create(default (K));
end;

function TLinkedListMap<K, V>.TNode.ToString: string;
var
  value_key, value_value: TValue;
  str_key, str_value: string;
begin
  value_key := TValue.From<K>(Key);
  value_value := TValue.From<V>(Value);

  if not(value_key.IsObject) then
    str_key := value_key.ToString
  else
    str_key := value_key.AsObject.ToString;

  if not(value_value.IsObject) then
    str_value := value_value.ToString
  else
    str_value := value_value.AsObject.ToString;

  Result := str_key + ' : ' + str_value;
end;

{ TLinkedListMap<K, V> }

procedure TLinkedListMap<K, V>.Add(Key: K; Value: V);
var
  node: TNode;
begin
  node := __getNode(Key);

  if node = nil then
  begin
    __dummyHead.Next := TNode.Create(Key, Value, __dummyHead.Next);
    Inc(__size);
  end
  else
  begin
    node.Value := Value;
  end;
end;

function TLinkedListMap<K, V>.Contains(Key: K): Boolean;
begin
  Result := __getNode(Key) <> nil;
end;

constructor TLinkedListMap<K, V>.Create;
begin
  __dummyHead := TNode.Create();
  __comparer_K := TComparer<K>.Default;
  __size := 0;
end;

function TLinkedListMap<K, V>.Get(Key: K): TPtr_V;
var
  node: TNode;
begin
  node := __getNode(Key);

  if node = nil then
  begin
    Writeln(TValue.From<K>(Key).ToString + ' doesn''t exist!');
    Result.PValue := nil;
  end
  else
    Result.PValue := @node.Value;
end;

function TLinkedListMap<K, V>.GetSize: Integer;
begin
  Result := __size;
end;

function TLinkedListMap<K, V>.IsEmpty: Boolean;
begin
  Result := __size = 0;
end;

function TLinkedListMap<K, V>.KeySets: TArrayList_K;
var
  cur: TNode;
  list: TArrayList_K;
begin
  cur := __dummyHead.Next;
  list := TArrayList_K.Create();

  while cur <> nil do
  begin
    list.AddLast(cur.Key);
    cur := cur.Next;
  end;

  Result := list;
end;

function TLinkedListMap<K, V>.Remove(Key: K): TPtr_V;
var
  prev, delNode: TNode;
begin
  prev := __dummyHead;

  while prev.Next <> nil do
  begin
    if __comparer_K.Compare(prev.Next.Key, Key) = 0 then
      Break;

    prev := prev.Next;
  end;

  if prev.Next <> nil then
  begin
    delNode := prev.Next;
    prev.Next := delNode.Next;
    Dec(__size);
    Result.PValue := @delNode.Value;
    FreeAndNil(delNode);
  end
  else
  begin
    Result.PValue := nil;
  end;
end;

procedure TLinkedListMap<K, V>.Set_(Key: K; Value: V);
var
  node: TNode;
begin
  node := __getNode(Key);

  if node = nil then
    raise Exception.Create(TValue.From<K>(Key).ToString + ' doesn''t exist!');

  node.Value := Value;
end;

function TLinkedListMap<K, V>.__getNode(Key: K): TNode;
var
  cur: TNode;
begin
  cur := __dummyHead.Next;

  while cur <> nil do
  begin
    if __comparer_K.Compare(cur.Key, Key) = 0 then
      Exit(cur);

    cur := cur.Next;
  end;

  Result := nil;
end;

end.
