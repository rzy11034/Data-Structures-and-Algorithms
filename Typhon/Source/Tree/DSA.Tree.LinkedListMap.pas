unit DSA.Tree.LinkedListMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.Interfaces.Comparer,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type

  { TLinkedListMap }

  generic TLinkedListMap<K, V, TKeyComparer> =
    class(TInterfacedObject, specialize IMap<K, V>)
  private
    type

    { TNode }

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

    TPtrV = specialize TPtr_V<V>;

  var
    __dummyHead: TNode;
    __size: integer;
    __comparer: specialize IDSA_Comparer<K>;

    function __getNode(Key: K): TNode;

  public
    constructor Create;
    function Contains(Key: K): boolean;
    function Get(Key: K): TPtrV;
    function GetSize: integer;
    function IsEmpty: boolean;
    function Remove(Key: K): TPtrV;
    procedure Add(Key: K; Value: V);
    procedure Set_(Key: K; Value: V);
  end;

procedure Main();

implementation

type
  TLinkedListMap_str_int = specialize TLinkedListMap<string,
    integer, TComparer_str>;

procedure Main();
var
  words: TArrayList_str;
  map: TLinkedListMap_str_int;
  i, n: integer;
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
    begin
      n := map.Get(words[i]).PValue^;
      map.Set_(words[i], n + 1);
    end
    else
      map.Add(words[i], 1);
  end;

  Writeln('Total different words: ', map.GetSize);
  TDsaUtils.DrawLine;
  Writeln('Frequency of pride: ', map.Get('pride').PValue^);
  Writeln('Frequency of prejudice: ', map.Get('prejudice').PValue^);
end;

{ TLinkedListMap }

constructor TLinkedListMap.Create;
begin
  __comparer := TKeyComparer.Default;
  __dummyHead := TNode.Create();
  __size := 0;
end;

function TLinkedListMap.Contains(Key: K): boolean;
begin
  Result := __getNode(Key) <> nil;
end;

function TLinkedListMap.Get(Key: K): TPtrV;
var
  node: TNode;
begin
  node := __getNode(Key);

  if node = nil then
    Result.PValue := nil
  else
    Result.PValue := @node.Value;
end;

function TLinkedListMap.GetSize: integer;
begin
  Result := __size;
end;

function TLinkedListMap.__getNode(Key: K): TNode;
var
  cur: TNode;
begin
  cur := __dummyHead.Next;

  while cur <> nil do
  begin
    if __comparer.Compare(cur.Key, Key) = 0 then
      Exit(cur);

    cur := cur.Next;
  end;
  Result := nil;
end;

function TLinkedListMap.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

function TLinkedListMap.Remove(Key: K): TPtrV;
var
  prev, delNode: TNode;
begin
  prev := __dummyHead;

  while prev.Next <> nil do
  begin
    if __comparer.Compare(prev.Key, key) = 0 then
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

procedure TLinkedListMap.Add(Key: K; Value: V);
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

procedure TLinkedListMap.Set_(Key: K; Value: V);
var
  node: TNode;
  Val: Tvalue;
begin
  node := __getNode(Key);

  if node = nil then
  begin
    TValue.Make(@Key, TypeInfo(K), Val);
    raise Exception.Create(Val.ToString + ' doesn''t exist!');
  end;

  node.Value := Value;
end;

{ TLinkedListMap.TNode }

constructor TLinkedListMap.TNode.Create(newKey: K; newValue: V; newNext: TNode);
begin
  Self.Key := newKey;
  Self.Value := newValue;
  Self.Next := newNext;
end;

constructor TLinkedListMap.TNode.Create(newKey: K);
begin
  Self.Create(newKey, default(V), nil);
end;

constructor TLinkedListMap.TNode.Create;
begin
  Self.Create(default(K));
end;

function TLinkedListMap.TNode.ToString: string;
var
  value_key, value_value: TValue;
  str_key, str_value: string;
begin
  value_key := TValue.From(Key);
  value_value := TValue.From(Value);

  if not (value_key.IsObject) then
    str_key := value_key.ToString
  else
    str_key := value_key.AsObject.ToString;

  if not (value_value.IsObject) then
    str_value := value_value.ToString
  else
    str_value := value_value.AsObject.ToString;

  Result := str_key + ' : ' + str_value;
end;

end.
