unit DSA.Hash.HashTable;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Rtti,
  DSA.Interfaces.DataStructure,
  DSA.Tree.AVLTreeMap,
  DSA.Tree.BSTMap,
  DSA.Tree.RBTree,
  DSA.Utils;

type

  { THashTable }

  generic THashTable<K, V, TKeyCmp> = class
  private
    type
    TTreeMap_K_V = specialize TAVLTreeMap<K, V, TKeyCmp>;
    TArray_treeMap = array of TTreeMap_K_V;
    TPtrV = specialize TPtr_V<V>;

  const
    UPPER_TOL = 10; // 容量上界
    LOWER_TOL = 2; // 容量下界
    INIT_CAPCITY = 7; // 初始容量

  var
    __hashTable: TArray_treeMap;
    __capcity: integer;
    __size: integer;

    function __hash(key: K): integer;
    procedure __resize(newCapcity: integer);

  public
    constructor Create(newCapcity: integer = INIT_CAPCITY);
    destructor Destroy; override;

    function GetSize(): integer;
    procedure Add(key: K; Value: V);
    function Remove(key: K): V;
    procedure Set_(key: K; Value: V);
    function Contains(key: K): boolean;
    function Get(key: K): TPtrV;
  end;

procedure Main;

implementation

type
  TBSTMap_str_int = specialize TBSTMap<string, integer, TComparer_str>;
  TAVLTree_str_int = specialize TAVLTreeMap<string, integer, TComparer_str>;
  TRBTree_str_int = specialize TRBTree<string, integer, TComparer_str>;
  THashTable_str_int = specialize THashTable<string, integer, TComparer_str>;
  TDictionary_str_int = specialize TDictionary<string, integer>;

procedure Main;
var
  startTime, endTime: cardinal;
  words: TArrayList_str;
  i: integer;
  bst: TBSTMap_str_int;
  alt: TAVLTree_str_int;
  rbt: TRBTree_str_int;
  ht: THashTable_str_int;
  dtn: TDictionary_str_int;
  vTime: string;
begin
  words := TArrayList_str.Create;
  Writeln(A_FILE_NAME + ':');
  if TDsaUtils.ReadFile(FILE_PATH + A_FILE_NAME, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  // BSTMap
  startTime := TThread.GetTickCount64;
  bst := TBSTMap_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if bst.Contains(words[i]) then
      bst.Set_(words[i], bst.Get(words[i]).PValue^ + 1)
    else
      bst.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    bst.Contains(words[i]);

  endTime := TThread.GetTickCount64;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('BST: ', vTime, ' s');

  // AVLTree
  startTime := TThread.GetTickCount64;
  alt := TAVLTree_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if alt.Contains(words[i]) then
      alt.Set_(words[i], alt.Get(words[i]).PValue^ + 1)
    else
      alt.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    alt.Contains(words[i]);

  endTime := TThread.GetTickCount64;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('AVLTree: ', vTime, ' s');

  // RBTree
  startTime := TThread.GetTickCount64;
  rbt := TRBTree_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if rbt.Contains(words[i]) then
      rbt.Set_(words[i], rbt.Get(words[i]).PValue^ + 1)
    else
      rbt.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    rbt.Contains(words[i]);

  endTime := TThread.GetTickCount64;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('RBTree: ', vTime, ' s');

  // HashTable
  startTime := TThread.GetTickCount64;
  ht := THashTable_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if ht.Contains(words[i]) then
      ht.Set_(words[i], ht.Get(words[i]).PValue^ + 1)
    else
      ht.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    ht.Contains(words[i]);

  endTime := TThread.GetTickCount64;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('HashTable: ', vTime, ' s');

  // Dictionary
  startTime := TThread.GetTickCount64;
  dtn := TDictionary_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if dtn.containsKey(words[i]) then
    //dtn.KeyData[words[i]] := 1d
    else
      dtn.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    dtn.containsKey(words[i]);

  endTime := TThread.GetTickCount64;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('Dictionary: ', vTime, ' s');
end;

{ THashTable }

procedure THashTable.Add(key: K; Value: V);
var
  map: TTreeMap_K_V;
  hc: integer;
begin
  hc := __hash(key);
  map := __hashTable[hc];

  if map.Contains(key) then
  begin
    map.Set_(key, Value);
  end
  else
  begin
    map.Add(key, Value);
    Inc(__size);

    if __size >= UPPER_TOL * __capcity then
      __resize(2 * __capcity);
  end;
end;

function THashTable.Contains(key: K): boolean;
begin
  Result := __hashTable[__hash(key)].Contains(key);
end;

constructor THashTable.Create(newCapcity: integer);
var
  i: integer;
begin
  __capcity := newCapcity;
  __size := 0;
  SetLength(__hashTable, newCapcity);

  for i := 0 to newCapcity - 1 do
    __hashTable[i] := TTreeMap_K_V.Create;
end;

destructor THashTable.Destroy;
var
  i: integer;
begin
  for i := 0 to __capcity - 1 do
    __hashTable[i].Free;

  inherited;
end;

function THashTable.Get(key: K): TPtrV;
begin
  Result := __hashTable[__hash(key)].Get(key);
end;

function THashTable.GetSize(): integer;
begin
  Result := __size;
end;

function THashTable.Remove(key: K): V;
var
  index: integer;
  ret: V;
begin
  index := __hash(key);
  ret := default(V);

  if __hashTable[index].Contains(key) then
  begin
    __hashTable[index].Remove(key);
    Dec(__size);

    if (__size < LOWER_TOL * __capcity) and ((__capcity div 2) >=
      INIT_CAPCITY) then
      __resize(__capcity div 2);
  end;

  Result := ret;
end;

procedure THashTable.Set_(key: K; Value: V);
var
  map: TTreeMap_K_V;
begin
  map := __hashTable[__hash(key)];

  if not map.Contains(key) then
    raise Exception.Create('key doesn''t exist.');

  map.Set_(key, Value);
end;

function THashTable.__hash(key: K): integer;
var
  Value: TValue;
begin
  TValue.Make(@key, TypeInfo(K), Value);
  Result := (Value.ToString.GetHashCode and $7FFFFFFF) mod __capcity;
end;

procedure THashTable.__resize(newCapcity: integer);
var
  newHashTable: TArray_treeMap;
  i, oldCapcity: integer;
  map: TTreeMap_K_V;
  key: K;
begin
  SetLength(newHashTable, newCapcity);

  for i := 0 to newCapcity - 1 do
    newHashTable[i] := TTreeMap_K_V.Create;

  oldCapcity := __capcity;
  Self.__capcity := newCapcity;

  for i := 0 to oldCapcity - 1 do
  begin
    map := __hashTable[i];

    for key in map.KeySets.ToArray do
    begin
      newHashTable[__hash(key)].Add(key, map.Get(key).PValue^);
    end;
  end;

  __hashTable := newHashTable;
end;

end.
