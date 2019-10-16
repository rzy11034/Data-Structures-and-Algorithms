unit DSA.Hash.HashTable;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.Tree.AVLTreeMap,
  DSA.Tree.BSTMap,
  DSA.Tree.RBTree,
  DSA.Utils;

type

  THashTable<K, V> = class
  private type
    TTreeMap_K_V = TAVLTreeMap<K, V>;
    TArray_treeMap = array of TTreeMap_K_V;
    TPtrV = TPtr_V<V>;

  const
    UPPER_TOL = 10; // 容量上界
    LOWER_TOL = 2; // 容量下界
    INIT_CAPCITY = 7; // 初始容量

  var
    __hashTable: TArray_treeMap;
    __capcity: Integer;
    __size: Integer;

    function __hash(key: K): Integer;
    procedure __resize(newCapcity: Integer);

  public
    constructor Create(newCapcity: Integer = INIT_CAPCITY);
    destructor Destroy; override;

    function GetSize(): Integer;
    procedure Add(key: K; value: V);
    function Remove(key: K): V;
    procedure Set_(key: K; value: V);
    function Contains(key: K): Boolean;
    function Get(key: K): TPtrV;
  end;

procedure Main;

implementation

type
  TBSTMap_str_int = TBSTMap<string, Integer>;
  TAVLTree_str_int = TAVLTreeMap<string, Integer>;
  TRBTree_str_int = TRBTree<string, Integer>;
  THashTable_str_int = THashTable<string, Integer>;
  TDictionary_str_int = TDictionary<string, Integer>;

procedure Main;
var
  startTime, endTime: cardinal;
  words: TArrayList_str;
  i: Integer;
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
  startTime := TThread.GetTickCount;
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

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('BST: ', vTime, ' s');

  // AVLTree
  startTime := TThread.GetTickCount;
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

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('AVLTree: ', vTime, ' s');

  // RBTree
  startTime := TThread.GetTickCount;
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

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('RBTree: ', vTime, ' s');

  // HashTable
  startTime := TThread.GetTickCount;
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

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('HashTable: ', vTime, ' s');

  // Dictionary
  startTime := TThread.GetTickCount;
  dtn := TDictionary_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if dtn.ContainsKey(words[i]) then
      dtn.AddOrSetValue(words[i], dtn.Items[words[i]] + 1)
    else
      dtn.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    dtn.ContainsKey(words[i]);

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('Dictionary: ', vTime, ' s');
end;

{ THashTable<K, V> }

procedure THashTable<K, V>.Add(key: K; value: V);
var
  map: TTreeMap_K_V;
  hc: Integer;
begin
  hc := __hash(key);
  map := __hashTable[hc];

  if map.Contains(key) then
  begin
    map.Set_(key, value);
  end
  else
  begin
    map.Add(key, value);
    Inc(__size);

    if __size >= UPPER_TOL * __capcity then
      __resize(2 * __capcity);
  end;
end;

function THashTable<K, V>.Contains(key: K): Boolean;
begin
  Result := __hashTable[__hash(key)].Contains(key);
end;

constructor THashTable<K, V>.Create(newCapcity: Integer);
var
  i: Integer;
begin
  __capcity := newCapcity;
  __size := 0;
  SetLength(__hashTable, newCapcity);

  for i := 0 to newCapcity - 1 do
    __hashTable[i] := TTreeMap_K_V.Create;
end;

destructor THashTable<K, V>.Destroy;
var
  i: Integer;
begin
  for i := 0 to __capcity - 1 do
    __hashTable[i].Free;

  inherited;
end;

function THashTable<K, V>.Get(key: K): TPtrV;
begin
  Result := __hashTable[__hash(key)].Get(key);
end;

function THashTable<K, V>.GetSize: Integer;
begin
  Result := __size;
end;

function THashTable<K, V>.Remove(key: K): V;
var
  index: Integer;
  ret: V;
begin
  index := __hash(key);
  ret := default (V);

  if __hashTable[index].Contains(key) then
  begin
    __hashTable[index].Remove(key);
    Dec(__size);

    if (__size < LOWER_TOL * __capcity) and ((__capcity div 2) >= INIT_CAPCITY)
    then
      __resize(__capcity div 2);
  end;

  Result := ret;
end;

procedure THashTable<K, V>.Set_(key: K; value: V);
var
  map: TTreeMap_K_V;
begin
  map := __hashTable[__hash(key)];

  if not map.Contains(key) then
    raise Exception.Create('key doesn''t exist.');

  map.Set_(key, value);
end;

function THashTable<K, V>.__hash(key: K): Integer;
var
  value: TValue;
begin
  TValue.Make(@key, TypeInfo(K), value);
  Result := (value.ToString.GetHashCode and $7FFFFFFF) mod __capcity;
end;

procedure THashTable<K, V>.__resize(newCapcity: Integer);
var
  newHashTable: TArray_treeMap;
  i, oldCapcity: Integer;
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
