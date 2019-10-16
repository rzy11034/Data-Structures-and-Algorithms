unit DSA.Tree.AVLTreeMap;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.AVLTree,
  DSA.List_Stack_Queue.ArrayList;

type

  { TAVLTreeMap }

  TAVLTreeMap<K, V> = class(TInterfacedObject, IMap<K, V>)
  private type
    TAVLTree_K_V = TAVLTree<K, V>;
    TPtrV = TPtr_V<V>;
    TArrayList_K = TArrayList<K>;

  var
    __alt: TAVLTree_K_V;

  public
    constructor Create;
    destructor Destroy; override;

    function Contains(key: K): boolean;
    function Get(key: K): TPtrV;
    function GetSize: integer;
    function IsEmpty: boolean;
    function Remove(key: K): TPtrV;
    procedure Add(key: K; Value: V);
    procedure Set_(key: K; newValue: V);
    function KeySets: TArrayList_K;
  end;

implementation

{ TAVLTreeMap }

constructor TAVLTreeMap<K, V>.Create;
begin
  __alt := TAVLTree_K_V.Create();
end;

procedure TAVLTreeMap<K, V>.Add(key: K; Value: V);
begin
  __alt.Add(key, Value);
end;

function TAVLTreeMap<K, V>.Contains(key: K): boolean;
begin
  Result := __alt.Contains(key);
end;

destructor TAVLTreeMap<K, V>.Destroy;
begin
  FreeAndNil(__alt);
  inherited Destroy;
end;

function TAVLTreeMap<K, V>.Get(key: K): TPtrV;
begin
  Result := __alt.Get(key);
end;

function TAVLTreeMap<K, V>.GetSize: integer;
begin
  Result := __alt.GetSize;
end;

function TAVLTreeMap<K, V>.IsEmpty: boolean;
begin
  Result := __alt.IsEmpty;
end;

function TAVLTreeMap<K, V>.KeySets: TArrayList_K;
begin
  Result := __alt.KeySets;
end;

function TAVLTreeMap<K, V>.Remove(key: K): TPtrV;
begin
  Result := __alt.Remove(key);
end;

procedure TAVLTreeMap<K, V>.Set_(key: K; newValue: V);
begin
  __alt.Set_(key, newValue);
end;

end.
