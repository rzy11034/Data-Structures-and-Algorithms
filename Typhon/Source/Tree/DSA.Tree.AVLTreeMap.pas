unit DSA.Tree.AVLTreeMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Tree.AVLTree;

type

  { TAVLTreeMap }

  generic TAVLTreeMap<K, V, TKeyCmp> = class(TInterfacedObject,
    specialize IMap<K, V>)
  private
    type
    TAVLTree_K_V = specialize TAVLTree<K, V, TKeyCmp>;
    TPtrV = specialize TPtr_V<V>;
    TArrayList_K = specialize TArrayList<K>;

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

constructor TAVLTreeMap.Create;
begin
  __alt := TAVLTree_K_V.Create();
end;

procedure TAVLTreeMap.Add(key: K; Value: V);
begin
  __alt.Add(Key, Value);
end;

function TAVLTreeMap.Contains(key: K): boolean;
begin
  Result := __alt.Contains(key);
end;

destructor TAVLTreeMap.Destroy;
begin
  FreeAndNil(__alt);
  inherited Destroy;
end;

function TAVLTreeMap.Get(key: K): TPtrV;
begin
  Result := __alt.Get(Key);
end;

function TAVLTreeMap.GetSize: integer;
begin
  Result := __alt.GetSize;
end;

function TAVLTreeMap.IsEmpty: boolean;
begin
  Result := __alt.IsEmpty;
end;

function TAVLTreeMap.KeySets: TArrayList_K;
begin
  Result := __alt.KeySets;
end;

function TAVLTreeMap.Remove(key: K): TPtrV;
begin
  Result := __alt.Remove(Key);
end;

procedure TAVLTreeMap.Set_(key: K; newValue: V);
begin
  __alt.Set_(Key, newValue);
end;

end.
