unit DSA.Tree.AVLTreeSet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.AVLTree;

type

  { TAVLTreeSet }

  generic TAVLTreeSet<T, TKeyCmp> = class(TInterfacedObject,
    specialize ISet<T>)
  private
    type
    TAVLTree_T_V = specialize TAVLTree<T, TObject, TKeyCmp>;

  var
    __alt: TAVLTree_T_V;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(e: T);
    procedure Remove(e: T);
    function Contains(e: T): boolean;
    function GetSize: integer;
    function IsEmpty: boolean;
  end;

implementation

{ TAVLTreeSet }

constructor TAVLTreeSet.Create;
begin
  __alt := TAVLTree_T_V.Create();
end;

procedure TAVLTreeSet.Add(e: T);
begin
  __alt.Add(e, nil);
end;

function TAVLTreeSet.Contains(e: T): boolean;
begin
  Result := Contains(e);
end;

destructor TAVLTreeSet.Destroy;
begin
  FreeAndNil(__alt);
  inherited Destroy;
end;

function TAVLTreeSet.GetSize: integer;
begin
  Result := __alt.GetSize;
end;

function TAVLTreeSet.IsEmpty: boolean;
begin
  Result := __alt.IsEmpty;
end;

procedure TAVLTreeSet.Remove(e: T);
begin
  __alt.Remove(e);
end;

end.
