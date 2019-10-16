unit DSA.Tree.AVLTreeSet;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.AVLTree;

type
  TAVLTreeSet<T> = class(TInterfacedObject, ISet<T>)
  private type
    TAVLTree_T_V = TAVLTree<T, TObject>;

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

constructor TAVLTreeSet<T>.Create;
begin
  __alt := TAVLTree_T_V.Create();
end;

procedure TAVLTreeSet<T>.Add(e: T);
begin
  __alt.Add(e, nil);
end;

function TAVLTreeSet<T>.Contains(e: T): boolean;
begin
  Result := contains(e);
end;

destructor TAVLTreeSet<T>.Destroy;
begin
  FreeAndNil(__alt);
  inherited Destroy;
end;

function TAVLTreeSet<T>.GetSize: integer;
begin
  Result := __alt.GetSize;
end;

function TAVLTreeSet<T>.IsEmpty: boolean;
begin
  Result := __alt.IsEmpty;
end;

procedure TAVLTreeSet<T>.Remove(e: T);
begin
  __alt.Remove(e);
end;

end.
