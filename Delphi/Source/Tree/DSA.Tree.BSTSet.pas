unit DSA.Tree.BSTSet;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.BST;

type
  TBSTSet<T> = class(TInterfacedObject, ISet<T>)
  private type
    TBST_T = TBST<T>;

  var
    __bst: TBST<T>;

  public
    constructor Create;
    procedure Add(e: T);
    procedure Remove(e: T);
    function Contains(e: T): Boolean;
    function GetSize(): Integer;
    function IsEmpty(): Boolean;
  end;

procedure Main;

implementation

uses
  DSA.Utils;

type
  TBSTSet_str = TBSTSet<string>;

procedure Main;
var
  words1, words2: TArrayList_str;
  set1, set2: TBSTSet_str;
  i: Integer;
begin
  // Pride and Prejudice.txt
  words1 := TArrayList_str.Create;

  Writeln(A_FILE_NAME + ':');

  if TDsaUtils.ReadFile(FILE_PATH + A_FILE_NAME, words1) then
  begin
    Writeln('Total words: ', words1.GetSize);
  end;

  set1 := TBSTSet_str.Create;

  for i := 0 to words1.GetSize - 1 do
  begin
    set1.Add(words1[i]);
  end;

  Writeln('Total different words: ', set1.GetSize);

  TDsaUtils.DrawLine;

  // A Tale of Two Cities.txt
  words2 := TArrayList_str.Create;

  Writeln(B_FILE_NAME + ':');

  if TDsaUtils.ReadFile(FILE_PATH + B_FILE_NAME, words2) then
  begin
    Writeln('Total words: ', words2.GetSize);
  end;

  set2 := TBSTSet_str.Create;

  for i := 0 to words2.GetSize - 1 do
  begin
    set2.Add(words2[i]);
  end;
  Writeln('Total different words: ', set2.GetSize);
end;

{ TBSTSet<T> }

procedure TBSTSet<T>.Add(e: T);
begin
  __bst.Add(e);
end;

function TBSTSet<T>.Contains(e: T): Boolean;
begin
  Result := __bst.Contains(e);
end;

constructor TBSTSet<T>.Create;
begin
  __bst := TBST_T.Create;
end;

function TBSTSet<T>.GetSize: Integer;
begin
  Result := __bst.GetSize;
end;

function TBSTSet<T>.IsEmpty: Boolean;
begin
  Result := __bst.IsEmpty;
end;

procedure TBSTSet<T>.Remove(e: T);
begin
  __bst.Remove(e);
end;

end.
