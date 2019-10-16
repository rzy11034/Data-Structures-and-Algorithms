unit DSA.Tree.LinkedListSet;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.LinkedList;

type
  TLinkedListSet<T> = class(TInterfacedObject, ISet<T>)
  private type
    TLinkedList_T = TLinkedList<T>;

  var
    __list: TLinkedList<T>;

  public
    constructor Create;
    procedure Add(e: T);
    procedure Remove(e: T);
    function Contains(e: T): Boolean;
    function GetSize: Integer;
    function IsEmpty: Boolean;
  end;

procedure Main;

implementation

uses
  DSA.Utils;

type
  TLinkedListSet_str = TLinkedListSet<string>;

procedure Main;
var
  words1, words2: TArrayList_str;
  set1, set2: TLinkedListSet_str;
  i: Integer;
begin
  // Pride and Prejudice.txt
  words1 := TArrayList_str.Create;

  Writeln(A_FILE_NAME + ':');

  if TDsaUtils.ReadFile(FILE_PATH + A_FILE_NAME, words1) then
  begin
    Writeln('Total words: ', words1.GetSize);
  end;

  set1 := TLinkedListSet_str.Create;

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

  set2 := TLinkedListSet_str.Create;

  for i := 0 to words2.GetSize - 1 do
  begin
    set2.Add(words2[i]);
  end;
  Writeln('Total different words: ', set2.GetSize);
end;

{ TLinkedListSet<T> }

procedure TLinkedListSet<T>.Add(e: T);
begin
  if not(__list.Contains(e)) then
    __list.AddFirst(e);
end;

function TLinkedListSet<T>.Contains(e: T): Boolean;
begin
  Result := __list.Contains(e);
end;

constructor TLinkedListSet<T>.Create;
begin
  __list := TLinkedList_T.Create;
end;

function TLinkedListSet<T>.GetSize: Integer;
begin
  Result := __list.GetSize;
end;

function TLinkedListSet<T>.IsEmpty: Boolean;
begin
  Result := __list.IsEmpty;
end;

procedure TLinkedListSet<T>.Remove(e: T);
begin
  __list.RemoveElement(e);
end;

end.
