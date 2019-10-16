unit DSA.Tree.LinkedListSet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.LinkedList;

type

  { TLinkedListSet }

  generic TLinkedListSet<T> = class(TInterfacedObject, specialize ISet<T>)
  private
    type
    TLinkedList_T = specialize TLinkedList<T>;

  var
    __list: specialize TLinkedList<T>;

  public
    constructor Create;
    procedure Add(e: T);
    procedure Remove(e: T);
    function Contains(e: T): boolean;
    function GetSize: integer;
    function IsEmpty: boolean;
  end;


procedure Main;

implementation

uses
  DSA.Utils;

type
  TLinkedListSet_str = specialize TLinkedListSet<string>;

procedure Main;
var
  words1, words2: TArrayList_str;
  set1, set2: TLinkedListSet_str;
  i: integer;
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

{ TLinkedListSet }

procedure TLinkedListSet.Add(e: T);
begin
  if not (__list.Contains(e)) then
    __list.AddFirst(e);
end;

function TLinkedListSet.Contains(e: T): boolean;
begin
  Result := __list.Contains(e);
end;

constructor TLinkedListSet.Create;
begin
  __list := TLinkedList_T.Create;
end;

function TLinkedListSet.GetSize: integer;
begin
  Result := __list.GetSize;
end;

function TLinkedListSet.IsEmpty: boolean;
begin
  Result := __list.IsEmpty;
end;

procedure TLinkedListSet.Remove(e: T);
begin
  __list.RemoveElement(e);
end;

end.
