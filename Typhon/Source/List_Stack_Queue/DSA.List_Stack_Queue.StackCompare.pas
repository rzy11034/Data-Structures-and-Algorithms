unit DSA.List_Stack_Queue.StackCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayListStack,
  DSA.List_Stack_Queue.LinkedListStack;

procedure Main;

implementation

function testTime(q: specialize IStack<integer>; opCount: integer): string;
var
  startTime, endTime: cardinal;
  i: integer;
begin
  Randomize;
  startTime := TThread.GetTickCount64;

  for i := 0 to opCount - 1 do
    q.Push(Random(integer.MaxValue));
  for i := 0 to opCount - 1 do
    q.Pop;

  endTime := TThread.GetTickCount64;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TArrayListStack_int = specialize TArrayListStack<integer>;
  TLinkedListStack_int = specialize TLinkedListStack<integer>;

procedure Main;
var
  ArrayStack: TArrayListStack_int;
  LinkedStack: TLinkedListStack_int;
  vTime: string;
  opCount: integer;
begin
  opCount := 10000000;

  LinkedStack := TLinkedListStack_int.Create();
  vTime := testTime(LinkedStack, opCount);
  Writeln('LinkedStack, time: ' + vTime + ' s');

  ArrayStack := TArrayListStack_int.Create();
  vTime := testTime(ArrayStack, opCount);
  Writeln('ArrayStack, time: ' + vTime + ' s');
end;

end.
