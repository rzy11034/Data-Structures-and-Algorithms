unit DSA.List_Stack_Queue.StackCompare;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayListStack,
  DSA.List_Stack_Queue.LinkedListStack;

procedure Main;

implementation

function testTime(q: IStack<Integer>; opCount: Integer): string;
var
  startTime, endTime: Cardinal;
  i: Integer;
begin
  Randomize;
  startTime := TThread.GetTickCount;

  for i := 0 to opCount - 1 do
    q.Push(Random(Integer.MaxValue));
  for i := 0 to opCount - 1 do
    q.Pop;

  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TArrayListStack_int = TArrayListStack<Integer>;
  TLinkedListStack_int = TLinkedListStack<Integer>;

procedure Main;
var
  ArrayStack: TArrayListStack_int;
  LinkedStack: TLinkedListStack_int;
  vTime: string;
  opCount: Integer;
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
