unit DSA.List_Stack_Queue.QueueCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayListQueue,
  DSA.List_Stack_Queue.LoopListQueue,
  DSA.List_Stack_Queue.LinkedListQueue;

procedure Main;

implementation

function testTime(q: specialize IQueue<integer>; opCount: integer): string;
var
  startTime, endTime: cardinal;
  i: integer;
begin
  Randomize;
  startTime := TThread.GetTickCount64;

  for i := 0 to opCount - 1 do
    q.EnQueue(Random(integer.MaxValue));
  for i := 0 to opCount - 1 do
    q.DeQueue;

  endTime := TThread.GetTickCount64;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TArrayListQueue_int = specialize TArrayListQueue<integer>;
  TLoopListQueue_int = specialize TLoopListQueue<integer>;
  TLinkedListQueue_int = specialize TLinkedListQueue<integer>;

procedure Main;
var
  ArrayQueue: TArrayListQueue_int;
  LoopQueue: TLoopListQueue_int;
  LinkedListQueue: TLinkedListQueue_int;
  vTime: string;
  opCount: integer;
begin
  opCount := 1000000;

  ArrayQueue := TArrayListQueue_int.Create();
  vTime := testTime(ArrayQueue, opCount);
  Writeln('ArrayQueue, time: ' + vTime + ' s');

  LoopQueue := TLoopListQueue_int.Create();
  vTime := testTime(LoopQueue, opCount);
  Writeln('LoopQueue, time: ' + vTime + ' s');

  LinkedListQueue := TLinkedListQueue_int.Create();
  vTime := testTime(LinkedListQueue, opCount);
  Writeln('LinkedlistQueue, time: ' + vTime + ' s');
end;

end.
