unit DSA.List_Stack_Queue.QueueCompare;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayListQueue,
  DSA.List_Stack_Queue.LoopListQueue,
  DSA.List_Stack_Queue.LinkedListQueue;

procedure Main;

implementation

function testTime(q: IQueue<Integer>; opCount: Integer): string;
var
  startTime, endTime: Cardinal;
  i: Integer;
begin
  Randomize;
  startTime := TThread.GetTickCount;

  for i := 0 to opCount - 1 do
    q.EnQueue(Random(Integer.MaxValue));
  for i := 0 to opCount - 1 do
    q.DeQueue;

  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TArrayListQueue_int = TArrayListQueue<Integer>;
  TLoopListQueue_int = TLoopListQueue<Integer>;
  TLinkedListQueue_int = TLinkedListQueue<Integer>;

procedure Main;
var
  ArrayListQueue: TArrayListQueue_int;
  LoopListQueue: TLoopListQueue_int;
  LinkedListQueue: TLinkedListQueue_int;

  vTime: string;
  opCount: Integer;
begin
  opCount := 10000000;

  ArrayListQueue := TArrayListQueue_int.Create();
  vTime := testTime(ArrayListQueue, opCount);
  Writeln('ArrayListQueue, time: ' + vTime + ' s');

  LoopListQueue := TLoopListQueue_int.Create();
  vTime := testTime(LoopListQueue, opCount);
  Writeln('LoopListQueue, time: ' + vTime + ' s');

  LinkedListQueue := TLinkedListQueue_int.Create();
  vTime := testTime(LinkedListQueue, opCount);
  Writeln('LinkedlistQueue, time: ' + vTime + ' s');
end;

end.
