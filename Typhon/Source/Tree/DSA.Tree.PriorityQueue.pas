unit DSA.Tree.PriorityQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Interfaces.DataStructure,
  DSA.Tree.Heap;

type
  TQueueKind = (Min, Max);

  { TPriorityQueue }

  generic TPriorityQueue<T, TComparer> = class(TInterfacedObject,
    specialize IQueue<T>)
  private
    type
    THeap_T = specialize THeap<T, TComparer>;
    IComparer_T = specialize IDSA_Comparer<T>;

  var
    __heap: THeap_T;

  public
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
    constructor Create(queueKind: TQueueKind); overload;
    constructor Create(queueKind: TQueueKind; c: IComparer_T); overload;
  end;

implementation

{ TPriorityQueue }

constructor TPriorityQueue.Create(queueKind: TQueueKind);
begin
  case queueKind of
    Min:
      __heap := THeap_T.Create(10, THeapkind.Min);
    Max:
      __heap := THeap_T.Create(10, THeapkind.Max);
  end;
end;

constructor TPriorityQueue.Create(queueKind: TQueueKind; c: IComparer_T);
begin
  Self.Create(queueKind);
  __heap.SetComparer(c);
end;

function TPriorityQueue.DeQueue: T;
begin
  Result := __heap.ExtractFirst;
end;

procedure TPriorityQueue.EnQueue(e: T);
begin
  __heap.Add(e);
end;

function TPriorityQueue.Peek: T;
begin
  Result := __heap.FindFirst;
end;

function TPriorityQueue.GetSize: integer;
begin
  Result := __heap.Size;
end;

function TPriorityQueue.IsEmpty: boolean;
begin
  Result := __heap.IsEmpty;
end;

end.
