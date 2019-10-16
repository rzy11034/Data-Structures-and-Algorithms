unit DSA.Tree.PriorityQueue;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Tree.Heap;

type
  TQueueKind = (Min, Max);

  TPriorityQueue<T> = class(TInterfacedObject, IQueue<T>)
  private type
    THeap_T = THeap<T>;

  var
    __heap: THeap<T>;

  public
    function GetSize: Integer;
    function IsEmpty: Boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
    constructor Create(queueKind: TQueueKind); overload;
    constructor Create(queueKind: TQueueKind; c: IComparer<T>); overload;
  end;

implementation

{ TPriorityQueue<T> }

constructor TPriorityQueue<T>.Create(queueKind: TQueueKind);
begin
  case queueKind of
    Min:
      __heap := THeap_T.Create(10, THeapkind.Min);
    Max:
      __heap := THeap_T.Create(10, THeapkind.Max);
  end;
end;

constructor TPriorityQueue<T>.Create(queueKind: TQueueKind; c: IComparer<T>);
begin
  Self.Create(queueKind);
  __heap.SetComparer(c);
end;

function TPriorityQueue<T>.DeQueue: T;
begin
  Result := __heap.ExtractFirst;
end;

procedure TPriorityQueue<T>.EnQueue(e: T);
begin
  __heap.Add(e);
end;

function TPriorityQueue<T>.Peek: T;
begin
  Result := __heap.FindFirst;
end;

function TPriorityQueue<T>.GetSize: Integer;
begin
  Result := __heap.Size;
end;

function TPriorityQueue<T>.IsEmpty: Boolean;
begin
  Result := __heap.IsEmpty;
end;

end.
