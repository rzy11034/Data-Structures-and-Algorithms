unit DSA.List_Stack_Queue.LinkedListQueue;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure;

type
  TLinkedListQueue<T> = class(TInterfacedObject, IQueue<T>)
  private type
    TNode = class
    public
      Elment: T;
      Next: TNode;
      constructor Create(newE: T; newNext: TNode = nil); overload;
      constructor Create(); overload;
      function ToString: string; override;
    end;

  var
    __head, __tail: TNode;
    __size: Integer;
  public
    function GetSize: Integer;
    function IsEmpty: Boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;

    constructor Create;
    function ToString: string; override;
  end;

procedure Main;

implementation

type
  TLinkedListQueue_int = TLinkedListQueue<Integer>;

procedure Main;
var
  queue: TLinkedListQueue_int;
  i: Integer;
begin
  queue := TLinkedListQueue_int.Create();

  for i := 0 to 9 do
  begin
    queue.EnQueue(i);
    Writeln(queue.ToString);

    if i mod 3 = 2 then
    begin
      queue.DeQueue;
      Writeln(queue.ToString);
    end;
  end;

  Writeln(queue.DeQueue);
end;

{ TLinkedQueue<T>.TNode }

constructor TLinkedListQueue<T>.TNode.Create(newE: T; newNext: TNode);
begin
  Elment := newE;
  Next := newNext;
end;

constructor TLinkedListQueue<T>.TNode.Create;
begin
  Self.Create(default (T));
end;

function TLinkedListQueue<T>.TNode.ToString: string;
var
  value: TValue;
  res: string;
begin
  TValue.Make(@Elment, TypeInfo(T), value);

  if not(value.IsObject) then
    res := value.ToString
  else
    res := value.AsObject.ToString;

  Result := res;
end;

{ TLinkedQueue<T> }

constructor TLinkedListQueue<T>.Create;
begin
  __head := nil;
  __tail := nil;
  __size := 0;
end;

function TLinkedListQueue<T>.DeQueue: T;
var
  resNode: TNode;
begin
  if IsEmpty then
    raise Exception.Create('Cannot dequeue from an empty queue.');

  resNode := __head;
  __head := __head.Next;
  resNode.Next := nil;

  if __head = nil then
    __tail := nil;

  Dec(__size);

  Result := resNode.Elment;
  FreeAndNil(resNode);
end;

procedure TLinkedListQueue<T>.EnQueue(e: T);
begin
  if __tail = nil then
  begin
    __tail := TNode.Create(e);
    __head := __tail;
  end
  else
  begin
    __tail.Next := TNode.Create(e);
    __tail := __tail.Next;
  end;

  Inc(__size);
end;

function TLinkedListQueue<T>.Peek: T;
begin
  Result := __head.Elment;
end;

function TLinkedListQueue<T>.GetSize: Integer;
begin
  Result := __size;
end;

function TLinkedListQueue<T>.IsEmpty: Boolean;
begin
  Result := __size = 0;
end;

function TLinkedListQueue<T>.ToString: string;
var
  res: TStringBuilder;
  cur: TNode;
begin
  res := TStringBuilder.Create;
  try
    res.Append('Queue: front ');
    cur := __head;

    while cur <> nil do
    begin
      res.Append(cur.ToString + ' -> ');
      cur := cur.Next;
    end;

    res.Append('nil');

    Result := res.ToString;

  finally
    res.Free;
  end;
end;

end.
