unit DSA.List_Stack_Queue.LinkedListQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.Interfaces.DataStructure;

type

  { TLinkedListQueue }

  generic TLinkedListQueue<T> = class(TInterfacedObject, specialize IQueue<T>)
  private
    type

    { TNode }

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
    __size: integer;
  public
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;

    constructor Create;
    function ToString: string; override;
  end;

implementation

{ TLinkedListQueue }

constructor TLinkedListQueue.Create;
begin
  __head := nil;
  __tail := nil;
  __size := 0;
end;

function TLinkedListQueue.DeQueue: T;
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

procedure TLinkedListQueue.EnQueue(e: T);
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

function TLinkedListQueue.Peek: T;
begin
  Result := __head.Elment;
end;

function TLinkedListQueue.GetSize: integer;
begin
  Result := __size;
end;

function TLinkedListQueue.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

function TLinkedListQueue.ToString: string;
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

{ TLinkedListQueue.TNode }

constructor TLinkedListQueue.TNode.Create(newE: T; newNext: TNode);
begin
  Elment := newE;
  Next := newNext;
end;

constructor TLinkedListQueue.TNode.Create;
begin
  Self.Create(default(T));
end;

function TLinkedListQueue.TNode.ToString: string;
var
  Value: TValue;
  res: string;
begin
  TValue.Make(@Elment, TypeInfo(T), Value);

  if not (Value.IsObject) then
    res := Value.ToString
  else
    res := Value.AsObject.ToString;

  Result := res;
end;

end.
