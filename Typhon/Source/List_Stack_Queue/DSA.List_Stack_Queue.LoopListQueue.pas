unit DSA.List_Stack_Queue.LoopListQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.Interfaces.DataStructure;

type

  { TLoopListQueue }

  generic TLoopListQueue<T> = class(TInterfacedObject, specialize IQueue<T>)
  private
    type arr_T = specialize TArray<T>;

  var
    __data: arr_T;
    __front, __tail, __size: integer;

    procedure __reSize(newCapacity: integer);

  public
    function GetCapacity: integer;
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
    function ToString: string; override;

    constructor Create(capacity: integer = 10);
  end;

procedure Main;

implementation

procedure Main;
type
  TLoopListQueue_int = specialize TLoopListQueue<integer>;
var
  queue: TLoopListQueue_int;
  i: integer;
begin
  queue := TLoopListQueue_int.Create(5);

  for i := 0 to 9 do
  begin
    queue.EnQueue(i);
    Writeln(queue.ToString);
  end;

  while not (queue.IsEmpty) do
  begin
    queue.DeQueue;
    Writeln(queue.ToString);
  end;
end;

{ TLoopListQueue }

constructor TLoopListQueue.Create(capacity: integer);
begin
  SetLength(Self.__data, capacity + 1);
end;

function TLoopListQueue.DeQueue: T;
var
  res: T;
begin
  if IsEmpty then
    raise Exception.Create('Cannot dequeue from an empty queue.');

  res := __data[__front];
  __front := (__front + 1) mod Length(Self.__data);

  if (__size = GetCapacity div 4) and (GetCapacity div 2 <> 0) then
    __reSize(GetCapacity div 2);

  Dec(__size);
  Result := res;
end;

procedure TLoopListQueue.EnQueue(e: T);
begin
  if (__tail + 1) mod Length(__data) = __front then
    __reSize(GetCapacity * 2);

  __data[__tail] := e;
  __tail := (__tail + 1) mod Length(__data);
  Inc(__size);
end;

function TLoopListQueue.GetCapacity: integer;
begin
  Result := Length(Self.__data);
end;

function TLoopListQueue.Peek: T;
begin
  Result := __data[__front];
end;

function TLoopListQueue.GetSize: integer;
begin
  Result := __size;
end;

function TLoopListQueue.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

function TLoopListQueue.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  Value: TValue;
  index: integer;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Queue: Size = %d, capacity = %d',
      [Self.__size, Self.GetCapacity]);
    res.AppendLine;
    res.Append('  front [');

    for i := 0 to GetSize - 1 do
    begin
      index := (i + __front) mod Length(Self.__data);
      TValue.Make(@__data[index], TypeInfo(T), Value);

      if not (Value.IsObject) then
        res.Append(Value.ToString)
      else
        res.Append(Value.AsObject.ToString);

      if i <> __size - 1 then
        res.Append(', ');
    end;

    res.Append('] tail');
    Result := res.ToString;
  finally
    res.Free;
  end;
end;

procedure TLoopListQueue.__reSize(newCapacity: integer);
var
  newData: arr_T;
  i: integer;
begin
  SetLength(newData, newCapacity + 1);

  for i := 0 to GetSize - 1 do
    newData[i] := __data[(i + __front) mod Length(__data)];

  __data := newData;
  __front := 0;
  __tail := __size - 1;
end;

end.
