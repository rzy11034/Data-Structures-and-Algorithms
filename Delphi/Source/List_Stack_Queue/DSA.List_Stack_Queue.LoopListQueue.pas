unit DSA.List_Stack_Queue.LoopListQueue;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure;

type
  TLoopListQueue<T> = class(TInterfacedObject, IQueue<T>)
  private type
    TArray_T = TArray<T>;

  var
    __data: TArray_T;
    __front, __tail, __size: Integer;

    procedure __reSize(newCapacity: Integer);

  public
    function GetCapacity: Integer;
    function GetSize: Integer;
    function IsEmpty: Boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
    function ToString: string; override;

    constructor Create(capacity: Integer = 10);
  end;

procedure Main;

implementation

procedure Main;
var
  queue: TLoopListQueue<Integer>;
  i: Integer;
begin
  queue := TLoopListQueue<Integer>.Create(5);

  for i := 0 to 9 do
  begin
    queue.EnQueue(i);
    Writeln(queue.ToString);
  end;

  while not(queue.IsEmpty) do
  begin
    queue.DeQueue;
    Writeln(queue.ToString);
  end;

end;

{ TLoopQueue<T> }

constructor TLoopListQueue<T>.Create(capacity: Integer);
begin
  SetLength(Self.__data, capacity + 1);
end;

function TLoopListQueue<T>.DeQueue: T;
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

procedure TLoopListQueue<T>.EnQueue(e: T);
begin
  if (__tail + 1) mod Length(__data) = __front then
    __reSize(GetCapacity * 2);

  __data[__tail] := e;
  __tail := (__tail + 1) mod Length(__data);
  Inc(__size);
end;

function TLoopListQueue<T>.GetCapacity: Integer;
begin
  Result := Length(Self.__data);
end;

function TLoopListQueue<T>.Peek: T;
begin
  Result := __data[__front];
end;

function TLoopListQueue<T>.GetSize: Integer;
begin
  Result := __size;
end;

function TLoopListQueue<T>.IsEmpty: Boolean;
begin
  Result := __size = 0;
end;

function TLoopListQueue<T>.ToString: string;
var
  res: TStringBuilder;
  i: Integer;
  value: TValue;
  index: Integer;
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
      TValue.Make(@__data[index], TypeInfo(T), value);

      if not(value.IsObject) then
        res.Append(value.ToString)
      else
        res.Append(value.AsObject.ToString);

      if i <> __size - 1 then
        res.Append(', ');
    end;

    res.Append('] tail');
    Result := res.ToString;
  finally
    res.Free;
  end;
end;

procedure TLoopListQueue<T>.__reSize(newCapacity: Integer);
var
  newData: TArray_T;
  i: Integer;
begin
  SetLength(newData, newCapacity + 1);

  for i := 0 to GetSize - 1 do
    newData[i] := __data[(i + __front) mod Length(__data)];

  __data := newData;
  __front := 0;
  __tail := __size - 1;
end;

end.
