unit DSA.List_Stack_Queue.ArrayListQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayList;

type

  { TArrayListQueue }

  generic TArrayListQueue<T> = class(TInterfacedObject, specialize IQueue<T>)
  private
    type
    TArrayList_T = specialize TArrayList<T>;

  var
    __arrayList: TArrayList_T;

  public
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
    function GetCapactiy: integer;
    function ToString: string; override;

    constructor Create(capacity: integer = 10);
  end;

procedure Main;

implementation

type
  TArrayListQueue_int = specialize TArrayListQueue<integer>;

procedure Main;
var
  queue: TArrayListQueue_int;
  i: integer;
begin
  queue := TArrayListQueue_int.Create();

  for i := 0 to 4 do
  begin
    queue.EnQueue(i);
    Writeln(queue.ToString);

    if i mod 3 = 2 then
    begin
      queue.DeQueue;
      Writeln(queue.ToString);
    end;
  end;
end;

{ TArrayListQueue }

constructor TArrayListQueue.Create(capacity: integer);
begin
  __arrayList := TArrayList_T.Create();
end;

function TArrayListQueue.DeQueue: T;
begin
  Result := __arrayList.RemoveFirst;
end;

procedure TArrayListQueue.EnQueue(e: T);
begin
  __arrayList.AddLast(e);
end;

function TArrayListQueue.GetCapactiy: integer;
begin
  Result := __arrayList.GetCapacity;
end;

function TArrayListQueue.Peek: T;
begin
  Result := __arrayList.GetFirst;
end;

function TArrayListQueue.GetSize: integer;
begin
  Result := __arrayList.GetSize;
end;

function TArrayListQueue.IsEmpty: boolean;
begin
  Result := __arrayList.IsEmpty;
end;

function TArrayListQueue.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  Value: TValue;
  a: T;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Queue: Size = %d, capacity = %d',
      [Self.GetSize, Self.GetCapactiy]);
    res.AppendLine;
    res.Append('  front [');

    for i := 0 to __arrayList.GetSize - 1 do
    begin
      a := __arrayList[i];
      TValue.Make(@a, TypeInfo(T), Value);

      if not (Value.IsObject) then
        res.Append(Value.ToString)
      else
        res.Append(Value.AsObject.ToString);

      if i <> __arrayList.GetSize - 1 then
        res.Append(', ');
    end;

    res.Append('] tail');

    Result := res.ToString;

  finally
    res.Free;
  end;
end;

end.
