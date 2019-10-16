unit DSA.List_Stack_Queue.ArrayListQueue;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayList;

type
  TArrayListQueue<T> = class(TInterfacedObject, IQueue<T>)
  private type
    TArrayList_T = TArrayList<T>;

  var
    __arrayList: TArrayList<T>;

  public
    function GetSize: Integer;
    function IsEmpty: Boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
    function GetCapactiy: Integer;
    function ToString: string; override;

    constructor Create(capacity: Integer = 10);
  end;

procedure Main;

implementation

type
  TArrayListQueue_int = TArrayListQueue<Integer>;

procedure Main;
var
  queue: TArrayListQueue_int;
  i: Integer;
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

{ TArrayListQueue<T> }

constructor TArrayListQueue<T>.Create(capacity: Integer);
begin
  __arrayList := TArrayList_T.Create();
end;

function TArrayListQueue<T>.DeQueue: T;
begin
  Result := __arrayList.RemoveFirst;
end;

procedure TArrayListQueue<T>.EnQueue(e: T);
begin
  __arrayList.AddLast(e);
end;

function TArrayListQueue<T>.GetCapactiy: Integer;
begin
  Result := __arrayList.GetCapacity;
end;

function TArrayListQueue<T>.Peek: T;
begin
  Result := __arrayList.GetFirst;
end;

function TArrayListQueue<T>.GetSize: Integer;
begin
  Result := __arrayList.GetSize;
end;

function TArrayListQueue<T>.IsEmpty: Boolean;
begin
  Result := __arrayList.IsEmpty;
end;

function TArrayListQueue<T>.ToString: string;
var
  res: TStringBuilder;
  i: Integer;
  value: TValue;
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
      TValue.Make(@a, TypeInfo(T), value);

      if not(value.IsObject) then
        res.Append(value.ToString)
      else
        res.Append(value.AsObject.ToString);

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
