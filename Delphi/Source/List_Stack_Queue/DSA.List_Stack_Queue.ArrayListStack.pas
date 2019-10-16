unit DSA.List_Stack_Queue.ArrayListStack;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Interfaces.DataStructure;

type
  TArrayListStack<T> = class(TInterfacedObject, IStack<T>)
  private type
    TArrayList_T = TArrayList<T>;

  var
    __arrayList: TArrayList<T>;

  public
    function GetSize: Integer;
    function IsEmpty: Boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
    function GetCapactiy: Integer;
    function ToString: string; override;

    constructor Create(capacity: Integer = 10);
  end;

procedure Main;

implementation

type
  TArrayStack_int = TArrayListStack<Integer>;

procedure Main;
var
  stack: TArrayStack_int;
  i: Integer;
begin
  stack := TArrayStack_int.Create();

  for i := 0 to 4 do
  begin
    stack.Push(i);
    Writeln(stack.ToString);
  end;

  stack.Pop;
  Writeln(stack.ToString);
end;

{ TArrayListStack<T> }

constructor TArrayListStack<T>.Create(capacity: Integer);
begin
  __arrayList := TArrayList_T.Create(capacity);
end;

function TArrayListStack<T>.GetCapactiy: Integer;
begin
  Result := __arrayList.GetCapacity;
end;

function TArrayListStack<T>.GetSize: Integer;
begin
  Result := __arrayList.GetSize;
end;

function TArrayListStack<T>.IsEmpty: Boolean;
begin
  Result := __arrayList.IsEmpty;
end;

function TArrayListStack<T>.Peek: T;
begin
  Result := __arrayList.GetLast;
end;

function TArrayListStack<T>.Pop: T;
begin
  Result := __arrayList.RemoveLast;
end;

procedure TArrayListStack<T>.Push(e: T);
begin
  __arrayList.AddLast(e);
end;

function TArrayListStack<T>.ToString: string;
var
  res: TStringBuilder;
  i: Integer;
  value: TValue;
  p: T;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Stack: Size = %d, capacity = %d',
      [Self.GetSize, Self.GetCapactiy]);
    res.AppendLine;
    res.Append('  [');

    for i := 0 to __arrayList.GetSize - 1 do
    begin
      p := __arrayList[i];
      TValue.Make(@p, TypeInfo(T), value);

      if not(value.IsObject) then
        res.Append(value.ToString)
      else
        res.Append(value.AsObject.ToString);

      if i <> __arrayList.GetSize - 1 then
        res.Append(', ');
    end;

    res.Append('] top');

    Result := res.ToString;
  finally
    res.Free;
  end;
end;

end.
