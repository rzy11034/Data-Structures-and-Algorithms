unit DSA.List_Stack_Queue.ArrayListStack;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Interfaces.DataStructure;

type

  { TArrayListStack }

  generic TArrayListStack<T> = class(TInterfacedObject, specialize IStack<T>)
  private
    type
    TArrayList_T = specialize TArrayList<T>;

  var
    __arrayList: specialize TArrayList<T>;

  public
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
    function GetCapactiy: integer;
    function ToString: string; override;

    constructor Create(capacity: integer = 10);
  end;

procedure Main;

implementation

type
  TArrayStack_int = specialize TArrayListStack<integer>;

procedure Main;
var
  Stack: TArrayStack_int;
  i: integer;
begin
  Stack := TArrayStack_int.Create();

  for i := 0 to 4 do
  begin
    Stack.Push(i);
    Writeln(Stack.ToString);
  end;

  Stack.Pop;
  Writeln(Stack.ToString);
end;

{ TArrayListStack }

constructor TArrayListStack.Create(capacity: integer);
begin
  __arrayList := TArrayList_T.Create(capacity);
end;

function TArrayListStack.GetCapactiy: integer;
begin
  Result := __arrayList.GetCapacity;
end;

function TArrayListStack.GetSize: integer;
begin
  Result := __arrayList.GetSize;
end;

function TArrayListStack.IsEmpty: boolean;
begin
  Result := __arrayList.IsEmpty;
end;

function TArrayListStack.Peek: T;
begin
  Result := __arrayList.GetLast;
end;

function TArrayListStack.Pop: T;
begin
  Result := __arrayList.RemoveLast;
end;

procedure TArrayListStack.Push(e: T);
begin
  __arrayList.AddLast(e);
end;

function TArrayListStack.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  Value: TValue;
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
      TValue.Make(@p, TypeInfo(T), Value);

      if not (Value.IsObject) then
        res.Append(Value.ToString)
      else
        res.Append(Value.AsObject.ToString);

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
