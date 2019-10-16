unit DSA.List_Stack_Queue.LinkedListStack;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.LinkedList;

type
  TLinkedListStack<T> = class(TInterfacedObject, IStack<T>)
  private type
    TLinkedList_T = TLinkedList<T>;

  var
    __list: TLinkedList<T>;

  public
    function GetSize: Integer;
    function IsEmpty: Boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;

    constructor Create;
    function ToString: string; override;
  end;

procedure Main;

implementation

type
  TLinkedListStack_int = TLinkedListStack<Integer>;

procedure Main;
var
  Stack: TLinkedListStack_int;
  i: Integer;
begin
  Stack := TLinkedListStack_int.Create();

  for i := 0 to 4 do
  begin
    Stack.Push(i);
    Writeln(Stack.ToString);
  end;

  Stack.Pop;
  Writeln(Stack.ToString);
end;

{ TLinkedStack<T> }

constructor TLinkedListStack<T>.Create;
begin
  __list := TLinkedList<T>.Create;
end;

function TLinkedListStack<T>.GetSize: Integer;
begin
  Result := __list.GetSize;
end;

function TLinkedListStack<T>.IsEmpty: Boolean;
begin
  Result := __list.IsEmpty;
end;

function TLinkedListStack<T>.Peek: T;
begin
  Result := __list.GetFirst;
end;

function TLinkedListStack<T>.Pop: T;
begin
  Result := __list.ReMoveFirst;
end;

procedure TLinkedListStack<T>.Push(e: T);
begin
  __list.AddFirst(e);
end;

function TLinkedListStack<T>.ToString: string;
var
  res: TStringBuilder;
begin
  res := TStringBuilder.Create;
  res.Append('Stack: top ');
  res.Append(__list.ToString);
  Result := res.ToString;
end;

end.
