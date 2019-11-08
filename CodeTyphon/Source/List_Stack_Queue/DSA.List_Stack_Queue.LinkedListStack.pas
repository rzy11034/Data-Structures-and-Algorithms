unit DSA.List_Stack_Queue.LinkedListStack;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.LinkedList;

type
  generic TLinkedListStack<T> = class(TInterfacedObject, specialize IStack<T>)
  private
    type
    TLinkedList_T = specialize TLinkedList<T>;

  var
    __list: specialize TLinkedList<T>;

  public
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;

    constructor Create;
    function ToString: string; override;
  end;

procedure Main;

implementation

type
  TLinkedListStack_int = specialize TLinkedListStack<integer>;

procedure Main;
var
  Stack: TLinkedListStack_int;
  i: integer;
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

{ TLinkedStack }

constructor TLinkedListStack.Create;
begin
  __list := TLinkedList_T.Create;
end;

function TLinkedListStack.GetSize: integer;
begin
  Result := __list.GetSize;
end;

function TLinkedListStack.IsEmpty: boolean;
begin
  Result := __list.IsEmpty;
end;

function TLinkedListStack.Peek: T;
begin
  Result := __list.GetFirst;
end;

function TLinkedListStack.Pop: T;
begin
  Result := __list.ReMoveFirst;
end;

procedure TLinkedListStack.Push(e: T);
begin
  __list.AddFirst(e);
end;

function TLinkedListStack.ToString: string;
var
  res: TStringBuilder;
begin
  res := TStringBuilder.Create;
  res.Append('Stack: top ');
  res.Append(__list.ToString);
  Result := res.ToString;
end;

end.
