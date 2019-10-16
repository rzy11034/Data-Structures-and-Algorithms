unit DSA.List_Stack_Queue.LinkedList;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Defaults;

type
  TLinkedList<T> = class
  private type
    TNode = class
    public
      Elment: T;
      Next: TNode;
      constructor Create(newElment: T; newNext: TNode = nil); overload;
      constructor Create(); overload;
      function ToString: string; override;
    end;

    TComparer_T = TComparer<T>;

  var
    __dummyHead: TNode;
    __size: integer;
  public
    /// <summary> 获取链表中的元素个数 </summary>
    function GetSize: integer;
    /// <summary> 返回链表是否为空 </summary>
    function IsEmpty: Boolean;
    /// <summary> 链表的index(0-based)位置中添加新的元素e </summary>
    procedure Add(index: integer; e: T);
    /// <summary> 在链表头添加新的元素e </summary>
    procedure AddFirst(e: T);
    /// <summary> 在链表末尾添加新的元素e </summary>
    procedure AddLast(e: T);
    /// <summary> 获取链表index(0-based)位置中的元素e </summary>
    function Get(index: integer): T;
    /// <summary> 获取链表(0-based)第一个元素 e </summary>
    function GetFirst(): T;
    /// <summary> 获取链表(0-based)最后一个元素e  </summary>
    function GetLast(): T;
    /// <summary> 修改链表所有 d 元素为 e </summary>
    procedure SetElment(d, e: T);
    /// <summary> 修改链表index(0-based)位置中的元素e </summary>
    procedure Set_(index: integer; e: T);
    /// <summary> 查找链表中是否有元素e </summary>
    function Contains(e: T): Boolean;
    /// <summary> 删除链表index(0-based)位置的元素e, 返回链表删除元素 </summary>
    function Remove(index: integer): T;
    /// <summary> 删除链表(0-based)第一个位置的元素e，返回链表删除元素 </summary>
    function RemoveFirst(): T;
    /// <summary> 删除链表index(0-based)最后一个位置的元素e，返回链表删除元素 </summary>
    function RemoveLast(): T;
    /// <summary> 删除链表所有为e的元素 </summary>
    procedure RemoveElement(e: T);
    function ToString: string; override;
    property Items[i: integer]: T read Get write Set_; default;

    constructor Create;
    destructor Destroy; override;
  end;

procedure Main;

implementation

type
  TLinkedList_int = TLinkedList<integer>;

procedure Main;
var
  LinkedList: TLinkedList_int;
  i: integer;
begin
  LinkedList := TLinkedList_int.Create;

  for i := 0 to 9 do
  begin
    LinkedList.AddFirst(i mod 2);
    Writeln(LinkedList.ToString);
  end;

  LinkedList.Add(2, 666);
  Writeln(LinkedList.ToString);

  LinkedList.Remove(2);
  Writeln(LinkedList.ToString);

  LinkedList.RemoveFirst;
  Writeln(LinkedList.ToString);

  LinkedList.RemoveLast;
  Writeln(LinkedList.ToString);

  LinkedList.SetElment(1, 9);
  Writeln(LinkedList.ToString);

end;

{ TLinkedList<T>.TNode }

constructor TLinkedList<T>.TNode.Create(newElment: T; newNext: TNode);
begin
  Elment := newElment;
  Next := newNext;
end;

constructor TLinkedList<T>.TNode.Create;
begin
  Self.Create(default (T));
end;

function TLinkedList<T>.TNode.ToString: string;
var
  value: TValue;
  res: string;
begin
  TValue.Make(@Self.Elment, TypeInfo(T), value);

  if not(value.IsObject) then
    res := value.ToString
  else
    res := value.AsObject.ToString;

  Result := res;
end;

{ TLinkedList<T> }

procedure TLinkedList<T>.Add(index: integer; e: T);
var
  prev: TNode;
  i: integer;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Add failed. Index is Illegal.');

  prev := __dummyHead;
  for i := 0 to index - 1 do
    prev := prev.Next;

  // node := TNode.Create(e);
  // node.FNext = prev.FNext;
  // prev.FNext = node;
  // 以上三句等同于下面一句
  prev.Next := TNode.Create(e, prev.Next);
  Inc(Self.__size);
end;

procedure TLinkedList<T>.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TLinkedList<T>.AddLast(e: T);
begin
  Add(__size, e);
end;

function TLinkedList<T>.Contains(e: T): Boolean;
var
  cur: TNode;
  comparer: IComparer<T>;
begin
  cur := __dummyHead.Next;
  Result := False;
  comparer := TComparer_T.Default;

  while cur <> nil do
  begin
    if comparer.Compare(cur.Elment, e) = 0 then
      Result := True;

    cur := cur.Next;
  end;
end;

constructor TLinkedList<T>.Create;
begin
  __dummyHead := TNode.Create();
  __size := 0;
end;

destructor TLinkedList<T>.Destroy;
var
  delNode, cur: TNode;
begin
  cur := __dummyHead;

  while cur <> nil do
  begin
    delNode := cur;
    cur := cur.Next;
    FreeAndNil(delNode);
  end;

  inherited;
end;

function TLinkedList<T>.Get(index: integer): T;
var
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= __size) then
    raise Exception.Create('Get failed. Index is Illegal.');

  cur := __dummyHead.Next;
  for i := 0 to index - 1 do
    cur := cur.Next;

  Result := cur.Elment;
end;

function TLinkedList<T>.GetFirst: T;
begin
  Result := Get(0);
end;

function TLinkedList<T>.GetLast: T;
begin
  Result := Get(__size - 1);
end;

function TLinkedList<T>.GetSize: integer;
begin
  Result := __size;
end;

function TLinkedList<T>.IsEmpty: Boolean;
begin
  Result := __size = 0;
end;

function TLinkedList<T>.Remove(index: integer): T;
var
  prev: TNode;
  i: integer;
  res: TNode;
begin
  if (index < 0) or (index >= __size) then
    raise Exception.Create('Remove failed. Index is Illegal.');

  prev := __dummyHead;

  for i := 0 to index - 1 do
    prev := prev.Next;

  res := prev.Next;
  prev.Next := res.Next;
  Dec(__size);

  Result := res.Elment;
  FreeAndNil(res);
end;

procedure TLinkedList<T>.RemoveElement(e: T);
var
  comparer: IComparer<T>;
  prev, del: TNode;
begin
  prev := __dummyHead;
  comparer := TComparer_T.Default;

  while prev.Next <> nil do
  begin
    if comparer.Compare(prev.Next.Elment, e) = 0 then
    begin
      del := prev.Next;
      prev.Next := del.Next;
      Dec(__size);
      FreeAndNil(del);
    end
    else
    begin
      prev := prev.Next;
    end;
  end;
end;

function TLinkedList<T>.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TLinkedList<T>.RemoveLast: T;
begin
  Result := Remove(__size - 1);
end;

procedure TLinkedList<T>.SetElment(d, e: T);
var
  cur: TNode;
  comparer: IComparer<T>;
begin
  cur := __dummyHead.Next;
  comparer := TComparer_T.Default;

  while cur <> nil do
  begin
    if comparer.Compare(cur.Elment, d) = 0 then
      cur.Elment := e;

    cur := cur.Next;
  end;
end;

procedure TLinkedList<T>.Set_(index: integer; e: T);
var
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= __size) then
    raise Exception.Create('Set failed. Index is Illegal.');

  cur := __dummyHead.Next;
  for i := 0 to index - 1 do
    cur := cur.Next;

  cur.Elment := e;
end;

function TLinkedList<T>.ToString: string;
var
  res: TStringBuilder;
  cur: TNode;
begin
  res := TStringBuilder.Create;
  try
    cur := __dummyHead.Next;

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
