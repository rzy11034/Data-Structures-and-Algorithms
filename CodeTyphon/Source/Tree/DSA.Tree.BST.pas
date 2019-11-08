unit DSA.Tree.BST;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.List_Stack_Queue.ArrayListStack,
  DSA.List_Stack_Queue.LoopListQueue,
  DSA.Interfaces.Comparer;

type

  { TBST }

  generic TBST<T, TComparer> = class
  private
    type

    { TNode }

    TNode = class
    public
      Elment: T;
      Left, Right: TNode;
      constructor Create(e: T); overload;
    end;


    TArrayListStack_TNode = specialize TArrayListStack<TNode>;
    TLoopListQueue_TNode = specialize TLoopListQueue<TNode>;

  var
    __root: TNode;
    __size: integer;
    __cmp: specialize IDSA_Comparer<T>;

    /// <summary> 以TNode为根的二分搜索树中是否包含元素e，递归算法 </summary>
    function __contains(node: TNode; e: T): boolean;
    /// <summary>
    /// 向以TNode为根的二分搜索树中插入元素e，递归算法.
    /// 返回插入新节点后二分搜索树的根
    /// </summary>
    function __add(node: TNode; e: T): TNode;
    /// <summary> 前序遍历以node为根的二分搜索树，递归算法 </summary>
    procedure __preOrder(node: TNode);
    /// <summary> 中序遍历以node为根的二分搜索树，递归算法 </summary>
    procedure __inOrder(node: TNode);
    /// <summary> 后序遍历以node为根的二分搜索树，递归算法 </summary>
    procedure __postOrder(node: TNode);
    /// <summary> 返回以node为根的二分搜索树的最小值所在的节点 </summary>
    function __minimum(node: TNode): TNode;
    /// <summary> 返回以node为根的二分搜索树的最大值所在的节点 </summary>
    function __maximum(node: TNode): TNode;
    /// <summary>
    /// 删除二分搜索树的最小值所在的节点, 返回删除节点后二分搜索树的根
    /// </summary>
    function __removeMin(node: TNode): TNode;
    /// <summary>
    /// 删除二分搜索树的最大值所在的节点, 返回删除节点后二分搜索树的根
    /// </summary>
    function __removeMax(node: TNode): TNode;
    /// <summary>
    /// 删除以node为根的二分搜索树中值为e的节点，递归算法.
    /// 返回删除节点后新的二分搜索树的根
    /// </summary>
    function __remove(node: TNode; e: T): TNode;
    /// <summary> 返回e的字符串 </summary>
    function __fromValueToStr(const e: T): string;
    function __destroy(node: TNode): TNode;

  public
    constructor Create;
    destructor Destroy; override;

    function GetSize: integer;
    function IsEmpty: boolean;
    /// <summary> 二分搜索树中是否包含元素e </summary>
    function Contains(e: T): boolean;
    /// <summary> 向二分搜索树中添加新的元素e </summary>
    procedure Add(e: T);
    /// <summary> 二分搜索树的前序遍历 </summary>
    procedure PreOrderNR;
    /// <summary> 二分搜索树的前序遍历 </summary>
    procedure PreOrder;
    /// <summary> 二分搜索树的中序遍历 </summary>
    procedure InOrder;
    /// <summary> 二分搜索树的后序遍历 </summary>
    procedure PostOrder;
    /// <summary> 二分搜索树的层序遍历 </summary>
    procedure LevelOrder;
    /// <summary> 寻找二分搜索树的最小元素 </summary>
    function Minimum: T;
    /// <summary> 寻找二分搜索树的最大元素 </summary>
    function Maximum: T;
    /// <summary> 删除二分搜索树的最小值所在的节点，返回最小值 </summary>
    function RemoveMin: T;
    /// <summary> 删除二分搜索树的最大值所在的节点，返回最大值 </summary>
    function RemoveMax: T;
    /// <summary> 删除二分搜索树中值为e的节点 </summary>
    procedure Remove(e: T);
    /// <summary> 清空BST </summary>
    procedure Clear;
  end;

procedure Main;

implementation

uses
  DSA.List_Stack_Queue.ArrayList;

type
  TComparer_int = specialize TComparer<integer>;
  TBST_int = specialize TBST<integer, TComparer_int>;
  TArrayList_int = specialize TArrayList<integer>;

procedure Main;
var
  arr: array of integer;
  BST: TBST_int;
  i, n: integer;
  nums: TArrayList_int;
begin
  BST := TBST_int.Create;
  arr := [5, 3, 7, 8, 4, 2, 6];

  for i := low(arr) to high(arr) do
    BST.Add(arr[i]);

  BST.PreOrder;
  WriteLn;

  BST.PreOrderNR;
  WriteLn;

  BST.InOrder;
  WriteLn;

  BST.PostOrder;
  WriteLn;

  BST.LevelOrder;
  WriteLn;


  n := 1000;
  Randomize;
  for i := 0 to n - 1 do
    BST.Add(Random(10000) + 1);

  nums := TArrayList_int.Create();
  while BST.IsEmpty = False do
    nums.AddLast(BST.RemoveMin);

  for i := 1 to nums.GetSize - 1 do
    if nums[i - 1] > nums[i] then
      raise Exception.Create('Error.');

  WriteLn(nums.ToString);
  WriteLn('RemoveMin test completed.');

end;

{ TBST<T>.TNode }

constructor TBST.TNode.Create(e: T);
begin
  Elment := e;
  Left := nil;
  Right := nil;
end;

{ TBST<T> }

procedure TBST.Add(e: T);
begin
  __root := __add(__root, e);
end;

function TBST.Contains(e: T): boolean;
begin
  Result := __contains(__root, e);
end;

constructor TBST.Create;
begin
  __root := nil;
  __cmp := TComparer.Default;
end;

destructor TBST.Destroy;
begin
  __root := __destroy(__root);
  FreeAndNil(__root);
  inherited;
end;

function TBST.GetSize: integer;
begin
  Result := __size;
end;

procedure TBST.InOrder;
begin
  __inOrder(__root);
end;

function TBST.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

procedure TBST.LevelOrder;
var
  queue: TLoopListQueue_TNode;
  cur: TNode;
begin
  queue := TLoopListQueue_TNode.Create();
  try
    queue.EnQueue(__root);

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;
      Write(__fromValueToStr(cur.Elment), ' ');

      if cur.Left <> nil then
        queue.EnQueue(cur.Left);
      if cur.Right <> nil then
        queue.EnQueue(cur.Right);
    end;
  finally
    FreeAndNil(queue);
  end;
end;

function TBST.Maximum: T;
begin
  if IsEmpty then
    raise Exception.Create('BST is empty');

  Result := __maximum(__root).Elment;
end;

function TBST.Minimum: T;
begin
  if IsEmpty then
    raise Exception.Create('BST is empty');

  Result := __minimum(__root).Elment;
end;

procedure TBST.PostOrder;
begin
  __postOrder(__root);
end;

procedure TBST.PreOrder;
begin
  __preOrder(__root);
end;

procedure TBST.PreOrderNR;
var
  stack: TArrayListStack_TNode;
  cur: TNode;
begin
  stack := TArrayListStack_TNode.Create();
  try
    stack.Push(__root);

    while not (stack.IsEmpty) do
    begin
      cur := stack.Pop;
      Write(__fromValueToStr(cur.Elment), ' ');

      if cur.Right <> nil then
        stack.Push(cur.Right);
      if cur.Left <> nil then
        stack.Push(cur.Left);
    end;
  finally
    FreeAndNil(stack);
  end;
end;

procedure TBST.Remove(e: T);
begin
  __root := __remove(__root, e);
end;

procedure TBST.Clear;
begin
  __root := __destroy(__root);
end;

function TBST.RemoveMax: T;
begin
  Result := Maximum;
  __root := __removeMax(__root);
end;

function TBST.RemoveMin: T;
begin
  Result := Minimum;
  __root := __removeMin(__root);
end;

function TBST.__add(node: TNode; e: T): TNode;
var
  bool: integer;
begin
  if node = nil then
  begin
    Inc(__size);
    Result := TNode.Create(e);
    Exit;
  end;

  bool := __cmp.Compare(e, node.Elment);
  if bool < 0 then
    node.Left := __add(node.Left, e)
  else if bool > 0 then
    node.Right := __add(node.Right, e);

  Result := node;
end;

function TBST.__contains(node: TNode; e: T): boolean;
var
  bool: integer;
begin
  if node = nil then
  begin
    Result := False;
    Exit;
  end;

  bool := __cmp.Compare(e, node.Elment);
  if bool < 0 then
    Result := __contains(node.Left, e)
  else if bool > 0 then
    Result := __contains(node.Right, e)
  else
    Result := True;
end;

function TBST.__destroy(node: TNode): TNode;
begin
  if node = nil then
    Exit(nil);

  __destroy(node.Left);
  __destroy(node.Right);
  FreeAndNil(node);
  Dec(__size);
  Result := node;
end;

function TBST.__fromValueToStr(const e: T): string;
var
  val: TValue;
begin
  TValue.Make(@e, TypeInfo(T), val);
  Result := val.ToString;
end;

procedure TBST.__inOrder(node: TNode);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left);
  Write(__fromValueToStr(node.Elment), ' ');
  __inOrder(node.Right);
end;

function TBST.__maximum(node: TNode): TNode;
begin
  if node.Right = nil then
  begin
    Exit(node);
  end;

  Result := __maximum(node.Right);
end;

function TBST.__minimum(node: TNode): TNode;
begin
  if node.Left = nil then
  begin
    Exit(node);
  end;

  Result := __minimum(node.Left);
end;

procedure TBST.__postOrder(node: TNode);
begin
  if node = nil then
    Exit;

  __postOrder(node.Left);
  __postOrder(node.Right);
  Write(__fromValueToStr(node.Elment), ' ');
end;

procedure TBST.__preOrder(node: TNode);
begin
  if node = nil then
    Exit;

  Write(__fromValueToStr(node.Elment), ' ');
  __preOrder(node.Left);
  __preOrder(node.Right);
end;

function TBST.__remove(node: TNode; e: T): TNode;
var
  succesorNode: TNode;
  bool: integer;
begin
  if node = nil then
    Exit(nil);

  bool := __cmp.Compare(e, node.Elment);
  if bool < 0 then
  begin
    node.Left := __remove(node.Left, e);
    Result := node;
  end
  else if bool > 0 then
  begin
    node.Right := __remove(node.Right, e);
    Result := node;
  end
  else // e = node.Elment
  begin
    if node.Left = nil then
    begin
      Result := node.Right;
      FreeAndNil(node);
      Dec(__size);
    end
    else if node.Right = nil then
    begin
      Result := node.Left;
      FreeAndNil(node);
      Dec(__size);
    end
    else
    begin
      // 待删除节点左右子树均不空的情况
      // 找到比待删除节点大的最小节点，即待删除节点右子树的最小节点
      // 用这个节点顶替待删除节点的位置
      succesorNode := TNode.Create(__minimum(node.Right).Elment);
      succesorNode.Right := __removeMin(node.Right);
      succesorNode.Left := node.Left;
      FreeAndNil(node);
      Result := succesorNode;
    end;
  end;
end;

function TBST.__removeMax(node: TNode): TNode;
var
  leftNode: TNode;
begin
  if node.Right = nil then
  begin
    leftNode := node.Left;
    Result := leftNode;
    Dec(__size);
    FreeAndNil(node);
    Exit;
  end;

  node.Right := __removeMax(node.Right);
  Result := node;
end;

function TBST.__removeMin(node: TNode): TNode;
var
  rightNode: TNode;
begin
  if node.Left = nil then
  begin
    rightNode := node.Right;
    Result := rightNode;
    Dec(__size);
    FreeAndNil(node);
    Exit;
  end;

  node.Left := __removeMin(node.Left);
  Result := node;
end;

end.
