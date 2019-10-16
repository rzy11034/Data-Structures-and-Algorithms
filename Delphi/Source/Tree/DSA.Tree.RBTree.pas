unit DSA.Tree.RBTree;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type

  TRBTree<K, V> = class
  private type

    TNode = class
    public
      key: K;
      Value: V;
      Left, Right: TNode;
      Color: Boolean;
      constructor Create(newkey: K; newValue: V);
    end;

    TPtrV = TPtr_V<V>;
    TArrayList_K = TArrayList<K>;

  const
    RED = True;
    BLACK = False;

  var
    __root: TNode;
    __size: integer;
    __comparer: IComparer<K>;

    /// <summary>
    /// 向以node为根的红黑树中插入元素(key, value)，递归算法.
    /// 返回插入新节点后红黑树的根
    /// </summary>
    function __add(node: TNode; key: K; Value: V): TNode;
    function __getNode(node: TNode; key: K): TNode;
    function __isRBTree(node: TNode; const blackCount: integer;
      count: integer): Boolean;
    function __remove(node: TNode; key: K): TNode;
    function __minimum(node: TNode): TNode;
    procedure __inOrder(node: TNode; list: TArrayList_K);
    /// <summary> 判断节点node的颜色 </summary>
    function __isRed(node: TNode): Boolean;
    /// <summary> 颜色翻转 </summary>
    procedure __flipColors(node: TNode);

    /// <summary>
    /// 对节点y进行向右旋转操作，返回旋转后新的根节点x
    /// </summary>
    /// <remarks>
    /// <code>
    /// 右旋转操作:
    /// <para>       node                   x               </para>
    /// <para>      /   \     右旋转       /  \             </para>
    /// <para>     x    T2   ------->   y    node           </para>
    /// <para>    / \                        /  \           </para>
    /// <para>   y  T1                     T1   T2          </para>
    /// </code>
    /// </remarks>
    function __rightRotate(node: TNode): TNode;

    /// <summary>
    /// 向左旋转
    /// </summary>
    ///  /// <remarks>
    /// <code>
    /// 左旋转操作:
    /// <para>     node                     x              </para>
    /// <para>    /   \     左旋转        /  \             </para>
    /// <para>   T1    x   --------->   node   T3          </para>
    /// <para>        / \              /   \               </para>
    /// <para>      T2  T3           T1    T2              </para>
    /// </code>
    /// </remarks>
    function __leftRotate(node: TNode): TNode;

  public
    constructor Create();
    function Contains(key: K): Boolean;
    /// <summary> 判断该二叉树是否是一棵二分搜索树 </summary>
    function IsBST: Boolean;
    function Get(key: K): TPtrV;
    function GetSize: integer;
    function IsEmpty: Boolean;
    function IsRBTree: Boolean;
    function Remove(key: K): TPtrV;
    procedure Add(key: K; Value: V);
    procedure Set_(key: K; newValue: V);
    function KeySets: TArrayList_K;
  end;

procedure Main;

implementation

type
  TRBTree_str_int = TRBTree<string, integer>;

procedure Main;
var
  words: TArrayList_str;
  rbt: TRBTree_str_int;
  i: integer;
begin
  words := TArrayList_str.Create();
  if TDsaUtils.ReadFile(FILE_PATH + A_File_Name, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  rbt := TRBTree_str_int.Create;
  for i := 0 to words.GetSize - 1 do
  begin
    if rbt.Contains(words[i]) then
      rbt.Set_(words[i], rbt.Get(words[i]).PValue^ + 1)
    else
      rbt.Add(words[i], 1);
  end;

  Writeln('Total different words: ', rbt.GetSize);
  TDsaUtils.DrawLine;
  Writeln('Frequency of pride: ', rbt.Get('pride').PValue^);
  Writeln('Frequency of prejudice: ', rbt.Get('prejudice').PValue^);
  Writeln('IsBST: ', rbt.IsBST);
  Writeln('IsRBTree: ', rbt.IsRBTree);
end;

{ TRBTree<K, V>.TNode }

constructor TRBTree<K, V>.TNode.Create(newkey: K; newValue: V);
begin
  Self.key := newkey;
  Self.Value := newValue;
  Left := nil;
  Right := nil;
  Color := RED;
end;

{ TRBTree<K, V> }

procedure TRBTree<K, V>.Add(key: K; Value: V);
begin
  __root := __add(__root, key, Value);
  __root.Color := BLACK; // 最终根节点为黑色节点
end;

function TRBTree<K, V>.Contains(key: K): Boolean;
begin
  Result := __getNode(__root, key) <> nil;
end;

constructor TRBTree<K, V>.Create;
begin
  inherited;
  __comparer := TComparer<K>.Default;
end;

procedure TRBTree<K, V>.__flipColors(node: TNode);
begin
  node.Color := RED;
  node.Left.Color := BLACK;
  node.Right.Color := BLACK;
end;

function TRBTree<K, V>.Get(key: K): TPtrV;
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
    Result.PValue := nil
  else
    Result.PValue := @node.Value;
end;

function TRBTree<K, V>.GetSize: integer;
begin
  Result := __size;
end;

function TRBTree<K, V>.IsBST: Boolean;
var
  list: TArrayList_K;
  i: integer;
  bool: Boolean;
begin
  bool := True;
  list := TArrayList_K.Create();
  __inOrder(__root, list);

  for i := 1 to list.GetSize - 1 do
    if __comparer.Compare(list.Get(i - 1), list.Get(i)) > 0 then
      bool := False
    else
      bool := True;

  Result := bool;
end;

function TRBTree<K, V>.IsEmpty: Boolean;
begin
  Result := __size = 0;
end;

function TRBTree<K, V>.IsRBTree: Boolean;
var
  cur: TNode;
  blackCount: integer;
begin
  //  1、红黑树所有的节点都有颜色（红或黑）
  //  2、红黑树的根结点是黑色的
  //  3、红黑树的两个红色节点不能相连
  //  4、红黑树的每一条链的黑节点的个数相同
  //  5、所有空的节点都是黑色的

  if __root = nil then
  begin
    Result := True;
    Exit;
  end;

  if __isRed(__root) then // 违反 2.红黑树的根结点是黑色的
  begin
    Result := False;
    Exit;
  end;

  // 统计单一链上黑色结点的个数 , 这里以最左边的那一条链统计
  blackCount := 0;
  cur := __root;
  while cur <> nil do
  begin
    if cur.Color = BLACK then
      inc(blackCount);

    cur := cur.Left;
  end;

  // 验证性质 4.每条链上的黑色结点都相等”，验证性质 3.红色结点不能相连
  Result := __isRBTree(__root, blackCount, 0)
end;

function TRBTree<K, V>.KeySets: TArrayList_K;
var
  list: TArrayList_K;
begin
  list := TArrayList_K.Create;
  __inOrder(__root, list);
  Result := list;
end;

function TRBTree<K, V>.Remove(key: K): TPtrV;
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
  begin
    Result.PValue := nil;
    Exit;
  end;

  __root := __remove(__root, key);
  Result.PValue := @node.Value;
end;

procedure TRBTree<K, V>.Set_(key: K; newValue: V);
var
  node: TNode;
  Value: TValue;
  s: string;
begin
  node := __getNode(__root, key);

  if node = nil then
  begin
    TValue.Make(@key, TypeInfo(K), Value);
    s := Value.ToString;
    raise Exception.Create(s + ' doesn''t exist!');
  end
  else
    node.Value := newValue;
end;

function TRBTree<K, V>.__add(node: TNode; key: K; Value: V): TNode;
var
  bool: integer;
begin
  if node = nil then
  begin
    inc(__size);
    Result := TNode.Create(key, Value);
    Exit;
  end;

  bool := __comparer.Compare(key, node.key);
  if bool < 0 then
    node.Left := __add(node.Left, key, Value)
  else if bool > 0 then
    node.Right := __add(node.Right, key, Value)
  else
    node.Value := Value;

  if __isRed(node.Right) and not(__isRed(node.Left)) then
    node := __leftRotate(node);

  if (__isRed(node.Left)) and __isRed(node.Left.Left) then
    node := __rightRotate(node);

  if __isRed(node.Left) and __isRed(node.Right) then
    __flipColors(node);

  Result := node;
end;

function TRBTree<K, V>.__getNode(node: TNode; key: K): TNode;
var
  bool: integer;
begin
  if node = nil then
    Exit(nil);

  bool := __comparer.Compare(key, node.key);
  if bool < 0 then
    Result := __getNode(node.Left, key)
  else if bool > 0 then
    Result := __getNode(node.Right, key)
  else
    Result := node;
end;

procedure TRBTree<K, V>.__inOrder(node: TNode; list: TArrayList_K);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node.key);
  __inOrder(node.Right, list);
end;

function TRBTree<K, V>.__isRBTree(node: TNode; const blackCount: integer;
  count: integer): Boolean;
begin
  if node = nil then
  begin
    if count = blackCount then
      Result := True
    else
      Result := False;

    Exit;
  end;

  if (node.Color = BLACK) then
    inc(count);

  //违反性质 3.红色结点不能相连
  if (node.Left <> nil) and (__isRed(node) and __isRed(node.Left)) then
  begin
    Result := False;
    Exit;
  end;

  if (node.Right <> nil) and (__isRed(node) and __isRed(node.Right)) then
  begin
    Result := False;
    Exit;
  end;

  Result := __isRBTree(node.Left, blackCount, count) and
    __isRBTree(node.Right, blackCount, count);
end;

function TRBTree<K, V>.__isRed(node: TNode): Boolean;
begin
  if node = nil then
  begin
    Result := BLACK;
    Exit;
  end;

  Result := node.Color;
end;

function TRBTree<K, V>.__leftRotate(node: TNode): TNode;
var
  x: TNode;
begin
  x := node.Right;

  // 左旋转
  node.Right := x.Left;
  x.Left := node;

  x.Color := node.Color;
  node.Color := RED;

  Result := x;
end;

function TRBTree<K, V>.__minimum(node: TNode): TNode;
begin
  if node.Left = nil then
  begin
    Result := node;
    Exit;
  end;

  Result := __minimum(node.Left);
end;

function TRBTree<K, V>.__remove(node: TNode; key: K): TNode;
var
  leftNode, rightNode, succesorNode, minNode, retNode: TNode;
  bool: integer;
begin
  leftNode := nil;
  rightNode := nil;
  succesorNode := nil;

  if node = nil then
  begin
    Result := nil;
    Exit;
  end;

  bool := __comparer.Compare(key, node.key);
  if bool < 0 then
  begin
    node.Left := __remove(node.Left, key);
    retNode := node;
  end
  else if bool > 0 then
  begin
    node.Right := __remove(node.Right, key);
    retNode := node;
  end
  else // e = node.e
  begin
    if node.Left = nil then
    begin
      rightNode := node.Right;
      FreeAndNil(node);
      Dec(__size);
      retNode := rightNode;
    end
    else if node.Right = nil then
    begin
      leftNode := node.Left;
      FreeAndNil(node);
      Dec(__size);
      retNode := leftNode;
    end
    else
    begin
      // 待删除节点左右子树均不空的情况
      // 找到比待删除节点大的最小节点，即待删除节点右子树的最小节点
      // 用这个节点顶替待删除节点的位置
      minNode := __minimum(node.Right);
      succesorNode := TNode.Create(minNode.key, minNode.Value);
      succesorNode.Right := __remove(node.Right, succesorNode.key);
      succesorNode.Left := node.Left;
      FreeAndNil(node);
      retNode := succesorNode;
    end;
  end;

  // todo: 2019-08-29 删除代没有完成.

  Result := retNode;
end;

function TRBTree<K, V>.__rightRotate(node: TNode): TNode;
var
  x: TNode;
begin
  x := node.Left;

  // 右旋转
  node.Left := x.Right;
  x.Right := node;

  x.Color := node.Color;
  node.Color := RED;

  Result := x;
end;

end.
