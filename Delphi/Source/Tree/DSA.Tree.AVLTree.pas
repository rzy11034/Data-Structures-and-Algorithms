unit DSA.Tree.AVLTree;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Math,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type

  { TAVLTree }

  TAVLTree<K, V> = class
  private type

    { TNode }

    TNode = class
    public
      key: K;
      Value: V;
      Left, Right: TNode;
      Height: integer;
      constructor Create(newkey: K; newValue: V);
    end;

    TPtrV = TPtr_V<V>;
    TArrayList_K = TArrayList<K>;

  var
    __root: TNode;
    __size: integer;
    __comparer: IComparer<K>;

    function __add(node: TNode; key: K; Value: V): TNode;
    /// <summary> 获得节点node的平衡因子 </summary>
    function __getBalanceFactor(node: TNode): integer;
    /// <summary> 获得节点node的高度 </summary>
    function __getHeight(node: TNode): integer;
    function __getNode(node: TNode; key: K): TNode;
    function __remove(node: TNode; key: K): TNode;
    function __minimum(node: TNode): TNode;
    procedure __inOrder(node: TNode; list: TArrayList_K);
    /// <summary> 判断以node为根的二叉树是否是一棵平衡二叉树， 递归算法。 </summary>
    function __isBalanced(node: TNode): boolean;

    /// <summary>
    /// 对节点y进行向右旋转操作，返回旋转后新的根节点x
    /// </summary>
    /// <remarks>
    /// <code>
    /// 右旋转操作:
    /// <para>          y                          x       </para>
    /// <para>         / \                       /   \     </para>
    /// <para>        x  T4     向右旋转        z     y    </para>
    /// <para>       / \       - - - - - ->    / \   / \   </para>
    /// <para>      z   T3                   T1  T2 T3 T4  </para>
    /// <para>     / \                                     </para>
    /// <para>    T1 T2                                    </para>
    /// </code>
    /// </remarks>
    function __rightRotate(node: TNode): TNode;

    /// <summary>
    /// 对节点y进行向左旋转操作，返回旋转后新的根节点x
    /// </summary>
    /// <remarks>
    /// <code>
    /// 左旋转操作:
    /// <para>          y                          x       </para>
    /// <para>         / \                       /   \     </para>
    /// <para>        T1  x      向左旋转       y     z    </para>
    /// <para>           / \    - - - - - ->   / \   / \   </para>
    /// <para>          T2  z                 T1 T2 T3 T4  </para>
    /// <para>             / \                             </para>
    /// <para>            T3 T4                            </para>
    /// </code>
    /// </remarks>
    function __leftRotate(node: TNode): TNode;

  public
    constructor Create();
    function Contains(key: K): boolean;
    /// <summary> 判断该二叉树是否是一棵平衡二叉树 </summary>
    function IsBalanced: boolean;
    /// <summary> 判断该二叉树是否是一棵二分搜索树 </summary>
    function IsBST: boolean;
    function Get(key: K): TPtrV;
    function GetSize: integer;
    function IsEmpty: boolean;
    function Remove(key: K): TPtrV;
    procedure Add(key: K; Value: V);
    procedure Set_(key: K; newValue: V);
    function KeySets: TArrayList_K;

  end;

procedure Main;

implementation

type
  TAVLTree_str_int = TAVLTree<string, integer>;

procedure Main;
var
  words: TArrayList_str;
  alt: TAVLTree_str_int;
  i: integer;
  str: string;
begin
  words := TArrayList_str.Create();
  if TDsaUtils.ReadFile(FILE_PATH + A_File_Name, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  alt := TAVLTree_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if alt.Contains(words[i]) then
      alt.Set_(words[i], alt.Get(words[i]).PValue^ + 1)
    else
      alt.Add(words[i], 1);
  end;

  Writeln('Total different words: ', alt.GetSize);
  TDsaUtils.DrawLine;
  Writeln('Frequency of pride: ', alt.Get('pride').PValue^);
  Writeln('Frequency of prejudice: ', alt.Get('prejudice').PValue^);
  Writeln('IsBST: ', alt.IsBST);
  Writeln('IsBalanced :', alt.IsBalanced);

  for str in alt.KeySets.ToArray do
  begin
    alt.Remove(str);

    if (not alt.IsBalanced) or (not alt.IsBST) then
      Writeln(alt.GetSize);
  end;
end;

{ TAVLTree }

procedure TAVLTree<K, V>.Add(key: K; Value: V);
begin
  __root := __add(__root, key, Value);
end;

function TAVLTree<K, V>.Contains(key: K): boolean;
begin
  Result := __getNode(__root, key) <> nil;
end;

constructor TAVLTree<K, V>.Create();
begin
  inherited;
  __comparer := TComparer<K>.Default;
end;

function TAVLTree<K, V>.Get(key: K): TPtrV;
var
  node: TNode;
begin
  node := __getNode(__root, key);

  if node = nil then
    Result.PValue := nil
  else
    Result.PValue := @node.Value;
end;

function TAVLTree<K, V>.GetSize: integer;
begin
  Result := __size;
end;

function TAVLTree<K, V>.IsBalanced: boolean;
begin
  Result := __isBalanced(__root);
end;

function TAVLTree<K, V>.IsBST: boolean;
var
  list: TArrayList_K;
  i: integer;
  bool: boolean;
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

function TAVLTree<K, V>.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

function TAVLTree<K, V>.Remove(key: K): TPtrV;
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

procedure TAVLTree<K, V>.Set_(key: K; newValue: V);
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

function TAVLTree<K, V>.KeySets: TArrayList_K;
var
  list: TArrayList_K;
begin
  list := TArrayList_K.Create;
  __inOrder(__root, list);
  Result := list;
end;

function TAVLTree<K, V>.__add(node: TNode; key: K; Value: V): TNode;
var
  bool: integer;
  balanceFactor, Left, Right: integer;
begin
  if node = nil then
  begin
    Inc(__size);
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

  // 更新Height
  node.Height := Max(__getHeight(node.Left), __getHeight(node.Right)) + 1;

  // 计算平衡因子
  balanceFactor := __getBalanceFactor(node);

  // 平衡维护
  // LL
  if (balanceFactor > 1) and (__getBalanceFactor(node.Left) >= 0) then
  begin
    Result := __rightRotate(node);
    Exit;
  end;

  // RR
  if (balanceFactor < -1) and (__getBalanceFactor(node.Right) <= 0) then
  begin
    Result := __leftRotate(node);
    Exit;
  end;

  // LR
  if (balanceFactor > 1) and (__getBalanceFactor(node.Left) < 0) then
  begin
    node.Left := __leftRotate(node.Left);
    Result := __rightRotate(node);
    Exit;
  end;

  // RL
  if (balanceFactor < -1) and (__getBalanceFactor(node.Right) > 0) then
  begin
    node.Right := __rightRotate(node.Right);
    Result := __leftRotate(node);
    Exit;
  end;

  Result := node;
end;

function TAVLTree<K, V>.__getBalanceFactor(node: TNode): integer;
begin
  if node = nil then
  begin
    Result := 0;
    Exit;
  end;

  Result := __getHeight(node.Left) - __getHeight(node.Right);
end;

function TAVLTree<K, V>.__getHeight(node: TNode): integer;
begin
  if node = nil then
  begin
    Result := 0;
    Exit;
  end;

  Result := node.Height;
end;

function TAVLTree<K, V>.__getNode(node: TNode; key: K): TNode;
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

function TAVLTree<K, V>.__minimum(node: TNode): TNode;
begin
  if node.Left = nil then
  begin
    Result := node;
    Exit;
  end;

  Result := __minimum(node.Left);
end;

procedure TAVLTree<K, V>.__inOrder(node: TNode; list: TArrayList_K);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node.key);
  __inOrder(node.Right, list);
end;

function TAVLTree<K, V>.__isBalanced(node: TNode): boolean;
var
  balanceFactor: integer;
begin
  if node = nil then
  begin
    Result := True;
    Exit;
  end;

  balanceFactor := __getBalanceFactor(node);
  if Abs(balanceFactor) > 1 then
    Exit(False);

  Result := __isBalanced(node.Left) and __isBalanced(node.Right);
end;

function TAVLTree<K, V>.__leftRotate(node: TNode): TNode;
var
  x, y, T2: TNode;
begin
  y := node;

  x := y.Right;
  T2 := x.Left;

  // 向左旋转过程
  x.Left := y;
  y.Right := T2;

  // 更新height
  y.Height := Max(__getHeight(y.Left), __getHeight(y.Right)) + 1;
  x.Height := Max(__getHeight(x.Left), __getHeight(x.Right)) + 1;

  Result := x;
end;

function TAVLTree<K, V>.__remove(node: TNode; key: K): TNode;
var
  leftNode, rightNode, succesorNode, minNode, retNode: TNode;
  bool, balanceFactor: integer;
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

  if retNode = nil then
  begin
    Result := nil;
    Exit;
  end;

  // 更新Height
  retNode.Height := 1 + Max(__getHeight(retNode.Left),
    __getHeight(retNode.Right));

  // 计算平衡因子
  balanceFactor := __getBalanceFactor(retNode);

  // 平衡维护
  // LL
  if (balanceFactor > 1) and (__getBalanceFactor(retNode.Left) >= 0) then
  begin
    Result := __rightRotate(retNode);
    Exit;
  end;

  // RR
  if (balanceFactor < -1) and (__getBalanceFactor(retNode.Right) <= 0) then
  begin
    Result := __leftRotate(retNode);
    Exit;
  end;

  // LR
  if (balanceFactor > 1) and (__getBalanceFactor(retNode.Left) < 0) then
  begin
    retNode.Left := __leftRotate(retNode.Left);
    Result := __rightRotate(retNode);
    Exit;
  end;

  // RL
  if (balanceFactor < -1) and (__getBalanceFactor(retNode.Right) > 0) then
  begin
    retNode.Right := __rightRotate(retNode.Right);
    Result := __leftRotate(retNode);
    Exit;
  end;

  Result := retNode;
end;

function TAVLTree<K, V>.__rightRotate(node: TNode): TNode;
var
  x, y, T3: TNode;
begin
  y := node;

  x := y.Left;
  T3 := x.Right;

  // 右旋转过程
  x.Right := y;
  y.Left := T3;

  // 更新height
  y.Height := Max(__getHeight(y.Left), __getHeight(y.Right)) + 1;
  x.Height := Max(__getHeight(x.Left), __getHeight(x.Right)) + 1;

  Result := x;
end;

{ TAVLTree.TNode }

constructor TAVLTree<K, V>.TNode.Create(newkey: K; newValue: V);
begin
  Self.key := newkey;
  Self.Value := newValue;
  Left := nil;
  Right := nil;
  Height := 1;
end;

end.
