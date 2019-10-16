//
//  677. 键值映射
//
//  实现一个 MapSum 类里的两个方法，insert 和 sum。
//
//  对于方法 insert，你将得到一对（字符串，整数）的键值对。字符串表示键，整数表示值。如果键已经存在，那么原来的键值对将被替代成新的键值对。
//
//  对于方法 sum，你将得到一个表示前缀的字符串，你需要返回所有以该前缀开头的键的值的总和。
//
//  示例 1:
//
//  输入: insert("apple", 3), 输出: Null
//  输入: sum("ap"), 输出: 3
//  输入: insert("app", 2), 输出: Null
//  输入: sum("ap"), 输出: 5
//
//    class MapSum {
//
//      /** Initialize your data structure here. */
//      public MapSum() {
//
//      }
//
//      public void insert(String key, int val) {
//
//      }
//
//      public int sum(String prefix) {
//
//      }
//    }
//
//    /**
//     * Your MapSum object will be instantiated and called as such:
//     * MapSum obj = new MapSum();
//     * obj.insert(key,val);
//     * int param_2 = obj.sum(prefix);
//     */

unit DSA.LeetCode._677;

interface

uses
  System.SysUtils,
  DSA.Tree.BSTMap;

type
  TMapSum = class
  private type
    TNode = class
    private type
      TreeMap = TBSTMap<Char, TNode>;
    public
      Value: Integer;
      Next: TreeMap;
      constructor Create(newValue: Integer = 0);
    end;

  var
    __root: TNode;

    function __sum(node: TNode): Integer;

  public
    constructor Create;
    procedure Insert(word: string; val: Integer);
    function Sum(prefix: string): Integer;
  end;

procedure Main();

implementation

procedure Main();
var
  ms: TMapSum;
  i: Integer;
begin
  ms := TMapSum.Create;

  with ms do
  begin
    Insert('apple', 3);
    i := Sum('ap');
    Writeln(i);

    Insert('app', 2);
    i := Sum('ap');
    Writeln(i);
  end;
end;

{ TMapSum.TNode }

constructor TMapSum.TNode.Create(newValue: Integer);
begin
  Value := newValue;
  Next := TreeMap.Create;
end;

{ TMapSum }

constructor TMapSum.Create;
begin
  __root := TNode.Create();
end;

procedure TMapSum.Insert(word: string; val: Integer);
var
  cur: TNode;
  i: Integer;
  c: Char;
begin
  cur := __root;

  for i := low(word) to high(word) do
  begin
    c := word[i];

    if cur.Next.Contains(c) = False then
      cur.Next.Add(c, TNode.Create());

    cur := cur.Next.Get(c).PValue^;
  end;

  cur.Value := val;
end;

function TMapSum.Sum(prefix: string): Integer;
var
  cur: TNode;
  i: Integer;
  c: Char;
begin
  cur := __root;

  for i := 0 to prefix.Length - 1 do
  begin
    c := prefix.Chars[i];

    if cur.Next.Get(c).PValue = nil then
      Exit(0);

    cur := cur.Next.Get(c).PValue^;
  end;

  Result := __sum(cur);
end;

function TMapSum.__sum(node: TNode): Integer;
var
  res: Integer;
  c: Char;
begin
  if node.Next.IsEmpty then
    Exit(node.Value);

  res := node.Value;
  for c in node.Next.KeySets.ToArray do
  begin
    res := res + __sum(node.Next.Get(c).PValue^);
  end;

  Result := res;
end;

end.
