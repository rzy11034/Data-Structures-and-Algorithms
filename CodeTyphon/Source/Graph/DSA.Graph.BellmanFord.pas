unit DSA.Graph.BellmanFord;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Graph.Edge,
  DSA.List_Stack_Queue.ArrayList,
  DSA.List_Stack_Queue.LinkedListStack;

type
  generic TBellmanFord<T> = class
  private
    type
    TEdge_T = specialize TEdge<T>;
    TList_Edge = specialize TArrayList<TEdge_T>;
    TArrEdge = specialize TArray<TEdge_T>;
    TWeightGraph_T = specialize TWeightGraph<T>;
    TCmp_T = specialize TComparer<T>;
    TStack_Edge = specialize TLinkedListStack<TEdge_T>;

  var
    __g: TWeightGraph_T;
    __s: integer;         // 起始点
    __distTo: array of T; // distTo[i]存储从起始点s到i的最短路径长度
    __from: TArrEdge; // from[i]记录最短路径中, 到达i点的边是哪一条,可以用来恢复整个最短路径
    __hasNegativeCycle: boolean; // 标记图中是否有负权环

    /// <summary> 判断图中是否有负权环 </summary>
    function __detectNegativeCycle: boolean;

  public
    constructor Create(g: TWeightGraph_T; s: integer);
    destructor Destroy; override;

    /// <summary> // 返回图中是否有负权环 </summary>
    function NegativeCycle: boolean;
    /// <summary> 返回从a点到b点的最短路径长度 </summary>
    function ShortestPathTo(b: integer): T;
    /// <summary> 判断从a点到b点是否联通 </summary>
    function HasPathTo(b: integer): boolean;
    /// <summary> 寻找从a到b的最短路径, 将整个路径经过的边存放在vec中 </summary>
    function ShortestPath(b: integer): TList_Edge;
    /// <summary> 打印出从a点到b点的路径 </summary>
    procedure ShowPath(b: integer);
  end;

procedure Main;

implementation

uses
  DSA.Graph.SparseWeightedGraph,
  DSA.Utils;

type
  TSparseWeightedGraph_int = specialize TSparseWeightedGraph<integer>;
  TBellmanFord_int = specialize TBellmanFord<integer>;
  TReadGraphWeight_int = specialize TReadGraphWeight<integer>;

procedure Main;
var
  g: TSparseWeightedGraph_int;
  b: TBellmanFord_int;
  fileName: string;
  i, v, s: integer;
begin
  //fileName := WEIGHT_GRAPH_FILE_NAME_6;
  fileName := WEIGHTGRAPH_NEGATIVE_CIRCLE_FILE_NAME;
  v := 5;

  g := TSparseWeightedGraph_int.Create(v, True);
  TReadGraphWeight_int.Execute(g, FILE_PATH + fileName);

  WriteLn('Test Bellman-Ford:');

  s := 0;
  b := TBellmanFord_int.Create(g, s);
  if b.NegativeCycle then
    WriteLn('The graph contain negative cycle!')
  else
  begin
    for i := 0 to g.Vertex - 1 do
    begin
      if i = s then Continue;

      if b.HasPathTo(i) then
      begin
        WriteLn('Shortest Path to ', i, ' : ', b.ShortestPathTo(i));
        b.ShowPath(i);
      end
      else
        WriteLn('No Path to ', i);

      WriteLn('----------');
    end;
  end;
end;

{ TBellmanFord }

constructor TBellmanFord.Create(g: TWeightGraph_T; s: integer);
var
  bool, pass, i: integer;
  e: TEdge_T;
  cmp: TCmp_T;
begin
  __s := s;
  __g := g;

  SetLength(__distTo, g.Vertex);
  SetLength(__from, g.Vertex);
  cmp := TCmp_T.Default;

  // 初始化所有的节点s都不可达, 由from数组来表示
  for i := 0 to g.Vertex - 1 do
    __from[i] := nil;

  // 设置distTo[s] = 0, 并且让from[s]不为 nil, 表示初始s节点可达且距离为 0
  __distTo[s] := Default(T);
  __from[s] := TEdge_T.Create(s, s, Default(T));

  // Bellman-Ford的过程
  // 进行V-1次循环, 每一次循环求出从起点到其余所有点, 最多使用pass步可到达的最短距离
  for pass := 1 to g.Vertex - 1 do
  begin
    // 每次循环中对所有的边进行一遍松弛操作
    // 遍历所有边的方式是先遍历所有的顶点, 然后遍历和所有顶点相邻的所有边
    for i := 0 to g.Vertex - 1 do
    begin
      // 使用我们实现的邻边迭代器遍历和所有顶点相邻的所有边
      for e in g.AdjIterator(i) do
      begin
        // 对于每一个边首先判断e->v()可达
        // 之后看如果e->w()以前没有到达过， 显然我们可以更新distTo[e->w()]
        // 或者e->w()以前虽然到达过, 但是通过这个e我们可以获得一个更短的距离,
        // 即可以进行一次松弛操作, 我们也可以更新distTo[e->w()]
        bool := cmp.Compare(__distTo[e.VertexA] + e.Weight, __distTo[e.VertexB]);
        if (__from[e.VertexA] <> nil) and ((__from[e.VertexB] = nil) or (bool < 0)) then
        begin
          __distTo[e.VertexB] := __distTo[e.VertexA] + e.Weight;
          __from[e.VertexB] := e;
        end;
      end;
    end;
  end;

  __hasNegativeCycle := __detectNegativeCycle;
end;

destructor TBellmanFord.Destroy;
begin
  inherited Destroy;
end;

function TBellmanFord.HasPathTo(b: integer): boolean;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Result := __from[b] <> nil;
end;

function TBellmanFord.NegativeCycle: boolean;
begin
  Result := __hasNegativeCycle;
end;

function TBellmanFord.ShortestPath(b: integer): TList_Edge;
var
  stack: TStack_Edge;
  e: TEdge_T;
  ret: TList_Edge;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Assert(not __hasNegativeCycle);
  Assert(HasPathTo(b));

  // 通过from数组逆向查找到从s到w的路径, 存放到栈中
  stack := TStack_Edge.Create;
  e := __from[b];
  while e.VertexA <> __s do
  begin
    stack.Push(e);
    e := __from[e.VertexA];
  end;
  stack.Push(e);

  ret := TList_Edge.Create;
  // 从栈中依次取出元素, 获得顺序的从s到w的路径
  while not stack.IsEmpty do
  begin
    e := stack.Pop;
    ret.AddLast(e);
  end;

  Result := ret;
end;

function TBellmanFord.ShortestPathTo(b: integer): T;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Assert(not __hasNegativeCycle);
  Assert(HasPathTo(b));
  Result := __distTo[b];
end;

procedure TBellmanFord.ShowPath(b: integer);
var
  list: TList_Edge;
  i: integer;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Assert(not __hasNegativeCycle);
  Assert(HasPathTo(b));

  list := ShortestPath(b);
  for i := 0 to list.GetSize - 1 do
  begin
    Write(list[i].VertexA, ' -> ');
    if i = list.GetSize - 1 then
      WriteLn(list[i].VertexB);
  end;
end;

function TBellmanFord.__detectNegativeCycle: boolean;
var
  i: integer;
  e: TEdge_T;
  cmp: TCmp_T;
  bool: integer;
begin
  cmp := TCmp_T.Default;
  Result := False;

  for i := 0 to __g.Vertex - 1 do
  begin
    for e in __g.AdjIterator(i) do
    begin
      bool := cmp.Compare(__distTo[e.VertexA] + e.Weight, __distTo[e.VertexB]);
      if (__from[e.VertexA] <> nil) and (bool < 0) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

end.
