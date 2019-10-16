unit DSA.Graph.Dijkstra;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Tree.IndexHeap,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Graph.Edge,
  DSA.List_Stack_Queue.ArrayList,
  DSA.List_Stack_Queue.LinkedListStack;

type
  generic TDijkstra<T> = class
  private
    type
    TArr_bool = specialize TArray<boolean>;
    TEdge_T = specialize TEdge<T>;
    TMinIndexHeap = specialize TIndexHeap<T>;
    TList_Edge = specialize TArrayList<TEdge_T>;
    TArrEdge = specialize TArray<TEdge_T>;
    TWeightGraph_T = specialize TWeightGraph<T>;
    TCmp_T = specialize TComparer<T>;
    TStack_Edge = specialize TLinkedListStack<TEdge_T>;

  var
    __g: TWeightGraph_T;
    __s: integer;         // 起始点
    __distTo: array of T; // distTo[i]存储从起始点s到i的最短路径长度
    __marked: TArr_bool;  // 标记数组, 在算法运行过程中标记节点i是否被访问
    __from: TArrEdge;     // from[i]记录最短路径中, 到达i点的边是哪一条,可以用来恢复整个最短路径

  public
    constructor Create(g: TWeightGraph_T; s: integer);
    destructor Destroy; override;

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
  TDijkstra_int = specialize TDijkstra<integer>;
  TReadGraphWeight_int = specialize TReadGraphWeight<integer>;

procedure Main;
var
  g: TSparseWeightedGraph_int;
  d: TDijkstra_int;
  fileName: string;
  i, v: integer;
begin
  fileName := WEIGHT_GRAPH_FILE_NAME_5;
  v := 5;


  g := TSparseWeightedGraph_int.Create(v, False);
  TReadGraphWeight_int.Execute(g, FILE_PATH + fileName);
  WriteLn('Test Dijkstra:'); d := TDijkstra_int.Create(g, 0);
  for i := 0 to v - 1 do
  begin
    if d.HasPathTo(i) then
    begin
      WriteLn('Shortest Path to ', i, ' : ', d.ShortestPathTo(i));
      d.ShowPath(i);
    end
    else
    begin
      WriteLn('No Path to ', i);
    end;

    WriteLn('----------');
  end;
end;

{ TDijkstra }

constructor TDijkstra.Create(g: TWeightGraph_T; s: integer);
var
  ipq: TMinIndexHeap;
  v, w, bool: integer;
  e: TEdge_T;
  cmp: TCmp_T;
begin
  __s := s;
  __g := g;

  SetLength(__distTo, g.Vertex);
  SetLength(__marked, g.Vertex);
  SetLength(__from, g.Vertex);
  cmp := TCmp_T.Default;

  // 使用索引堆记录当前找到的到达每个顶点的最短距离
  ipq := TMinIndexHeap.Create(g.Vertex, THeapkind.Min);

  __distTo[s] := Default(T);
  __from[s] := TEdge_T.Create(s, s, Default(T));
  __marked[s] := True;
  ipq.Insert(s, __distTo[s]);
  while not ipq.IsEmpty do
  begin
    v := ipq.ExtractFirstIndex;

    // distTo[v]就是s到v的最短距离
    __marked[v] := True;

    // 对v的所有相邻节点进行更新
    for e in g.AdjIterator(v) do
    begin
      w := e.OtherVertex(v);

      if not __marked[w] then
      begin
        // 如果w点以前没有访问过,
        // 或者访问过, 但是通过当前的v点到w点距离更短, 则进行更新
        bool := cmp.Compare(__distTo[v] + e.Weight, __distTo[w]);
        if (__from[w] = nil) or (bool < 0) then
        begin
          __distTo[w] := __distTo[v] + e.Weight;
          __from[w] := e;

          if ipq.Contain(w) then
            ipq.Change(w, __distTo[w])
          else
            ipq.Insert(w, __distTo[w]);
        end;
      end;
    end;
  end;
end;

destructor TDijkstra.Destroy;
begin
  inherited Destroy;
end;

function TDijkstra.HasPathTo(b: integer): boolean;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Result := __marked[b];
end;

function TDijkstra.ShortestPath(b: integer): TList_Edge;
var
  stack: TStack_Edge;
  e: TEdge_T;
  ret: TList_Edge;
begin
  Assert((b >= 0) and (b < __g.Vertex));
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

function TDijkstra.ShortestPathTo(b: integer): T;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Assert(HasPathTo(b));
  Result := __distTo[b];
end;

procedure TDijkstra.ShowPath(b: integer);
var
  list: TList_Edge;
  i: integer;
begin
  Assert((b >= 0) and (b < __g.Vertex));
  Assert(HasPathTo(b));

  list := ShortestPath(b);
  for i := 0 to list.GetSize - 1 do
  begin
    Write(list[i].VertexA, ' -> ');
    if i = list.GetSize - 1 then
      WriteLn(list[i].VertexB);
  end;
end;

end.
