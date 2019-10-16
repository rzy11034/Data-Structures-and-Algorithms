unit DSA.Graph.KruskalMST;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Tree.PriorityQueue,
  DSA.Tree.UnionFind6,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Graph.Edge;

type
  generic TKruskalMST<T> = class
  private
    type
    TArr_bool = specialize TArray<boolean>;
    TEdge_T = specialize TEdge<T>;
    TPriorityQueue = specialize TPriorityQueue<TEdge_T, TEdge_T.TEdgeComparer>;
    TList_Edge = specialize TArrayList<TEdge_T>;
    TArrEdge = specialize TArray<TEdge_T>;
    TWeightGraph_T = specialize TWeightGraph<T>;

  var
    __pq: TPriorityQueue; // 最小堆, 算法辅助数据结构
    __mstList: TList_Edge; // 最小生成树所包含的所有边
    __mstWeight: T; // 最小生成树的权值

  public
    constructor Create(g: TWeightGraph_T);
    destructor Destroy; override;

    /// <summary> 返回最小生成树的所有边 </summary>
    function MstEdges: TArrEdge;
    /// <summary> 返回最小生成树的权值 </summary>
    function Weight: T;
  end;

procedure Main;

implementation

uses
  DSA.Graph.DenseWeightedGraph,
  DSA.Graph.SparseWeightedGraph,
  DSA.Utils;

type
  TDenseWeightedGraph_dbl = specialize TDenseWeightedGraph<double>;
  TSparseWeightedGraph_dbl = specialize TSparseWeightedGraph<double>;
  TWeightGraph_dbl = specialize TWeightGraph<double>;
  TReadGraphWeight_dbl = specialize TReadGraphWeight<double>;
  TKruskalMST_dbl = specialize TKruskalMST<double>;

procedure Main;
var
  fileName: string;
  v, i: integer;
  g: TWeightGraph_dbl;
  lazyPrimMst: TKruskalMST_dbl;
  mst: TKruskalMST_dbl.TArrEdge;
begin
  fileName := WEIGHT_GRAPH_FILE_NAME_1;
  v := 8;

  begin
    g := TDenseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Kruskal MST:');
    lazyPrimMst := TKruskalMST_dbl.Create(g);

    mst := lazyPrimMst.MstEdges;
    for i := 0 to High(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The Dense Graph MST weight is: ', lazyPrimMst.Weight.ToString);
  end;

  TDSAUtils.DrawLine;

  begin
    g := TSparseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Kruskal MST:');
    LazyPrimMST := TKruskalMST_dbl.Create(g);

    mst := LazyPrimMST.MstEdges;
    for i := 0 to high(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The Sparse Graph MST weight is: ', LazyPrimMST.Weight.ToString);
  end;
end;

{ TKruskalMST }

constructor TKruskalMST.Create(g: TWeightGraph_T);
var
  v, i: integer;
  e: TEdge_T;
  uf: TUnionFind6;
begin
  __mstList := TList_Edge.Create;
  __pq := TPriorityQueue.Create(TQueueKind.Min);

  // 将图中的所有边存放到一个最小优先队列中
  for v := 0 to g.Vertex - 1 do
  begin
    for e in g.AdjIterator(v) do
    begin
      if e.VertexA < e.VertexB then
        __pq.EnQueue(e);
    end;
  end;

  // 创建一个并查集, 来查看已经访问的节点的联通情况
  uf := TUnionFind6.Create(g.Vertex);
  while not __pq.IsEmpty do
  begin
    // 从最小堆中依次从小到大取出所有的边
    e := __pq.DeQueue;

    // 如果该边的两个端点是联通的, 说明加入这条边将产生环, 扔掉这条边
    if uf.IsConnected(e.VertexA, e.VertexB) then
      Continue;

    // 否则, 将这条边添加进最小生成树, 同时标记边的两个端点联通
    __mstList.AddLast(e);
    uf.UnionElements(e.VertexA, e.VertexB);
  end;

  // 计算最小生成树的权值
  __mstWeight := Default(T);
  for i := 0 to __mstList.GetSize - 1 do
  begin
    __mstWeight += __mstList[i].Weight;
  end;
end;

destructor TKruskalMST.Destroy;
begin
  FreeAndNil(__pq);
  FreeAndNil(__mstList);
  inherited Destroy;
end;

function TKruskalMST.MstEdges: TArrEdge;
begin
  Result := __mstList.ToArray;
end;

function TKruskalMST.Weight: T;
begin
  Result := __mstWeight;
end;

end.
