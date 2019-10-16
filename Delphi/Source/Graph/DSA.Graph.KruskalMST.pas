unit DSA.Graph.KruskalMST;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Tree.PriorityQueue,
  DSA.Tree.UnionFind6,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Graph.Edge;

type
  TKruskalMST<T> = class
  private type
    TArr_bool = TArray<boolean>;
    TEdge_T = TEdge<T>;
    TPriorityQueue = TPriorityQueue<TEdge_T>;
    TList_Edge = TArrayList<TEdge_T>;
    TArrEdge = TArray<TEdge_T>;
    TWeightGraph_T = TWeightGraph<T>;

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
  TDenseWeightedGraph_dbl = TDenseWeightedGraph<double>;
  TSparseWeightedGraph_dbl = TSparseWeightedGraph<double>;
  TWeightGraph_dbl = TWeightGraph<double>;
  TReadGraphWeight_dbl = TReadGraphWeight<double>;
  TKruskalMST_dbl = TKruskalMST<double>;

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
    for i := 0 to high(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The Dense Graph MST weight is: ', lazyPrimMst.Weight.ToString);
  end;

  TDSAUtils.DrawLine;

  begin
    g := TSparseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Kruskal MST:');
    lazyPrimMst := TKruskalMST_dbl.Create(g);

    mst := lazyPrimMst.MstEdges;
    for i := 0 to high(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The Sparse Graph MST weight is: ', lazyPrimMst.Weight.ToString);
  end;
end;

{ TKruskalMST<T> }

constructor TKruskalMST<T>.Create(g: TWeightGraph_T);
var
  v, i: integer;
  e: TEdge_T;
  uf: TUnionFind6;
  wt: Variant;
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
  wt := 0;
  for i := 0 to __mstList.GetSize - 1 do
  begin
    wt := wt + TValue.From<T>(__mstList[i].Weight).AsVariant;
  end;
  TValue.FromVariant(wt).ExtractRawData(@__mstWeight);
end;

destructor TKruskalMST<T>.Destroy;
begin
  FreeAndNil(__pq);
  FreeAndNil(__mstList);
  inherited Destroy;
end;

function TKruskalMST<T>.MstEdges: TArrEdge;
begin
  Result := __mstList.ToArray;
end;

function TKruskalMST<T>.Weight: T;
begin
  Result := __mstWeight;
end;

end.
