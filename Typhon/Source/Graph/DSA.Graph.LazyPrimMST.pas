/// <summary> 使用Prim算法求图的最小生成树 </summary>
unit DSA.Graph.LazyPrimMST;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Tree.Heap,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Graph.Edge;

type
  generic TLazyPrimMST<T> = class
  private
    type
    TArr_bool = specialize TArray<boolean>;
    TEdge_T = specialize TEdge<T>;
    TMinHeap = specialize THeap<TEdge_T, TEdge_T.TEdgeComparer>;
    TList_Edge = specialize TArrayList<TEdge_T>;
    TArrEdge = specialize TArray<TEdge_T>;
    TWeightGraph_T = specialize TWeightGraph<T>;

  var
    __g: TWeightGraph_T;
    __pq: TMinHeap; // 最小堆, 算法辅助数据结构
    __marked: TArr_bool; // 标记数组, 在算法运行过程中标记节点i是否被访问
    __mstList: TList_Edge; // 最小生成树所包含的所有边
    __mstWeight: T; // 最小生成树的权值

    /// <summary> 访问节点v  </summary>
    procedure __visit(v: integer);

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
  TLazyPrimMST_dbl = specialize TLazyPrimMST<double>;

procedure Main;
var
  fileName: string;
  v, i: integer;
  g: TWeightGraph_dbl;
  lazyPrimMst: TLazyPrimMST_dbl;
  mst: TLazyPrimMST_dbl.TArrEdge;
begin
  fileName := WEIGHT_GRAPH_FILE_NAME_1;
  v := 8;

  begin
    g := TDenseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Lazy Prim MST:');
    lazyPrimMst := TLazyPrimMST_dbl.Create(g);

    mst := lazyPrimMst.MstEdges;
    for i := 0 to High(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The MST weight is: ', lazyPrimMst.Weight.ToString);
  end;

  TDSAUtils.DrawLine;

  begin
    g := TSparseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Lazy Prim MST:');
    LazyPrimMST := TLazyPrimMST_dbl.Create(g);

    mst := LazyPrimMST.MstEdges;
    for i := 0 to high(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The MST weight is: ', LazyPrimMST.Weight.ToString);
  end;
end;

{ TLazyPrimMST }

constructor TLazyPrimMST.Create(g: TWeightGraph_T);
var
  i: integer;
  e: TEdge_T;
begin
  __g := g;
  __mstList := TList_Edge.Create;
  __pq := TMinHeap.Create(g.Edge);

  // 算法初始化
  SetLength(__marked, g.Vertex);
  for i := 0 to Pred(g.Vertex) do
    __marked[i] := False;

  // Lazy Prim
  __visit(0);
  while not __pq.IsEmpty do
  begin
    // 使用最小堆找出已经访问的边中权值最小的边
    e := __pq.ExtractFirst;

    // 如果这条边的两端都已经访问过了, 则扔掉这条边
    if __marked[e.VertexA] = __marked[e.VertexB] then
      Continue;

    // 这条边存于最小生成树中
    __mstList.AddLast(e);

    // 访问和这条边连接的还没有被访问过的节点
    if __marked[e.VertexA] = False then
      __visit(e.VertexA)
    else
      __visit(e.VertexB);
  end;

  // 计算最小生成树的权值
  __mstWeight := Default(T);
  for i := 0 to __mstList.GetSize - 1 do
  begin
    __mstWeight += __mstList[i].Weight;
  end;
end;

destructor TLazyPrimMST.Destroy;
begin
  FreeAndNil(__pq);
  FreeAndNil(__mstList);
  inherited Destroy;
end;

function TLazyPrimMST.MstEdges: TArrEdge;
begin
  Result := __mstList.ToArray;
end;

function TLazyPrimMST.Weight: T;
begin
  Result := __mstWeight;
end;

procedure TLazyPrimMST.__visit(v: integer);
var
  e: TEdge_T;
begin
  Assert(__marked[v] = False);

  __marked[v] := True;
  for e in __g.AdjIterator(v) do
  begin
    if (__marked[e.OtherVertex(v)] = False) then
      __pq.Add(e);
  end;
end;

end.
