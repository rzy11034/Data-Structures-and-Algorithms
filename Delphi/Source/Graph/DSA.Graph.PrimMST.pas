unit DSA.Graph.PrimMST;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Tree.IndexHeap,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Graph.Edge,
  DSA.List_Stack_Queue.ArrayList;

type
  TPrimMst<T> = class
  private type
    TArr_bool = TArray<boolean>;
    TEdge_T = TEdge<T>;
    TMinIndexHeap = TIndexHeap<T>;
    TList_Edge = TArrayList<TEdge_T>;
    TArrEdge = TArray<TEdge_T>;
    TWeightGraph_T = TWeightGraph<T>;
    TCmp_T = TComparer<T>;

  var
    __g: TWeightGraph_T;
    __cmp: TCmp_T;
    __ipq: TMinIndexHeap; // 最小索引堆, 算法辅助数据结构
    __marked: TArr_bool; // 标记数组, 在算法运行过程中标记节点i是否被访问
    __mstList: TList_Edge; // 最小生成树所包含的所有边
    __mstWeight: T; // 最小生成树的权值
    __edgeTo: TArrEdge; // 访问的点所对应的边, 算法辅助数据结构

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
  TDenseWeightedGraph_dbl = TDenseWeightedGraph<double>;
  TSparseWeightedGraph_dbl = TSparseWeightedGraph<double>;
  TWeightGraph_dbl = TWeightGraph<double>;
  TReadGraphWeight_dbl = TReadGraphWeight<double>;
  TPrimMST_dbl = TPrimMst<double>;

procedure Main;
var
  fileName: string;
  v, i: integer;
  g: TWeightGraph_dbl;
  PrimMST: TPrimMST_dbl;
  mst: TPrimMST_dbl.TArrEdge;
begin
  fileName := WEIGHT_GRAPH_FILE_NAME_1;
  v := 8;

  begin
    g := TDenseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Dense Weighted Graph Prim MST:');
    PrimMST := TPrimMST_dbl.Create(g);

    mst := PrimMST.MstEdges;
    for i := 0 to high(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The MST weight is: ', PrimMST.Weight.ToString);
  end;

  TDSAUtils.DrawLine;

  begin
    g := TSparseWeightedGraph_dbl.Create(v, False);
    TReadGraphWeight_dbl.Execute(g, FILE_PATH + fileName);

    // Test Lazy Prim MST
    WriteLn('Test Sparse Weighted Graph Prim MST:');
    PrimMST := TPrimMST_dbl.Create(g);

    mst := PrimMST.MstEdges;
    for i := 0 to high(mst) do
      WriteLn(mst[i].ToString);

    WriteLn('The MST weight is: ', PrimMST.Weight.ToString);
  end;
end;

{ TPrimMst<T> }

constructor TPrimMst<T>.Create(g: TWeightGraph_T);
var
  i, v: integer;
  wt: Variant;
begin
  __g := g;
  __mstList := TList_Edge.Create;
  __ipq := TMinIndexHeap.Create(g.Vertex);
  __cmp := TCmp_T.Default;

  // 算法初始化
  SetLength(__edgeTo, g.Vertex);
  SetLength(__marked, g.Vertex);
  for i := 0 to Pred(g.Vertex) do
  begin
    __marked[i] := False;
    __edgeTo[i] := nil;
  end;

  // Prim
  __visit(0);
  while not __ipq.IsEmpty do
  begin
    // 使用最小索引堆找出已经访问的边中权值最小的边
    // 最小索引堆中存储的是点的索引, 通过点的索引找到相对应的边
    v := __ipq.ExtractFirstIndex;

    Assert(__edgeTo[v] <> nil);
    __mstList.AddLast(__edgeTo[v]);
    __visit(v);
  end;

  // 计算最小生成树的权值
  wt := 0;
  for i := 0 to __mstList.GetSize - 1 do
  begin
    wt := wt + TValue.From<T>(__mstList[i].Weight).AsVariant;
  end;
  TValue.FromVariant(wt).ExtractRawData(@__mstWeight);
end;

destructor TPrimMst<T>.Destroy;
begin
  FreeAndNil(__ipq);
  FreeAndNil(__mstList);
  FreeAndNil(__cmp);
  inherited Destroy;
end;

function TPrimMst<T>.MstEdges: TArrEdge;
begin
  Result := __mstList.ToArray;
end;

function TPrimMst<T>.Weight: T;
begin
  Result := __mstWeight;
end;

procedure TPrimMst<T>.__visit(v: integer);
var
  e: TEdge_T;
  w: integer;
begin
  Assert(__marked[v] = False);
  __marked[v] := True;

  // 将和节点v相连接的未访问的另一端点, 和与之相连接的边, 放入最小索引堆中
  for e in __g.AdjIterator(v) do
  begin
    w := e.OtherVertex(v);

    // 如果边的另一端点未被访问
    if __marked[w] = False then
    begin
      // 如果从没有考虑过这个端点, 直接将这个端点和与之相连接的边加入索引堆
      if __edgeTo[w] = nil then
      begin
        __edgeTo[w] := e;
        __ipq.Insert(w, e.Weight);
      end
      // 如果曾经考虑这个端点, 但现在的边比之前考虑的边更短, 则进行替换
      else if __cmp.Compare(e.Weight, __edgeTo[w].Weight) < 0 then
      begin
        __edgeTo[w] := e;
        __ipq.Change(w, e.Weight);
      end;
    end;
  end;
end;

end.
