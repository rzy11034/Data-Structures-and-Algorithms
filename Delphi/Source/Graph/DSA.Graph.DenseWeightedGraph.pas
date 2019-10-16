/// <summary> 稠密图 - 邻接矩阵 </summary>
unit DSA.Graph.DenseWeightedGraph;

interface

uses
  System.SysUtils,
  System.Rtti,
  DSA.Interfaces.DataStructure,
  DSA.Graph.Edge;

type
  TDenseWeightedGraph<T> = class(TWeightGraph<T>)
  private type
    TEdge_T = TEdge<T>;
    TArray_Edge = TArray<TEdge_T>;

  var
    __vertex: integer; // 节点数
    __edge: integer; // 边数
    __directed: boolean; // 是否为有向图
    __graphData: array of array of TEdge_T; // 图的具体数据

  public
    constructor Create(n: integer; directed: boolean);
    destructor Destroy; override;

    /// <summary> 向图中添加一个边 </summary>
    procedure AddEdge(v, w: integer; weight: T); override;
    /// <summary> 验证图中是否有从v到w的边 </summary>
    function HasEdge(v, w: integer): boolean; override;
    /// <summary> 返回节点个数 </summary>
    function Vertex: integer; override;
    /// <summary> 返回边的个数 </summary>
    function Edge: integer; override;
    /// <summary> 返回图中一个顶点的所有邻边 </summary>
    function AdjIterator(v: integer): TArray_Edge; override;
    procedure Show; override;
  end;

procedure Main;

implementation

uses
  DSA.Graph.SparseWeightedGraph,
  DSA.Utils;

type
  TDenseWeightedGraph_dbl = TDenseWeightedGraph<double>;
  TSparseWeightedGraph_dbl = TSparseWeightedGraph<double>;
  TWeightGraph_dbl = TWeightGraph<double>;
  TReadGraphWeight_dbl = TReadGraphWeight<double>;

procedure Main;
var
  g: TWeightGraph_dbl;
  FileName: string;
begin
  FileName := WEIGHT_GRAPH_FILE_NAME_1;

  g := TDenseWeightedGraph_dbl.Create(8, false);
  TReadGraphWeight_dbl.Execute(g, FILE_PATH + FileName);
  WriteLn('test G1 in Sparse Weighted Graph:');
  g.Show;

  TDSAUtils.DrawLine;

  g := TSparseWeightedGraph_dbl.Create(8, false);
  TReadGraphWeight_dbl.Execute(g, FILE_PATH + FileName);
  WriteLn('test G1 in Sparse Weighted Graph:');
  g.Show;
end;

{ TDenseWeightedGraph<T> }

constructor TDenseWeightedGraph<T>.Create(n: integer; directed: boolean);
begin
  Assert(n > 0);

  __vertex := n;
  __edge := 0;
  __directed := directed;
  SetLength(__graphData, n, n);
end;

procedure TDenseWeightedGraph<T>.AddEdge(v, w: integer; weight: T);
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  if (HasEdge(v, w) = True) or (v = w) then
    Exit;

  __graphData[v][w] := TEdge_T.Create(v, w, weight);
  if __directed = false then
    __graphData[w][v] := TEdge_T.Create(v, w, weight);

  Inc(__edge);
end;

function TDenseWeightedGraph<T>.AdjIterator(v: integer): TArray_Edge;
var
  i, n: integer;
  ret: TArray_Edge;
begin
  for i := 0 to high(__graphData[v]) do
  begin
    if __graphData[v][i] <> nil then
    begin
      n := Length(ret);
      SetLength(ret, n + 1);
      ret[n] := __graphData[v][i];
    end;
  end;

  Result := ret;
end;

destructor TDenseWeightedGraph<T>.Destroy;
var
  i, j: integer;
begin
  for i := 0 to high(__graphData) do
  begin
    for j := 0 to high(__graphData[i]) do
    begin
      if __graphData[i][j] <> nil then
        __graphData[i][j].Free;
    end;
  end;

  inherited Destroy;
end;

function TDenseWeightedGraph<T>.Edge: integer;
begin
  Result := __edge;
end;

function TDenseWeightedGraph<T>.HasEdge(v, w: integer): boolean;
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  Result := __graphData[v][w] <> nil;
end;

procedure TDenseWeightedGraph<T>.Show;
var
  i, j: integer;
  tmp: T;
  Value: TValue;
begin
  for i := 0 to high(__graphData) do
  begin
    for j := 0 to high(__graphData[i]) do
    begin
      if __graphData[i][j] <> nil then
      begin
        tmp := __graphData[i][j].weight;
        TValue.Make(@tmp, TypeInfo(T), Value);
        write(Value.ToString, #9);
      end
      else
        write('nil', #9);
    end;

    WriteLn;
  end;
end;

function TDenseWeightedGraph<T>.Vertex: integer;
begin
  Result := __vertex;
end;

end.
