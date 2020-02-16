/// <summary> 稀疏图 - 邻接表 </summary>
unit DSA.Graph.SparseWeightedGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSA.List_Stack_Queue.ArrayList,
  DSA.List_Stack_Queue.LinkedList,
  DSA.Interfaces.DataStructure;

type
  generic TSparseWeightedGraph<T> = class(specialize TWeightGraph<T>)
  private
    type
    TEdgeType = specialize TLinkedList<TEdge_T>;
    TGraphType = specialize TArrayList<TEdgeType>;

  var
    __vertex: integer; // 节点数
    __edge: integer; // 边数
    __directed: boolean; // 是否为有向图
    __graphData: TGraphType; // 图的具体数据

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
    function AdjIterator(v: integer): TArrEdge; override;
    procedure Show; override;
  end;

procedure Main;

implementation

uses
  DSA.Graph.DenseWeightedGraph,
  DSA.Utils;

type
  TDenseWeightedGraph_dbl = specialize TDenseWeightedGraph<double>;
  TSparseWeightedGraph_dbl = specialize TSparseWeightedGraph<double>;
  TWeightGraph_dbl = specialize TWeightGraph<double>;
  TReadGraphWeight_dbl = specialize TReadGraphWeight<double>;

procedure Main;
var
  g: TWeightGraph_dbl;
  FileName: string;
begin
  FileName := WEIGHT_GRAPH_FILE_NAME_1;

  g := TDenseWeightedGraph_dbl.Create(8, False);
  TReadGraphWeight_dbl.Execute(g, FILE_PATH + FileName);
  WriteLn('test G1 in Dense Weighted Graph:');
  g.Show;

  TDSAUtils.DrawLine;

  g := TSparseWeightedGraph_dbl.Create(8, False);
  TReadGraphWeight_dbl.Execute(g, FILE_PATH + FileName);
  WriteLn('test G1 in Sparse Weighted Graph:');
  g.Show;
end;

{ TSparseWeightedGraph }

constructor TSparseWeightedGraph.Create(n: integer; directed: boolean);
var
  i: integer;
begin
  Assert(n > 0);

  __vertex := n;
  __edge := 0;
  __directed := directed;

  __graphData := TGraphType.Create(n);
  for i := 0 to n - 1 do
    __graphData.AddLast(TEdgeType.Create);
end;

procedure TSparseWeightedGraph.AddEdge(v, w: integer; weight: T);
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  __graphData[v].AddLast(TEdge_T.Create(v, w, weight));
  if (v <> w) and (__directed = False) then
    __graphData[w].AddLast(TEdge_T.Create(w, v, weight));

  Inc(__edge);
end;

function TSparseWeightedGraph.AdjIterator(v: integer): TArrEdge;
var
  i: integer;
  ret: TArrEdge = nil;
begin
  for i := 0 to __graphData[v].GetSize - 1 do
  begin
    SetLength(ret, i + 1);
    ret[i] := __graphData[v][i];
  end;

  Result := ret;
end;

destructor TSparseWeightedGraph.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(__graphData.GetSize) do
  begin
    __graphData[i].Free;
  end;

  FreeAndNil(__graphData);
  inherited Destroy;
end;

function TSparseWeightedGraph.Edge: integer;
begin
  Result := __edge;
end;

function TSparseWeightedGraph.HasEdge(v, w: integer): boolean;
var
  i: integer;
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  for i := 0 to __graphData[v].GetSize - 1 do
  begin
    if (__graphData[v].Get(i).OtherVertex(v) = w) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TSparseWeightedGraph.Show;
var
  v: integer;
  e: TEdge_T;
begin
  for v := 0 to __graphData.GetSize - 1 do
  begin
    Write('Vertex ', v, ' : ', #9);

    for e in AdjIterator(v) do
    begin
      Write('( to:', e.VertexB, ' wt:', e.Weight.ToString, ')'#9);
    end;

    Writeln;
  end;
end;

function TSparseWeightedGraph.Vertex: integer;
begin
  Result := __vertex;
end;

end.
