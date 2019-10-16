/// <summary> 稠密图 - 邻接矩阵 </summary>
unit DSA.Graph.DenseGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  TDenseGraph = class(TInterfacedObject, IGraph)
  private
    __vertex: integer; // 节点数
    __edge: integer; // 边数
    __directed: boolean; // 是否为有向图
    __graphData: array of array of boolean; // 图的具体数据

  public
    /// <summary> 向图中添加一个边 </summary>
    procedure AddEdge(v, w: integer);
    /// <summary> 验证图中是否有从v到w的边 </summary>
    function HasEdge(v, w: integer): boolean;
    /// <summary> 返回节点个数 </summary>
    function Vertex: integer;
    /// <summary> 返回边的个数 </summary>
    function Edge: integer;
    /// <summary> 返回图中一个顶点的所有邻边 </summary>
    function AdjIterator(v: integer): TArray_int;
    procedure Show;

    constructor Create(n: integer; directed: boolean);
    destructor Destroy; override;

    type
    /// <summary>
    /// 邻边迭代器, 传入一个图和一个顶点,
    /// 迭代在这个图中和这个顶点向连的所有顶点
    /// </summary>
    TAdjIterator = class(TInterfacedObject, IIterator)
    private
      __dg: TDenseGraph;
      __v: integer;
      __index: integer;
    public
      constructor Create(const g: TDenseGraph; v: integer);
      /// <summary> 返回图G中与顶点v相连接的第一个顶点 </summary>
      function Start: integer;
      /// <summary> 返回图G中与顶点v相连接的下一个顶点 </summary>
      function Next: integer;
      /// <summary>  是否已经迭代完了图G中与顶点v相连接的所有顶点 </summary>
      function Finished: boolean;
    end;

  end;

procedure Main;

implementation

procedure Main;
var
  m, n, a, b, i, v, w: integer;
  dg: TDenseGraph;
  adj: IIterator;
begin
  n := 20;
  m := 100;
  Randomize;

  dg := TDenseGraph.Create(n, False);
  for i := 0 to m - 1 do
  begin
    a := Random(n);
    b := Random(n);
    dg.AddEdge(a, b);
  end;

  for v := 0 to n - 1 do
  begin
    Write(v, ' : ');
    adj := TDenseGraph.TAdjIterator.Create(dg, v);

    w := adj.Start;
    while not adj.Finished do
    begin
      Write(w, ' ');

      w := adj.Next;
    end;

    Writeln;
  end;

  TDSAUtils.DrawLine;

  for v := 0 to n - 1 do
  begin
    Write(v, ' : ');

    for i in dg.AdjIterator(v) do
    begin
      Write(i, ' ');
    end;

    Writeln;
  end;

  FreeAndNil(dg);
  TDSAUtils.DrawLine;

  dg := TDenseGraph.Create(13, False);
  TDSAUtils.ReadGraph(dg as IGraph, FILE_PATH + GRAPH_FILE_NAME_1);
  dg.Show;
end;

{ TDenseGraph.TAdjIterator }

constructor TDenseGraph.TAdjIterator.Create(const g: TDenseGraph; v: integer);
begin
  __dg := g;
  __v := v;
  __index := -1; // 索引从-1开始, 因为每次遍历都需要调用一次next()
end;

function TDenseGraph.TAdjIterator.Finished: boolean;
begin
  Result := __index > high(__dg.__graphData[__v]);
end;

function TDenseGraph.TAdjIterator.Next: integer;
begin
  Inc(__index);

  // 从当前index开始向后搜索, 直到找到一个g[v][index]为true
  while __index < __dg.Vertex do
  begin
    if __dg.__graphData[__v][__index] then
    begin
      Result := __index;
      Exit;
    end;

    Inc(__index);
  end;

  Result := -1; // 没有顶点和v相连接, 则返回-1
end;

function TDenseGraph.TAdjIterator.Start: integer;
begin
  __index := -1; // 索引从-1开始, 因为每次遍历都需要调用一次next
  Result := Next;
end;

{ TDenseGraph }

constructor TDenseGraph.Create(n: integer; directed: boolean);
begin
  Assert(n > 0);

  __vertex := n;
  __edge := 0;
  __directed := directed;
  SetLength(__graphData, n, n);
end;

procedure TDenseGraph.AddEdge(v, w: integer);
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  if (HasEdge(v, w) = True) or (v = w) then
    Exit;

  __graphData[v][w] := True;
  if __directed = False then
    __graphData[w][v] := True;

  Inc(__edge);
end;

function TDenseGraph.AdjIterator(v: integer): TArray_int;
var
  i, n: integer;
  ret: TArray_int = nil;
begin
  for i := 0 to High(__graphData[v]) do
  begin
    if __graphData[v][i] then
    begin
      n := Length(ret);
      SetLength(ret, n + 1);
      ret[n] := i;
    end;
  end;

  Result := ret;
end;

destructor TDenseGraph.Destroy;
begin
  inherited Destroy;
end;

function TDenseGraph.Edge: integer;
begin
  Result := __edge;
end;

function TDenseGraph.HasEdge(v, w: integer): boolean;
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  Result := __graphData[v][w];
end;

procedure TDenseGraph.Show;
var
  v: integer;
  e: boolean;
begin
  for v := 0 to High(__graphData) do
  begin
    Write('Vertex ', v, ' : ', #9);

    for e in __graphData[v] do
    begin
      if e then
        Write('1', ' ')
      else
        Write('0', ' ');
    end;

    WriteLn;
  end;
end;

function TDenseGraph.Vertex: integer;
begin
  Result := __vertex;
end;

end.
