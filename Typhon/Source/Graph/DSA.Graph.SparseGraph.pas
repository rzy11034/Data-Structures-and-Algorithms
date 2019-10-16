/// <summary> 稀疏图 - 邻接表 </summary>
unit DSA.Graph.SparseGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.List_Stack_Queue.ArrayList,
  DSA.List_Stack_Queue.LinkedList,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  TSparseGraph = class(TInterfacedObject, IGraph)
  private
    type
    TEdgeType = specialize TLinkedList<integer>;
    TGraphType = specialize TArrayList<TEdgeType>;

  var
    __vertex: integer; // 节点数
    __edge: integer; // 边数
    __directed: boolean; // 是否为有向图
    __graphData: TGraphType; // 图的具体数据

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
      __sg: TSparseGraph;
      __v: integer;
      __index: integer;
    public
      constructor Create(const g: TSparseGraph; v: integer);
      /// <summary> 返回图G中与顶点v相连接的第一个顶点 </summary>
      function Start: integer;
      /// <summary> 返回图G中与顶点v相连接的下一个顶点 </summary>
      function Next: integer;
      /// <summary> 是否已经迭代完了图G中与顶点v相连接的所有顶点 </summary>
      function Finished: boolean;
    end;

  end;

procedure Main;

implementation

procedure Main;
var
  m, n, a, b, i, v, w: integer;
  sg: TSparseGraph;
  adj: IIterator;
begin
  n := 20;
  m := 100;
  Randomize;

  sg := TSparseGraph.Create(n, False);
  for i := 0 to m - 1 do
  begin
    a := Random(n);
    b := Random(n);
    sg.AddEdge(a, b);
  end;

  for v := 0 to n - 1 do
  begin
    Write(v, ' : ');
    adj := TSparseGraph.TAdjIterator.Create(sg, v);

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

    for i in sg.AdjIterator(v) do
    begin
      Write(i, ' ');
    end;

    Writeln;
  end;

  FreeAndNil(sg);

  TDSAUtils.DrawLine;

  sg := TSparseGraph.Create(13, False);
  TDSAUtils.ReadGraph((sg as IGraph), FILE_PATH + GRAPH_FILE_NAME_1);
  sg.Show;
end;

{ TSparseGraph.TAdjIterator }

constructor TSparseGraph.TAdjIterator.Create(const g: TSparseGraph; v: integer);
begin
  __sg := g;
  __v := v;
  __index := -1;
end;

function TSparseGraph.TAdjIterator.Finished: boolean;
begin
  Result := __index >= __sg.__graphData[__v].GetSize;
end;

function TSparseGraph.TAdjIterator.Next: integer;
begin
  Inc(__index);

  if __index < __sg.__graphData[__v].GetSize then
  begin
    Result := __sg.__graphData[__v][__index];
    Exit;
  end;

  Result := -1;
end;

function TSparseGraph.TAdjIterator.Start: integer;
begin
  __index := 0;

  if __sg.__graphData[__v].GetSize > 0 then
  begin
    Result := __sg.__graphData[__v][__index];
    Exit;
  end;

  Result := -1; // 若没有顶点和v相连接, 则返回-1
end;

{ TSparseGraph }

constructor TSparseGraph.Create(n: integer; directed: boolean);
var
  i: integer;
begin
  Assert(n > 0);

  __vertex := n;
  __edge := 0;
  __directed := directed;

  __graphData := TGraphType.Create;
  for i := 0 to n - 1 do
    __graphData.AddLast(TEdgeType.Create);
end;

procedure TSparseGraph.AddEdge(v, w: integer);
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  __graphData[v].AddLast(w);
  if (v <> w) and (__directed = False) then
    __graphData[w].AddLast(v);

  __edge += 1;
end;

function TSparseGraph.AdjIterator(v: integer): TArray_int;
var
  i: integer;
  ret: TArray_int = nil;
begin
  for i := 0 to __graphData[v].GetSize - 1 do
  begin
    SetLength(ret, i + 1);
    ret[i] := __graphData[v][i];
  end;

  Result := ret;
end;

destructor TSparseGraph.Destroy;
begin
  FreeAndNil(__graphData);
  inherited Destroy;
end;

function TSparseGraph.Edge: integer;
begin
  Result := __edge;
end;

function TSparseGraph.HasEdge(v, w: integer): boolean;
var
  i: integer;
begin
  Assert((v >= 0) and (v < __vertex));
  Assert((w >= 0) and (w < __vertex));

  for i := 0 to __graphData[v].GetSize - 1 do
  begin
    if (__graphData[v][i] = w) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TSparseGraph.Show;
var
  v, e: integer;
begin
  for v := 0 to __graphData.GetSize - 1 do
  begin
    Write('Vertex ', v, ' : ', #9);

    for e in AdjIterator(v) do
    begin
      Write(e, #9);
    end;

    Writeln;
  end;
end;

function TSparseGraph.Vertex: integer;
begin
  Result := __vertex;
end;

end.
