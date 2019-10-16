unit DSA.Graph.Component;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  TComponent = class
  private
    __g: IGraph;
    __visited: array of boolean; // 记录dfs的过程中节点是否被访问
    __ccount: integer; // 记录联通分量个数
    __id: array of integer; // 每个节点所对应的联通分量标记

    /// <summary> 图的深度优先遍历 </summary>
    procedure __dfs(v: integer);

  public
    constructor Create(g: IGraph);

    /// <summary> 返回图的联通分量个数 </summary>
    function Count: integer;
    /// <summary> 查询点v和点w是否联通 </summary>
    function IsConnected(v, w: integer): boolean;
  end;

procedure Main;

implementation

uses
  DSA.Graph.SparseGraph,
  DSA.Graph.DenseGraph;

procedure Main;
var
  fileName: string;
  g: IGraph;
  cm: TComponent;
begin
  // TestG1.txt - g1 and g2
  begin
    fileName := FILE_PATH + GRAPH_FILE_NAME_1;

    g := TSparseGraph.Create(13, False);
    TDSAUtils.ReadGraph(g, fileName);
    cm := TComponent.Create(g);
    WriteLn('TestG1.txt, Using Sparse Graph, Component Count: ', cm.Count);
    WriteLn(cm.IsConnected(3, 4));

    WriteLn;

    g := TDenseGraph.Create(13, False);
    TDSAUtils.ReadGraph(g, fileName);
    cm := TComponent.Create(g);
    WriteLn('TestG1.txt, Using Dense Graph, Component Count: ', cm.Count);
    WriteLn(cm.IsConnected(3, 4));
  end;

  TDSAUtils.DrawLine;

  // TestG2.txt - g1 and g2
  begin
    fileName := FILE_PATH + GRAPH_FILE_NAME_2;

    g := TSparseGraph.Create(7, False);
    TDSAUtils.ReadGraph(g, fileName);
    cm := TComponent.Create(g);
    WriteLn('TestG2.txt, Using Sparse Graph, Component Count: ', cm.Count);
    WriteLn(cm.IsConnected(3, 4));

    WriteLn;

    g := TDenseGraph.Create(7, False);
    TDSAUtils.ReadGraph(g, fileName);
    cm := TComponent.Create(g);
    WriteLn('TestG2.txt, Using Dense Graph, Component Count: ', cm.Count);
    WriteLn(cm.IsConnected(3, 4));
  end;
end;

{ TComponent }

constructor TComponent.Create(g: IGraph);
var
  i: integer;
begin
  __g := g;
  __ccount := 0;
  SetLength(__visited, __g.Vertex);
  SetLength(__id, __g.Vertex);

  for i := 0 to __g.Vertex - 1 do
  begin
    __visited[i] := False;
    __id[i] := -1;
  end;

  // 求图的联通分量
  for i := 0 to __g.Vertex - 1 do
  begin
    if __visited[i] = False then
    begin
      __dfs(i);
      inc(__ccount);
    end;
  end;
end;

function TComponent.Count: integer;
begin
  Result := __ccount;
end;

function TComponent.IsConnected(v, w: integer): boolean;
begin
  Assert((v >= 0) and (v < __g.Vertex));
  Assert((w >= 0) and (w < __g.Vertex));

  Result := __id[v] = __id[w];
end;

procedure TComponent.__dfs(v: integer);
var
  i: integer;
begin
  __visited[v] := True;
  __id[v] := __ccount;

  for i in __g.AdjIterator(v) do
  begin
    if __visited[i] = False then
      __dfs(i);
  end;
end;

end.
