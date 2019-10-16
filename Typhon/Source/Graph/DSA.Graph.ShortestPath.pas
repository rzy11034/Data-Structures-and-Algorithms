unit DSA.Graph.ShortestPath;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  TShortestPath = class
  private
    __g: IGraph; // 图的引用
    __s: integer; // 起始点
    __visited: TArray_bool;  // 记录dfs的过程中节点是否被访问
    __from: TArray_int; // 记录路径, __from[i]表示查找的路径上i的上一个节点
    __ord: TArray_int; // 记录路径中节点的次序。ord[i]表示i节点在路径中的次序。

  public
    constructor Create(g: IGraph; s: integer);
    destructor Destroy; override;

    /// <summary> 查询从s点到w点是否有路径  </summary>
    function HasPath(w: integer): boolean;
    /// <summary> 查询从s点到w点的路径, 存放在list中 </summary>
    procedure Path(w: integer; list: TArrayList_int);
    /// <summary> 打印出从s点到w点的路径 </summary>
    procedure ShowPath(w: integer);
    /// <summary> 查看从s点到w点的最短路径长度 </summary>
    function length(w: integer): integer;
  end;

procedure Main;

implementation

uses
  DSA.Graph.Path,
  DSA.Graph.SparseGraph;

procedure Main;
var
  fileName: string;
  g: TSparseGraph;
  dfs: TPath;
  bfs: TShortestPath;
begin
  fileName := FILE_PATH + GRAPH_FILE_NAME_2;

  g := TSparseGraph.Create(7, False);
  TDSAUtils.ReadGraph(g as IGraph, fileName);
  g.Show;

  dfs := TPath.Create(g, 0);
  Write('DFS: ');
  dfs.ShowPath(6);

  bfs := TShortestPath.Create(g, 0);
  Write('BFS: ');
  bfs.ShowPath(6);
end;

{ TShortestPath }

constructor TShortestPath.Create(g: IGraph; s: integer);
var
  i: integer;
  queue: TQueue_int;
  v: integer;
begin
  Assert((s >= 0) and (s < g.Vertex));

  __g := g;
  __s := s;
  SetLength(__visited, __g.Vertex);
  SetLength(__from, __g.Vertex);
  SetLength(__ord, __g.Vertex);

  for i := 0 to __g.Vertex - 1 do
  begin
    __visited[i] := False;
    __from[i] := -1;
    __ord[i] := -1;
  end;

  queue := TQueue_int.Create;

  // 无向图最短路径算法, 从s开始广度优先遍历整张图
  queue.EnQueue(s);
  __visited[s] := True;
  __ord[s] := 0;

  while queue.IsEmpty = False do
  begin
    v := queue.DeQueue;

    for i in __g.AdjIterator(v) do
    begin
      if not __visited[i] then
      begin
        queue.EnQueue(i);
        __visited[i] := True;
        __from[i] := v;
        __ord[i] := __ord[v] + 1;
      end;
    end;
  end;
end;

destructor TShortestPath.Destroy;
begin
  inherited Destroy;
end;

function TShortestPath.HasPath(w: integer): boolean;
begin
  Assert((w >= 0) and (w < __g.Vertex));
  Result := __visited[w];
end;

function TShortestPath.length(w: integer): integer;
begin
  Assert((w >= 0) and (w < __g.Vertex));
  Result := __ord[w];
end;

procedure TShortestPath.Path(w: integer; list: TArrayList_int);
var
  s: TStack_int;
  p: integer;
begin
  Assert(HasPath(w));

  s := TStack_int.Create;

  // 通过__from数组逆向查找到从s到w的路径, 存放到栈中
  p := w;
  while p <> -1 do
  begin
    s.Push(p);
    p := __from[p];
  end;

  // 从栈中依次取出元素, 获得顺序的从s到w的路径
  while not s.IsEmpty do
  begin
    list.AddLast(s.Peek);
    s.Pop;
  end;
end;

procedure TShortestPath.ShowPath(w: integer);
var
  list: TArrayList_int;
  i: integer;
begin
  list := TArrayList_int.Create;
  Path(w, list);

  for i := 0 to list.GetSize - 1 do
  begin
    Write(list[i]);

    if i = list.GetSize - 1 then
      Writeln
    else
      Write(' -> ');
  end;
end;

end.
