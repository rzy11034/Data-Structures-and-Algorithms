unit DSA.Graph.Path;

interface

uses
  System.SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type
  /// <summary> 路径查询 </summary>
  TPath = class
  private
    __g: IGraph; // 图的引用
    __visited: TArray_bool; // 记录dfs的过程中节点是否被访问
    __s: integer; // 起始点

    __from: TArray_int; //记录路径, from[i]表示查找的路径上i的上一个节点

    /// <summary> 图的深度优先遍历 </summary>
    procedure __dfs(v: integer);

  public
    constructor Create(g: IGraph; s: integer);
    destructor Destroy; override;

    /// <summary> 查询从s点到w点是否有路径  </summary>
    function HasPath(w: integer): boolean;
    /// <summary> 查询从s点到w点的路径, 存放在list中 </summary>
    procedure Path(w: integer; list: TArrayList_int);
    /// <summary> 打印出从s点到w点的路径 </summary>
    procedure ShowPath(w: integer);
  end;

procedure Main;

implementation

uses
  DSA.Graph.SparseGraph;

procedure Main;
var
  fileName: string;
  g: TSparseGraph;
  Path: TPath;
begin
  fileName := FILE_PATH + GRAPH_FILE_NAME_2;

  g := TSparseGraph.Create(7, False);
  TDSAUtils.ReadGraph(g as IGraph, fileName);
  g.Show;

  Path := TPath.Create(g, 0);
  write('Path from 0 to 6 DFS: ');
  Path.ShowPath(6);
end;

{ TPath }

constructor TPath.Create(g: IGraph; s: integer);
var
  i: integer;
begin
  Assert((s >= 0) and (s < g.Vertex));

  __g := g;
  SetLength(__visited, __g.Vertex);
  SetLength(__from, __g.Vertex);

  for i := 0 to __g.Vertex - 1 do
  begin
    __visited[i] := False;
    __from[i] := -1;
  end;

  __s := s;

  // 寻路算法
  __dfs(s);
end;

destructor TPath.Destroy;
begin
  inherited Destroy;
end;

function TPath.HasPath(w: integer): boolean;
begin
  Assert((w >= 0) and (w < __g.Vertex));

  Result := __visited[w];
end;

procedure TPath.Path(w: integer; list: TArrayList_int);
var
  stack: TStack_int;
  p: integer;
begin
  Assert(HasPath(w));

  stack := TStack_int.Create;

  // 通过__from数组逆向查找到从s到w的路径, 存放到栈中
  p := w;
  while p <> -1 do
  begin
    stack.Push(p);
    p := __from[p];
  end;

  // 从栈中依次取出元素, 获得顺序的从s到w的路径
  while not stack.IsEmpty do
  begin
    list.AddLast(stack.Peek);
    stack.Pop;
  end;
end;

procedure TPath.ShowPath(w: integer);
var
  list: TArrayList_int;
  i: integer;
begin
  list := TArrayList_int.Create;
  Path(w, list);

  for i := 0 to list.GetSize - 1 do
  begin
    write(list[i]);

    if i = list.GetSize - 1 then
      WriteLn
    else
      write(' -> ');
  end;
end;

procedure TPath.__dfs(v: integer);
var
  i: integer;
begin
  __visited[v] := True;

  for i in __g.AdjIterator(v) do
  begin
    if __visited[i] = False then
    begin
      __from[i] := v;
      __dfs(i);
    end;
  end;
end;

end.
