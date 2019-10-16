unit DSA.Graph.Edge;

interface

uses
  Classes,
  SysUtils,
  Math,
  Rtti,
  DSA.Interfaces.Comparer;

type
  TEdge<T> = class
  private type
    TEdge_T = TEdge<T>;
    IC = IComparer<T>;
    TC = TComparer<T>;

  var
    __a, __b: integer; // 边的两个端点
    __weight: T; // 边的权值

  public
    constructor Create; overload;
    constructor Create(a, b: integer; weight: T); overload;
    destructor Destroy; override;

    /// <summary> 返回第一个顶点 </summary>
    function VertexA: integer;
    /// <summary> 返回第二个顶点 </summary>
    function VertexB: integer;
    /// <summary> 返回权值 </summary>
    function Weight: T;
    /// <summary> 给定一个顶点, 返回另一个顶点 </summary>
    function OtherVertex(x: integer): integer;
    function ToString: string; override;

    class function compare(a, b: TEdge_T): integer;

  type
    TEdgeComparer = class(TInterfacedObject, IComparer<TEdge_T>)
    public
      constructor Default;
      function compare(const left, right: TEdge_T): integer;
    end;
  end;

implementation

{ TEdge }

constructor TEdge<T>.Create(a, b: integer; weight: T);
begin
  __a := a;
  __b := b;
  __weight := weight;
end;

constructor TEdge<T>.Create;
begin
  Self.Create(0, 0, default (T));
end;

class function TEdge<T>.compare(a, b: TEdge_T): integer;
var
  c: IC;
begin
  c := TC.Default;

  Result := c.compare(a.weight, b.weight);
end;

destructor TEdge<T>.Destroy;
begin
  inherited Destroy;
end;

function TEdge<T>.OtherVertex(x: integer): integer;
begin
  Assert((x = __a) or (x = __b));

  Result := IfThen(x = __a, __b, __a);
end;

function TEdge<T>.ToString: string;
begin
  Result := __a.ToString + '-' + __b.ToString + ': ' + TValue.From<T>(weight)
    .ToString;
end;

function TEdge<T>.VertexA: integer;
begin
  Result := __a;
end;

function TEdge<T>.VertexB: integer;
begin
  Result := __b;
end;

function TEdge<T>.weight: T;
begin
  Result := __weight;
end;

{ TEdge<T>.TEdgeComparer }

function TEdge<T>.TEdgeComparer.compare(const left, right: TEdge_T): integer;
var
  cmp: IC;
begin
  cmp := TC.Default;
  Result := cmp.compare(left.weight, right.weight);
end;

constructor TEdge<T>.TEdgeComparer.Default;
begin
  inherited Create();
end;

end.
