unit DSA.Graph.Edge;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  Rtti,
  DSA.Interfaces.Comparer;

type
  generic TEdge<T> = class
  public
    type
    TEdge_T = specialize TEdge<T>;

  private
    type
    IC = specialize IDSA_Comparer<T>;
    TC = specialize TComparer<T>;

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

    type
    TEdgeComparer = class(TInterfacedObject, specialize IDSA_Comparer<TEdge_T>)
    public
      constructor Default;
      function Compare(const left, right: TEdge_T): integer;
    end;

  end;

implementation

{ TEdge.TEdgeComparer }

constructor TEdge.TEdgeComparer.Default;
begin
  inherited Create;
end;

function TEdge.TEdgeComparer.Compare(const left, right: TEdge_T): integer;
var
  cmp: TC;
begin
  cmp := TC.Default;
  Result := cmp.Compare(left.Weight, right.Weight);
end;

{ TEdge }

constructor TEdge.Create(a, b: integer; weight: T);
begin
  __a := a;
  __b := b;
  __weight := weight;
end;

constructor TEdge.Create;
begin
  Self.Create(0, 0, default(T));
end;

destructor TEdge.Destroy;
begin
  inherited Destroy;
end;

function TEdge.OtherVertex(x: integer): integer;
begin
  Assert((x = __a) or (x = __b));

  Result := IfThen(x = __a, __b, __a);
end;

function TEdge.ToString: string;
begin
  Result := __a.ToString + '-' + __b.ToString + ': ' + __weight.ToString;
end;

function TEdge.VertexA: integer;
begin
  Result := __a;
end;

function TEdge.VertexB: integer;
begin
  Result := __b;
end;

function TEdge.Weight: T;
begin
  Result := __weight;
end;

end.
