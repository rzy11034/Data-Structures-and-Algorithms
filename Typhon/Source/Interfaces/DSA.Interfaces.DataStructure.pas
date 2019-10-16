unit DSA.Interfaces.DataStructure;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Graph.Edge;

type

  generic IStack<T> = interface
    ['{F4C21C9B-5BB0-446D-BBA0-43343B7E8A04}']
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
  end;

  generic IQueue<T> = interface
    ['{1454F65C-3628-488C-891A-4A4F6EDECCDA}']
    function GetSize: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
  end;

  generic ISet<T> = interface
    ['{EB3DEBD8-1473-4AD1-90B2-C5CEF2AD2A97}']
    procedure Add(e: T);
    procedure Remove(e: T);
    function Contains(e: T): boolean;
    function GetSize: integer;
    function IsEmpty: boolean;
  end;

  generic TPtr_V<V> = packed object
  private
    type
    Ptr_V = ^V;
  public
    PValue: Ptr_V;
  end;

  generic IMap<K, V> = interface
    ['{4D344A23-A724-4120-80D8-C7F07F33D367}']
    function Contains(key: K): boolean;
    function Get(key: K): specialize TPtr_V<V>;
    function GetSize: integer;
    function IsEmpty: boolean;
    function Remove(key: K): specialize TPtr_V<V>;
    procedure Add(key: K; Value: V);
    procedure Set_(key: K; Value: V);
  end;

  generic IMerger<T> = interface
    ['{B417FA36-9603-4CDA-9AAE-1EA445B6E63E}']
    function Merge(Left, Right: T): T;
  end;

  IUnionFind = interface
    ['{3EFCB11A-32EE-4852-8D5D-AFC6F3665933}']
    function GetSize: integer;
    function IsConnected(p, q: integer): boolean;
    procedure UnionElements(p, q: integer);
  end;

  IGraph = interface
    ['{CEBEE316-FBAD-4C3D-A39E-B324AD097827}']
    function Vertex: integer;
    function Edge: integer;
    function HasEdge(v: integer; w: integer): boolean;
    procedure AddEdge(v: integer; w: integer);
    function AdjIterator(v: integer): specialize TArray<integer>;
    procedure Show;
  end;

  IIterator = interface
    ['{9D13D226-BE98-4658-B9A4-53513A3E24C7}']
    function Start: integer;
    function Next: integer;
    function Finished: boolean;
  end;

  generic TWeightGraph<T> = class abstract
  protected type
    TEdge_T = specialize TEdge<T>;
    TArrEdge = specialize TArray<TEdge_T>;

  public
    function Vertex: integer; virtual; abstract;
    function Edge: integer; virtual; abstract;
    function HasEdge(v: integer; w: integer): boolean; virtual; abstract;
    function AdjIterator(v: integer): TArrEdge; virtual; abstract;
    procedure AddEdge(v, w: integer; weight: T); virtual; abstract;
    procedure Show; virtual; abstract;
  end;

implementation

end.
