unit DSA.Graph.MstCompare;

interface

uses
  System.SysUtils,
  System.Classes;

procedure Main;

implementation

uses
  DSA.Utils,
  DSA.Interfaces.DataStructure,
  DSA.Graph.PrimMST,
  DSA.Graph.SparseWeightedGraph,
  DSA.Graph.KruskalMST,
  DSA.Graph.LazyPrimMST;

type
  TSparseWeightedGraph_dbl = TSparseWeightedGraph<double>;
  TWeightGraph_dbl = TWeightGraph<double>;
  TReadGraphWeight_dbl = TReadGraphWeight<double>;
  TPrimMST_dbl = TPrimMST<double>;
  TLazyPrimMST_dbl = TLazyPrimMST<double>;
  TKruskalMST_dbl = TKruskalMST<double>;

function TPrimTime(const g: TWeightGraph_dbl): string;
var
  startTime, endTime: cardinal;
  pm: TPrimMST_dbl;
begin
  startTime := TThread.GetTickCount;
  pm := TPrimMST_dbl.Create(g);
  pm.Weight;
  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

function TLazyPrimTime(const g: TWeightGraph_dbl): string;
var
  startTime, endTime: cardinal;
  lpm: TLazyPrimMST_dbl;
begin
  startTime := TThread.GetTickCount;
  lpm := TLazyPrimMST_dbl.Create(g);
  lpm.Weight;
  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

function TKruskalTime(const g: TWeightGraph_dbl): string;
var
  startTime, endTime: cardinal;
  kk: TKruskalMST_dbl;
begin
  startTime := TThread.GetTickCount;
  kk := TKruskalMST_dbl.Create(g);
  kk.Weight;
  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

// 比较Lazy Prim, Prim和Kruskal的时间性能
procedure Main;
var
  v1, v2, v3, v4: integer;
  fileName1, fileName2, fileName3, fileName4: string;
  g1, g2, g3, g4: TSparseWeightedGraph_dbl;
begin
  fileName1 := WEIGHT_GRAPH_FILE_NAME_1;
  v1 := 8;

  fileName2 := WEIGHT_GRAPH_FILE_NAME_2;
  v2 := 250;

  fileName3 := WEIGHT_GRAPH_FILE_NAME_3;
  v3 := 1000;

  fileName4 := WEIGHT_GRAPH_FILE_NAME_4;
  v4 := 10000;

  // 文件读取
  g1 := TSparseWeightedGraph_dbl.Create(v1, False);
  TReadGraphWeight_dbl.Execute(g1, FILE_PATH + fileName1);
  WriteLn(fileName1, ' load successfully.');

  g2 := TSparseWeightedGraph_dbl.Create(v2, False);
  TReadGraphWeight_dbl.Execute(g2, FILE_PATH + fileName2);
  WriteLn(fileName2, ' load successfully.');

  g3 := TSparseWeightedGraph_dbl.Create(v3, False);
  TReadGraphWeight_dbl.Execute(g3, FILE_PATH + fileName3);
  WriteLn(fileName3, ' load successfully.');

  g4 := TSparseWeightedGraph_dbl.Create(v4, False);
  TReadGraphWeight_dbl.Execute(g4, FILE_PATH + fileName4);
  WriteLn(fileName4, ' load successfully.');
  WriteLn;

  // Test Lazy Prim MST
  WriteLn('Test Lazy Prim MST:');

  WriteLn('Test for WeightGraph1: ', TLazyPrimTime(g1), ' s.');
  WriteLn('Test for WeightGraph2: ', TLazyPrimTime(g2), ' s.');
  WriteLn('Test for WeightGraph3: ', TLazyPrimTime(g3), ' s.');
  WriteLn('Test for WeightGraph4: ', TLazyPrimTime(g4), ' s.');
  WriteLn;

  // Test Prim MST
  WriteLn('Test Prim MST:');

  WriteLn('Test for WeightGraph1: ', TPrimTime(g1), ' s.');
  WriteLn('Test for WeightGraph2: ', TPrimTime(g2), ' s.');
  WriteLn('Test for WeightGraph3: ', TPrimTime(g3), ' s.');
  WriteLn('Test for WeightGraph4: ', TPrimTime(g4), ' s.');
  WriteLn;

  // Test Kruskal MST
  WriteLn('Test Kruskal MST:');

  WriteLn('Test for WeightGraph1: ', TKruskalTime(g1), ' s.');
  WriteLn('Test for WeightGraph2: ', TKruskalTime(g2), ' s.');
  WriteLn('Test for WeightGraph3: ', TKruskalTime(g3), ' s.');
  WriteLn('Test for WeightGraph4: ', TKruskalTime(g4), ' s.');
  WriteLn;
end;

end.
