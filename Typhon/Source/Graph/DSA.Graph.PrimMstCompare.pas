unit DSA.Graph.PrimMstCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;

implementation

uses
  DSA.Utils,
  DSA.Interfaces.DataStructure,
  DSA.Graph.PrimMST,
  DSA.Graph.LazyPrimMST,
  DSA.Graph.SparseWeightedGraph;

type
  TSparseWeightedGraph_dbl = specialize TSparseWeightedGraph<double>;
  TWeightGraph_dbl = specialize TWeightGraph<double>;
  TReadGraphWeight_dbl = specialize TReadGraphWeight<double>;
  TPrimMST_dbl = specialize TPrimMST<double>;
  TLazyPrimMST_dbl = specialize TLazyPrimMST<double>;

function TLazyPrimTime(const g: TWeightGraph_dbl): string;
var
  startTime, endTime: cardinal;
  lpm: TLazyPrimMST_dbl;
begin
  startTime := TThread.GetTickCount64;
  lpm := TLazyPrimMST_dbl.Create(g);
  lpm.Weight;
  endTime := TThread.GetTickCount64;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

function TPrimTime(const g: TWeightGraph_dbl): string;
var
  startTime, endTime: cardinal;
  pm: TPrimMST_dbl;
begin
  startTime := TThread.GetTickCount64;
  pm := TPrimMST_dbl.Create(g);
  pm.Weight;
  endTime := TThread.GetTickCount64;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

// 测试我们实现的两种Prim算法的性能差距
// 可以看出这一节使用索引堆实现的Prim算法优于上一小节的Lazy Prim算法
procedure Main;
var
  v1, v2, v3, v4: integer;
  fileName1, fileName2, fileName3, fileName4: string;
  g1, g2, g3, g4: TSparseWeightedGraph_dbl;
begin
  filename1 := WEIGHT_GRAPH_FILE_NAME_1;
  v1 := 8;

  filename2 := WEIGHT_GRAPH_FILE_NAME_2;
  v2 := 250;

  filename3 := WEIGHT_GRAPH_FILE_NAME_3;
  v3 := 1000;

  filename4 := WEIGHT_GRAPH_FILE_NAME_4;
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

  WriteLn('Test for WeightGraph1: ', TLazyPrimTime(g1), 's.');
  WriteLn('Test for WeightGraph2: ', TLazyPrimTime(g2), 's.');
  WriteLn('Test for WeightGraph3: ', TLazyPrimTime(g3), 's.');
  WriteLn('Test for WeightGraph4: ', TLazyPrimTime(g4), 's.');

  WriteLn;

  // Test Prim MST
  WriteLn('Test Prim MST:');

  WriteLn('Test for WeightGraph1: ', TPrimTime(g1), 's.');
  WriteLn('Test for WeightGraph2: ', TPrimTime(g2), 's.');
  WriteLn('Test for WeightGraph3: ', TPrimTime(g3), 's.');
  WriteLn('Test for WeightGraph4: ', TPrimTime(g4), 's.');

end;

end.
