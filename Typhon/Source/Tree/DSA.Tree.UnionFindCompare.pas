unit DSA.Tree.UnionFindCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  //DSA.Tree.UnionFind1,
  //DSA.Tree.UnionFind2,
  DSA.Tree.UnionFind3,
  DSA.Tree.UnionFind4,
  DSA.Tree.UnionFind5,
  DSA.Tree.UnionFind6;

procedure Main;

implementation

function testTime(uf: IUnionFind; m: integer): string;
var
  second: double;
  startTime, endTime: cardinal;
  i, a, b: integer;
  size: integer;
begin
  Randomize;
  size := uf.GetSize;
  startTime := TThread.GetTickCount64;

  for i := 0 to m - 1 do
  begin
    a := Random(size);
    b := Random(size);
    uf.UnionElements(a, b);
  end;

  for i := 0 to m - 1 do
  begin
    a := Random(size);
    b := Random(size);
    uf.IsConnected(a, b);
  end;

  endTime := TThread.GetTickCount64;
  second := (endTime - startTime) / 1000;
  Result := second.ToString;
end;

procedure Main();
var
  size, m: integer;
  //uf1: TUnionFind1;
  //uf2: TUnionFind2;
  uf3: TUnionFind3;
  uf4: TUnionFind4;
  uf5: TUnionFind5;
  uf6: TUnionFind6;
begin
  size := 1000000;
  m := 1000000;

  //uf1 := TUnionFind1.Create(size);
  //Writeln('UnionFind1: ', testTime(uf1, m), 's');
  //
  //uf2 := TUnionFind2.Create(size);
  //Writeln('UnionFind2: ', testTime(uf2, m), 's');

  uf3 := TUnionFind3.Create(size);
  Writeln('UnionFind3: ', testTime(uf3, m), 's');

  uf4 := TUnionFind4.Create(size);
  Writeln('UnionFind4: ', testTime(uf4, m), 's');

  uf5 := TUnionFind5.Create(size);
  Writeln('UnionFind5: ', testTime(uf5, m), 's');

  uf6 := TUnionFind6.Create(size);
  Writeln('UnionFind6: ', testTime(uf6, m), 's');
end;

end.
