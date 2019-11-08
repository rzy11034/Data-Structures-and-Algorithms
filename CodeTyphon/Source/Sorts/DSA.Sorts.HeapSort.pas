unit DSA.Sorts.HeapSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.Tree.Heap;

type

  { THeapSort }

  generic THeapSort<T> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;
    TCmp_T = specialize  TComparer<T>;
    THeap_T = specialize THeap<T, TCmp_T>;

  var
    class var __cmp: ICmp_T;
    class procedure __shiftDwon(var arr: TArr_T; k: integer; Count: integer);

  public
    /// <summary> 使用THeap类 </summary>
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
    /// <summary> 原地排序 </summary>
    class procedure Sort_Adv(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

uses
  DSA.Sorts.QuickSort,
  DSA.Sorts.MergeSort;

type
  THeapSort_int = specialize THeapSort<integer>;
  TMergeSort_int = specialize TMergeSort<integer>;
  TQuickSort_int = specialize TQuickSort<integer>;

procedure Main;
var
  sourceArr, targetArr: TArray_int;
  n, swapTimes: integer;
begin
  n := 1000000;

  WriteLn('Test for random array, size = ', n, ', random range [0, ', n, ']');
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateRandomArray(n, n);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, @TMergeSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, @TQuickSort_int(nil).Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort'#9#9, targetArr, @THeapSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort_Adv'#9#9, targetArr, @THeapSort_int(nil).Sort_Adv);

    Free;
  end;

  swapTimes := 100;
  WriteLn('Test for nearly ordered array, size = ', n, ', swap time = ', swapTimes);
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateNearlyOrderedArray(n, swapTimes);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, @TMergeSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, @TQuickSort_int(nil).Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort'#9#9, targetArr, @THeapSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort_Adv'#9#9, targetArr, @THeapSort_int(nil).Sort_Adv);

    Free;
  end;

  WriteLn('Test for random array, size = ', n, ', random range [0, ', 10, ']');
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateRandomArray(n, 10);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, @TMergeSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, @TQuickSort_int(nil).Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort'#9#9, targetArr, @THeapSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort_Adv'#9#9, targetArr, @THeapSort_int(nil).Sort_Adv);

    Free;
  end;
end;

{ THeapSort }

class procedure THeapSort.Sort(var arr: TArr_T; cmp: ICmp_T);
var
  heapMin: THeap_T;
  i: integer;
begin
  heapMin := THeap_T.Create(arr, cmp, THeapkind.Min);

  for i := 0 to Length(arr) - 1 do
    arr[i] := heapMin.ExtractFirst;
end;

class procedure THeapSort.Sort_Adv(var arr: TArr_T; cmp: ICmp_T);
var
  i: integer;
  tmp: T;
begin
  __cmp := cmp;

  for i := (Length(arr) - 1 - 1) div 2 downto 0 do
    __shiftDwon(arr, i, Length(arr));

  for i := Length(arr) - 1 downto 1 do
  begin
    tmp := arr[0];
    arr[0] := arr[i];
    arr[i] := tmp;

    __shiftDwon(arr, 0, i);
  end;
end;

class procedure THeapSort.__shiftDwon(var arr: TArr_T; k: integer; Count: integer);
var
  j: integer;
  e: T;
begin
  e := arr[k];

  while 2 * k + 1 < Count do
  begin
    j := 2 * k + 1;

    if (j + 1 < Count) and (__cmp.Compare(arr[j + 1], arr[j]) > 0) then
      j += 1;

    if __cmp.Compare(e, arr[j]) >= 0 then
      Break;

    arr[k] := arr[j];
    k := j;
  end;

  arr[k] := e;
end;

end.
