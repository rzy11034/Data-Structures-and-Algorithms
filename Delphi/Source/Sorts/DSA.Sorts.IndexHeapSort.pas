unit DSA.Sorts.IndexHeapSort;

interface

uses
  System.SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.Tree.IndexHeap;

type
  TIndexHeapSort<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;
    TIndexHeap_T = TIndexHeap<T>;

  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

uses
  DSA.Sorts.HeapSort,
  DSA.Sorts.QuickSort,
  DSA.Sorts.MergeSort;

type
  TIndexHeapSort_int = TIndexHeapSort<integer>;
  THeapSort_int = THeapSort<integer>;
  TMergeSort_int = TMergeSort<integer>;
  TQuickSort_int = TQuickSort<integer>;

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
    TestSort('MergeSort'#9#9, targetArr, TMergeSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, TQuickSort_int.Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort'#9#9, targetArr, THeapSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort_Adv'#9#9, targetArr, THeapSort_int.Sort_Adv);

    targetArr := CopyArray(sourceArr);
    TestSort('IndexHeapSort'#9#9, targetArr, TIndexHeapSort_int.Sort);

    Free;
  end;

  swapTimes := 100;
  WriteLn('Test for nearly ordered array, size = ', n, ', swap time = ',
    swapTimes);
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateNearlyOrderedArray(n, swapTimes);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, TMergeSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, TQuickSort_int.Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort'#9#9, targetArr, THeapSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort_Adv'#9#9, targetArr, THeapSort_int.Sort_Adv);

    targetArr := CopyArray(sourceArr);
    TestSort('IndexHeapSort'#9#9, targetArr, TIndexHeapSort_int.Sort);

    Free;
  end;

  WriteLn('Test for random array, size = ', n, ', random range [0, ', 10, ']');
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateRandomArray(n, 10);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, TMergeSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, TQuickSort_int.Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort'#9#9, targetArr, THeapSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('HeapSort_Adv'#9#9, targetArr, THeapSort_int.Sort_Adv);

    targetArr := CopyArray(sourceArr);
    TestSort('IndexHeapSort'#9#9, targetArr, TIndexHeapSort_int.Sort);

    Free;
  end;
end;

{ TIndexHeapSort<T> }

class procedure TIndexHeapSort<T>.Sort(var arr: TArr_T; cmp: ICmp_T);
var
  IH_Max: TIndexHeap_T;
  i: integer;
begin
  IH_Max := TIndexHeap_T.Create(arr, cmp, THeapkind.Min);

  for i := 0 to Length(arr) - 1 do
    arr[i] := IH_Max.ExtractFirst;

  FreeAndNil(IH_Max);
end;

end.
