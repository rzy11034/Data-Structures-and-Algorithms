unit DSA.Sorts.MergeSort;

interface

uses
  System.SysUtils,
  System.Math,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.Sorts.InsertionSort;

type
  TMergeSort<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;
    TInsertionSort_T = TInsertionSort<T>;

  var
    class var __cmp: ICmp_T;

    /// <summary> 将arr[l...mid]和arr[mid+1...r]两部分进行归并 </summary>
    class procedure __merge(var arr: TArr_T; l, mid, r: integer);
    /// <summary> 递归使用归并排序,对arr[l...r]的范围进行排序 </summary>
    class procedure __sort(var arr: TArr_T; l, r: integer);
  public
    /// <summary> 使用自顶向下的归并排序算法--递归 </summary>
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
    /// <summary> 使用自底向上的归并排序算法--迭代 </summary>
    class procedure Sort_BU(var arr: TArr_T; cmp: ICmp_T);

  end;

procedure Main;

implementation

type
  TMergeSort_int = TMergeSort<integer>;

procedure Main;
var
  sourceArr, targetArr: TArray_int;
  n, swapTimes: integer;
  MergeSort_int: TMergeSort_int;
begin
  n := 1000000;

  WriteLn('Test for random array, size = ', n, ', random range [0, ', n, ']');
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateRandomArray(n, n);

    MergeSort_int := TMergeSort_int.Create;
    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, MergeSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort_BU'#9#9, targetArr, MergeSort_int.Sort_BU);
    MergeSort_int.Free;

    Free;
  end;

  swapTimes := 6000;
  WriteLn('Test for nearly ordered array, size = ', n, ', swap time = ',
    swapTimes);
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateNearlyOrderedArray(n, swapTimes);

    MergeSort_int := TMergeSort_int.Create;
    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, MergeSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort_BU'#9#9, targetArr, MergeSort_int.Sort_BU);
    MergeSort_int.Free;

    Free;
  end;
end;

{ TMergeSort<T> }

class procedure TMergeSort<T>.Sort(var arr: TArr_T; cmp: ICmp_T);
begin
  __cmp := cmp;
  __sort(arr, 0, Length(arr) - 1);
end;

class procedure TMergeSort<T>.Sort_BU(var arr: TArr_T; cmp: ICmp_T);
var
  i, n, sz: integer;
begin
  n := Length(arr);

  // Merge Sort Bottom Up 优化
  // 对于小数组, 使用插入排序优化
  i := 0;
  while i < n do
  begin
    TInsertionSort_T.Sort(arr, i, min(i + 15, n - 1), __cmp);
    i := i + 16;
  end;

  sz := 16;
  while sz < n do
  begin
    i := 0;
    while i < (n - sz) do
    begin
      // 对于arr[mid] <= arr[mid+1]的情况,不进行merge
      if __cmp.Compare(arr[i + sz - 1], arr[i + sz]) > 0 then
        __merge(arr, i, i + sz - 1, min(i + sz + sz - 1, n - 1));

      i := i + sz * 2;
    end;

    sz := sz + sz;
  end;
end;

class procedure TMergeSort<T>.__merge(var arr: TArr_T; l, mid, r: integer);
var
  aux: TArr_T;
  i, j, k: integer;
begin
  SetLength(aux, r - l + 1);
  for i := l to r do
    aux[i - l] := arr[i];

  // 初始化，i指向左半部分的起始索引位置l；j指向右半部分起始索引位置mid+1
  i := l;
  j := mid + 1;

  for k := l to r do
  begin
    if (i > mid) then
    begin
      arr[k] := aux[j - l];
      Inc(j);
    end
    else if (j > r) then
    begin
      arr[k] := aux[i - l];
      Inc(i);
    end
    else if __cmp.Compare(aux[i - l], aux[j - l]) < 0 then
    begin
      arr[k] := aux[i - l];
      Inc(i);
    end
    else
    begin
      arr[k] := aux[j - l];
      Inc(j);
    end;
  end;
end;

class procedure TMergeSort<T>.__sort(var arr: TArr_T; l, r: integer);
var
  mid: integer;
begin
  //  if l >= r then
  //    Exit;
  if r - l <= 15 then
  begin
    TInsertionSort_T.Sort(arr, l, r, __cmp);
    Exit;
  end;

  mid := l + (r - l) shr 1;

  __sort(arr, l, mid);
  __sort(arr, mid + 1, r);

  if __cmp.Compare(arr[mid], arr[mid + 1]) > 0 then
    __merge(arr, l, mid, r);
end;

end.
