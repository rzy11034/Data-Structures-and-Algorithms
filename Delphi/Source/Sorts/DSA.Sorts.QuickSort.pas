unit DSA.Sorts.QuickSort;

interface

uses
  System.SysUtils,
  System.Math,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.Sorts.InsertionSort;

type

  { TQuickSort }

  TQuickSort<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;
    TInsertionSort_T = TInsertionSort<T>;

  var
    class var __cmp: ICmp_T;

    /// <summary> 对arr[l...r]部分进行快速排序 </summary>
    class procedure __sort(var arr: TArr_T; l, r: integer);

    /// <summary> 对arr[l...r]部分进行双路快速排序 </summary>
    class procedure __sort2(var arr: TArr_T; l, r: integer);

    /// <summary>
    /// 三路快速排序处理arr[l..r]
    /// 将arr[L..R]分为 < v ; ==v ; > v 三部分
    /// 之后递归对  < v ;  > v 两部分继续进行三路 快速排序
    /// </summary>
    class procedure __sort3Ways(var arr: TArr_T; l, r: integer);

    /// <summary>
    /// 对arr[l...r]部分进行partition操作,
    /// 返回p, 使得arr[l...p-1] < arr[p] ; arr[p+1...r] > arr[p]
    /// </summary>
    class function __partition(var arr: TArr_T; l, r: integer): integer;

    /// <summary>
    /// 双路快速排序的partition
    /// 返回p, 使得arr[l...p-1] <= arr[p] ; arr[p+1...r] >= arr[p]
    // 双路快排处理的元素正好等于arr[p]的时候要注意，详见下面的注释：）
    /// </summary>
    class function __partition2(var arr: TArr_T; l, r: integer): integer;

    class procedure __swap(var a, b: T);
  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);

    /// <summary> 双路快速排序 </summary>
    class procedure Sort2(var arr: TArr_T; cmp: ICmp_T);

    /// <summary> 三路快速排序 </summary>
    class procedure Sort3Ways(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

uses
  DSA.Sorts.MergeSort;

type
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
    TestSort('QuickSort'#9#9, targetArr, TQuickSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort2'#9#9, targetArr, TQuickSort_int.Sort2);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, TQuickSort_int.Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('DelphiSort'#9#9, targetArr, TDelphiSort<integer>.Sort);

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
    TestSort('QuickSort'#9#9, targetArr, TQuickSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort2'#9#9, targetArr, TQuickSort_int.Sort2);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, TQuickSort_int.Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('DelphiSort'#9#9, targetArr, TDelphiSort<integer>.Sort);

    Free;
  end;

  WriteLn('Test for random array, size = ', n, ', random range [0, ', 10, ']');
  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateRandomArray(n, 10);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, TMergeSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort2'#9#9, targetArr, TQuickSort_int.Sort2);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort3Ways'#9#9, targetArr, TQuickSort_int.Sort3Ways);

    targetArr := CopyArray(sourceArr);
    TestSort('DelphiSort'#9#9, targetArr, TDelphiSort<integer>.Sort);

    Free;
  end;
end;

{ TQuickSort }

class procedure TQuickSort<T>.Sort(var arr: TArr_T; cmp: ICmp_T);
begin
  __cmp := cmp;
  __sort(arr, 0, Length(arr) - 1);
end;

class procedure TQuickSort<T>.Sort2(var arr: TArr_T; cmp: ICmp_T);
begin
  __cmp := cmp;
  __sort2(arr, 0, Length(arr) - 1);
end;

class procedure TQuickSort<T>.Sort3Ways(var arr: TArr_T; cmp: ICmp_T);
begin
  __cmp := cmp;
  __sort3Ways(arr, 0, Length(arr) - 1);
end;

class function TQuickSort<T>.__partition(var arr: TArr_T;
  l, r: integer): integer;
var
  v: T;
  j, i: integer;
begin
  // 在arr[l...r]的范围中, 选择一个中间数值作为标定点pivot
  __swap(arr[l], arr[l + (r - l) shr 1]);
  v := arr[l];

  j := l; // arr[l+1...j] < v ; arr[j+1...i) > v
  for i := l + 1 to r do
  begin
    if __cmp.Compare(arr[i], v) < 0 then
    begin
      Inc(j);
      __swap(arr[j], arr[i]);
    end;
  end;

  __swap(arr[l], arr[j]);

  Result := j;
end;

class function TQuickSort<T>.__partition2(var arr: TArr_T;
  l, r: integer): integer;
var
  v: T;
  j, i: integer;
begin
  // 在arr[l...r]的范围中, 选择一个中间数值作为标定点pivot
  __swap(arr[l], arr[l + (r - l) shr 1]);
  v := arr[l];

  i := l + 1;
  j := r;
  while True do
  begin
    while (i <= r) and (__cmp.Compare(arr[i], v) < 0) do
      Inc(i);
    while (j >= l + 1) and (__cmp.Compare(arr[j], v) > 0) do
      Dec(j);

    if i > j then
      Break;

    __swap(arr[i], arr[j]);
    Inc(i);
    Dec(j);
  end;

  __swap(arr[l], arr[j]);
  Result := j;
end;

class procedure TQuickSort<T>.__sort(var arr: TArr_T; l, r: integer);
var
  p: integer;
begin
  if r - l <= 15 then
  begin
    TInsertionSort_T.Sort(arr, l, r, __cmp);
    Exit;
  end;

  p := __partition(arr, l, r);
  __sort(arr, l, p - 1);
  __sort(arr, p + 1, r);
end;

class procedure TQuickSort<T>.__sort2(var arr: TArr_T; l, r: integer);
var
  p: integer;
begin
  if r - l <= 15 then
  begin
    TInsertionSort_T.Sort(arr, l, r, __cmp);
    Exit;
  end;

  p := __partition2(arr, l, r);
  __sort2(arr, l, p - 1);
  __sort2(arr, p + 1, r);
end;

class procedure TQuickSort<T>.__sort3Ways(var arr: TArr_T; l, r: integer);
var
  i, gt, lt: integer;
  v: T;
begin
  if r - l <= 15 then
  begin
    TInsertionSort_T.Sort(arr, l, r, __cmp);
    Exit;
  end;

  // 在arr[l...r]的范围中, 选择一个中间数值作为标定点pivot
  __swap(arr[l], arr[l + (r - l) shr 1]);
  v := arr[l];

  lt := l; // arr[l+1...lt] < v
  gt := r + 1; // arr[gt...r] > v
  i := l + 1; // arr[lt+1...i) == v

  while i < gt do
  begin
    if __cmp.Compare(arr[i], v) < 0 then
    begin
      __swap(arr[i], arr[lt + 1]);
      Inc(lt);
      Inc(i);
    end
    else if __cmp.Compare(arr[i], v) > 0 then
    begin
      __swap(arr[i], arr[gt - 1]);
      Dec(gt);
    end
    else
    begin
      Inc(i);
    end;
  end;

  __swap(arr[l], arr[lt]);

  __sort3Ways(arr, l, lt - 1);
  __sort3Ways(arr, gt, r);
end;

class procedure TQuickSort<T>.__swap(var a, b: T);
var
  tmp: T;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

end.
