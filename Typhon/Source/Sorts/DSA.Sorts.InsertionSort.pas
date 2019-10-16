unit DSA.Sorts.InsertionSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils,
  DSA.Sorts.SelectionSort;

type

  { TInsertionSort }

  generic TInsertionSort<T> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;

  public
    /// <summary> 对arr数组进行插入排序 </summary>
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T); overload;
    /// <summary> 对arr[l...r]范围的数组进行插入排序 </summary>
    class procedure Sort(var arr: TArr_T; l, r: integer; cmp: ICmp_T); overload;
    /// <summary> 优化插入排序 </summary>
    class procedure Sort_Adv(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

type
  TSelectionSort_int = specialize TSelectionSort<integer>;
  TInsertionSort_int = specialize TInsertionSort<integer>;

procedure Main;
var
  arr1, arr2, arr3: TArray_int;
  n: integer;
  SortTestHelper: TSortTestHelper_int;
  ts1: TSelectionSort_int;
  ts2: TInsertionSort_int;
begin
  n := 20000;
  SortTestHelper := TSortTestHelper_int.Create;
  arr1 := SortTestHelper.GenerateNearlyOrderedArray(n, 1000);
  arr2 := SortTestHelper.CopyArray(arr1);
  arr3 := SortTestHelper.CopyArray(arr1);

  ts1 := TSelectionSort_int.Create;
  SortTestHelper.TestSort('SelectionSort'#9#9, arr1, @ts1.Sort);
  ts1.Free;

  ts2 := TInsertionSort_int.Create;
  SortTestHelper.TestSort('InsertionSort'#9#9, arr2, @ts2.Sort);
  SortTestHelper.TestSort('InsertionSort_Adv'#9, arr3, @ts2.Sort_Adv);
  ts2.Free;
end;

class procedure TInsertionSort.Sort(var arr: TArr_T; l, r: integer; cmp: ICmp_T);
var
  i, j: integer;
  e: T;
begin
  for i := l + 1 to r do
  begin
    e := arr[i];
    j := i;

    while (j > l) and (cmp.Compare(arr[j - 1], e) > 0) do
    begin
      arr[j] := arr[j - 1];
      Dec(j);
    end;

    arr[j] := e;
  end;
end;

class procedure TInsertionSort.Sort(var arr: TArr_T; cmp: ICmp_T);

  procedure __swap(var a, b: T);
  var
    tmp: T;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;

var
  i, j: integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    // 寻找元素arr[i]合适的插入位置
    for j := i downto 1 do
    begin
      if cmp.Compare(arr[j], arr[j - 1]) < 0 then
        __swap(arr[j], arr[j - 1])
      else
        Break;
    end;
  end;
end;

class procedure TInsertionSort.Sort_Adv(var arr: TArr_T; cmp: ICmp_T);
var
  i, j: integer;
  e: T;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    e := arr[i];
    j := i;

    while (j > 0) and (cmp.Compare(e, arr[j - 1]) < 0) do
    begin
      arr[j] := arr[j - 1];
      Dec(j);
    end;

    arr[j] := e;
  end;
end;

end.
