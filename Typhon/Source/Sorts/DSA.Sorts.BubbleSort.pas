unit DSA.Sorts.BubbleSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type

  { TBubbleSort }

  generic TBubbleSort<T> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;

  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

uses
  DSA.Sorts.InsertionSort,
  DSA.Sorts.SelectionSort;

type
  TSelectionSort_int = specialize TSelectionSort<integer>;
  TInsertionSort_int = specialize TInsertionSort<integer>;
  TBubbleSort_int = specialize TBubbleSort<integer>;

procedure Main;
var
  sourceArr, targetArr: TArray_int;
  n: integer;
begin
  n := 20000;

  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateNearlyOrderedArray(n, 1000);

    targetArr := CopyArray(sourceArr);
    TestSort('BubbleSort'#9#9, targetArr, @TBubbleSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('SelectionSort'#9#9, targetArr, @TSelectionSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('InsertionSort'#9#9, targetArr, @TInsertionSort_int(nil).Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('InsertionSort_Adv'#9, targetArr, @TInsertionSort_int(nil).Sort_Adv);

    Free;
  end;
end;

{ TBubbleSort }

class procedure TBubbleSort.Sort(var arr: TArr_T; cmp: ICmp_T);
var
  temp: T;
  unSorted: boolean;
  n: integer;
  i: integer;
  j: integer;
  bool: integer;
begin
  unSorted := True;
  n := Length(arr);
  j := 1;

  while unSorted do
  begin
    unSorted := False;
    Inc(j);

    for i := 0 to n - j do
    begin
      bool := cmp.Compare(arr[i], arr[i + 1]);

      if bool > 0 then
      begin
        temp := arr[i];
        arr[i] := arr[i + 1];
        arr[i + 1] := temp;

        unSorted := True;
      end;
    end;
  end;
end;

end.
