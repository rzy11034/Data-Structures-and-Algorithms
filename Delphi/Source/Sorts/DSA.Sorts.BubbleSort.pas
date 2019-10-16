unit DSA.Sorts.BubbleSort;

interface

uses
  System.SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type

  TBubbleSort<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;

  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

uses
  DSA.Sorts.InsertionSort,
  DSA.Sorts.SelectionSort;

type
  TSelectionSort_int = TSelectionSort<integer>;
  TInsertionSort_int = TInsertionSort<integer>;
  TBubbleSort_int = TBubbleSort<integer>;

procedure Main;
var
  sourceArr, targetArr: TArray_int;
  n: integer;
  selectionSort_int: TSelectionSort_int;
  InsertionSort_int: TInsertionSort_int;
  BubbleSort_int: TBubbleSort_int;
begin
  n := 20000;

  with TSortTestHelper_int.Create do
  begin
    sourceArr := GenerateNearlyOrderedArray(n, 1000);

    BubbleSort_int := TBubbleSort_int.Create;
    targetArr := CopyArray(sourceArr);
    TestSort('BubbleSort'#9#9, targetArr, BubbleSort_int.Sort);
    BubbleSort_int.Free;

    selectionSort_int := TSelectionSort_int.Create;
    targetArr := CopyArray(sourceArr);
    TestSort('SelectionSort'#9#9, targetArr, selectionSort_int.Sort);
    selectionSort_int.Free;

    InsertionSort_int := TInsertionSort_int.Create;
    targetArr := CopyArray(sourceArr);
    TestSort('InsertionSort'#9#9, targetArr, InsertionSort_int.Sort);

    targetArr := CopyArray(sourceArr);
    TestSort('InsertionSort_Adv'#9, targetArr, InsertionSort_int.Sort_Adv);
    InsertionSort_int.Free;
  end;
end;

{ TBubbleSort<T> }

class procedure TBubbleSort<T>.Sort(var arr: TArr_T; cmp: ICmp_T);
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
