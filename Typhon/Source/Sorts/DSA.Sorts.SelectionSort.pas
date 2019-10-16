unit DSA.Sorts.SelectionSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type

  { TSelectionSort }

  generic TSelectionSort<T> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;

  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
  end;

procedure Main;

implementation

type

  { TStudent }

  TStudent = class
    type

    { TComparer }

    TComparer = class(TInterfacedObject, specialize IDSA_Comparer<TStudent>)
    public
      constructor Default;
      function Compare(const Left, Right: TStudent): integer;
    end;

  var
    Name: string;
    Score: integer;

    constructor Create(newName: string; newScore: integer);
    function ToString: string; override;
  end;

  TSelectionSort_int = specialize TSelectionSort<integer>;
  TSelectionSort_TStudent = specialize TSelectionSort<TStudent>;
  TArr_student = specialize TArray<TStudent>;

procedure Main;
var
  arr: TArray_int;
  n: integer;
  SortTestHelper: TSortTestHelper_int;
  studentArr: TArr_student;
  i: integer;
  ts: TSelectionSort_int;
begin
  n := 10000;
  ts := TSelectionSort_int.Create;

  SortTestHelper := TSortTestHelper_int.Create;
  arr := SortTestHelper.GenerateRandomArray(n, n);
  SortTestHelper.TestSort('SelectionSort', arr, @ts.Sort);

  SetLength(studentArr, 4);
  studentArr := [TStudent.Create('D', 90), TStudent.Create('C', 100),
    TStudent.Create('B', 95), TStudent.Create('A', 95)];
  TSelectionSort_TStudent.Sort(studentArr, TStudent.TComparer.Default);

  for i := 0 to Length(studentArr) - 1 do
    Writeln(studentArr[i].ToString);

  ts.Free;
end;

{ TStudent.TComparer }

constructor TStudent.TComparer.Default;
begin
  inherited Create;
end;

function TStudent.TComparer.Compare(const Left, Right: TStudent): integer;
var
  bool: integer;
begin
  if Left.Score = Right.Score then
    bool := 0
  else if Left.Score < Right.Score then
    bool := -1
  else
    bool := 1;

  Result := bool;
end;

constructor TStudent.Create(newName: string; newScore: integer);
begin
  Name := newName;
  Score := newScore;
end;

function TStudent.ToString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  sb.Append('Student: ').Append(Name).Append(' ').Append(Score);

  Result := sb.ToString;
end;

{ TSelectionSort }

class procedure TSelectionSort.Sort(var arr: TArr_T; cmp: ICmp_T);

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
  minIndex: integer;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    minIndex := i;

    for j := i to Length(arr) - 1 do
    begin
      if cmp.Compare(arr[minIndex], arr[j]) > 0 then
        minIndex := j;
    end;

    __swap(arr[i], arr[minIndex]);
  end;
end;

end.
