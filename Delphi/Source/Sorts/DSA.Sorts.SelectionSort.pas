unit DSA.Sorts.SelectionSort;

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Interfaces.DataStructure,
  DSA.Utils;

type

  { TSelectionSort }

  TSelectionSort<T> = class
  private type
    TArray_T = TArray<T>;
    ICmp_T = IComparer<T>;

  var
    class procedure __swap(var a, b: T);

  public
    class procedure Sort(var arr: TArray_T; Comparer: ICmp_T);
  end;

procedure Main;

implementation

type

  TStudent = class
  type

    { TComparer }

    TComparer = class(TInterfacedObject, IComparer<TStudent>)
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

  TSelectionSort_int = TSelectionSort<integer>;
  TSelectionSort_TStudent = TSelectionSort<TStudent>;
  TArr_student = TArray<TStudent>;

procedure Main;
var
  n: integer;
  arr: TArray_int;
  studentArr: TArr_student;
  ts1: TSortTestHelper_int;
  i: integer;
begin
  n := 100000;

  ts1 := TSortTestHelper_int.Create;
  arr := ts1.GenerateRandomArray(n, n);
  //ts1.TestSort('SelectionSort', arr, TSelectionSort_int.Sort);

  SetLength(studentArr, 4);
  studentArr := [TStudent.Create('D', 90), TStudent.Create('C', 100),
    TStudent.Create('B', 95), TStudent.Create('A', 95)];
  TSelectionSort_TStudent.Sort(studentArr, TStudent.TComparer.Default);

  for i := 0 to Length(studentArr) - 1 do
    writeLn(studentArr[i].ToString, ' ');

end;

{ TSelectionSort<T> }

class procedure TSelectionSort<T>.Sort(var arr: TArray_T; Comparer: ICmp_T);
var
  i, j, minIndex, bool: integer;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    minIndex := i;

    for j := i + 1 to Length(arr) - 1 do
    begin
      bool := Comparer.Compare(arr[minIndex], arr[j]);
      if bool > 0 then
        minIndex := j;
    end;

    __swap(arr[i], arr[minIndex]);
  end;
end;

class procedure TSelectionSort<T>.__swap(var a, b: T);
var
  tmp: T;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

{ TStudent.TComparer }

function TStudent.TComparer.Compare(const Left, Right: TStudent): integer;
var
  bool: integer;
begin
  if Left.Score = Right.Score then
    bool := 0
  else if Left.Score < Right.Score then
    bool := 1
  else
    bool := -1;

  Result := bool;
end;

constructor TStudent.TComparer.Default;
begin
  inherited Create;
end;

{ TStudent }

constructor TStudent.Create(newName: string; newScore: integer);
begin
  name := newName;
  Score := newScore;
end;

function TStudent.ToString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  sb.Append('Student: ').Append(name).Append(' ').Append(Score);

  Result := sb.ToString;
end;

end.
