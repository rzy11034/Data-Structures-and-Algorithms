unit DSA.Search.BinarySearch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type
  generic TBinarySearch<T> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;

  var
    class function __search2(const arr: TArr_T; target: T;
      l, r: integer; const cmp: ICmp_T): integer;

  public
    /// <summary>
    /// 二分查找法,在有序数组arr中,查找target
    /// 如果找到target,返回相应的索引index
    /// 如果没有找到target,返回-1
    /// </summary>
    class function Search(const arr: TArr_T; target: T; const cmp: ICmp_T): integer;

    /// <summary> 用递归的方式写二分查找法 </summary>
    class function Search2(const arr: TArr_T; target: T;
      const cmp: ICmp_T): integer;
  end;

procedure Main;

implementation

type
  TBinarySearch_int = specialize TBinarySearch<integer>;

procedure Main;
var
  n, i, v: integer;
  arr: TArray_int;
  startTime, endTime: double;
begin
  n := 1000000;
  SetLength(arr, n);

  for i := 0 to n - 1 do
    arr[i] := i;

  // 测试非递归二分查找法
  startTime := TThread.GetTickCount64;
  for i := 0 to 2 * n - 1 do
  begin
    v := TBinarySearch_int.Search(arr, i, TComparer_int.Default);
    if (i < n) then
      Assert(v = i)
    else
      Assert(v = -1);
  end;
  endTime := TThread.GetTickCount64;
  Writeln('Binary Search (Without Recursion): ',
    ((endTime - startTime) / 1000).ToString, ' s');

  // 测试非递归二分查找法
  startTime := TThread.GetTickCount64;
  for i := 0 to 2 * n - 1 do
  begin
    v := TBinarySearch_int.Search2(arr, i, TComparer_int.Default);
    if (i < n) then
      Assert(v = i)
    else
      Assert(v = -1);
  end;
  endTime := TThread.GetTickCount64;
  Writeln('Binary Search (Recursion): ',
    ((endTime - startTime) / 1000).ToString, ' s');
end;

{ TBinarySearch }

class function TBinarySearch.Search(const arr: TArr_T; target: T;
  const cmp: ICmp_T): integer;
var
  l, r, mid: integer;
begin
  // 在arr[l...r]之中查找target
  l := 0;
  r := Length(arr) - 1;

  while l <= r do
  begin
    mid := l + (r - l) div 2;

    if cmp.Compare(arr[mid], target) = 0 then
    begin
      Result := mid;
      Exit;
    end;

    if cmp.Compare(arr[mid], target) > 0 then
      r := mid - 1
    else
      l := mid + 1;
  end;

  Result := -1;
end;

class function TBinarySearch.Search2(const arr: TArr_T; target: T;
  const cmp: ICmp_T): integer;
begin
  Result := __search2(arr, target, 0, Length(arr) - 1, cmp);
end;

class function TBinarySearch.__search2(const arr: TArr_T; target: T;
  l, r: integer; const cmp: ICmp_T): integer;
var
  mid: integer;
begin
  if l > r then
  begin
    Result := -1;
    Exit;
  end;

  mid := l + (r - l) div 2;

  if cmp.Compare(arr[mid], target) = 0 then
  begin
    Result := mid;
    Exit;
  end
  else if cmp.Compare(arr[mid], target) > 0 then
    Result := __Search2(arr, target, l, mid - 1, cmp)
  else
    Result := __Search2(arr, target, mid + 1, r, cmp);
end;

end.
