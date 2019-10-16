//  350. 两个数组的交集 II
//
//  给定两个数组，编写一个函数来计算它们的交集。
//
//  示例 1:
//
//  输入: nums1 = [1,2,2,1], nums2 = [2,2]
//  输出: [2,2]
//  示例 2:
//
//  输入: nums1 = [4,9,5], nums2 = [9,4,9,8,4]
//  输出: [4,9]
//  说明：
//
//  输出结果中每个元素出现的次数，应与元素在两个数组中出现的次数一致。
//  我们可以不考虑输出结果的顺序。
//  进阶:
//
//  如果给定的数组已经排好序呢？你将如何优化你的算法？
//  如果 nums1 的大小比 nums2 小很多，哪种方法更优？
//  如果 nums2 的元素存储在磁盘上，磁盘内存是有限的，并且你不能一次加载所有的元素到
//  内存中，你该怎么办？
//
//  class Solution {
//    public int[] intersect(int[] nums1, int[] nums2) {
//
//    }
//  }
unit DSA.LeetCode._350;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Utils,
  DSA.Tree.BSTMap;

type

  { Solution }

  TSolution = class
    function intersect(nums1, nums2: TArray_int): TArray_int;
  end;

procedure Main;

implementation

procedure Main;
var
  slt: TSolution;
  nums1, nums2, tempNums: TArray_int;
  i: integer;
begin
  slt := TSolution.Create;
  nums1 := [1, 2, 2, 1];
  nums2 := [2, 2];
  tempNums := slt.intersect(nums1, nums2);

  for i := 0 to Length(tempNums) - 1 do
    Write(tempNums[i], #9);
  WriteLn;

  nums1 := [4, 9, 5];
  nums2 := [9, 4, 9, 8, 4];
  tempNums := slt.intersect(nums1, nums2);

  for i := 0 to Length(tempNums) - 1 do
    Write(tempNums[i], #9);
  WriteLn;
end;

type
  TBSTMap_int_int = specialize TBSTMap<integer, integer, TComparer_int>;

{ Solution }

function TSolution.intersect(nums1, nums2: TArray_int): TArray_int;
var
  map: TBSTMap_int_int;
  i, n: integer;
  arr: TArrayList_int;
begin
  map := TBSTMap_int_int.Create;

  for i := 0 to Length(nums1) - 1 do
  begin
    if not map.Contains(nums1[i]) then
      map.Add(nums1[i], 1)
    else
      map.Set_(nums1[i], map.Get(nums1[i]).PValue^ + 1);
  end;

  arr := TArrayList_int.Create;
  for i := 0 to Length(nums2) - 1 do
  begin
    if map.Contains(nums2[i]) then
    begin
      n := map.Get(nums2[i]).PValue^;

      if n > 0 then
      begin
        arr.AddLast(nums2[i]);
        Dec(n);
        map.Set_(nums2[i], n);
      end;

      if n <= 0 then
        map.Remove(nums2[i]);
    end;
  end;

  Result := arr.ToArray;
end;

end.
