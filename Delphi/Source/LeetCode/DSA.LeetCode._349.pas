//  349. 两个数组的交集
//
//  给定两个数组，编写一个函数来计算它们的交集。
//
//  示例 1:
//
//  输入: nums1 = [1,2,2,1], nums2 = [2,2]
//  输出: [2]
//  示例 2:
//
//  输入: nums1 = [4,9,5], nums2 = [9,4,9,8,4]
//  输出: [9,4]
//  说明:
//
//  输出结果中的每个元素一定是唯一的。
//  我们可以不考虑输出结果的顺序。
//
//  class Solution {
//      public int[] intersection(int[] nums1, int[] nums2) {
//
//      }
//  }

unit DSA.LeetCode._349;

interface

uses
  System.SysUtils,
  DSA.Utils,
  DSA.Tree.BSTSet;

type
  TSolution = class
    function intersection(nums1: TArray_int; nums2: TArray_int): TArray_int;
  end;

procedure Main;

implementation

procedure Main;
var
  slt: TSolution;
  nums1, nums2, nums3: TArray_int;
  i: integer;
begin
  slt := TSolution.Create;
  nums1 := [1, 2, 2, 1];
  nums2 := [2, 2];

  nums3 := slt.intersection(nums1, nums2);
  for i := 0 to Length(nums3) - 1 do
    write(nums3[i].toString + #9);

  WriteLn;
end;

type
  TBSTSet_int = TBSTSet<integer>;

  { TSolution }

function TSolution.intersection(nums1, nums2: TArray_int): TArray_int;
var
  list: TArrayList_int;
  BSTSet: TBSTSet_int;
  n: integer;
begin
  BSTSet := TBSTSet_int.Create;

  for n in nums1 do
  begin
    BSTSet.Add(n);
  end;

  list := TArrayList_int.Create();
  for n in nums2 do
  begin
    if BSTSet.Contains(n) then
    begin
      list.AddLast(n);
      BSTSet.Remove(n);
    end;
  end;

  Result := list.ToArray;
end;

end.
