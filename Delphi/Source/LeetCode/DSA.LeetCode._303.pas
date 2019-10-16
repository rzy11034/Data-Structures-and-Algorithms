//  303. 区域和检索 - 数组不可变
//
//  给定一个整数数组  nums，求出数组从索引 i 到 j  (i ≤ j) 范围内元素的总和，包含 i,  j 两点。
//
//  示例：
//
//  给定 nums = [-2, 0, 3, -5, 2, -1]，求和函数为 sumRange()
//
//  sumRange(0, 2) -> 1
//  sumRange(2, 5) -> -1
//  sumRange(0, 5) -> -3
//  说明:
//
//  1.你可以假设数组不可变。
//  2.会多次调用 sumRange 方法。
//
//  class NumArray
//  {
//
//      public NumArray(int[] nums) {
//
//      }
//
//      public int sumRange(int i, int j) {
//
//      }
//  }
//
//  /**
//   * Your NumArray object will be instantiated and called as such:
//   * NumArray obj = new NumArray(nums);
//   * int param_1 = obj.sumRange(i,j);
//   */

unit DSA.LeetCode._303;

interface

uses
  System.SysUtils,
  DSA.Tree.SegmentTree,
  DSA.Utils;

type
  TsegmentTree_int = TSegmentTree<integer>;

  TNumArray = class
  private
    __sgt: TsegmentTree_int;
  public
    constructor Create(nums: TArray_int);
    function sumRange(i: integer; j: integer): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  nums: TArray_int;
  numArray: TNumArray;
begin
  nums := [-2, 0, 3, -5, 2, -1];
  numArray := TNumArray.Create(nums);

  Writeln(numArray.sumRange(0, 2));
  Writeln(numArray.sumRange(2, 5));
  Writeln(numArray.sumRange(0, 5));
end;

{ TNumArray }

constructor TNumArray.Create(nums: TArray_int);
begin
  __sgt := TsegmentTree_int.Create(nums, TMerger.Create);
end;

function TNumArray.sumRange(i, j: integer): integer;
begin
  Result := __sgt.Query(i, j);
end;

end.
