//  307. 区域和检索 - 数组可修改
//
//  给定一个整数数组  nums，求出数组从索引 i 到 j  (i ≤ j) 范围内元素的总和，包含 i,  j 两点。
//
//  update(i, val) 函数可以通过将下标为 i 的数值更新为 val，从而对数列进行修改。
//
//  示例:
//
//  Given nums = [1, 3, 5]
//
//  sumRange(0, 2) -> 9
//  update(1, 2)
//  sumRange(0, 2) -> 8
//  说明:
//
//  1.数组仅可以在 update 函数下进行修改。
//  2.你可以假设 update 函数与 sumRange 函数的调用次数是均匀分布的。
//
//  class NumArray {
//
//    public NumArray(int[] nums) {
//
//    }
//
//    public void update(int i, int val) {
//
//    }
//
//    public int sumRange(int i, int j) {
//
//    }
//  }
//
//  /**
//   * Your NumArray object will be instantiated and called as such:
//   * NumArray obj = new NumArray(nums);
//   * obj.update(i,val);
//   * int param_2 = obj.sumRange(i,j);
//   */

unit DSA.LeetCode._307;

interface

uses
  System.SysUtils,
  System.rtti,
  DSA.Utils,
  DSA.Tree.SegmentTree;

type
  TsegmentTree_int = TSegmentTree<Integer>;

  TNumArray = class
  private
    __sgt: TsegmentTree_int;
  public
    constructor Create(nums: TArray_int);
    procedure update(i, val: Integer);
    function sumRange(i, j: Integer): Integer;
  end;

procedure Main;

implementation

procedure Main;
var
  nums: TArray_int;
  numArray: TNumArray;
begin
  nums := [1, 3, 5];
  numArray := TNumArray.Create(nums);

  Writeln(numArray.sumRange(0, 2));
  numArray.update(1, 2);
  Writeln(numArray.sumRange(0, 2));
end;

{ TNumArray }

constructor TNumArray.Create(nums: TArray_int);
begin
  __sgt := TsegmentTree_int.Create(nums, TMerger.Create);
end;

function TNumArray.sumRange(i, j: Integer): Integer;
begin
  Result := __sgt.Query(i, j);
end;

procedure TNumArray.update(i, val: Integer);
begin
  __sgt.Set_(i, val);
end;

end.
