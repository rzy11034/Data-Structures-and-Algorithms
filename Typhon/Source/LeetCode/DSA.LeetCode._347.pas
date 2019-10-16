//  347. 前 K 个高频元素
//
//  给定一个非空的整数数组，返回其中出现频率前 k 高的元素。
//
//  示例 1:
//
//  输入: nums = [1,1,1,2,2,3], k = 2
//  输出: [1,2]
//  示例 2:
//
//  输入: nums = [1], k = 1
//  输出: [1]
//  说明：
//
//  你可以假设给定的 k 总是合理的，且 1 ≤ k ≤ 数组中不相同的元素的个数。
//  你的算法的时间复杂度必须优于 O(n log n) , n 是数组的大小。
//
//  class Solution {
//      public List<Integer> topKFrequent(int[] nums, int k) {
//
//      }
//  }
unit DSA.LeetCode._347;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Tree.BSTMap,
  DSA.Tree.PriorityQueue,
  DSA.Utils;

type

  { TSolution }

  TSolution = class
  private
    type

    { TFreq }

    TFreq = class
    public
      E, Freq: integer;
      constructor Create(newE, newFreq: integer);
    end;

    { TFreqComparer }

    TFreqComparer = class(TInterfacedObject, specialize IDSA_Comparer<TFreq>)
      constructor Default;
      function Compare(const Left, Right: TFreq): integer;
    end;

  public
    function topKFrequent(nums: TArray_int; k: integer): TArrayList_int;
  end;

procedure Main;

implementation

procedure Main;
var
  list: TArrayList_int;
  slt: TSolution;
  i: integer;
begin
  slt := TSolution.Create;
  list := slt.topKFrequent([1, 1, 1, 2, 2, 3], 2);
  for i := 0 to list.GetSize - 1 do
    Write(list[i], #9);
  Writeln;

  TDSAUtils.DrawLine;

  list := slt.topKFrequent([1], 1);
  for i := 0 to list.GetSize - 1 do
    Write(list[i], #9);
  Writeln;
end;

{ TSolution }

function TSolution.topKFrequent(nums: TArray_int; k: integer): TArrayList_int;
type
  TBSTMap_int_int = specialize TBSTMap<integer, integer, TComparer_int>;
  TPriorityQueue_TSolution_TFreq = specialize TPriorityQueue<TSolution.TFreq,
    TSolution.TFreqComparer>;
var
  map: TBSTMap_int_int;
  queue: TPriorityQueue_TSolution_TFreq;
  i, n, key: integer;
  list: TArrayList_int;
  tempFreg: TFreq;
begin
  map := TBSTMap_int_int.Create;
  for i := 0 to Length(nums) - 1 do
  begin
    n := nums[i];
    if map.Contains(n) then
      map.Set_(n, map.Get(n).PValue^ + 1)
    else
      map.Add(n, 1);
  end;

  queue := TPriorityQueue_TSolution_TFreq.Create(TQueueKind.Min,
    TFreqComparer.Create);
  for key in map.KeySets.ToArray do
  begin
    if (queue.GetSize < k) then
    begin
      tempFreg := TFreq.Create(key, map.Get(key).PValue^);
      queue.EnQueue(tempFreg);
    end
    else
    begin
      if queue.Peek.E < map.Get(key).PValue^ then
      begin
        tempFreg := TFreq.Create(key, map.Get(key).PValue^);
        queue.DeQueue;
        queue.EnQueue(tempFreg);
        map.Remove(key);
      end;
    end;
  end;

  list := TArrayList_int.Create();
  while not (queue.IsEmpty) do
  begin
    list.AddLast(queue.DeQueue.E);
  end;

  Result := list;
end;

{ TSolution.TFreqComparer }

constructor TSolution.TFreqComparer.Default;
begin
  inherited;
end;

function TSolution.TFreqComparer.Compare(const Left, Right: TFreq): integer;
begin
  Result := Left.Freq - Right.Freq;
end;

{ TSolution.TFreq }

constructor TSolution.TFreq.Create(newE, newFreq: integer);
begin
  Self.E := newE;
  Self.Freq := newFreq;
end;

end.
