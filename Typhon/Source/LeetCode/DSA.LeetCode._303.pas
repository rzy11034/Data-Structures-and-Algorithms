unit DSA.LeetCode._303;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.SegmentTree,
  DSA.Utils;

type
  TsegmentTree_int = specialize TSegmentTree<integer>;

  { TNumArray }

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
