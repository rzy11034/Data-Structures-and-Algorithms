unit DSA.Tree.IndexHeap;

interface

uses
  System.SysUtils,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type
  THeapkind = (Min, Max);

  TIndexHeap<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;
    TCmp_T = TComparer<T>;

  var
    __data: TArr_T;
    __index: TArray_int;
    __reverse: TArray_int;
    __cmp: ICmp_T;
    __heapKind: THeapkind;
    __count: integer;
    __capacity: integer;

    procedure __shiftUp(k: integer);
    procedure __shiftDown(k: integer);
    procedure __swap(var a, b: integer); inline;

  public
    constructor Create(capacity: integer = 10; heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(capacity: integer; cmp: ICmp_T; heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(const arr: TArr_T; cmp: ICmp_T; heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(const arr: TArr_T; heapKind: THeapkind = THeapkind.Min); overload;
    destructor Destroy; override;

    /// <summary> 返回索引堆中的元素个数 </summary>
    function Size: integer;
    /// <summary> 返回一个布尔值, 表示索引堆中是否为空 </summary>
    function IsEmpty: boolean;
    /// <summary> 向索引堆中插入一个新元素，新元素的索引为 i 的元素 e </summary>
    procedure Insert(index: integer; e: T);
    /// <summary> 从索引堆中取出堆顶元素, 即索引堆中所存储的第一个数据 </summary>
    function ExtractFirst: T;
    /// <summary> 从索引堆中取出堆顶元素的索引 </summary>
    function ExtractFirstIndex: integer;
    /// <summary> 返回索引堆中的堆顶元素 </summary>
    function FindFirst: T;
    /// <summary> 返回索引堆中的堆顶元素的索引 </summary>
    function FindFirstIndex: integer;
    /// <summary> 返回索引堆中索引为i的元素 </summary>
    function Find(i: integer): T;
    /// <summary> 将索引堆中索引为i的元素修改为e </summary>
    procedure Change(i: integer; e: T);
    /// <summary> 引i所在的位置是否存在元素 </summary>
    function Contain(i: integer): boolean;
  end;

procedure Main;

implementation

type
  TIndexHeap_T = TIndexHeap<integer>;

procedure Main;
var
  n: integer;
  i: integer;
  IH_Max, IH_Min: TIndexHeap_T;
  sourceArr, testArr: TArray_int;
begin
  n := 1000000;
  Randomize;

  IH_Max := TIndexHeap_T.Create(n, THeapkind.Max);
  with IH_Max do
  begin
    for i := 0 to n - 1 do
      Insert(i, Random(n));

    SetLength(testArr, n);
    for i := 0 to n - 1 do
      testArr[i] := ExtractFirst;

    for i := 1 to n - 1 do
      if (testArr[i - 1] < testArr[i]) then
        raise Exception.Create('Error');

    Writeln('Test MaxIndexHeap completed.');

    Free;
  end;

  IH_Min := TIndexHeap_T.Create(n, THeapkind.Min);
  with IH_Min do
  begin
    for i := 0 to n - 1 do
      Insert(i, Random(1000));

    SetLength(testArr, n);
    for i := 0 to n - 1 do
      testArr[i] := ExtractFirst;

    for i := 1 to n - 1 do
      if (testArr[i - 1] > testArr[i]) then
        raise Exception.Create('Error');

    Writeln('Test MinIndexHeap completed.');

    Free;
  end;

  TDSAUtils.DrawLine;
  SetLength(sourceArr, n);
  for i := 0 to n - 1 do
    sourceArr[i] := Random(n);

  IH_Max := TIndexHeap_T.Create(sourceArr, THeapkind.Max);
  with IH_Max do
  begin
    SetLength(testArr, n);
    for i := 0 to n - 1 do
      testArr[i] := ExtractFirst;

    for i := 1 to n - 1 do
      if (testArr[i - 1] < testArr[i]) then
        raise Exception.Create('Error');

    Writeln('Test MinIndexHeap completed.');

    Free;
  end;

  IH_Min := TIndexHeap_T.Create(sourceArr, THeapkind.Min);
  with IH_Min do
  begin
    SetLength(testArr, n);
    for i := 0 to n - 1 do
      testArr[i] := ExtractFirst;

    for i := 1 to n - 1 do
      if (testArr[i - 1] > testArr[i]) then
        raise Exception.Create('Error');

    Writeln('Test MinIndexHeap completed.');

    Free;
  end;
end;

{ TIndexHeap<T> }

procedure TIndexHeap<T>.Change(i: integer; e: T);
begin
  Assert(Contain(i));

  inc(i);
  __data[i] := e;

  // 找到indexes[j] = i, j表示data[i]在堆中的位置
  // 之后shiftUp(j), 再shiftDown(j)
  //for j := 1 to __count do
  //begin
  //  if __index[j] = i then
  //  begin
  //    __data[__index[j]] := e;
  //    __shiftUp(j);
  //    __shiftDown(j);
  //  end;
  //end;

  // 通过reverse直接定位索引i在indexes中的位置
  __shiftUp(__reverse[i]);
  __shiftDown(__reverse[i]);
end;

function TIndexHeap<T>.Contain(i: integer): boolean;
begin
  Assert((i + 1 >= 1) and (i + 1 <= __capacity));
  Result := __reverse[i + 1] <> 0;
end;

constructor TIndexHeap<T>.Create(capacity: integer; heapKind: THeapkind);
begin
  Self.Create(capacity, TCmp_T.Default, heapKind);
end;

constructor TIndexHeap<T>.Create(const arr: TArr_T; heapKind: THeapkind);
begin
  Self.Create(arr, TCmp_T.Default, heapKind);
end;

constructor TIndexHeap<T>.Create(capacity: integer; cmp: ICmp_T; heapKind: THeapkind);
begin
  SetLength(__data, capacity + 1);
  SetLength(__index, capacity + 1);
  SetLength(__reverse, capacity + 1);

  __count := 0;
  __capacity := capacity;
  __heapKind := heapKind;
  __cmp := cmp;
end;

constructor TIndexHeap<T>.Create(const arr: TArr_T; cmp: ICmp_T; heapKind: THeapkind);
var
  i: integer;
begin
  __capacity := Length(arr);

  SetLength(__data, __capacity + 1);
  SetLength(__index, __capacity + 1);
  SetLength(__reverse, __capacity + 1);

  __count := 0;
  __heapKind := heapKind;
  __cmp := cmp;

  for i := 0 to Length(arr) - 1 do
    Self.Insert(i, arr[i]);
end;

destructor TIndexHeap<T>.Destroy;
begin
  inherited Destroy;
end;

function TIndexHeap<T>.ExtractFirst: T;
var
  ret: T;
begin
  Assert(__count > 0);

  ret := __data[__index[1]];

  __swap(__index[1], __index[__count]);
  __reverse[__index[1]] := 1;
  __reverse[__index[__count]] := 0;

  Dec(__count);
  __shiftDown(1);

  Result := ret;
end;

function TIndexHeap<T>.ExtractFirstIndex: integer;
var
  ret: integer;
begin
  Assert(__count > 0);

  ret := __index[1] - 1;

  __swap(__index[1], __index[__count]);
  __reverse[__index[1]] := 1;
  __reverse[__index[__count]] := 0;

  Dec(__count);
  __shiftDown(1);

  Result := ret;
end;

function TIndexHeap<T>.Find(i: integer): T;
begin
  Assert(Contain(i));
  Result := __data[i + 1];
end;

function TIndexHeap<T>.FindFirst: T;
begin
  if __count <= 0 then
    raise Exception.Create('Can not FindFirst when heap is empty.');

  Result := __data[__index[1]];
end;

function TIndexHeap<T>.FindFirstIndex: integer;
begin
  Result := __index[1] - 1;
end;

procedure TIndexHeap<T>.Insert(index: integer; e: T);
begin
  Assert(__count + 1 <= __capacity);
  Assert((index + 1 >= 1) and (index + 1 <= __capacity));

  // 再插入一个新元素前,还需要保证索引i所在的位置是没有元素的。
  Assert(not Contain(index));

  inc(index);

  __data[index] := e;
  __index[__count + 1] := index;
  __reverse[index] := __count + 1;

  inc(__count);
  __shiftUp(__count);
end;

function TIndexHeap<T>.IsEmpty: boolean;
begin
  Result := __count = 0;
end;

function TIndexHeap<T>.Size: integer;
begin
  Result := __count;
end;

procedure TIndexHeap<T>.__shiftDown(k: integer);
var
  j: integer;
begin
  case __heapKind of
    THeapkind.Min:
      while 2 * k <= __count do
      begin
        j := 2 * k;

        if (j + 1 <= __count) and (__cmp.Compare(__data[__index[j + 1]], __data[__index[j]]) < 0) then
          inc(j);

        if __cmp.Compare(__data[__index[k]], __data[__index[j]]) <= 0 then
          Break;

        __swap(__index[k], __index[j]);
        __reverse[__index[k]] := k;
        __reverse[__index[j]] := j;

        k := j;
      end;

    THeapkind.Max:
      while 2 * k <= __count do
      begin
        j := 2 * k;

        if (j + 1 <= __count) and (__cmp.Compare(__data[__index[j + 1]], __data[__index[j]]) > 0) then
          inc(j);

        if __cmp.Compare(__data[__index[k]], __data[__index[j]]) >= 0 then
          Break;

        __swap(__index[k], __index[j]);
        __reverse[__index[k]] := k;
        __reverse[__index[j]] := j;

        k := j;
      end;
  end;
end;

procedure TIndexHeap<T>.__shiftUp(k: integer);
begin
  case __heapKind of
    THeapkind.Max:
      while (k > 1) and (__cmp.Compare(__data[__index[k div 2]], __data[__index[k]]) < 0) do
      begin
        __swap(__index[k div 2], __index[k]);
        __reverse[__index[k div 2]] := k div 2;
        __reverse[__index[k]] := k;

        k := k div 2;
      end;

    THeapkind.Min:
      while (k > 1) and (__cmp.Compare(__data[__index[k div 2]], __data[__index[k]]) > 0) do
      begin
        __swap(__index[k div 2], __index[k]);
        __reverse[__index[k div 2]] := k div 2;
        __reverse[__index[k]] := k;

        k := k div 2;
      end;
  end;
end;

procedure TIndexHeap<T>.__swap(var a, b: integer);
var
  tmp: integer;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

end.
