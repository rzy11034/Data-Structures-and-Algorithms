unit DSA.Tree.Heap;

interface

uses
  System.SysUtils,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer,
  DSA.Utils;

type
  THeapkind = (Min, Max);

  THeap<T> = class
  private type
    TArr_T = TArray<T>;
    TArrayList_T = TArrayList<T>;
    TComparer_T = TComparer<T>;

  var
    __data: TArrayList<T>;
    __comparer: IComparer<T>;
    __heapKind: THeapkind;

    /// <summary> 返回完全二叉树的数组表示中，一个索引所表示的元素的父亲节点的索引 </summary>
    function __parent(index: Integer): Integer;
    /// <summary> 返回完全二叉树的数组表示中，一个索引所表示的元素的左孩子节点的索引 </summary>
    function __leftChild(index: Integer): Integer;
    /// <summary> 返回完全二叉树的数组表示中，一个索引所表示的元素的右孩子节点的索引 </summary>
    function __rightChild(index: Integer): Integer;
    /// <summary> 交换索引为 i ， j 的节点元素 </summary>
    procedure __swap(i: Integer; j: Integer);
    /// <summary> 元素上浮过程 </summary>
    procedure __shiftUp(k: Integer);
    /// <summary> 元素下沉过程 </summary>
    procedure __shiftDown(k: Integer);

  public
    constructor Create(capacity: Integer = 10;
      heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(const arr: TArr_T;
      heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(const arr: TArr_T; cmp: IComparer<T>;
      heapKind: THeapkind = THeapkind.Min); overload;
    destructor Destroy; override;

    /// <summary> 返回堆中元素个数 </summary>
    function Size: Integer;
    /// <summary> 返回天所布尔值，表示堆中是否为空 </summary>
    function IsEmpty: Boolean;
    /// <summary> 向堆中添加元素 </summary>
    procedure Add(e: T);
    /// <summary> 返回堆中的第一个元素的值  </summary>
    function FindFirst: T;
    /// <summary> 取出堆中第一个元素 </summary>
    function ExtractFirst: T;
    /// <summary> 设置比较器 </summary>
    procedure SetComparer(c: IComparer<T>);
  end;

procedure Main;

implementation

type
  THeap_int = THeap<Integer>;

procedure Main;
var
  n: Integer;
  maxHeap, minHeap: THeap_int;
  i: Integer;
  arr: TArray_int;
begin
  n := 10;
  Randomize;

  maxHeap := THeap_int.Create(n, THeapkind.Max);
  //maxHeap.SetComparer(TComparer<Integer>.Default);
  for i := 0 to n - 1 do
    maxHeap.Add(Random(1000));

  SetLength(arr, n);
  for i := 0 to n - 1 do
    arr[i] := maxHeap.ExtractFirst;

  for i := 1 to n - 1 do
    if (arr[i - 1] < arr[i]) then
      raise Exception.Create('Error');

  Writeln('Test MaxHeap completed.');

  TDSAUtils.DrawLine;

  minHeap := THeap_int.Create(n, THeapkind.Min);
  for i := 0 to n - 1 do
    minHeap.Add(Random(1000));

  SetLength(arr, n);
  for i := 0 to n - 1 do
    arr[i] := minHeap.ExtractFirst;

  for i := 1 to n - 1 do
    if (arr[i - 1] > arr[i]) then
      raise Exception.Create('Error');

  Writeln('Test MinHeap completed.');
end;

{ THeap<T> }

constructor THeap<T>.Create(capacity: Integer; heapKind: THeapkind);
begin
  __comparer := TComparer_T.Default;
  __data := TArrayList_T.Create(capacity);
  __heapKind := heapKind;
end;

procedure THeap<T>.Add(e: T);
begin
  __data.AddLast(e);
  __shiftUp(__data.GetSize - 1);
end;

constructor THeap<T>.Create(const arr: TArr_T;
  heapKind: THeapkind = THeapkind.Min);
begin
  Self.Create(arr, TComparer_T.Default, heapKind);
end;

constructor THeap<T>.Create(const arr: TArr_T; cmp: IComparer<T>;
  heapKind: THeapkind);
var
  i: Integer;
begin
  __comparer := cmp;
  __data := TArrayList_T.Create(arr);
  __heapKind := heapKind;

  for i := __parent(__data.GetSize - 1) downto 0 do
  begin
    __shiftDown(i);
  end;
end;

destructor THeap<T>.Destroy;
begin
  FreeAndNil(__comparer);
  FreeAndNil(__data);
  inherited;
end;

function THeap<T>.ExtractFirst: T;
var
  ret: T;
begin
  ret := FindFirst;
  __swap(0, __data.GetSize - 1);
  __data.RemoveLast;
  __shiftDown(0);
  Result := ret;
end;

function THeap<T>.FindFirst: T;
begin
  if __data.GetSize = 0 then
    raise Exception.Create('Can not FindFirst when heap is empty.');

  Result := __data.Get(0);
end;

function THeap<T>.IsEmpty: Boolean;
begin
  Result := __data.IsEmpty;
end;

procedure THeap<T>.SetComparer(c: IComparer<T>);
begin
  __comparer := c;
end;

function THeap<T>.Size: Integer;
begin
  Result := __data.GetSize;
end;

function THeap<T>.__leftChild(index: Integer): Integer;
begin
  Result := index * 2 + 1;
end;

function THeap<T>.__parent(index: Integer): Integer;
begin
  if index = 0 then
    raise Exception.Create('index-0 doesn''t have parent.');

  Result := (index - 1) div 2;
end;

function THeap<T>.__rightChild(index: Integer): Integer;
begin
  Result := index * 2 + 2;
end;

procedure THeap<T>.__shiftDown(k: Integer);
var
  j: Integer;
begin
  case __heapKind of
    Min:
      while __leftChild(k) < __data.GetSize do
      begin
        j := __leftChild(k);

        if ((j + 1) < __data.GetSize) and
          (__comparer.Compare(__data.Get(j + 1), __data.Get(j)) < 0) then
          j := __rightChild(k); // __data[j] 是 leftChild 和 rightChild 中的最小值

        if __comparer.Compare(__data[k], __data[j]) <= 0 then
          Break;

        __swap(k, j);
        k := j;
      end;

    Max:
      while __leftChild(k) < __data.GetSize do
      begin
        j := __leftChild(k);

        if ((j + 1) < __data.GetSize) and
          (__comparer.Compare(__data.Get(j + 1), __data.Get(j)) > 0) then
          j := __rightChild(k); // __data[j] 是 leftChild 和 rightChild 中的最大值

        if __comparer.Compare(__data[k], __data[j]) >= 0 then
          Break;

        __swap(k, j);
        k := j;
      end;
  end;
end;

procedure THeap<T>.__shiftUp(k: Integer);
begin
  case __heapKind of
    Min:
      while (k > 0) and (__comparer.Compare(__data.Get(__parent(k)),
        __data.Get(k)) > 0) do
      begin
        __swap(k, __parent(k));
        k := __parent(k);
      end;

    Max:
      while (k > 0) and (__comparer.Compare(__data.Get(__parent(k)),
        __data.Get(k)) < 0) do
      begin
        __swap(k, __parent(k));
        k := __parent(k);
      end;
  end;
end;

procedure THeap<T>.__swap(i, j: Integer);
var
  temp: T;
begin
  if (i < 0) or (i >= Size) or (j < 0) or (j >= Size) then
    raise Exception.Create('index is illegal.');

  temp := __data[i];
  __data[i] := __data[j];
  __data[j] := temp;
end;

end.
