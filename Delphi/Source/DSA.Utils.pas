unit DSA.Utils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  DSA.List_Stack_Queue.ArrayList,
  DSA.List_Stack_Queue.LinkedListStack,
  DSA.List_Stack_Queue.ArrayListStack,
  DSA.Interfaces.DataStructure,
  DSA.Interfaces.Comparer;

type
  TArray_int = TArray<Integer>;
  TArray_str = TArray<string>;
  TArray_chr = TArray<Char>;
  TArray_dbl = TArray<Double>;
  TArray_bool = TArray<Boolean>;

  TArrayList_str = TArrayList<string>;
  TArrayList_int = TArrayList<Integer>;
  TArrayList_chr = TArrayList<Char>;
  TArrayList_dbl = TArrayList<Double>;

  TStack_int = TArrayListStack<Integer>;
  TQueue_int = TLinkedListStack<Integer>;

  TDSAUtils = class
  public
    class procedure DrawLine;
    class function ReadFile(fileName: string; words: TArrayList_str): Boolean;
    class procedure ReadGraph(const Graph: IGraph; const fileName: string);
  end;

  TReadGraphWeight<TWeight> = class
  private type
    TWeightGraph_TWight = TWeightGraph<TWeight>;

  var
    class function __getStringArray(s: string): TArrayList_str;
  public
    class procedure Execute(Graph: TWeightGraph_TWight; const fileName: string);
  end;

  TSortTestHelper<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;
    TCmp_T = TComparer<T>;
    TSorts = procedure(var arr: TArr_T; Comparer: ICmp_T) of object;

  var
    function __isSorted(arr: TArr_T; cmp: ICmp_T): Boolean;
    procedure __swap(var a, b: T);

  public
    procedure PrintArray(arr: TArr_T);
    /// <summary> 生成有n个元素的随机数组,每个元素的随机范围为[1, range] </summary>
    function GenerateRandomArray(arrSize, range: Integer): TArray_int;
    function GenerateNearlyOrderedArray(arrSize, swapTime: Integer): TArray_int;
    procedure TestSort(sortName: string; var arr: TArr_T; pSort: TSorts); overload;
    procedure TestSort(sortName: string; var arr: TArr_T; pSort: TSorts; cmp: ICmp_T); overload;
    function CopyArray(const arr: TArr_T): TArr_T;
  end;

  TSortTestHelper_int = TSortTestHelper<Integer>;

  TDelphiSort<T> = class
  private type
    TArr_T = TArray<T>;
    ICmp_T = IComparer<T>;
  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
  end;

const
  END_OF_PROGRAM_EN: string = 'Press any key to continue...';
  END_OF_PROGRAM_CH: string = '按任意键继续...';
  FILE_PATH: string = '..\..\..\Resources\';
  A_FILE_NAME: string = 'Pride and Prejudice.txt';
  B_FILE_NAME: string = 'A Tale of Two Cities.txt';
  GRAPH_FILE_NAME_1: string = 'G1.txt';
  GRAPH_FILE_NAME_2: string = 'G2.txt';
  WEIGHT_GRAPH_FILE_NAME_1: string = 'WeightGraph1.txt';
  WEIGHT_GRAPH_FILE_NAME_2: string = 'WeightGraph2.txt';
  WEIGHT_GRAPH_FILE_NAME_3: string = 'WeightGraph3.txt';
  WEIGHT_GRAPH_FILE_NAME_4: string = 'WeightGraph4.txt';
  WEIGHT_GRAPH_FILE_NAME_5: string = 'WeightGraph5.txt';
  WEIGHT_GRAPH_FILE_NAME_6: string = 'WeightGraph6.txt';
  WEIGHTGRAPH_NEGATIVE_CIRCLE_FILE_NAME: string = 'WeightGraph_negative_circle.txt';

implementation

{ TDSAUtils }

class procedure TDSAUtils.DrawLine;
var
  i: Integer;
begin
  // Writeln;
  for i := 0 to 70 do
  begin
    write('-');
  end;
  Writeln;
end;

class function TDSAUtils.ReadFile(fileName: string; words: TArrayList_str): Boolean;
const
  Letter = ['a' .. 'z', 'A' .. 'Z'];
var
  strlist: TStrings;
  s_line: string;
  sb: TStringBuilder;
  c: Char;
begin
  strlist := TStringList.Create;
  try
    try
      strlist.LoadFromFile(fileName);

      for s_line in strlist do
      begin
        sb := TStringBuilder.Create;
        for c in s_line do
        begin
          if CharInSet(c, Letter) then
          begin
            sb.Append(c);
          end
          else
          begin
            if sb.ToString <> '' then
            begin
              words.AddLast(sb.ToString);
              sb.Clear;
            end;
          end;
        end;
      end;

      Result := True;
    except
      Result := False
    end;
  finally
    FreeAndNil(strlist);
  end;
end;

class procedure TDSAUtils.ReadGraph(const Graph: IGraph; const fileName: string);
var
  strlist: TStringList;
  line, s: string;
  v, e, i, a, b: Integer;
begin
  strlist := TStringList.Create;
  try
    strlist.LoadFromFile(fileName);

    line := strlist[0];
    s := Copy(line, 1, Pos(#9, line) - 1);
    v := s.ToInteger;

    s := Copy(line, Pos(#9, line) + 1, line.Length);
    e := s.ToInteger;

    Assert(v = Graph.vertex);
    for i := 1 to e do
    begin
      line := strlist[i];

      s := Copy(line, 1, Pos(#9, line) - 1);
      a := s.ToInteger;

      s := Copy(line, Pos(#9, line) + 1, line.Length);
      b := s.ToInteger;

      Assert((a >= 0) and (a < v));
      Assert((b >= 0) and (b < v));

      Graph.AddEdge(a, b);
    end;

  finally
    FreeAndNil(strlist);
  end;
end;

{ TSortTestHelper<T> }

function TSortTestHelper<T>.CopyArray(const arr: TArr_T): TArr_T;
begin
  Result := Copy(arr);
end;

function TSortTestHelper<T>.GenerateNearlyOrderedArray(arrSize, swapTime: Integer): TArray_int;
var
  arr1: TArray_int;
  i, tmp, posX, posY: Integer;
begin
  SetLength(arr1, arrSize);
  for i := 0 to arrSize - 1 do
    arr1[i] := i;

  Randomize;
  for i := 0 to swapTime - 1 do
  begin
    posX := Random(arrSize) mod arrSize;
    posY := Random(arrSize) mod arrSize;

    __swap(arr1[posX], arr1[posY]);
  end;

  Result := arr1;
end;

function TSortTestHelper<T>.GenerateRandomArray(arrSize, range: Integer): TArray_int;
var
  arr: TArray_int;
  i: Integer;
begin
  SetLength(arr, arrSize);
  Randomize;

  for i := 0 to arrSize - 1 do
    arr[i] := Random(range);

  Result := arr;
end;

procedure TSortTestHelper<T>.PrintArray(arr: TArr_T);
var
  i: Integer;
begin
  for i := 0 to Length(arr) - 1 do
    write(TValue.From<T>(arr[i]).ToString, ' ');
  Writeln;
end;

procedure TSortTestHelper<T>.TestSort(sortName: string; var arr: TArr_T; pSort: TSorts);
begin
  Self.TestSort(sortName, arr, pSort, TCmp_T.Default);
end;

procedure TSortTestHelper<T>.TestSort(sortName: string; var arr: TArr_T; pSort: TSorts; cmp: ICmp_T);
var
  i: Integer;
  startTime, endTime: Cardinal;
begin
  startTime := TThread.GetTickCount;
  pSort(arr, cmp);
  endTime := TThread.GetTickCount;

  // --------------
  if not(__isSorted(arr, cmp)) then
    raise Exception.Create('Sort Error.');

  write(sortName, ' Size is: ', Length(arr).ToString, '. ');
  Writeln('Total Time : ', ((endTime - startTime) / 1000).ToString, ' s');
end;

function TSortTestHelper<T>.__isSorted(arr: TArr_T; cmp: ICmp_T): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    if cmp.Compare(arr[i - 1], arr[i]) > 0 then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TSortTestHelper<T>.__swap(var a, b: T);
var
  tmp: T;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

{ TDelphiSort<T> }

class procedure TDelphiSort<T>.Sort(var arr: TArray<T>; cmp: IComparer<T>);
begin
  TArray.Sort<T>(arr);
end;

{ TReadGraphWeight<TWeight> }

class procedure TReadGraphWeight<TWeight>.Execute(Graph: TWeightGraph_TWight; const fileName: string);
var
  v1, v2, i, a, b: Integer;
  w: TWeight;
  strlist: TStringList;
  line: string;
  ss: TArray_str;
  Value, value_w: TValue;
begin
  strlist := TStringList.Create;
  try
    strlist.LoadFromFile(fileName);
    line := strlist[0];
    ss := __getStringArray(line).ToArray;
    v1 := ss[0].ToInteger;
    v2 := ss[1].ToInteger;

    Assert(v1 = Graph.vertex);
    for i := 1 to v2 do
    begin
      line := strlist[i];
      ss := __getStringArray(line).ToArray;
      a := ss[0].ToInteger;
      b := ss[1].ToInteger;

      TValue.Make(@w, TypeInfo(TWeight), value_w);
      case value_w.Kind of
        tkFloat:
          begin
            Value := ss[2].ToDouble;
          end;
        tkInteger:
          begin
            Value := ss[2].ToInteger;
          end;
      end;

      Value.ExtractRawData(@w);
      Assert((a >= 0) and (a < v1));
      Assert((b >= 0) and (b < v1));

      Graph.AddEdge(a, b, w);
    end;
  finally
    FreeAndNil(strlist);
  end;
end;

class function TReadGraphWeight<TWeight>.__getStringArray(s: string): TArrayList_str;
const
  Chars = ['0' .. '9', '.', '+', '-'];
var
  ss: TArrayList_str;
  i: Integer;
  sb: TStringBuilder;
begin
  ss := TArrayList_str.Create(1);

  sb := TStringBuilder.Create;
  try
    for i := low(s) to high(s) do
    begin
      if CharInSet(s[i], Chars) then
      begin
        sb.Append(s[i]);
      end
      else
      begin
        ss.AddLast(sb.ToString);
        sb.Clear;
      end;

      if i >= high(s) then
        ss.AddLast(sb.ToString);
    end;

    Result := ss;
  finally
    FreeAndNil(sb);
  end;
end;

end.
