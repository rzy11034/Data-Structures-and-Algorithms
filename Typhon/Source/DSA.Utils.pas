unit DSA.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  DSA.Interfaces.Comparer,
  DSA.Interfaces.DataStructure,
  DSA.List_Stack_Queue.ArrayList,
  DSA.List_Stack_Queue.ArrayListStack,
  DSA.List_Stack_Queue.LinkedListQueue;

type
  TArray_int = specialize TArray<integer>;
  TArray_str = specialize TArray<string>;
  TArray_chr = specialize TArray<char>;
  TArray_dbl = specialize TArray<double>;
  TArray_bool = specialize TArray<boolean>;

  TArrayList_str = specialize TArrayList<string>;
  TArrayList_int = specialize TArrayList<integer>;
  TArrayList_chr = specialize TArrayList<char>;
  TArrayList_dbl = specialize TArrayList<double>;

  TStack_int = specialize TArrayListStack<integer>;
  TQueue_int = specialize TLinkedListQueue<integer>;

  TComparer_str = specialize TComparer<string>;
  TComparer_int = specialize TComparer<integer>;
  TComparer_chr = specialize TComparer<char>;
  TComparer_dbl = specialize TComparer<double>;


  { TDSAUtils }

  TDSAUtils = class
  public
    class procedure DrawLine;
    class function ReadFile(fileName: string; words: TArrayList_str): boolean;
    class procedure ReadGraph(const Graph: IGraph; const fileName: string);
  end;

  generic TReadGraphWeight<TWeight> = class
  private
    type
    TWeightGraph_TWight = specialize TWeightGraph<TWeight>;

  var
    class function __getStringArray(s: string): TArrayList_str;
  public
    class procedure Execute(Graph: TWeightGraph_TWight; const fileName: string);
  end;

  { TSortTestHelper }

  generic TSortTestHelper<T, TCmp> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;
    TSorts =
    procedure(var arr: TArr_T; cmp: ICmp_T) of object;

  var
    function __isSorted(arr: TArr_T; cmp: ICmp_T): boolean;
    procedure __swap(var a, b: T);

  public
    procedure PrintArray(arr: TArr_T);
    /// <summary> 生成有n个元素的随机数组,每个元素的随机范围为[1, range]。 </summary>
    function GenerateRandomArray(arrSize, range: integer): TArray_int;
    /// <summary> 生成有n个元素的有序数组, 并执行swapTime乱序次数。 </summary>
    function GenerateNearlyOrderedArray(arrSize, swapTime: integer): TArray_int;
    procedure TestSort(sortName: string; var arr: TArr_T; pSort: TSorts); overload;
    procedure TestSort(sortName: string; var arr: TArr_T; pSort: TSorts; cmp: ICmp_T); overload;
    function CopyArray(const arr: TArr_T): TArr_T;
  end;

  TSortTestHelper_int = specialize TSortTestHelper<integer, TComparer_int>;

  { TDelphiSort }

  generic TDelphiSort<T> = class
  private
    type
    TArr_T = specialize TArray<T>;
    ICmp_T = specialize IDSA_Comparer<T>;
    TArrayHelper_T = specialize TArrayHelper<T>;
  public
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
  end;

const
  END_OF_PROGRAM_EN: string = 'Press any key to continue...';
  END_OF_PROGRAM_CH: string = '按任意键继续...';
  FILE_PATH: string = '..\..\..\Resources\';
  A_FILE_NAME: string = 'Pride and Prejudice.txt';
  B_FILE_NAME: string = 'A Tale of Two Cities.txt';
  GRAPH_FILE_NAME_1: string = 'GRAPH1.txt';
  GRAPH_FILE_NAME_2: string = 'GRAPH2.txt';
  GRAPH_FILE_NAME_3: string = 'GRAPH3.txt';
  WEIGHT_GRAPH_FILE_NAME_1: string = 'WeightGraph1.txt';
  WEIGHT_GRAPH_FILE_NAME_2: string = 'WeightGraph2.txt';
  WEIGHT_GRAPH_FILE_NAME_3: string = 'WeightGraph3.txt';
  WEIGHT_GRAPH_FILE_NAME_4: string = 'WeightGraph4.txt';
  WEIGHT_GRAPH_FILE_NAME_5: string = 'WeightGraph5.txt';
  WEIGHT_GRAPH_FILE_NAME_6: string = 'WeightGraph6.txt';
  WEIGHTGRAPH_NEGATIVE_CIRCLE_FILE_NAME: string = 'WeightGraph_negative_circle.txt';

implementation

{ TReadGraphWeight }

class procedure TReadGraphWeight.Execute(Graph: TWeightGraph_TWight; const fileName: string);
var
  v1, v2, i, a, b: integer;
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
          //w := Value.AsExtended
        end;
        tkInteger:
        begin
          Value := ss[2].ToInteger;
          //w := Value.AsInteger;
        end;
        else

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

class function TReadGraphWeight.__getStringArray(s: string): TArrayList_str;
const
  Chars = ['0' .. '9', '.', '+', '-'];
var
  ss: TArrayList_str;
  i: integer;
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

{ TDelphiSort }

class procedure TDelphiSort.Sort(var arr: TArr_T; cmp: ICmp_T);
begin
  TArrayHelper_T.Sort(arr);
end;

{ TSortTestHelper }

function TSortTestHelper.CopyArray(const arr: TArr_T): TArr_T;
begin
  Result := Copy(arr);
end;

function TSortTestHelper.GenerateNearlyOrderedArray(arrSize: integer; swapTime: integer): TArray_int;
var
  arr1: TArray_int;
  i, tmp, posX, posY: integer;
begin
  SetLength(arr1, arrSize);
  for i := 0 to arrSize - 1 do
    arr1[i] := i;

  Randomize;
  for i := 0 to swapTime - 1 do
  begin
    posX := Random(arrSize) mod arrSize;
    posY := Random(arrSize) mod arrSize;

    tmp := arr1[posX];
    arr1[posX] := arr1[posY];
    arr1[posY] := tmp;
  end;

  Result := arr1;
end;

function TSortTestHelper.GenerateRandomArray(arrSize, range: integer): TArray_int;
var
  arr: TArray_int;
  i: integer;
begin
  SetLength(arr, arrSize);
  Randomize;

  for i := 0 to arrSize - 1 do
    arr[i] := Random(range);

  Result := arr;
end;

procedure TSortTestHelper.PrintArray(arr: TArr_T);
var
  i: integer;
  Value: TValue;
  s: string;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    TValue.Make(@arr[i], TypeInfo(T), Value);
    s := Value.ToString;
    Write(s, ' ');
  end;

  Writeln;
end;

procedure TSortTestHelper.TestSort(sortName: string; var arr: TArr_T; pSort: TSorts);
begin
  Self.TestSort(sortName, arr, pSort, TCmp.Default);
end;

procedure TSortTestHelper.TestSort(sortName: string; var arr: TArr_T; pSort: TSorts; cmp: ICmp_T);
var
  startTime, endTime: cardinal;
begin
  startTime := TThread.GetTickCount64;
  pSort(arr, cmp);
  endTime := TThread.GetTickCount64;

  // --------------
  if not (__isSorted(arr, cmp)) then
    raise Exception.Create('Sort Error.');

  Write(sortName, ' Size is: ', Length(arr).ToString, '. ');
  Writeln('Total Time : ', ((endTime - startTime) / 1000).ToString, ' s');
end;

function TSortTestHelper.__isSorted(arr: TArr_T; cmp: ICmp_T): boolean;
var
  i: integer;
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

procedure TSortTestHelper.__swap(var a, b: T);
var
  tmp: T;
begin
  tmp := a;
  a := b;
  b := tmp;

end;

{ TDSAUtils }

class procedure TDSAUtils.DrawLine;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('-');
  end;
  Writeln;
end;

class function TDSAUtils.ReadFile(fileName: string; words: TArrayList_str): boolean;
const
  Letter = ['a' .. 'z', 'A' .. 'Z'];
var
  strlist: TStrings;
  s_line: string;
  sb: TStringBuilder;
  c: char;
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
  v, e, i, a, b: integer;
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

end.
