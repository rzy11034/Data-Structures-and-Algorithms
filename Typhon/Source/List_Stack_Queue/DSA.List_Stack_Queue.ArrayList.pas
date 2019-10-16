unit DSA.List_Stack_Queue.ArrayList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Rtti;

type

  { TArrayList }

  generic TArrayList<T> = class
  private
    type
    TArray_T = array of T;

  var
    __data: array of T;
    __size: integer;

    procedure __reSize(newCapacity: integer);

  public
    ///<summary> 获取数组中的元数个数 </summary>
    function GetSize: integer;
    ///<summary> 获取数组的容量 </summary>
    function GetCapacity: integer;
    ///<summary> 返回数组是否有空 </summary>
    function IsEmpty: boolean;
    ///<summary> 获取index索引位置元素 </summary>
    function Get(index: integer): T;
    ///<summary> 获取第一个元素</summary>
    function GetFirst: T;
    ///<summary> 获取最后一个元素</summary>
    function GetLast: T;
    ///<summary> 修改index索引位置元素 </summary>
    procedure Set_(index: integer; e: T);
    ///<summary> 向所有元素后添加一个新元素 </summary>
    procedure AddLast(e: T);
    ///<summary> 在第index个位置插入一个新元素e </summary>
    procedure Add(index: integer; e: T);
    ///<summary> 在所有元素前添加一个新元素 </summary>
    procedure AddFirst(e: T);
    ///<summary> 查找数组中是否有元素e </summary>
    function Contains(e: T): boolean;
    ///<summary> 查找数组中元素e忆的索引，如果不存在元素e，则返回-1 </summary>
    function Find(e: T): integer;
    ///<summary> 从数组中删除index位置的元素，返回删除的元素 </summary>
    function Remove(index: integer): T;
    ///<summary> 从数组中删除第一个元素，返回删除的元素 </summary>
    function RemoveFirst: T;
    ///<summary> 从数组中删除i最后一个元素，返回删除的元素 </summary>
    function RemoveLast: T;
    ///<summary> 从数组中删除元素e </summary>
    procedure RemoveElement(e: T);
    ///<summary> 返回一个数组 </summary>
    function ToArray: TArray_T;
    function ToString: string; override;
    property Items[i: integer]: T read Get write Set_; default;

    ///<summary> 构造函数，传入数组构造Array </summary>
    constructor Create(const arr: array of T); overload;
    ///<summary>
    ///构造函数，传入数组的容量capacity构造Array。
    ///默认数组的容量capacity:=10
    ///</summary>
    constructor Create(capacity: integer = 10); overload;
  end;

procedure Main;

implementation

uses
  DSA.Utils;

type

  TStudent = class
  private
    Fname: string;
    FScore: integer;
  public
    constructor Create(Name: string; score: integer);
    function ToString: string; override;
  end;

  T_int = TArrayList_str;
  T_Student = specialize TArrayList<TStudent>;

procedure Main;
var
  a_int: T_int;
  a_student: T_Student;
  i: integer;
  ss: TStudent;
begin
  a_int := T_int.Create;

  for i := 0 to 9 do
    a_int.AddLast(i.ToString);
  Writeln(a_int.ToString);

  a_int.Add(1, '100');
  Writeln(a_int.ToString);

  a_int.AddFirst('-1');
  Writeln(a_int.ToString);

  a_int.Remove(2);
  Writeln(a_int.ToString);

  a_int.RemoveElement('4');
  Writeln(a_int.ToString);

  a_int.RemoveFirst;
  Writeln(a_int.ToString);

  Writeln(a_int.Contains('2'));
  TDsaUtils.DrawLine;
  a_student := T_Student.Create();
  a_student.AddLast(TStudent.Create('Alice', 100));
  a_student.AddLast(TStudent.Create('Bob', 60));
  a_student.AddLast(TStudent.Create('CharLie', 88));

  ss := a_student[0];
  Writeln(a_student.Contains(ss));
  Writeln(a_student.ToString);

end;

{ TArrayList }

procedure TArrayList.Add(index: integer; e: T);
var
  i: integer;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create(
      'Add failed. Require index >= 0 and index <= Size.');

  if (__size = Length(__data)) then
    __reSize(2 * Length(Self.__data));

  for i := __size - 1 downto index do
    __data[i + 1] := __data[i];

  __data[index] := e;
  Inc(__size);
end;

procedure TArrayList.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TArrayList.AddLast(e: T);
begin
  Add(__size, e);
end;

function TArrayList.Contains(e: T): boolean;
var
  i: integer;
begin
  for i := 0 to __size - 1 do
  begin
    if __data[i] = e then
      Exit(True);
  end;

  Result := False;
end;

constructor TArrayList.Create(capacity: integer);
begin
  SetLength(__data, capacity);
end;

constructor TArrayList.Create(const arr: array of T);
var
  i: integer;
begin
  SetLength(Self.__data, Length(arr));

  for i := 0 to Length(arr) - 1 do
    __data[i] := arr[i];

  __size := Length(arr);

end;

function TArrayList.Find(e: T): integer;
var
  i: integer;
begin
  for i := 0 to __size - 1 do
  begin
    if __data[i] = e then
      Exit(i);
  end;

  Result := -1;
end;

function TArrayList.Get(index: integer): T;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Get failed. Index is illegal.');

  Result := __data[index];
end;

function TArrayList.GetCapacity: integer;
begin
  Result := Length(Self.__data);
end;

function TArrayList.GetFirst: T;
begin
  Result := Get(0);
end;

function TArrayList.GetLast: T;
begin
  Result := Get(__size - 1);
end;

function TArrayList.GetSize: integer;
begin
  Result := Self.__size;
end;

function TArrayList.IsEmpty: boolean;
begin
  Result := Self.__size = 0;
end;

function TArrayList.Remove(index: integer): T;
var
  i: integer;
  res: T;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Remove failed. Index is illegal.');

  res := __data[index];

  for i := index + 1 to __size - 1 do
    __data[i - 1] := __data[i];

  Dec(Self.__size);

  if (__size = Length(Self.__data) div 4) and (Length(Self.__data) div 2 <> 0) then
    __reSize(Length(Self.__data) div 2);

  Result := res;
end;

procedure TArrayList.RemoveElement(e: T);
var
  index, i: integer;
begin
  for i := 0 to __size - 1 do
  begin
    index := Find(e);

    if index <> -1 then
      Remove(index);
  end;
end;

function TArrayList.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TArrayList.RemoveLast: T;
begin
  Result := Remove(__size - 1);
end;

procedure TArrayList.Set_(index: integer; e: T);
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  __data[index] := e;
end;

function TArrayList.ToArray: TArray_T;
var
  i: integer;
  arr: TArray_T;
begin
  SetLength(arr, __size);

  for i := 0 to __size - 1 do
    arr[i] := __data[i];

  Result := arr;
end;

function TArrayList.ToString: string;
var
  res: TAnsiStringBuilder;
  i: integer;
  Value: TValue;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Array: Size = %d, capacity = %d',
      [Self.__size, Length(Self.__data)]);
    res.AppendLine;
    res.Append('  [');

    for i := 0 to __size - 1 do
    begin
      TValue.Make(@__data[i], TypeInfo(T), Value);

      if not (Value.IsObject) then
        res.Append(Value.ToString)
      else
        res.Append(Value.AsObject.ToString);

      if i <> __size - 1 then
        res.Append(', ');
    end;

    res.Append(']');
    Result := res.ToString;

  finally
    res.Free;
  end;
end;

procedure TArrayList.__reSize(newCapacity: integer);
begin
  SetLength(Self.__data, newCapacity);
end;

{ TStudent }

constructor TStudent.Create(Name: string; score: integer);
begin
  Fname := Name;
  FScore := score;
end;

function TStudent.ToString: string;
begin
  Result := Format('Student(name:%s, score:%d)', [Fname, FScore]);
end;

end.
