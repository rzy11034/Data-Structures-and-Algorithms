unit DSA.Hash.HashCode;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  { TStudent }

  TStudent = class(TObject)
  private
    __grade: integer;
    __cls: integer;
    __firstName: string;
    __lastName: string;
  public
    constructor Create(grade, cls: integer; firstName, lastName: string);
    function GetHashCode: PtrInt; override;
    function Equals(Obj: TObject): boolean; override;
  end;

procedure Main;

implementation

procedure Main;
var
  a: integer;
  c: double;
  d: string;
  student1, student2: TStudent;
begin
  a := 42;
  Writeln((a.ToString.GetHashCode and $7fffffff));
  Writeln((a.ToString.GetHashCode));

  a := -42;
  Writeln(a.ToString.GetHashCode);

  c := 3.1415926;
  Writeln(c.ToString.GetHashCode);

  d := 'imooc';
  Writeln(d.GetHashCode);

  student1 := TStudent.Create(3, 2, '振勇', '任');
  Writeln(student1.GetHashCode);

  student2 := TStudent.Create(3, 2, '振勇', '任');
  Writeln(student2.GetHashCode);

  Writeln(student1.Equals(student2));
end;

{ TStudent }

constructor TStudent.Create(grade, cls: integer; firstName, lastName: string);
begin
  __grade := grade;
  __cls := cls;
  __firstName := firstName;
  __lastName := lastName;
end;

function TStudent.Equals(Obj: TObject): boolean;
var
  another: TStudent;
begin
  if Self = Obj then
    Exit(True);

  if Obj = nil then
    Exit(False);

  if ClassType <> Obj.ClassType then
    Exit(False);

  another := Obj as TStudent;

  Result := (__grade = another.__grade) and (__cls = another.__cls) and
    (__firstName.ToLower.Equals(another.__firstName.ToLower)) and
    (__lastName.ToLower.Equals(another.__lastName.ToLower));
end;

function TStudent.GetHashCode: PtrInt;
var
  B, vHash: integer;
begin
  B := 31;
  vHash := 0;

  vHash := vHash * B + __grade;
  vHash := vHash * B + __cls;
  vHash := vHash * B + __firstName.ToLower.GetHashCode;
  vHash := vHash * B + __lastName.ToLower.GetHashCode;

  Result := vHash;
end;

end.
