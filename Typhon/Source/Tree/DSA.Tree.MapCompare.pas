unit DSA.Tree.MapCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.BSTMap,
  DSA.Tree.LinkedListMap,
  DSA.Tree.AVLTreeMap,
  DSA.Utils;

procedure Main;

implementation

function testTime(map: specialize IMap<string, integer>; fileName: string): string;
var
  startTime, endTime: cardinal;
  words: TArrayList_str;
  i, n: integer;
begin
  startTime := TThread.GetTickCount64;

  words := TArrayList_str.Create;

  Writeln(fileName + ':');

  if TDsaUtils.ReadFile(FILE_PATH + fileName, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  for i := 0 to words.GetSize - 1 do
  begin
    if map.Contains(words[i]) then
      map.Set_(words[i], map.Get(words[i]).PValue^ + 1)
    else
      map.add(words[i], 1);
  end;

  Writeln('Total different words: ', map.GetSize);
  n := map.Get('pride').PValue^;
  Writeln('Frequency of pride: ', n);

  n := map.Get('prejudice').PValue^;
  Writeln('Frequency of prejudice: ', n);

  endTime := TThread.GetTickCount64;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TBSTMap_str_int = specialize TBSTMap<string, integer, TComparer_str>;
  TLinkListMap_str_int = specialize TLinkedListMap<string,
    integer, TComparer_str>;
  TAVLTreeMap_str_int = specialize TAVLTreeMap<string, integer, TComparer_str>;

procedure Main;
var
  fileName, vTime: string;
  vBSTMap: TBSTMap_str_int;
  vLinkListMap: TLinkListMap_str_int;
  vAVLTreeMap: TAVLTreeMap_str_int;
begin
  fileName := A_FILE_NAME;

  vBSTMap := TBSTMap_str_int.Create;
  vTime := testTime(vBSTMap, fileName);
  Writeln('BSTMap, time: ' + vTime + ' s');

  TDSAUtils.DrawLine;

  vLinkListMap := TLinkListMap_str_int.Create;
  vTime := testTime(vLinkListMap, fileName);
  Writeln('LinkedListMap, time: ' + vTime + ' s');

  TDSAUtils.DrawLine;

  vAVLTreeMap := TAVLTreeMap_str_int.Create;
  vTime := testTime(vAVLTreeMap, fileName);
  Writeln('AVLTreeMap, time: ' + vTime + ' s');
end;

end.
