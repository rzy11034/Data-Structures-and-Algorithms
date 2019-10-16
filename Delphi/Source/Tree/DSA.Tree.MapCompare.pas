unit DSA.Tree.MapCompare;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.Interfaces.DataStructure,
  DSA.Tree.BSTMap,
  DSA.Tree.LinkedListMap,
  DSA.Tree.AVLTreeMap,
  DSA.Utils;

procedure Main;

implementation

function testTime(map: IMap<string, integer>; fileName: string): string;
var
  startTime, endTime: cardinal;
  words: TArrayList_str;
  i: integer;
begin
  startTime := TThread.GetTickCount;

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
  Writeln('Frequency of pride: ', map.Get('pride').PValue^);
  Writeln('Frequency of prejudice: ', map.Get('prejudice').PValue^);

  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TBSTMap_str_int = TBSTMap<string, integer>;
  TLinkListMap_str_int = TLinkedListMap<string, integer>;
  TAVLTreeMap_str_int = TAVLTreeMap<string, integer>;

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

  TDsaUtils.DrawLine;

  vLinkListMap := TLinkListMap_str_int.Create;
  vTime := testTime(vLinkListMap, fileName);
  Writeln('LinkedListMap, time: ' + vTime + ' s');

  TDsaUtils.DrawLine;

  vAVLTreeMap := TAVLTreeMap_str_int.Create;
  vTime := testTime(vAVLTreeMap, fileName);
  Writeln('AVLTreeMap, time: ' + vTime + ' s');
end;

end.
