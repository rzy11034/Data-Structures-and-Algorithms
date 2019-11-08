unit DSA.Tree.SetCompare;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Interfaces.DataStructure,
  DSA.Tree.BSTSet,
  DSA.Tree.LinkedListSet,
  DSA.Tree.AVLTreeSet,
  DSA.Utils;

procedure Main;

implementation

function testTime(newSet: specialize ISet<string>; fileName: string): string;
var
  startTime, endTime: cardinal;
  words: TArrayList_str;
  i: integer;
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
    newSet.Add(words[i]);
  end;

  Writeln('Total different words: ', newSet.GetSize);

  endTime := TThread.GetTickCount64;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TBSTSet_str = specialize TBSTSet<string, TComparer_str>;
  TLinkListSet_str = specialize TLinkedListSet<string>;
  TAVLTreeSet_str = specialize TAVLTreeSet<string, TComparer_str>;

procedure Main;
var
  vBSTSet: TBSTSet_str;
  vLinkListSet: TLinkListSet_str;
  vAVLTreeSet: TAVLTreeSet_str;
  fileName: string;
  vTime: string;
begin
  fileName := A_FILE_NAME;

  vBSTSet := TBSTSet_str.Create;
  vTime := testTime(vBSTSet, fileName);
  Writeln('BSTSet, time: ' + vTime + ' s');

  TDSAUtils.DrawLine;

  vLinkListSet := TLinkListSet_str.Create;
  vTime := testTime(vLinkListSet, fileName);
  Writeln('LinkedListSet, time: ' + vTime + ' s');

  TDSAUtils.DrawLine;

  vAVLTreeSet := TAVLTreeSet_str.Create;
  vTime := testTime(vAVLTreeSet, fileName);
  Writeln('AVLTreeSet, time: ' + vTime + ' s');
end;

end.
