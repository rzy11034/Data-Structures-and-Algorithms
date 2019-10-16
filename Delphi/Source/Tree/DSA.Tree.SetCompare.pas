unit DSA.Tree.SetCompare;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.Interfaces.DataStructure,
  DSA.Tree.BSTSet,
  DSA.Tree.LinkedListSet,
  DSA.Utils,
  DSA.Tree.AVLTreeSet;

procedure Main;

implementation

function testTime(newSet: ISet<string>; fileName: string): string;
var
  startTime, endTime: Cardinal;
  words: TArrayList_str;
  i: Integer;
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
    newSet.Add(words[i]);
  end;

  Writeln('Total different words: ', newSet.GetSize);

  endTime := TThread.GetTickCount;

  Result := FloatToStr((endTime - startTime) / 1000);
end;

type
  TBSTSet_str = TBSTSet<string>;
  TLinkListSet_str = TLinkedListSet<string>;
  TAVLTreeSet_str = TAVLTreeSet<string>;

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

  TDsaUtils.DrawLine;

  vLinkListSet := TLinkListSet_str.Create;
  vTime := testTime(vLinkListSet, fileName);
  Writeln('LinkedListSet, time: ' + vTime + ' s');

  TDsaUtils.DrawLine;

  vAVLTreeSet := TAVLTreeSet_str.Create;
  vTime := testTime(vAVLTreeSet, fileName);
  Writeln('AVLTreeSet, time: ' + vTime + ' s');
end;

end.
