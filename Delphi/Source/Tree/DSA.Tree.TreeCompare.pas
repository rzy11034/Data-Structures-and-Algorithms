unit DSA.Tree.TreeCompare;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.List_Stack_Queue.ArrayList,
  DSA.Tree.AVLTree,
  DSA.Tree.BSTMap,
  DSA.Tree.RBTree,
  DSA.Interfaces.DataStructure,
  DSA.Utils;
procedure Main;

implementation

type
  TBSTMap_str_int = TBSTMap<string, integer>;
  TAVLTree_str_int = TAVLTree<string, integer>;
  TRBTree_str_int = TRBTree<string, integer>;

  TBSTMap_int_int = TBSTMap<integer, integer>;
  TAVLTree_int_int = TAVLTree<integer, integer>;
  TRBTree_int_int = TRBTree<integer, integer>;

procedure Compare1;
var
  startTime, endTime: cardinal;
  words: TArrayList_str;
  i: integer;
  bst: TBSTMap_str_int;
  alt: TAVLTree_str_int;
  rbt: TRBTree_str_int;
  vTime: string;
begin
  words := TArrayList_str.Create;
  Writeln(A_FILE_NAME + ':');
  if TDsaUtils.ReadFile(FILE_PATH + A_FILE_NAME, words) then
  begin
    Writeln('Total words: ', words.GetSize);
  end;

  // BSTMap
  startTime := TThread.GetTickCount;
  bst := TBSTMap_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if bst.Contains(words[i]) then
      bst.Set_(words[i], bst.Get(words[i]).PValue^ + 1)
    else
      bst.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    bst.Contains(words[i]);

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('BST: ', vTime, ' s');

  // AVLTree
  startTime := TThread.GetTickCount;
  alt := TAVLTree_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if alt.Contains(words[i]) then
      alt.Set_(words[i], alt.Get(words[i]).PValue^ + 1)
    else
      alt.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    alt.Contains(words[i]);

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('AVLTree: ', vTime, ' s');

  // RBTree
  startTime := TThread.GetTickCount;
  rbt := TRBTree_str_int.Create;

  for i := 0 to words.GetSize - 1 do
  begin
    if rbt.Contains(words[i]) then
      rbt.Set_(words[i], rbt.Get(words[i]).PValue^ + 1)
    else
      rbt.Add(words[i], 1);
  end;

  for i := 0 to words.GetSize - 1 do
    rbt.Contains(words[i]);

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('RBTree: ', vTime, ' s');
end;

procedure Compare2;
var
  n, i, num: integer;
  startTime, endTime: cardinal;
  testData: TArrayList_int;
  bst: TBSTMap_int_int;
  alt: TAVLTree_int_int;
  rbt: TRBTree_int_int;
  vTime: string;
begin
  n := 20000000;
  Randomize;

  testData := TArrayList_int.Create();
  for i := 0 to n - 1 do
    testData.AddLast(Random(integer.MaxValue));

  // BSTMap
  startTime := TThread.GetTickCount;
  bst := TBSTMap_int_int.Create;

  for num in testData.ToArray do
  begin
    bst.Add(num, 0);
  end;

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('BST: ', vTime, ' s');
  FreeAndNil(bst);

  // AVLTree
  startTime := TThread.GetTickCount;
  alt := TAVLTree_int_int.Create;

  for num in testData.ToArray do
  begin
    alt.Add(num, 0);
  end;

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('AVLTree: ', vTime, ' s');
  FreeAndNil(alt);

  // RBTree
  startTime := TThread.GetTickCount;
  rbt := TRBTree_int_int.Create;

  for num in testData.ToArray do
  begin
    rbt.Add(num, 0);
  end;

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('RBTree: ', vTime, ' s');
  FreeAndNil(rbt);
end;

procedure Compare3;
var
  n, i, num: integer;
  startTime, endTime: cardinal;
  testData: TArrayList_int;
  alt: TAVLTree_int_int;
  rbt: TRBTree_int_int;
  vTime: string;
begin
  n := 20000000;

  testData := TArrayList_int.Create();
  for i := 0 to n - 1 do
    testData.AddLast(i);

  // AVLTree
  startTime := TThread.GetTickCount;
  alt := TAVLTree_int_int.Create;

  for num in testData.ToArray do
  begin
    alt.Add(num, 0);
  end;

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('AVLTree: ', vTime, ' s');
  FreeAndNil(alt);

  // RBTree
  startTime := TThread.GetTickCount;
  rbt := TRBTree_int_int.Create;

  for num in testData.ToArray do
  begin
    rbt.Add(num, 0);
  end;

  endTime := TThread.GetTickCount;
  vTime := FloatToStr((endTime - startTime) / 1000);
  Writeln('RBTree: ', vTime, ' s');
  FreeAndNil(rbt);
end;

procedure Main;
begin
  Compare1;
  TDsaUtils.DrawLine;
  Compare2;
end;

end.
