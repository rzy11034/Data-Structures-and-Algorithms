unit DSA.Tree.Trie;

interface

uses
  System.SysUtils,
  System.Classes,
  DSA.Tree.BSTMap,
  DSA.Utils,
  DSA.Tree.BSTSet;

type
  TTrie = class
  private type
    TNode = class
    private type
      TreeMap = TBSTMap<char, TNode>;

    public
      IsWord: boolean;
      Next: TreeMap;

      constructor Create(newIsWord: boolean = False);
    end;

  var
    __root: TNode;
    __size: integer;

  public
    constructor Create;
    /// <summary> 获取Trie中存储的单词数量 </summary>
    function GetSize: integer;
    /// <summary> 向Trie中添加一个新的单词word </summary>
    procedure Add(words: string);
    /// <summary> 查询单词words是否在Trie中 </summary>
    function Contains(words: string): boolean;
    /// <summary> 查询是否在Trie中有单词以prefix为前缀 </summary>
    function IsPerFix(prefix: string): boolean;
  end;

procedure Main;

implementation

type
  TBSTSet_str = TBSTSet<string>;

procedure Main;
var
  words: TArrayList_str;
  set_: TBSTSet_str;
  startTime, endTime, time: Double;
  i: integer;
  tr: TTrie;
begin
  words := TArrayList_str.Create();
  if TDsaUtils.ReadFile(FILE_PATH + A_FILE_NAME, words) then
  begin
    Writeln(words.GetSize);;

    startTime := TThread.GetTickCount;
    set_ := TBSTSet_str.Create;
    for i := 0 to words.GetSize - 1 do
    begin
      set_.Add(words[i]);
    end;

    for i := 0 to words.GetSize - 1 do
    begin
      set_.Contains(words[i]);
    end;

    endTime := TThread.GetTickCount;
    time := (endTime - startTime) / 1000;

    Writeln('Total different words: ', set_.GetSize);
    Writeln('TBSTSet: ', time.ToString + ' s');

    // ------------
    startTime := TThread.GetTickCount;
    tr := TTrie.Create;
    for i := 0 to words.GetSize - 1 do
    begin
      tr.Add(words[i]);
    end;

    for i := 0 to words.GetSize - 1 do
    begin
      tr.Contains(words[i]);
    end;

    endTime := TThread.GetTickCount;
    time := (endTime - startTime) / 1000;

    Writeln('Total different words: ', tr.GetSize);
    Writeln('Trie: ', time.ToString, ' s');
  end;
end;

{ TTrie.TNode }

constructor TTrie.TNode.Create(newIsWord: boolean);
begin
  IsWord := newIsWord;
  Next := TreeMap.Create;
end;

{ TTrie }

procedure TTrie.Add(words: string);
var
  cur: TNode;
  i: integer;
  c: char;
begin
  cur := __root;

  for i := low(words) to high(words) do
  begin
    c := words[i];

    if cur.Next.Contains(c) = False then
      cur.Next.Add(c, TNode.Create());

    cur := cur.Next.Get(c).PValue^;
  end;

  if cur.IsWord = False then
  begin
    cur.IsWord := True;
    Inc(__size);
  end;
end;

function TTrie.Contains(words: string): boolean;
var
  cur: TNode;
  i: integer;
  c: char;
begin
  cur := __root;

  for i := 0 to words.Length - 1 do
  begin
    c := words.Chars[i];

    if cur.Next.Contains(c) = False then
      Exit(False);

    cur := cur.Next.Get(c).PValue^;
  end;

  Result := cur.IsWord;
end;

constructor TTrie.Create;
begin
  __root := TNode.Create();
  __size := 0;
end;

function TTrie.GetSize: integer;
begin
  Result := __size;
end;

function TTrie.IsPerFix(prefix: string): boolean;
var
  cur: TNode;
  i: integer;
  c: char;
begin
  cur := __root;

  for i := 0 to prefix.Length - 1 do
  begin
    c := prefix.Chars[i];

    if cur.Next.Get(c).PValue^ = nil then
    begin
      Exit(False);
    end;

    cur := cur.Next.Get(c).PValue^;
  end;

  Result := True;
end;

end.
