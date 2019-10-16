//  211. 添加与搜索单词 - 数据结构设计
//
//  设计一个支持以下两种操作的数据结构：
//
//  void addWord(word)
//  bool search(word)
//  search(word) 可以搜索文字或正则表达式字符串，字符串只包含字母 . 或 a-z 。 . 可以表示任何一个字母。
//
//  示例:
//
//  addWord("bad")
//  addWord("dad")
//  addWord("mad")
//  search("pad") -> false
//  search("bad") -> true
//  search(".ad") -> true
//  search("b..") -> true
//  说明:
//
//  你可以假设所有单词都是由小写字母 a-z 组成的。
//
//  class WordDictionary
//  {
//
//
//    /** Initialize your data structure here. */
//    public WordDictionary() {
//
//    }
//
//    /** Adds a word into the data structure. */
//    public void addWord(String word) {
//
//    }
//
//    /** Returns if the word is in the data structure. A word could contain the dot character '.' to represent any one letter. */
//    public boolean search(String word) {
//
//    }
//  }
//
//  /**
//   * Your WordDictionary object will be instantiated and called as such:
//   * WordDictionary obj = new WordDictionary();
//   * obj.addWord(word);
//   * boolean param_2 = obj.search(word);
//   */

unit DSA.LeetCode._211;

interface

uses
  System.SysUtils,
  DSA.Tree.BSTMap;

type
  TWordDictionary = class
  private type
    TNode = class
    private type
      TreeMap = TBSTMap<Char, TNode>;
    public
      IsWord: Boolean;
      Next: TreeMap;
      constructor Create(newIsWord: Boolean = False);
    end;

  var
    __root: TNode;

    function __match(node: TNode; word: string; index: Integer): Boolean;

  public
    /// <summary> Initialize your data structure here. </summary>
    constructor Create;
    /// <summary> Adds a word into the data structure. </summary>
    procedure AddWord(word: string);
    /// <summary> Returns if the word is in the data structure.
    /// A word could contain the dot character '.' to represent any one letter.
    /// </summary>
    function Search(word: string): Boolean;
  end;

procedure Main();

implementation

procedure Main();
var
  wd: TWordDictionary;
  b: Boolean;
begin
  wd := TWordDictionary.Create;

  with wd do
  begin
    AddWord('bad');
    AddWord('dad');
    AddWord('mad');

    b := Search('pad'); // - > false
    Writeln(b);

    b := Search('bad'); // - > true
    Writeln(b);

    b := Search('.ad'); // - > true
    Writeln(b);

    b := Search('b..'); // - > true
    Writeln(b);
  end;
end;

{ TWordDictionary.TNode }

constructor TWordDictionary.TNode.Create(newIsWord: Boolean);
begin
  IsWord := newIsWord;
  Next := TreeMap.Create;
end;

{ TWordDictionary }

procedure TWordDictionary.AddWord(word: string);
var
  cur: TNode;
  i: Integer;
  c: Char;
begin
  cur := __root;

  for i := low(word) to high(word) do
  begin
    c := word[i];

    if cur.Next.Contains(c) = False then
      cur.Next.Add(c, TNode.Create());

    cur := cur.Next.Get(c).PValue^;
  end;

  if cur.IsWord = False then
  begin
    cur.IsWord := True;
  end;
end;

constructor TWordDictionary.Create;
begin
  __root := TNode.Create();
end;

function TWordDictionary.Search(word: string): Boolean;
begin
  Result := __match(__root, word, 0)
end;

function TWordDictionary.__match(node: TNode; word: string;
  index: Integer): Boolean;
var
  c: Char;
  nextChar: Char;
begin
  if index = word.Length then
    Exit(node.IsWord);

  c := word.Chars[index];

  if c <> '.' then
  begin
    if node.Next.Get(c).PValue = nil then
      Exit(False);

    Result := __match(node.Next.Get(c).PValue^, word, index + 1);
  end
  else
  begin
    for nextChar in node.Next.KeySets.ToArray do
    begin
      if __match(node.Next.Get(nextChar).PValue^, word, index + 1) then
        Exit(True);
    end;

    Result := False;
  end;
end;

end.
