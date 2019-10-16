//  387. 字符串中的第一个唯一字符
//
//  给定一个字符串，找到它的第一个不重复的字符，并返回它的索引。
//  如果不存在，则返回 -1。
//
//  案例:
//
//  s = "leetcode"
//  返回 0.
//
//  s = "loveleetcode",
//  返回 2.
//
//  class Solution {
//      public int firstUniqChar(String s) {
//
//      }
//  }

unit DSA.LeetCode._387;

interface

uses
  System.SysUtils;

type
  TSolution = class
    function firstUniqChar(s: string): Integer;
  end;

procedure Main;

implementation

procedure Main;
var
  ts: TSolution;
begin
  ts := TSolution.Create;
  Writeln(ts.firstUniqChar('leetcode'));
  Writeln(ts.firstUniqChar('loveleetcode'));
end;

{ TSolution }

function TSolution.firstUniqChar(s: string): Integer;
var
  freq: array of Integer;
  i: Integer;
begin
  SetLength(freq, 26);

  for i := 0 to s.Length - 1 do
    inc(freq[ord(s.Chars[i]) - ord('a')]);

  for i := 0 to s.Length - 1 do
    if freq[ord(s.Chars[i]) - ord('a')] = 1 then
      Break;

  Result := i;
end;

end.
