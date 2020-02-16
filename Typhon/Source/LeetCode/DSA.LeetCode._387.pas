unit DSA.LeetCode._387;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TSolution = class
    function firstUniqChar(s: string): integer;
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

function TSolution.firstUniqChar(s: string): integer;
var
  freq: array of integer;
  i: integer;
begin
  SetLength(freq, 26);

  for i := 0 to s.Length - 1 do
    Inc(freq[Ord(s.Chars[i]) - Ord('a')]);

  for i := 0 to s.Length - 1 do
    if freq[Ord(s.Chars[i]) - Ord('a')] = 1 then
      Break;

  Result := i;
end;

end.
