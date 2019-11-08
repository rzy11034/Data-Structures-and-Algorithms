unit DSA.LeetCode._804;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DSA.Tree.BSTSet,
  DSA.Interfaces.Comparer;

type
  TSolution = class
  public
    class function uniqueMorseRepresentations(words: array of string): integer;
  end;

type
  TBSTSet_str = specialize TBSTSet<string, specialize TComparer<string>>;

procedure main;

implementation

procedure main;
var
  words: array of string;
begin
  words := ['gin', 'zen', 'gig', 'msg'];

  Writeln(TSolution.uniqueMorseRepresentations(words));
end;

{ TSolution }

class function TSolution.uniqueMorseRepresentations
  (words: array of string): integer;
const
  codes: array of string = ('.-', '-...', '-.-.', '-..', '.', '..-.', '--.',
    '....', '..', '.---', '-.-', '.-..', '--', '-.', '---', '.--.', '--.-',
    '.-.', '...', '-', '..-', '...-', '.--', '-..-', '-.--', '--..');
var
  BSTSet: TBSTSet_str;
  i: integer;
  c: char;
  s: string;
  res: TStringBuilder;
begin
  BSTSet := TBSTSet_str.Create;
  try

    for s in words do
    begin
      for c in s do
      begin
        res := TStringBuilder.Create;
        i := Ord(c) - Ord('a');
        res.Append(codes[i]);
      end;

      BSTSet.Add(res.ToString);
      FreeAndNil(res);
    end;

    Result := BSTSet.GetSize;

  finally
    BSTSet.Free;
  end;
end;

end.
