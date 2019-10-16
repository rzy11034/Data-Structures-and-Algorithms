//  20. 有效的括号
//  给定一个只包括 '('，')'，'{'，'}'，'['，']' 的字符串，判断字符串是否有效。
//
//  有效字符串需满足：
//
//  左括号必须用相同类型的右括号闭合。
//  左括号必须以正确的顺序闭合。
//  注意空字符串可被认为是有效字符串。
//
//  示例 1:
//
//  输入: "()"
//  输出: true
//  示例 2:
//
//  输入: "()[]{}"
//  输出: true
//  示例 3:
//
//  输入: "(]"
//  输出: false
//  示例 4:
//
//  输入: "([)]"
//  输出: false
//  示例 5:
//
//  输入: "{[]}"
//  输出: true
//
//  class Solution {
//      public boolean isValid(String s) {
//
//      }
//  }

unit DSA.LeetCode._020;

interface

uses
  System.SysUtils,
  DSA.List_Stack_Queue.ArrayListStack;

type
  Solution = class
    function isValid(s: string): Boolean;
  end;

procedure Main;

implementation

procedure Main;
begin
  with Solution.Create do
  begin
    Writeln(isValid('()'));
    Writeln(isValid('()[]{}'));
    Writeln(isValid('(]'));
    Writeln(isValid('([)]'));
    Writeln(isValid('{[]}'));

  end;
end;

{ Solution }

function Solution.isValid(s: string): Boolean;
var
  stack: TArrayListStack<string>;
  i: Integer;
  c: Char;
begin
  stack := TArrayListStack<string>.Create();

  for i := low(s) to high(s) do
  begin
    c := s[i];
    if CharInSet(c, ['(', '{', '[']) then
      stack.Push(c)
    else
    begin
      if (c = ')') and (stack.Pop <> '(') then
        Exit(False)
      else if (c = ']') and (stack.Pop <> '[') then
        Exit(False)
      else if (c = '}') and (stack.Pop <> '{') then
        Exit(False);
    end;
  end;

  Result := stack.IsEmpty;
end;

end.
