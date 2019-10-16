//  203. 移除链表元素

//  删除链表中等于给定值 val 的所有节点。

//  示例:

//  输入: 1->2->6->3->4->5->6, val = 6
//  输出: 1->2->3->4->5

//  /**
//   * Definition for singly-linked list.
//   * public class ListNode {
//   *     int val;
//   *     ListNode next;
//   *     ListNode(int x) { val = x; }
//   * }
//   */
//  class Solution {
//      public ListNode removeElements(ListNode head, int val) {

//      }
//  }
unit DSA.LeetCode._203;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  { TListNode }

  TListNode = class
  public
    val: integer;
    Next: TListNode;
    constructor Create(x: integer); overload;
  end;

  { TSolution }

  TSolution = class
  public
    function RemoveElements(head: TListNode; val: integer): TListNode;
  end;

procedure Main;

implementation

function InitListNode(arr: array of integer): TListNode;
var
  i: integer;
  head, cur: TListNode;
begin
  head := TListNode.Create(-1);
  cur := head;

  for i := Low(arr) to High(arr) do
  begin
    cur.Next := TListNode.Create(arr[i]);
    cur := cur.Next;
  end;

  Result := head.Next;
end;

procedure Main;
var
  head: TListNode;
begin
  head := InitListNode([1, 2, 6, 3, 4, 5, 6]);

  with TSolution.Create do
    RemoveElements(head, 6);

  while head <> nil do
  begin
    if head.Next <> nil then
      Write(head.val, '->')
    else
      WriteLn(head.val);

    head := head.Next;
  end;
end;

{ TSolution }

function TSolution.RemoveElements(head: TListNode; val: integer): TListNode;
var
  ret: TListNode;
begin
  if head = nil then
    Exit(nil);

  head.Next := RemoveElements(head.Next, val);

  if head.val = val then
    ret := head.Next
  else
    ret := head;

  Result := ret;
end;

{ TListNode }

constructor TListNode.Create(x: integer);
begin
  val := x;
end;

end.
