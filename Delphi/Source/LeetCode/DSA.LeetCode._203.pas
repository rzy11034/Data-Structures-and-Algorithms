//  203. 移除链表元素
//
//  删除链表中等于给定值 val 的所有节点。
//
//  示例:
//
//  输入: 1->2->6->3->4->5->6, val = 6
//  输出: 1->2->3->4->5
//
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
//
//      }
//  }
unit DSA.LeetCode._203;

interface

uses
  System.SysUtils;

type
  TListNode = class
  public
    val: Integer;
    next: TListNode;
    constructor Create(x: Integer); overload;
  end;

  TSolution = class
  public
    function RemoveElements(head: TListNode; val: Integer): TListNode;
  end;

procedure Main;

implementation

function InitListNode(arr: array of Integer): TListNode;
var
  i: Integer;
  head, cur: TListNode;
begin
  head := TListNode.Create(-1);
  cur := head;

  for i := low(arr) to high(arr) do
  begin
    cur.next := TListNode.Create(arr[i]);
    cur := cur.next;
  end;

  Result := head.next;
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
    if head.next <> nil then
      write(head.val, '->')
    else
      WriteLn(head.val);

    head := head.next;
  end;
end;

{ TListNode }

constructor TListNode.Create(x: Integer);
begin
  val := x;
end;

{ TSolution }

function TSolution.RemoveElements(head: TListNode; val: Integer): TListNode;
var
  ret: TListNode;
begin
  if head = nil then
    Exit(nil);

  head.next := RemoveElements(head.next, val);

  if head.val = val then
    ret := head.next
  else
    ret := head;

  Result := ret;
end;

end.
