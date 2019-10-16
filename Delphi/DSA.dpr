program DSA;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  DSA.Utils in 'Source\DSA.Utils.pas',
  DSA.Main in 'Source\DSA.Main.pas',
  DSA.List_Stack_Queue.ArrayList in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.ArrayList.pas',
  DSA.List_Stack_Queue.ArrayListStack in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.ArrayListStack.pas',
  DSA.List_Stack_Queue.ArrayListQueue in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.ArrayListQueue.pas',
  DSA.LeetCode._020 in 'Source\LeetCode\DSA.LeetCode._020.pas',
  DSA.List_Stack_Queue.LoopListQueue in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.LoopListQueue.pas',
  DSA.List_Stack_Queue.QueueCompare in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.QueueCompare.pas',
  DSA.List_Stack_Queue.LinkedList in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.LinkedList.pas',
  DSA.List_Stack_Queue.LinkedListStack in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.LinkedListStack.pas',
  DSA.List_Stack_Queue.StackCompare in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.StackCompare.pas',
  DSA.List_Stack_Queue.LinkedListQueue in 'Source\List_Stack_Queue\DSA.List_Stack_Queue.LinkedListQueue.pas',
  DSA.LeetCode._203 in 'Source\LeetCode\DSA.LeetCode._203.pas',
  DSA.Tree.BST in 'Source\Tree\DSA.Tree.BST.pas',
  DSA.Tree.BSTSet in 'Source\Tree\DSA.Tree.BSTSet.pas',
  DSA.Tree.LinkedListSet in 'Source\Tree\DSA.Tree.LinkedListSet.pas',
  DSA.Tree.SetCompare in 'Source\Tree\DSA.Tree.SetCompare.pas',
  DSA.LeetCode._804 in 'Source\LeetCode\DSA.LeetCode._804.pas',
  DSA.Tree.LinkedListMap in 'Source\Tree\DSA.Tree.LinkedListMap.pas',
  DSA.Tree.BSTMap in 'Source\Tree\DSA.Tree.BSTMap.pas',
  DSA.Tree.MapCompare in 'Source\Tree\DSA.Tree.MapCompare.pas',
  DSA.LeetCode._349 in 'Source\LeetCode\DSA.LeetCode._349.pas',
  DSA.LeetCode._350 in 'Source\LeetCode\DSA.LeetCode._350.pas',
  DSA.Tree.Heap in 'Source\Tree\DSA.Tree.Heap.pas',
  DSA.Tree.PriorityQueue in 'Source\Tree\DSA.Tree.PriorityQueue.pas',
  DSA.LeetCode._347 in 'Source\LeetCode\DSA.LeetCode._347.pas',
  DSA.Tree.SegmentTree in 'Source\Tree\DSA.Tree.SegmentTree.pas',
  DSA.LeetCode._303 in 'Source\LeetCode\DSA.LeetCode._303.pas',
  DSA.LeetCode._307 in 'Source\LeetCode\DSA.LeetCode._307.pas',
  DSA.Tree.Trie in 'Source\Tree\DSA.Tree.Trie.pas',
  DSA.LeetCode._211 in 'Source\LeetCode\DSA.LeetCode._211.pas',
  DSA.LeetCode._677 in 'Source\LeetCode\DSA.LeetCode._677.pas',
  DSA.Tree.UnionFind1 in 'Source\Tree\DSA.Tree.UnionFind1.pas',
  DSA.Tree.UnionFind2 in 'Source\Tree\DSA.Tree.UnionFind2.pas',
  DSA.Tree.UnionFindCompare in 'Source\Tree\DSA.Tree.UnionFindCompare.pas',
  DSA.Tree.UnionFind3 in 'Source\Tree\DSA.Tree.UnionFind3.pas',
  DSA.Tree.UnionFind4 in 'Source\Tree\DSA.Tree.UnionFind4.pas',
  DSA.Tree.UnionFind5 in 'Source\Tree\DSA.Tree.UnionFind5.pas',
  DSA.Tree.UnionFind6 in 'Source\Tree\DSA.Tree.UnionFind6.pas',
  DSA.Tree.AVLTree in 'Source\Tree\DSA.Tree.AVLTree.pas',
  DSA.Tree.TreeCompare in 'Source\Tree\DSA.Tree.TreeCompare.pas',
  DSA.Tree.AVLTreeMap in 'Source\Tree\DSA.Tree.AVLTreeMap.pas',
  DSA.Tree.AVLTreeSet in 'Source\Tree\DSA.Tree.AVLTreeSet.pas',
  DSA.Tree.RBTree in 'Source\Tree\DSA.Tree.RBTree.pas',
  DSA.LeetCode._387 in 'Source\LeetCode\DSA.LeetCode._387.pas',
  DSA.Hash.HashCode in 'Source\Hash\DSA.Hash.HashCode.pas',
  DSA.Hash.HashTable in 'Source\Hash\DSA.Hash.HashTable.pas',
  DSA.Sorts.SelectionSort in 'Source\Sorts\DSA.Sorts.SelectionSort.pas',
  DSA.Sorts.InsertionSort in 'Source\Sorts\DSA.Sorts.InsertionSort.pas',
  DSA.Sorts.BubbleSort in 'Source\Sorts\DSA.Sorts.BubbleSort.pas',
  DSA.Sorts.MergeSort in 'Source\Sorts\DSA.Sorts.MergeSort.pas',
  DSA.Sorts.QuickSort in 'Source\Sorts\DSA.Sorts.QuickSort.pas',
  DSA.Sorts.HeapSort in 'Source\Sorts\DSA.Sorts.HeapSort.pas',
  DSA.Tree.IndexHeap in 'Source\Tree\DSA.Tree.IndexHeap.pas',
  DSA.Sorts.IndexHeapSort in 'Source\Sorts\DSA.Sorts.IndexHeapSort.pas',
  DSA.Search.BinarySearch in 'Source\Search\DSA.Search.BinarySearch.pas',
  DSA.Graph.SparseGraph in 'Source\Graph\DSA.Graph.SparseGraph.pas',
  DSA.Graph.DenseGraph in 'Source\Graph\DSA.Graph.DenseGraph.pas',
  DSA.Graph.Component in 'Source\Graph\DSA.Graph.Component.pas',
  DSA.Graph.Path in 'Source\Graph\DSA.Graph.Path.pas',
  DSA.Graph.ShortestPath in 'Source\Graph\DSA.Graph.ShortestPath.pas',
  DSA.Graph.Edge in 'Source\Graph\DSA.Graph.Edge.pas',
  DSA.Graph.DenseWeightedGraph in 'Source\Graph\DSA.Graph.DenseWeightedGraph.pas',
  DSA.Graph.SparseWeightedGraph in 'Source\Graph\DSA.Graph.SparseWeightedGraph.pas',
  DSA.Graph.LazyPrimMST in 'Source\Graph\DSA.Graph.LazyPrimMST.pas',
  DSA.Interfaces.Comparer in 'Source\Interfaces\DSA.Interfaces.Comparer.pas',
  DSA.Interfaces.DataStructure in 'Source\Interfaces\DSA.Interfaces.DataStructure.pas',
  DSA.Graph.PrimMST in 'Source\Graph\DSA.Graph.PrimMST.pas',
  DSA.Graph.PrimMstCompare in 'Source\Graph\DSA.Graph.PrimMstCompare.pas',
  DSA.Graph.KruskalMST in 'Source\Graph\DSA.Graph.KruskalMST.pas',
  DSA.Graph.MstCompare in 'Source\Graph\DSA.Graph.MstCompare.pas',
  DSA.Graph.Dijkstra in 'Source\Graph\DSA.Graph.Dijkstra.pas',
  DSA.Graph.BellmanFord in 'Source\Graph\DSA.Graph.BellmanFord.pas';

begin
  try
    Run;
    TDSAUtils.DrawLine;
    Writeln(END_OF_PROGRAM_EN);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
