-module(aufgaben).
-author("hugop").

%% API
-export([start/0]).
-import(avltree, [initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1,
deleteBT/2, listAppend/2, printBT/2, rotateR/1, rotateL/1, buildNodeAndRotateIfNeeded/1]).

start() ->
  avl_aufgabe_1_5(),
  splay_aufgabe_1_6()
.

avl_aufgabe_1_5() ->
  Tree = treeInsertFromList(initBT(), util:randomliste(100)),
  printBT(Tree, 'aufgabe1_6_before.gv'),
  Tree88deleted = treeDeleteFromList(Tree, util:randomliste(88)),
  printBT(Tree88deleted, 'aufgabe1_6_after.gv').

treeInsertFromList(Tree, [H | T]) ->
  treeInsertFromList(insertBT(Tree, H), T);
treeInsertFromList(Tree, []) -> Tree.

treeDeleteFromList(Tree, [H | T]) ->
  treeDeleteFromList(deleteBT(Tree, H), T);
treeDeleteFromList(Tree, []) -> Tree.


splay_aufgabe_1_6() -> notimplementedgv.