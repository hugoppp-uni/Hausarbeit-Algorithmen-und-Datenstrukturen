-module(aufgaben).
-author("hugop").

%% API
-export([start/0]).
-import(avltree, [initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1,
deleteBT/2, listAppend/2, printBT/2, rotateR/1, rotateL/1, buildNodeAndRotateIfNeeded/1]).

start() ->
  avl_aufgabe_1_5(),
  splay_aufgabe_1_6(),
  splay_aufgabe_2_6()
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

splay_aufgabe_2_6() ->
  Tree = treeInsertFromList(initBT(), [4, 7, 2, 3, 5, 9, 6, 8, 1]),
  splaytree:printBT(Tree, 'aufg2_6_2.gv'),
  splaytree:printBT(splaytree:deleteBT(Tree, 4), 'aufg2_6_Delete4.gv'),
  splaytree:printBT(splaytree:insertBT(Tree, 10), 'aufg2_6_2_Insert_10.gv'),

  {_, BT4} = splaytree:findBT(Tree, 4),
  splaytree:printBT(BT4, 'aufg2_6_FindBT4.gv'),
  {_, BT99} = splaytree:findBT(Tree, 99),
  splaytree:printBT(BT99, 'aufg2_6_FindBT99.gv'),

  {_, TP4} = splaytree:findTP(Tree, 4),
  splaytree:printBT(TP4, 'aufg2_6_FindTP4.gv'),
  {_, TP99} = splaytree:findTP(Tree, 99),
  splaytree:printBT(TP99, 'aufg2_6_FindTP99.gv'),

  splaytree:printBT(splaytree:deleteBT(Tree, 99), 'aufg2_6_Delete99.gv')
.


splay_aufgabe_1_6() -> notimplementedgv.