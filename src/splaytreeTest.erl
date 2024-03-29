-module(splaytreeTest).
-author("Hugo Protsch").

-include_lib("eunit/include/eunit.hrl").
-import(splaytree, [initBT/0, insertBT/2, findBT/2, equalBT/2, deleteBT/2]).

print_test() ->
  Tree = treeInsertFromList(initBT(), util:randomliste(100)),
  splaytree:printBT(Tree, 'splay100Random.gv').

aufgabe26_test() ->
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

notfound_test() ->
  ?assertEqual({0, {}}, splaytree:findBT({}, 1)),
  ?assertEqual({0, tree4()}, splaytree:findBT(tree4(), 1)).

insertAlreadyThere_test() ->
  ?assertEqual(tree2(), insertBT(tree2(), 1000)),
  ?assertEqual(tree2_find250(), insertBT(tree2(), 250)).

emptyFind_test() ->
  ?assertEqual({0, {}}, splaytree:findBT({}, 1)),
  ?assertEqual({0, {}}, splaytree:findTP({}, 1)).

findTP_test() ->
  ?assertEqual({3, tree2()}, splaytree:findTP(tree2(), 1000))
.

splay_leftleft_test() ->
  Res = splaytree:splay(splayZigZigBefore50(), left, left),
  ?assertEqual({splayZigZigAfter50(), here}, Res),
  ?assertEqual({2, splayZigZigAfter50()}, findBT(splayZigZigBefore50(), 50)),
  ?assertEqual({0, splayZigZigAfter50_noleave()}, findBT(splayZigZigBefore50_noleave(), 51)).

splayZigZigBefore50() ->
  {100, 4, {75, 3, {50, 2, leaf(41), leaf(61)}, leaf(81)}, leaf(101)}.
splayZigZigAfter50() ->
  {50, 4, leaf(41), {75, 3, leaf(61), {100, 2, leaf(81), leaf(101)}}}.
splayZigZigBefore50_noleave() ->
  {100, 3, {75, 2, leaf(50), leaf(81)}, leaf(101)}.
splayZigZigAfter50_noleave() ->
  {50, 4, {}, {75, 3, {}, {100, 2, leaf(81), leaf(101)}}}.

leaf(El) -> {El, 1, {}, {}}.

emptyDelete_test() ->
  ?assertEqual({}, splaytree:deleteBT({}, 1)).

splayLargest_test() ->
  ?assertEqual(tree2(), splaytree:splayLargestBT(tree2())),
  ?assertEqual({1000, 3, {250, 2, {}, {500, 1, {}, {}}}, {}}, splaytree:splayLargestBT(tree4())).

delete_test() ->
  ?assert(equalBT({75, 2, {}, {100, 1, {}, {}}}, deleteBT(tree3(), 50))).

delete_random_test() ->
  Tree = treeInsertFromList(initBT(), util:randomliste(1000)),
  deleteListAndAssert(Tree, util:randomliste(1000)).

deleteListAndAssert(_, []) -> ok;
deleteListAndAssert(Tree, [H | T]) ->
  Before = splaytree:inOrderBT(Tree),
  AfterCorrect = lists:delete(H, Before),
  AfterActualTree = deleteBT(Tree, H),
  ?assertEqual(AfterCorrect, splaytree:inOrderBT(AfterActualTree)),
  deleteListAndAssert(AfterActualTree, T).

insert_test() ->
  Insert100 = {100, 1, {}, {}},
  Insert200 = {200, 2, {100, 1, {}, {}}, {}},
  Insert150 = {150, 2, {100, 1, {}, {}}, {200, 1, {}, {}}},
  Insert125 = {125, 3, {100, 1, {}, {}}, {150, 2, {}, {200, 1, {}, {}}}},
  ?assertEqual(Insert100, splaytree:insertBT(initBT()
    , 100)),
  ?assertEqual(Insert200, splaytree:insertBT(splaytree:insertBT(initBT(),
    100), 200)),
  ?assertEqual(Insert150, splaytree:insertBT(splaytree:insertBT(splaytree:insertBT(initBT(),
    100), 200), 150)),
  Insert125actual = splaytree:insertBT(splaytree:insertBT(avltree:insertBT(initBT(),
    100), 200), 150),
  ?assertEqual(Insert125,
    splaytree:insertBT(Insert125actual, 125)),
  splaytree:printBT(Insert125, 'aufg2_6_1a.gv'),
  splaytree:printBT(Insert125actual, 'aufg2_6_1b.gv')
.


zigzig_test() ->
  initBT(),
  {1, Tree2Find250} = splaytree:findBT(tree2(), 250),
  ?assertEqual(2, util:countread(rightrotate)),
  ?assertEqual(tree2_find250(), Tree2Find250),

  initBT(),
  {1, Tree2} = splaytree:findBT(tree2_find250(), 1000),
  ?assertEqual(2, util:countread(leftrotate)),
  ?assertEqual(tree2(), Tree2).

zigzagLR_test() ->
  initBT(),
  {1, Tree3Find75} = splaytree:findBT(tree3(), 75),
  ?assertEqual(1, util:countread(leftrotate)),
  ?assertEqual(1, util:countread(rightrotate)),
  ?assertEqual(tree3_find75(), Tree3Find75).


zigzagRL_test() ->
  initBT(),
  {1, Tree4Find500} = splaytree:findBT(tree4(), 500),
  ?assertEqual(1, util:countread(leftrotate)),
  ?assertEqual(1, util:countread(rightrotate)),
  ?assertEqual(tree4_find500(), Tree4Find500).


tree2() ->
  {1000, 3,
    {500, 2,
      {250, 1, {}, {}},
      {}},
    {}
  }.

tree2_find250() ->
  {
    250, 3, {}, {
    500, 2, {}, {
      1000, 1, {}, {}}
  }}
.

tree3() ->
  {100, 3,
    {50, 2,
      {},
      {75, 1, {}, {}}
    },
    {}
  }.

tree3_find75() -> {75, 2, {50, 1, {}, {}}, {100, 1, {}, {}}}.

tree4() ->
  {250, 3,
    {},
    {1000, 2, {500, 1, {}, {}}, {}}
  }.

tree4_find500() -> {500, 2, {250, 1, {}, {}}, {1000, 1, {}, {}}}.

treeInsertFromList(Tree, [H | T]) ->
  treeInsertFromList(splaytree:insertBT(Tree, H), T);
treeInsertFromList(Tree, []) -> Tree.
