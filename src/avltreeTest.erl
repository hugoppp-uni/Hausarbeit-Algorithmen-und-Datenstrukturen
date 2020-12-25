-module(avltreeTest).
-author("Hugo Protsch").
-import(avltree, [initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1,
deleteBT/2, listAppend/2, printBT/2, rotateR/1, rotateL/1, buildNodeAndRotateIfNeeded/1]).
-include_lib("eunit/include/eunit.hrl").



initBT_test() ->
  ?assertEqual({}, initBT()).

isEmptyBT_test() ->
  ?assert(isEmptyBT({})),
  ?assertNot(isEmptyBT({5, 1, {}, {}})).

findBT_test() ->
  ?assertEqual(-1, findBT({}, 50)),
  ?assertEqual(1, findBT({50, 1, {}, {}}, 50)),
  ?assertEqual(2, findBT(correctTree3H(), 500)),
  ?assertEqual(1, findBT(correctTree3H(), 1250)),
  ?assertEqual(1, findBT(correctTree3H(), 2000)),
  ?assertEqual(-1, findBT(correctTree3H(), 1999)).

inOrderBT_test() ->
  ?assertEqual([], inOrderBT({})),
  ?assertEqual([250 | [500 | [750 | [1000 | [1250 | [1500 | [2000]]]]]]]
    , inOrderBT(correctTree3H())).

insertBT_test() ->
  ?assertEqual({1, 1, {}, {}}, insertBT({}, 1)),
  ?assertEqual(correctTree3HInsert100(), insertBT(correctTree3H(), 100)),
  ?assertEqual(correctTree3HInsert100(), insertBT(correctTree3HInsert100(), 100)).

equalBT_test() ->
  ?assertNot(equalBT(correctTree3H(), {})),
  ?assertNot(equalBT(correctTree3H(), correctTree3HInsert100())),
  ?assertNot(equalBT(correctTree3H(), insertBT(correctTree3H(), 999))),
  ?assert(equalBT(correctTree3H(), correctTree3H())).

deleteBT_test() ->
  %element gets deleted at the end
  ?assertEqual(correctTree3H(), deleteBT(correctTree3HInsert100(), 100)),
  %element doesn't exists, return same tree
  ?assertEqual(correctTree3H(), deleteBT(correctTree3H(), 100)),
  %element gets deleted at the top
  ?assertEqual(inOrderBT(correctTree3HRemove1000()), inOrderBT(deleteBT(correctTree3H(), 1000))),
  ?assertEqual({1000, 2, {}, {2000, 1, {}, {}}}, deleteBT(tree1(), 1500)),
  ?assertEqual({1000, 2, {250, 1, {}, {}}, {}}, deleteBT(tree2(), 500)).

print_test() ->
  ?assertEqual(ok, printBT(correctTree3HInsert100(), lolu)),
  ?assertEqual(ok, printBT({10, 1, {}, {}}, lolu2))
.

rotateLeft_test() ->
  ?assertEqual(treeExampleBalanced(), rotateL(treeExampleRightRight())).
rotateRight_test() ->
  ?assertEqual(treeExampleBalanced(), rotateR(treeExampleLeftLeft())).

buildElementAndRotate_test() ->
  ?assertEqual(treeExampleBalanced(), buildNodeAndRotateIfNeeded(treeExampleRightRight())),
  ?assertEqual(treeExampleBalanced(), buildNodeAndRotateIfNeeded(treeExampleLeftLeft())).
buildElementAndRotateDoubleRot_test() ->
  ?assertEqual(treeExampleBalanced(), buildNodeAndRotateIfNeeded(treeExampleLeftRight())),
  ?assertEqual(treeExampleBalanced(), buildNodeAndRotateIfNeeded(treeExampleRightLeft()))
.

insertNoRotate_test() ->
  ?assertEqual(correctTree3HInsert100(), insertBT(correctTree3H(), 100)).
insertSingleRotate_test() ->
  ?assertEqual(treeExampleBalanced(), insertBT(treeExampleLeftLeft_beforeInsert3(), 3)),
  ?assertEqual(treeExampleBalanced(), insertBT(treeExampleRightRight_beforeInsert5(), 5)).
insertDoubleRotate_test() ->
  ?assertEqual(treeExampleBalanced(), insertBT(treeExampleRightLeft_beforeInsert4(), 4)),
  ?assertEqual(treeExampleBalanced(), insertBT(treeExampleLeftRight_beforeInsert4(), 4))
.

insertAsc_test() ->
  ?assertEqual(insert3(), insertBT(insert2(), 3)),
  ?assertEqual(insert4(), insertBT(insert3(), 4)),
  ?assertEqual(insert5(), insertBT(insert4(), 5)),
  ?assertEqual(insert6(), insertBT(insert5(), 6)),
  ?assertEqual(insert7(), insertBT(insert6(), 7)),
  ?assertEqual(doublAdd2(), insertBT(doubl(), 2)).

isBt_test() ->
  ?assertNot(isBT(incorrectTree2())),
  ?assertNot(isBT(incorrectTree1())),
  ?assert(isBT(treeExampleBalanced())),
  ?assert(isBT(treeExampleLeftLeft_beforeInsert3())),
  ?assertNot(isBT(treeExampleLeftLeft())),
  ?assert(isBT(treeExampleRightRight_beforeInsert5())),
  ?assertNot(isBT(treeExampleRightRight())),
  ?assert(isBT(treeExampleLeftRight_beforeInsert4())),
  ?assertNot(isBT(treeExampleLeftRight())),
  ?assert(isBT(treeExampleRightLeft_beforeInsert4())),
  ?assertNot(isBT(treeExampleRightLeft())).

randomInsert_test() ->
  Numbers = util:randomliste(1000),
  ?assertEqual(ok, insertList(initBT(), Numbers)).

randomDelete_test() ->
  Numbers = util:randomliste(1000),
  ?assertEqual(ok, insertList(initBT(), Numbers)),
  Numbers2 = util:randomliste(1000),
  ?assertEqual(ok,deleteList(initBT(),Numbers2)).


deleteList(Tree, [H | T]) ->
  Res = deleteBT(Tree, H),
  ?assert(isBT(Res)),
  deleteList(Res, T);
deleteList(Tree, []) -> printBT(Tree, randomRes), ok.

insertList(Tree, [H | T]) ->
  Res = insertBT(Tree, H),
  ?assert(isBT(Res)),
  insertList(Res, T);
insertList(Tree, []) -> printBT(Tree, randomRes), ok.

treeExampleRightLeft_beforeInsert4() ->
  {3, 2,
    {},
    {5, 1,
      {},
      {}}
  }.
treeExampleRightLeft() ->
  {3, 3,
    {},
    {5, 2,
      {4, 1, {}, {}},
      {}}
  }.
treeExampleLeftRight_beforeInsert4() ->
  {5, 2,
    {3, 1,
      {},
      {}},
    {}
  }.
treeExampleLeftRight() ->
  {5, 3,
    {3, 2,
      {},
      {4, 1, {}, {}}},
    {}
  }.
treeExampleLeftLeft_beforeInsert3() ->
  {5, 2,
    {4, 1, {}, {}},
    {}
  }.
treeExampleLeftLeft() ->
  {5, 3,
    {4, 2,
      {3, 1, {}, {}},
      {}},
    {}
  }.
treeExampleRightRight_beforeInsert5() ->
  {3, 2,
    {},
    {4, 1,
      {},
      {}}
  }.
treeExampleRightRight() ->
  {3, 3,
    {},
    {4, 2,
      {},
      {5, 1, {}, {}}}
  }.
treeExampleBalanced() ->
  {4, 2,
    {3, 1, {}, {}},
    {5, 1, {}, {}}
  }.

tree1() ->
  {1000, 3,
    {},
    {1500, 2,
      {},
      {2000, 1, {}, {}}
    }
  }.

tree2() ->
  {1000, 3,
    {500, 2,
      {250, 1, {}, {}},
      {}},
    {}
  }.

correctTree3HRemove1000() ->
  {750, 3,
    {500, 2,
      {250, 1, {}, {}},
      {}},
    {1500, 2,
      {1250, 1, {}, {}},
      {2000, 1, {}, {}}}
  }.

correctTree3H() ->
  {1000, 3,
    {500, 2,
      {250, 1, {}, {}},
      {750, 1, {}, {}}},
    {1500, 2,
      {1250, 1, {}, {}},
      {2000, 1, {}, {}}}
  }.

correctTree3HInsert100() ->
  {1000, 4,
    {500, 3,
      {250, 2,
        {100, 1, {}, {}},
        {}},
      {750, 1, {}, {}}},
    {1500, 2,
      {1250, 1, {}, {}},
      {2000, 1, {}, {}}}
  }.

insert2() ->
  {1, 2, {}, {2, 1, {}, {}}}.

insert3() ->
  {2, 2, {1, 1, {}, {}}, {3, 1, {}, {}}}.

insert4() ->
  {2, 3, {1, 1, {}, {}}, {3, 2, {}, {4, 1, {}, {}}}}.

insert5() ->
  {2, 3, {1, 1, {}, {}}, {4, 2, {3, 1, {}, {}}, {5, 1, {}, {}}}}.

insert6() ->
  {4, 3,
    {2, 2,
      {1, 1, {}, {}},
      {3, 1, {}, {}}},
    {5, 2,
      {},
      {6, 1, {}, {}}}
  }.

insert7() ->
  {4, 3,
    {2, 2,
      {1, 1, {}, {}},
      {3, 1, {}, {}}},
    {6, 2,
      {5, 1, {}, {}},
      {7, 1, {}, {}}}
  }.

doubl() ->
  {1, 2, {}, {3, 1, {}, {}}}.

doublAdd2() ->
  {2, 2, {1, 1, {}, {}}, {3, 1, {}, {}}}.

incorrectTree1() ->
  {100, 2,
    {50, 1, {}, {}},
    {20, 1, {}, {}}
  }.
incorrectTree2() ->
  {100, 3,
    {50, 2,
      {45, 1, {}, {}}, {}},
    {120, 2,
      {90, 1, {}, {}}, {}}
  }.