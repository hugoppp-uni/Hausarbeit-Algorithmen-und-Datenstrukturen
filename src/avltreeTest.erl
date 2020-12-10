-module(avltreeTest).
-author("Hugo Protsch").
-import(avltree, [initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1,
deleteBT/2, listAppend/2]).
-include_lib("eunit/include/eunit.hrl").



initBT_test() ->
  ? assertEqual({}, initBT()).

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
  ?assertEqual({1,1,{},{}}, insertBT({}, 1)),
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
  ?assertEqual({1000,2,{},{2000,1,{},{}}},deleteBT(tree1(),1500)),
?assertEqual({1000,2,{250,1,{},{}},{}},deleteBT(tree2(),500)).

tree1() ->
  {1000,3,
    {},
    {1500,2,
      {},
      {2000, 1, {}, {}}
    }
  }.

tree2() ->
  {1000,3,
    {500,2,
      {250, 1, {},{}},
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
