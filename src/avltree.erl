-module(avltree).
-author("Hugo Protsch").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

-export([initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1, deleteBT/2,
  listAppend/2]).

initBT() -> {}.

isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

isBT(Btree) -> isBT(Btree, -1, ok).
isBT({}, _, _) -> true;
isBT({Element, Height, {}, {}}, LowerLimit, UpperLimit) ->
  (is_integer(Element)) and
  (Element >= 0) and
  (Element > LowerLimit) and
  (Element < UpperLimit) and
  (Height == 1);
isBT({Element, Height, Left, {}}, LowerLimit, UpperLimit) ->
  {_, HeightL, _, _} = Left,
  (is_integer(Element)) and
  (Element >= 0) and
  (Element > LowerLimit) and
  (Element < UpperLimit) and
  (Height > 0) and
  (Height == (HeightL + 1)) and
  (isBT(Left, LowerLimit, Element));
isBT({Element, Height, {}, Right}, LowerLimit, UpperLimit) ->
  {_, HeightR, _, _} = Right,
  (is_integer(Element)) and
  (Element >= 0) and 
  (Element > LowerLimit) and
  (Element < UpperLimit) and
  (Height > 0) and
  (Height == (HeightR + 1)) and
  (isBT(Right, Element, UpperLimit));
isBT({Element, Height, Left, Right}, LowerLimit, UpperLimit) ->
  {_, HeightL, _, _} = Left,
  {_, HeightR, _, _} = Right,
  (is_integer(Element)) and
  (Element >= 0) and
  (Element > LowerLimit) and
  (Element < UpperLimit) and
  (Height > 0) and
  (Height == (maxInt(HeightL, HeightR) + 1)) and
  (isBT(Left, LowerLimit, Element)) and
  (isBT(Right, Element, UpperLimit)).

equalBT(Btree1, Btree2) -> equalList(inOrderBT(Btree1), inOrderBT(Btree2)).

equalList([], []) -> true;
equalList(List, List) -> true;
equalList(_, _) -> false.

listAppend([Head | Tail], List) ->
  [Head | listAppend(Tail, List)];
listAppend([], List) ->
  List.

inOrderBT({}) -> [];
inOrderBT({Element, _, Left, Right}) ->
  listAppend(inOrderBT(Left), [Element | inOrderBT(Right)]).

insertBT({}, Element) ->
  {Element, 1, {}, {}};
insertBT({Element, Height, Left, Right}, Element) ->
  {Element, Height, Left, Right};
insertBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeft = insertBT(Left, Element),
  {NodeElement, max(getHeight(Right), getHeight(NewLeft)) + 1, NewLeft, Right};
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
  NewRight = insertBT(Right, Element),
  {NodeElement, maxInt(getHeight(Left), getHeight(NewRight)) + 1, Left, NewRight}.

maxInt(Int1, Int2) when Int1 > Int2 -> Int1;
maxInt(Int1, Int2) when Int2 > Int1 -> Int2;
maxInt(Int, Int) -> Int.

findBT({Element, Height, _, _}, Element) -> Height;
findBT({NodeElement, _, Left, _}, Element) when Element < NodeElement ->
  findBT(Left, Element);
findBT({NodeElement, _, _, Right}, Element) when Element > NodeElement ->
  findBT(Right, Element);
findBT(_, _) -> -1.

deleteBT({}, _) -> {};
deleteBT({Element, _, {}, Right}, Element) -> Right;
deleteBT({Element, _, Left, {}}, Element) -> Left;
deleteBT({Element, _, Left, Right}, Element) ->
  {Found, NewLeftTree} = findAndDeleteMax(Left),
  {Found, maxInt(getHeight(NewLeftTree), getHeight(Right)) + 1, NewLeftTree, Right};
deleteBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeftTree = deleteBT(Left, Element),
  {NodeElement, maxInt(getHeight(NewLeftTree), getHeight(Right)) + 1, NewLeftTree, Right};
deleteBT({NodeElement, _, Left, Right}, Element) ->
  NewRightTree = deleteBT(Right, Element),
  {NodeElement, maxInt(getHeight(Left), getHeight(NewRightTree)) + 1, Left, NewRightTree}.

findAndDeleteMax({Found, _, _, {}}) -> {Found, {}};
findAndDeleteMax({NodeElement, _, Left, Right}) ->
  {Found, NewRight} = findAndDeleteMax(Right),
  {Found, {NodeElement, maxInt(getHeight(Left), getHeight(NewRight)) + 1, Left, NewRight}}.

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.