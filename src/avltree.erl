-module(avltree).
-author("Hugo Protsch").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

-export([initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1, deleteBT/2,
  listAppend/2, printBT/2, rotateL/1, rotateR/1]).

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
  buildNode(NodeElement, NewLeft, Right);
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
  NewRight = insertBT(Right, Element),
  buildNode(NodeElement, Left, NewRight).

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
  buildNode(Found, NewLeftTree, Right);
deleteBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeftTree = deleteBT(Left, Element),
  buildNode(NodeElement, NewLeftTree, Right);
deleteBT({NodeElement, _, Left, Right}, Element) ->
  NewRightTree = deleteBT(Right, Element),
  buildNode(NodeElement, Left, NewRightTree).

findAndDeleteMax({Found, _, _, {}}) -> {Found, {}};
findAndDeleteMax({NodeElement, _, Left, Right}) ->
  {Found, NewRight} = findAndDeleteMax(Right),
  {Found, buildNode(NodeElement, Left, NewRight)}.

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.

getBalance({_, _, L, R}) -> getHeight(R) - getHeight(L).
getBalance(L, R) -> getHeight(R) - getHeight(L).

rotateR({RootEl, _, {RotateEl, _, RotateL, RotateR}, RootR}) ->
  NewRight = buildNode(RootEl, RotateR, RootR),
  buildNode(RotateEl, RotateL, NewRight).

rotateL({RootEl, _, RootL, {RotateEl, _, RotateL, RotateR}}) ->
  NewLeft = buildNode(RootEl, RootL, RotateL),
  buildNode(RotateEl, NewLeft, RotateR).

buildNode(Value, Left, Right) ->
  {Value, maxInt(getHeight(Left), getHeight(Right)) + 1, Left, Right}.


printBT(Tree, Filename) ->
  {ok, IODevice} = file:open(Filename, write),
  io:format(IODevice, "digraph G{~n", []),
  printBT2(Tree, IODevice),
  io:format(IODevice, "}", []).
printBT2({From, H, {}, {To, ToH, ToL, ToR}}, IODevice) ->
  printElement(IODevice, From, To, H),
  printBT2({To, ToH, ToL, ToR}, IODevice);
printBT2({From, H, {To, ToH, ToL, ToR}, {}}, IODevice) ->
  printElement(IODevice, From, To, H),
  printBT2({To, ToH, ToL, ToR}, IODevice);
printBT2({El, _, {}, {}}, IODevice) ->
  io:format(IODevice, "~p;~n", [El]);
printBT2({El, H, L, R}, IODevice) ->
  printBT2({El, H, L, {}}, IODevice),
  printBT2({El, H, {}, R}, IODevice);
printBT2({}, _IODevice) -> ok.
printElement(IODevice, From, To, Height) ->
  io:format(IODevice, "~p -> ~p [label = ~p];~n", [From, To, Height]).







