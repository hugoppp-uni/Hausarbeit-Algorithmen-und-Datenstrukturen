-module(avltree).
-author("Hugo Protsch").

-import(erlang, [timestamp/0]).
-import(timer, [now_diff/2]).

-export([initBT/0, isEmptyBT/1, inOrderBT/1, insertBT/2, findBT/2, equalBT/2, isBT/1, deleteBT/2,
  listAppend/2, printBT/2, rotateL/1, rotateR/1, buildNodeAndRotateIfNeeded/1]).

initBT() ->
  util:countreset(leftrotate),
  util:countreset(rightrotate),
  util:countreset(ddrightrotate),
  util:countreset(ddleftrotate),
  {}.

isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

isBT(Btree) -> isBT(Btree, -1, ok).
isBT({}, _, _) -> true;
isBT({Element, Height, {}, {}}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height)
    and (Height == 1);
isBT({Element, Height, Left, {}}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == 2) and
    (isBT(Left, LowerLimit, Element));
isBT({Element, Height, {}, Right}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == 2) and
    (isBT(Right, Element, UpperLimit));
isBT({Element, Height, Left, Right}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == (maxInt(getHeight(Left), getHeight(Right)) + 1)) and
    (getBalance(Left, Right) < 2) and
    (isBT(Left, LowerLimit, Element)) and
    (isBT(Right, Element, UpperLimit)).
basicChecks(Element, LowerLimit, UpperLimit, Height) ->
  (is_integer(Element)) and
    (Height > 0) and
    (Element >= 0) and
    (Element > LowerLimit) and
    (Element < UpperLimit).

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
  buildNodeAndRotateIfNeeded(NodeElement, NewLeft, Right);
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
  NewRight = insertBT(Right, Element),
  buildNodeAndRotateIfNeeded(NodeElement, Left, NewRight).


buildNodeAndRotateIfNeeded({El, _, L, R}) ->
  buildNodeAndRotateIfNeeded(El, L, R).
buildNodeAndRotateIfNeeded(El, L, R) ->
  Wurzel = buildNode(El, L, R),
  Balance = getBalance(L, R),
  if
    Balance == -2 ->
      %% Left Right Case <OR> Left Left Case
      {WurzelEl, _, RotationsKnoten, NonRotate} = Wurzel,
      RotationsKnotenBalance = getBalance(RotationsKnoten),
      if
        RotationsKnotenBalance == 1 ->
          NewRotate = rotateL(RotationsKnoten),
          util:counting1(ddrightrotate),
          rotateR(buildNode(WurzelEl, NewRotate, NonRotate));
        true -> util:counting1(rightrotate), rotateR(Wurzel)
      end;
    Balance == 2 ->
      %% Right Left Case <OR> Right Right Case
      {WurzelEl, _, NonRotate, RotationsKnoten} = Wurzel,
      RotationsKnotenBalance = getBalance(RotationsKnoten),
      if
        RotationsKnotenBalance == -1 ->
          NewRotate = rotateR(RotationsKnoten),
          util:counting1(ddleftrotate),
          rotateL(buildNode(WurzelEl, NonRotate, NewRotate));
        true -> util:counting1(leftrotate), rotateL(Wurzel)
      end;
    true -> Wurzel
  end.

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
  buildNodeAndRotateIfNeeded(Found, NewLeftTree, Right);
deleteBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  NewLeftTree = deleteBT(Left, Element),
  buildNodeAndRotateIfNeeded(NodeElement, NewLeftTree, Right);
deleteBT({NodeElement, _, Left, Right}, Element) ->
  NewRightTree = deleteBT(Right, Element),
  buildNodeAndRotateIfNeeded(NodeElement, Left, NewRightTree).

findAndDeleteMax({Found, _, _, {}}) -> {Found, {}};
findAndDeleteMax({NodeElement, _, Left, Right}) ->
  {Found, NewRight} = findAndDeleteMax(Right),
  {Found, buildNode(NodeElement, Left, NewRight)}.

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.

getBalance({}) -> 0;
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







