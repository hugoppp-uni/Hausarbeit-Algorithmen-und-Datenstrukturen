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
    % balance
  (Height == 2) and
    (isBT(Left, LowerLimit, Element));
isBT({Element, Height, {}, Right}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    % balance
  (Height == 2) and
    (isBT(Right, Element, UpperLimit));
isBT({Element, Height, Left, Right}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == (maxInt(getHeight(Left), getHeight(Right)) + 1)) and
    % balance
  (abs(getBalance(Left, Right)) < 2) and
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


%% Überprüft die AVL-Bedingung und führt
%% Einzel- / Doppelrotationen durch, falls
%% diese verletzt wird.
%%
%% Gibt einen Knoten zurück
buildNodeAndRotateIfNeeded({El, _, L, R}) ->
  buildNodeAndRotateIfNeeded(El, L, R).
%% Setzt einen Knoten zusammen.
%% Überprüft die AVL-Bedingung und führt
%% Einzel- / Doppelrotationen durch, falls
%% diese verletzt wird.
%%
%% Gibt einen Knoten zurück
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
          %% 3. Left Right Case
          NewRotate = rotateL(RotationsKnoten),
          util:counting1(ddrightrotate),
          rotateR(buildNode(WurzelEl, NewRotate, NonRotate));
        true ->
          %% 1. Left Left Case
          rotateR(Wurzel)
      end;
    Balance == 2 ->
      %% Right Left Case <OR> Right Right Case
      {WurzelEl, _, NonRotate, RotationsKnoten} = Wurzel,
      RotationsKnotenBalance = getBalance(RotationsKnoten),
      if
        RotationsKnotenBalance == -1 ->
          %% 4. Right Left Case
          NewRotate = rotateR(RotationsKnoten),
          util:counting1(ddleftrotate),
          rotateL(buildNode(WurzelEl, NonRotate, NewRotate));
        true ->
          %% 2. Right Right Case
          rotateL(Wurzel)
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
  {Found, buildNodeAndRotateIfNeeded(NodeElement, Left, NewRight)}.

% siehe Formel 1
getBalance({}) -> 0;
getBalance({_, _, L, R}) -> getBalance(L, R).
getBalance(L, R) -> getHeight(R) - getHeight(L).

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.


%% Rotiert einen Knoten nach rechts,
%% die Höhe des Wurzel- und Rotationsknoten wird neu berechnet.
%% Referenzen beziehen sich auf 2.1 Abschnitt "Rotation"
rotateR({WurzelKnotenEl, _, {RotationsKnotenEl, _, RotationsL, RotationsR}, WurzelR}) ->
  util:counting1(rightrotate),
  %                                   |Operation 2|
  NewRight = buildNode(WurzelKnotenEl, RotationsR, WurzelR),
  %                                      |Operation 1|
  buildNode(RotationsKnotenEl, RotationsL, NewRight)
% Rückgabe = Operation 3
.

%% Rotiert einen Knoten nach links,
%% die Höhe des Wurzel- und Rotationsknoten wird neu berechnet.
%% Referenzen beziehen sich auf 2.1 Abschnitt "Rotation"
%% und sind symmetrisch zu diesen.
rotateL({WurzelKnotenEl, _, WurzelL, {RotationsKnotenEl, _, RotationsL, RotationsR}}) ->
  util:counting1(leftrotate),
  %                                           |Operation 2|
  NewLeft = buildNode(WurzelKnotenEl, WurzelL, RotationsL),
  %                         |Operation 1|
  buildNode(RotationsKnotenEl, NewLeft, RotationsR)
% Rückgabe = Operation 3
.

buildNode(Value, Left, Right) ->
  {Value, maxInt(getHeight(Left), getHeight(Right)) + 1, Left, Right}.


printBT(Tree, Filename) ->
  {ok, IODevice} = file:open(Filename, write),
  io:format("digraph G{~n", []),
  io:format(IODevice, "digraph G{~n", []),
  printBT2(Tree, IODevice),
  printHeightAndBalance(Tree, IODevice),
  io:format("}", []),
  io:format(IODevice, "}", []).
printBT2({From, H, {}, {To, ToH, ToL, ToR}}, IODevice) ->
  printElement(IODevice, From, To, ToH),
  printBT2({To, ToH, ToL, ToR}, IODevice);
printBT2({From, H, {To, ToH, ToL, ToR}, {}}, IODevice) ->
  printElement(IODevice, From, To, ToH),
  printBT2({To, ToH, ToL, ToR}, IODevice);
printBT2({El, _, {}, {}}, IODevice) -> ok;
%%  io:format("~p;~n", [El]),
%%  io:format(IODevice, "~p;~n", [El]);
printBT2({El, H, L, R}, IODevice) ->
  printBT2({El, H, L, {}}, IODevice),
  printBT2({El, H, {}, R}, IODevice);
printBT2({}, _IODevice) -> ok.
printElement(IODevice, From, To, Height) ->
  io:format("~p -> ~p;~n", [From, To]),
  io:format(IODevice, "~p -> ~p;~n", [From, To]).

printHeightAndBalance({}, _) -> ok;
printHeightAndBalance({Value, H, L, R}, IODevice) ->
  printHeightAndBalance(IODevice, Value, getBalance(L, R), H),
  printHeightAndBalance(L, IODevice),
  printHeightAndBalance(R, IODevice).
printHeightAndBalance(IODevice, Value, Balance, Height) ->
  if
    Balance == -1 -> io:format(IODevice,
      "~p [label = \"~p\\n(~p)\" color=purple];~n", [Value, Value, Height]);
    Balance == 0 -> io:format(IODevice,
      "~p [label = \"~p\\n(~p)\" color=black];~n", [Value, Value, Height]);
    Balance == 1 -> io:format(IODevice,
      "~p [label = \"~p\\n(~p)\" color=blue];~n", [Value, Value, Height]);
    true -> io:format(IODevice, "~p\\n(~p)[color=red]~n", [Value, Height])
  end.






