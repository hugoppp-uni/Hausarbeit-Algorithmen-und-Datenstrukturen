%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Author:
%%%   Hugo Protsch, 2020
%%%
%%%  Referenzen:
%%%   Referenzen beziehen sich, wenn nicht anders spezifiziert,
%%%   jeweils auf den entsprechenden Abschnitt im Entwurf.
%%%   Diese sind in Kommentaren mit einem '%' wie folgt kodiert:
%%%     - <R_> im Klartext zu finden
%%%     - <D_> Decision-Node (Raute) in einem Flussdiagramm
%%%     - <N_> Statement-Node (Rechteck) in einem Flussdiagramm
%%%
%%%  Anmerkungen:
%%%   Im Entwurf wurde die Höhe jeweils in den entsprechenden
%%%   Methoden berechnet. Dies wurde in der Implementation in
%%%   die Methode 'buildNodeAndRotateIfNeeded' und 'buildNode'
%%%   extrahiert.
%%%     Die Methode printBT() wurde erweitert. Nun wird an einer
%%%   Kante immer angegeben, ob es sich um das linke oder rechte
%%%   Kind handelt.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Implementation aus Binärbaum übernommen
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
    (Height == (max(getHeight(Left), getHeight(Right)) + 1)) and
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

% Implementation aus Binärbaum übernommen
inOrderBT({}) -> [];
inOrderBT({Element, _, Left, Right}) ->
  listAppend(inOrderBT(Left), [Element | inOrderBT(Right)]).

insertBT({}, Element) ->
  % <N1>
  {Element, 1, {}, {}};
insertBT({Element, Height, Left, Right}, Element) ->
  % <D2> -> <N3>
  {Element, Height, Left, Right};
insertBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  % <D2> -> <N2>
  NewLeft = insertBT(Left, Element),
  % <N5>, <N6>
  buildNodeAndRotateIfNeeded(NodeElement, NewLeft, Right);
insertBT({NodeElement, _, Left, Right}, Element) when Element > NodeElement ->
  % <D2> -> <N4>
  NewRight = insertBT(Right, Element),
  % <N5>, <N6>
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
% Referenzen beziehen sich auf Abbildung 4
buildNodeAndRotateIfNeeded(El, L, R) ->
  Wurzel = buildNode(El, L, R),
  % <D1>
  Balance = getBalance(L, R),
  if
    Balance == -2 ->
      % <D1> -> <D2>
      {WurzelEl, _, RotationsKnoten, NonRotate} = Wurzel,
      RotationsKnotenBalance = getBalance(RotationsKnoten),
      if
        RotationsKnotenBalance == 1 ->
          % <D2> -> <N12>
          NewRotate = rotateL(RotationsKnoten),
          % <N14>
          util:counting1(ddrightrotate),
          % <N13>
          rotateR(buildNode(WurzelEl, NewRotate, NonRotate));
        true ->
          % <D2> -> <N11>
          rotateR(Wurzel)
      end;
    Balance == 2 ->
      % <D1> -> <D3>
      {WurzelEl, _, NonRotate, RotationsKnoten} = Wurzel,
      RotationsKnotenBalance = getBalance(RotationsKnoten),
      if
        RotationsKnotenBalance == -1 ->
          % <D3> -> <N22>
          NewRotate = rotateR(RotationsKnoten),
          % <N24>
          util:counting1(ddleftrotate),
          % <N23>
          rotateL(buildNode(WurzelEl, NonRotate, NewRotate));
        true ->
          % <D3> -> <N21>
          rotateL(Wurzel)
      end;
  % <D1> -> <N1>
    true -> Wurzel
  end.

% Implementation aus Binärbaum übernommen
findBT({Element, Height, _, _}, Element) -> Height;
findBT({NodeElement, _, Left, _}, Element) when Element < NodeElement ->
  findBT(Left, Element);
findBT({NodeElement, _, _, Right}, Element) when Element > NodeElement ->
  findBT(Right, Element);
findBT(_, _) -> 0.

% <N1>
deleteBT({}, _) -> {};
deleteBT({Element, _, {}, Right}, Element) -> Right;
deleteBT({Element, _, Left, {}}, Element) -> Left;
deleteBT({Element, _, Left, Right}, Element) ->
  % <D1> -> <N2>
  {Found, NewLeftTree} = findAndDeleteMax(Left),
  % <N5>, <N6>, <N7>
  buildNodeAndRotateIfNeeded(Found, NewLeftTree, Right);
deleteBT({NodeElement, _, Left, Right}, Element) when Element < NodeElement ->
  % <D1> -> <N4>
  NewLeftTree = deleteBT(Left, Element),
  % <N6>, <N7>
  buildNodeAndRotateIfNeeded(NodeElement, NewLeftTree, Right);
deleteBT({NodeElement, _, Left, Right}, Element) ->
  % <N4> -> <N3>
  NewRightTree = deleteBT(Right, Element),
  % <N6>, <N7>
  buildNodeAndRotateIfNeeded(NodeElement, Left, NewRightTree).

% Referenzen beziehen sich auf Abbildung 3
%  <N12>
findAndDeleteMax({Found, _, Left, {}}) ->
  % <D11> -> <N12>, <N11>
  {Found, Left};
findAndDeleteMax({NodeElement, _, Left, Right}) ->
  % <D11> -> <N13>
  {Found, NewRight} = findAndDeleteMax(Right),
  % <N14>, <N15>, <N16>
  {Found, buildNodeAndRotateIfNeeded(NodeElement, Left, NewRight)}.

% siehe Formel 1
getBalance({}) -> 0;
getBalance({_, _, L, R}) -> getBalance(L, R).
getBalance(L, R) -> getHeight(R) - getHeight(L).

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.


%% Rotiert einen Knoten nach rechts,
%% die Höhe des Wurzel- und Rotationsknoten wird neu berechnet.
% Referenzen beziehen sich auf 1.1 (Algorithmus) Abschnitt "Rotation"
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
% Referenzen beziehen sich auf 2.1 Abschnitt "Rotation"
% und sind symmetrisch zu diesen.
rotateL({WurzelKnotenEl, _, WurzelL, {RotationsKnotenEl, _, RotationsL, RotationsR}}) ->
  util:counting1(leftrotate),
  %                                           |Operation 2|
  NewLeft = buildNode(WurzelKnotenEl, WurzelL, RotationsL),
  %                         |Operation 1|
  buildNode(RotationsKnotenEl, NewLeft, RotationsR)
% Rückgabe = Operation 3
.

buildNode(Value, Left, Right) ->
  {Value, max(getHeight(Left), getHeight(Right)) + 1, Left, Right}.


printBT(Tree, Filename) ->
  {ok, IODevice} = file:open(Filename, write),
  io:format("digraph G{~n", []),
  io:format(IODevice, "digraph G{~n", []),
  printBT2(Tree, IODevice),
  printHeightAndBalance(Tree, IODevice),
  io:format("}", []),
  io:format(IODevice, "}", []).
printBT2({From, H, {}, {To, ToH, ToL, ToR}}, IODevice) ->
  printElement(IODevice, From, To, r),
  printBT2({To, ToH, ToL, ToR}, IODevice);
printBT2({From, H, {To, ToH, ToL, ToR}, {}}, IODevice) ->
  printElement(IODevice, From, To, l),
  printBT2({To, ToH, ToL, ToR}, IODevice);
printBT2({El, _, {}, {}}, IODevice) -> ok;
printBT2({El, H, L, R}, IODevice) ->
  printBT2({El, H, L, {}}, IODevice),
  printBT2({El, H, {}, R}, IODevice);
printBT2({}, _IODevice) -> ok.
printElement(IODevice, From, To, LR) ->
  % Erweiterung, wie in Anmerkungen beschrieben
  io:format(IODevice, "~p -> ~p[label = ~p];~n", [From, To, atom_to_list(LR)]).

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
    true -> io:format(IODevice,
      "~p [label = \"~p\\n(~p)\" color=red];~n", [Value, Value, Height])
  end.
