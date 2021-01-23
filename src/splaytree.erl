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
%%%   Die im Abschnitt "FindBT" <R1> / <R2> erwähnte Funktionalität wurde
%%%   in "Splay" verschoben, da diese auch für  "FindTP" benötigt wird.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(splaytree).
-author("Hugo Protsch").

-export([findBT/2, equalBT/2, isEmptyBT/1, initBT/0, insertBT2/2, insertBT/2, printBT/2,
  splayLargestBT/1, deleteBT/2, isBT/1, inOrderBT/1, findTP/2, splay/3]).

initBT() -> avltree:initBT().

isEmptyBT(BTree) -> avltree:isEmptyBT(BTree).

equalBT(BTree, BTree2) -> avltree:equalBT(BTree, BTree2).

printBT(BTree, Filename) -> avltree:printBT(BTree, Filename).

% Implementation übernommen aus btree.erl
isBT(Btree) -> isBT(Btree, -1, ok).
isBT({}, _, _) -> true;
isBT({Element, Height, {}, {}}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height)
    and (Height == 1);
isBT({Element, Height, Left, {}}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == getHeight(Left) + 1) and
    (isBT(Left, LowerLimit, Element));
isBT({Element, Height, {}, Right}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == getHeight(Right) + 1) and
    (isBT(Right, Element, UpperLimit));
isBT({Element, Height, Left, Right}, LowerLimit, UpperLimit) ->
  basicChecks(Element, LowerLimit, UpperLimit, Height) and
    (Height == (max(getHeight(Left), getHeight(Right)) + 1)) and
    (isBT(Left, LowerLimit, Element)) and
    (isBT(Right, Element, UpperLimit)).
basicChecks(Element, LowerLimit, UpperLimit, Height) ->
  (is_integer(Element)) and
    (Height > 0) and
    (Element >= 0) and
    (Element > LowerLimit) and
    (Element < UpperLimit).


inOrderBT(BTree) -> avltree:inOrderBT(BTree).

findTP({SearchElement, H, L, R}, SearchElement) -> {H, {SearchElement, H, L, R}};
findTP(BTree, SearchElement) ->
  {H, Res} = findTP2(BTree, SearchElement),
  if
    (Res == notfound) -> {0, {}};
  % <R2>
    (Res == here) -> {H, BTree};
    true -> {H, Res}
  end.

findTP2({El, H, L, R}, SearchEl) when SearchEl < El ->
  {Height, NewLeft} = findTP2(L, SearchEl),
  if
  % <R2>
    (NewLeft == here) -> {Height, avltree:rotateR({El, H, L, R})};
  % <R5>
    (NewLeft == notfound) -> {0, here};
  % <R3>
    true -> {Height, buildNode(El, NewLeft, R)}
  end;
findTP2({El, H, L, R}, SearchEl) when SearchEl > El ->
  {Height, NewRight} = findTP2(R, SearchEl),
  if
  % <R2>
    (NewRight == here) -> {Height, avltree:rotateL({El, H, L, R})};
  % <R5>
    (NewRight == notfound) -> {0, here};
  % <R3>
    true -> {Height, buildNode(El, L, NewRight)}
  end;
% <R1>
findTP2({SearchEl, H, _, _}, SearchEl) -> {H, here};
% <R4>
findTP2({}, _) -> {0, notfound}.


findBT({SearchEl, H, L, R}, SearchEl) -> {H, {SearchEl, H, L, R}};
findBT(BTree, SearchEl) ->
  {{NewBTree, D}, FoundHeight} = findBT2(BTree, SearchEl),
  {FoundHeight, zigIfNeeded({NewBTree, D})}.

insertBT(BTree, InsertElement) ->
  zigIfNeeded(insertBT2(BTree, InsertElement)).

deleteBT(BTree, DeleteElement) ->
  %<R1>
  {H, NewBTree} = findBT(BTree, DeleteElement),
  if
    (H == 0) ->
      NewBTree;
    true ->
      {_, _, L, R} = NewBTree,
      %<R2>
      Res = joinBT(L, R),
      Res
  end.

%% returns {{Root, direction}, H}
% Abbildung 9
findBT2({El, H, L, R}, SearchEl) when SearchEl < El ->
  % <N2>
  {{NewL, D}, FoundHeight} = findBT2(L, SearchEl),
  NewRoot = {El, H, NewL, R},
  % <N6> / <N8> / <N9>
  {splay(NewRoot, left, D), FoundHeight};
% <N0>
findBT2({SearchEl, H, L, R}, SearchEl) -> {{{SearchEl, H, L, R}, here}, H};
findBT2({El, H, L, R}, SearchEl) ->
  % <N1>
  {{NewR, D}, FoundHeight} = findBT2(R, SearchEl),
  NewRoot = {El, H, L, NewR},
  % <N5> / <N7> / <N9>
  {splay(NewRoot, right, D), FoundHeight};
% <R1>
findBT2({}, _) -> {{{}, notfound}, 0}.

insertBT2({El, _, L, R}, InsertEl) when InsertEl < El ->
  % <R1>
  {NewL, D} = insertBT2(L, InsertEl),
  % <R2>
  splay({El, 0, NewL, R}, left, D); %% height is recalculated when rotating
insertBT2({El, _, L, R}, InsertEl) when InsertEl > El ->
  % <R1>
  {NewR, D} = insertBT2(R, InsertEl),
  % <R2>
  splay({El, 0, L, NewR}, right, D); %% height is recalculated when rotating
% <R3>
insertBT2({}, InsertEl) -> {{InsertEl, 1, {}, {}}, here};
% FindBT <D1> -> <N0>
insertBT2({InsertEl, H, L, R}, InsertEl) -> {{InsertEl, H, L, R}, here}.

splayLargestBT(BTree) -> zigIfNeeded(splayLargestBT2(BTree)).

splayLargestBT2({El, H, L, {}}) -> {{El, H, L, {}}, here};
splayLargestBT2({El, H, L, R}) ->
  {NewR, D} = splayLargestBT2(R),
  splay({El, H, L, NewR}, right, D);
splayLargestBT2({}) -> {{}, notfound}.


%% returns {Root, here} if has been splayed, {Root, <left,right>} if element is parent of root.
% Abschnitt "FindBT" <R1>
splay(Root, left, notfound) -> {Root, here};
% Abschnitt "FindBT" <R1>
splay(Root, right, notfound) -> {Root, here};
% Abbildung 9 <D2> <D3>
splay(Root, D, here) -> {Root, D};
% <R1>
splay(Root, left, left) ->
  NewRoot = avltree:rotateR(Root),
  {avltree:rotateR(NewRoot), here};
% <R2>
splay(Root, right, right) ->
  NewRoot = avltree:rotateL(Root),
  {avltree:rotateL(NewRoot), here};
% <R3>
splay({El, H, L, R}, right, left) -> {avltree:rotateL({El, H, L, avltree:rotateR(R)}), here};
% <R4>
splay({El, H, L, R}, left, right) -> {avltree:rotateR({El, H, avltree:rotateL(L), R}), here}.

% Abbildung 10
zigIfNeeded({BTree, D}) ->
  if
    (D == here) -> BTree;
    (D == left) -> avltree:rotateR(BTree);
    (D == right) -> avltree:rotateL(BTree);
    (D == notfound) -> BTree;
    true -> err
  end.

joinBT({}, R) -> R;
joinBT(L, R) ->
  % <R1>
  {El, _, NewL, {}} = splayLargestBT(L),
  % <R2>
  buildNode(El, NewL, R).

buildNode(Value, Left, Right) ->
  {Value, max(getHeight(Left), getHeight(Right)) + 1, Left, Right}.

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.
