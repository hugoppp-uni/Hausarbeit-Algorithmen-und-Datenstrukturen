-module(splaytree).
-author("Hugo Protsch").

-export([findBT/2, equalBT/2, isEmptyBT/1, initBT/0, insertBT2/2, insertBT/2, printBT/2, splayLargestBT/1, deleteBT/2, isBT/1, inOrderBT/1, findTP/2]).

initBT() -> avltree:initBT().

isEmptyBT(BTree) -> avltree:isEmptyBT(BTree).

equalBT(BTree, BTree2) -> avltree:equalBT(BTree, BTree2).

printBT(BTree, Filename) -> avltree:printBT(BTree, Filename).

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
  io:format("num1~n"),
  {H, Res} = findTP2(BTree, SearchElement),
  if
    (Res == notfound) -> {0, {}};
    (Res == here) -> {H, BTree};
    true -> {H, Res}
  end.

findTP2({El, H, L, R}, SearchEl) when SearchEl < El ->
  io:format("starting~n"),
  {Height, NewLeft} = findTP2(L, SearchEl),
  if
    (NewLeft == here) -> {Height, avltree:rotateR({El, H, L, R})};
    (NewLeft == notfound) -> {0, here};
    true -> {Height, buildNode(El, NewLeft, R)}
  end;
findTP2({El, H, L, R}, SearchEl) when SearchEl > El ->
  {Height, NewRight} = findTP2(R, SearchEl),
  if
    (NewRight == here) -> {Height, avltree:rotateL({El, H, L, R})};
    (NewRight == notfound) -> {0, here};
    true -> {Height, buildNode(El, L, NewRight)}
  end;
findTP2({SearchEl, H, _, _}, SearchEl) -> {H, here};
findTP2({}, _) -> {0, notfound}.


findBT({SearchEl, H, L, R}, SearchEl) -> {H, {SearchEl, H, L, R}};
findBT(BTree, SearchEl) ->
  {{NewBTree, D}, FoundHeight} = findBT2(BTree, SearchEl),
  {FoundHeight, zigIfNeeded({NewBTree, D})}.

insertBT(BTree, InsertElement) ->
  zigIfNeeded(insertBT2(BTree, InsertElement)).

deleteBT(BTree, DeleteElement) ->
  {H, NewBTree} = findBT(BTree, DeleteElement),
  if
    (H == 0) -> BTree;
    true ->
      {_, _, L, R} = NewBTree,
      joinBT(L, R)
  end.

%% returns {{Root, direction}, H}
findBT2({El, H, L, R}, SearchEl) when SearchEl < El ->
  {{NewL, D}, FoundHeight} = findBT2(L, SearchEl),
  NewRoot = {El, H, NewL, R},
  {splay(NewRoot, left, D), FoundHeight};
findBT2({SearchEl, H, L, R}, SearchEl) -> {{{SearchEl, H, L, R}, here}, H};
findBT2({El, H, L, R}, SearchEl) ->
  {{NewR, D}, FoundHeight} = findBT2(R, SearchEl),
  NewRoot = {El, H, L, NewR},
  {splay(NewRoot, right, D), FoundHeight};
findBT2({}, _) -> {{{}, notfound}, 0}.

insertBT2({El, _, L, R}, InsertEl) when InsertEl < El ->
  {NewL, D} = insertBT2(L, InsertEl),
  NewRoot = {El, 0, NewL, R}, % height is recalculated when rotating
  splay(NewRoot, left, D);
insertBT2({El, _, L, R}, InsertEl) when InsertEl > El ->
  {NewR, D} = insertBT2(R, InsertEl),
  NewRoot = {El, 0, L, NewR}, % height is recalculated when rotating
  splay(NewRoot, right, D);
insertBT2({}, InsertEl) -> {{InsertEl, 1, {}, {}}, here};
insertBT2({InsertEl, H, L, R}, InsertEl) -> {{InsertEl, H, L, R}, here}.

splayLargestBT(BTree) -> zigIfNeeded(splayLargestBT2(BTree)).

splayLargestBT2({El, H, L, {}}) -> {{El, H, L, {}}, here};
splayLargestBT2({El, H, L, R}) ->
  {NewR, D} = splayLargestBT2(R),
  splay({El, H, L, NewR}, right, D);
splayLargestBT2({}) -> {{}, notfound}.

%%% returns {Root, here} if has been splayed, {Root, <left,right>} if element is parent of root.
splay(Root, D, here) -> {Root, D};
splay(Root, left, left) ->
  NewRoot = avltree:rotateR(Root),
  {avltree:rotateR(NewRoot), here};
splay(Root, right, right) ->
  NewRoot = avltree:rotateL(Root),
  {avltree:rotateL(NewRoot), here};
splay({El, H, L, R}, right, left) -> {avltree:rotateL({El, H, L, avltree:rotateR(R)}), here};
splay({El, H, L, R}, left, right) -> {avltree:rotateR({El, H, avltree:rotateL(L), R}), here};
splay(Root, left, notfound) -> {Root, here};
splay(Root, right, notfound) -> {Root, here}.

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
  {El, _, NewL, {}} = splayLargestBT(L),
  buildNode(El, NewL, R).

buildNode(Value, Left, Right) ->
  {Value, max(getHeight(Left), getHeight(Right)) + 1, Left, Right}.

getHeight({}) -> 0;
getHeight({_, Height, _, _}) -> Height.