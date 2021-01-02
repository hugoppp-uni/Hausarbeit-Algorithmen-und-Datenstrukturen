-module(splaytree).
-author("Hugo Protsch").

-export([findBT2/2, findBT/2, equalBT/2, isEmptyBT/1, initBT/0, insertBT2/2, insertBT/2, printBT/2]).

initBT() -> avltree:initBT().

isEmptyBT(BTree) -> avltree:isEmptyBT(BTree).

equalBT(BTree, BTree) -> avltree:equalBT(BTree, BTree).

printBT(BTree, Filename) -> avltree:printBT(BTree, Filename).

findBT({SearchEl, H, L, R}, SearchEl) -> {H, {SearchEl, H, L, R}};
findBT(BTree, SearchEl) ->
  {NewBTree, D, FoundHeight} = findBT2(BTree, SearchEl),
  if
    (D == notfound) -> {BTree, 0};
    true -> {FoundHeight, zigIfNeeded({NewBTree, D})}
  end.

insertBT(BTree, InsertElement) ->
  zigIfNeeded(insertBT2(BTree, InsertElement)).

findBT2({El, H, L, R}, SearchEl) when SearchEl < El ->
  {NewL, D, FoundHeight} = findBT2(L, SearchEl),
  NewRoot = {El, H, NewL, R},
  if
    (D == here) -> {NewRoot, left, FoundHeight};
    (D == notfound) -> {NewRoot, notfound, FoundHeight};
    true -> {splay(NewRoot, left, D), here, FoundHeight}
  end;
findBT2({SearchEl, H, L, R}, SearchEl) -> {{SearchEl, H, L, R}, here, H};
findBT2({El, H, L, R}, SearchEl) ->
  {NewR, D, FoundHeight} = findBT2(R, SearchEl),
  NewRoot = {El, H, L, NewR},
  if
    (D == here) -> {NewRoot, right, FoundHeight};
    (D == notfound) -> {NewRoot, notfound, FoundHeight};
    true -> {splay(NewRoot, right, D), here, FoundHeight}
  end;
findBT2({}, _) -> {{}, notfound, 0}.

insertBT2({El, H, L, R}, InsertEl) when InsertEl < El ->
  {NewL, D} = insertBT2(L, InsertEl),
  NewRoot = {El, -1, NewL, R }, % height is recalculated when rotating
  if
    (D == here) -> {NewRoot, left};
    true -> {splay(NewRoot, left, D), here}
  end;
insertBT2({El, H, L, R}, InsertEl) when InsertEl > El ->
  {NewR, D} = insertBT2(R, InsertEl),
  NewRoot = {El, -1, L, NewR}, % height is recalculated when rotating
  if
    (D == here) -> {NewRoot, right};
    true -> {splay(NewRoot, right, D), here}
  end;
insertBT2({}, InsertEl) -> {{InsertEl, 1, {}, {}}, here};
insertBT2({InsertEl, H, L, R}, InsertEl) -> {{InsertEl, H, L, R}, here}.

splay(Root, left, left) ->
  NewRoot = avltree:rotateR(Root),
  avltree:rotateR(NewRoot);
splay(Root, right, right) ->
  NewRoot = avltree:rotateL(Root),
  avltree:rotateL(NewRoot);
splay({El, H, L, R}, right, left) -> avltree:rotateL({El, H, L, avltree:rotateR(R)});
splay({El, H, L, R}, left, right) -> avltree:rotateR({El, H, avltree:rotateL(L), R}).

zigIfNeeded({NewBTree, D}) ->
  if
    (D == here) -> NewBTree;
    (D == left) -> avltree:rotateR(NewBTree);
    (D == right) -> avltree:rotateL(NewBTree);
    true -> err
  end.
