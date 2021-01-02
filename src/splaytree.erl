-module(splaytree).
-author("Hugo Protsch").

-export([findBT2/2, findBT/2, equalBT/2, isEmptyBT/1, initBT/0, insertBT2/2, insertBT/2, printBT/2]).

initBT() -> avltree:initBT().

isEmptyBT(BTree) -> avltree:isEmptyBT(BTree).

equalBT(BTree, BTree) -> avltree:equalBT(BTree, BTree).

printBT(BTree, Filename) -> avltree:printBT(BTree, Filename).

findBT({SearchEl, H, L, R}, SearchEl) -> {H, {SearchEl, H, L, R}};
findBT(BTree, SearchEl) ->
  {{NewBTree, D}, FoundHeight} = findBT2(BTree, SearchEl),
  {FoundHeight, zigIfNeeded({NewBTree, D})}.

insertBT(BTree, InsertElement) ->
  zigIfNeeded(insertBT2(BTree, InsertElement)).

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
  NewRoot = {El, -1, NewL, R }, % height is recalculated when rotating
  splay(NewRoot, left, D);
insertBT2({El, _, L, R}, InsertEl) when InsertEl > El ->
  {NewR, D} = insertBT2(R, InsertEl),
  NewRoot = {El, -1, L, NewR}, % height is recalculated when rotating
  splay(NewRoot, right, D);
insertBT2({}, InsertEl) -> {{InsertEl, 1, {}, {}}, here};
insertBT2({InsertEl, H, L, R}, InsertEl) -> {{InsertEl, H, L, R}, here}.

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
splay(Root, _, notfound) -> {Root, notfound}.

zigIfNeeded({BTree, D}) ->
  if
    (D == here) -> BTree;
    (D == left) -> avltree:rotateR(BTree);
    (D == right) -> avltree:rotateL(BTree);
    (D == notfound) -> BTree;
    true -> err
  end.
