-module(zeitmessung).
-author("hugop").

%% API
-export([start/0, splay/0, avl/0]).

start() ->
  io:format("- - - - - - - - - - STARTING - - - - - - - - - - ~n"),
  avl(),
  splay(),
  io:format("- - - - - - - - - - FINISHED - - - - - - - - - - ~n")
.

mittlen_ueber() -> 20.

avl() -> io:format("- - - - - - - - - - - AVL - - - - - - - - - - - ~n"),
  avl(500, 1000, 50)
.
splay() -> io:format("- - - - - - - - - - SPLAY - - - - - - - - - - ~n"),
  messung(splay, 500, 1000, 20, rand),
  messung(splay, 500, 1000, 5, auf),
  messung(splay, 500, 1000, 5, ab)
.

avl(Start, Schritg, Anz) ->
  messung(avl, Start, Schritg, Anz, rand),
  messung(avl, Start, Schritg, Anz, auf),
  messung(avl, Start, Schritg, Anz, ab).

splay(Start, Schritg, Anz) ->
  messung(splay, Start, Schritg, Anz, rand),
  messung(splay, Start, Schritg, Anz, auf),
  messung(splay, Start, Schritg, Anz, ab).

messung(Typ, Start, Schrittgr, AnzSchritte, RandAufAb) ->
  io:format("---------------------------------------------------------------------------------------~n"),
  io:format(atom_to_list(Typ)),
  io:format(" mit "),
  io:format(atom_to_list(RandAufAb)),
  io:format(" zahlen~n~p +~p (~p mal), mittel ueber ~p~n",
    [Start, Schrittgr, AnzSchritte, mittlen_ueber()]),
  io:format("---------------------------------------------------------------------------------------~n"),
  if
    Typ == avl ->
      %Aufruf mit zeitBT:messung(<Anzahl Startelemente>,<Schrittgroee>,<Anzahl Schritte>,<DurchlΣufe pro Messung>,<rand|auf|ab>
      zeitAVLBT:messung(Start, Schrittgr, AnzSchritte, mittlen_ueber(), RandAufAb);
    Typ == splay ->
      zeitsBT:messung(Start, Schrittgr, AnzSchritte, mittlen_ueber(), RandAufAb);
    true -> err
  end.
