-module(zeitmessung).
-author("hugop").

%% API
-export([start/0]).

start() ->
  io:format("- - - - - - - - - - STARTING - - - - - - - - - - ~n"),
  avl(),
  io:format("- - - - - - - - - - FINISHED - - - - - - - - - - ~n")
  .

avl() -> io:format("- - - - - - - - - - - AVL - - - - - - - - - - - ~n"),
  avl1(),
  avl2()
.

avl1() ->
  messung(avl, 500, 1000, 50, 20, rand),
  messung(avl, 500, 1000, 50, 20, auf),
  messung(avl, 500, 1000, 50, 20, ab).

avl2() ->
  messung(avl, 10000, 10000, 100, 10, rand),
  messung(avl, 10000, 10000, 100, 10, auf),
  messung(avl, 10000, 10000, 100, 10, ab).

messung(Typ, Start, Schrittgr, AnzSchritte, MittelnUeber, RandAufAb) ->
  io:format("-----------------------------------------------~n"),
  io:format(atom_to_list(Typ)),
  io:format(" mit "),
  io:format(atom_to_list(RandAufAb)),
  io:format(" zahlen~n~p +~p (~p mal), mittel ueber ~p~n",
    [Start,Schrittgr,AnzSchritte, MittelnUeber]),
  io:format("-----------------------------------------------~n"),
  if
    Typ == avl ->
    %Aufruf mit zeitBT:messung(<Anzahl Startelemente>,<Schrittgroee>,<Anzahl Schritte>,<DurchlΣufe pro Messung>,<rand|auf|ab>
    zeitAVLBT:messung(Start, Schrittgr, AnzSchritte, MittelnUeber, RandAufAb);
    true -> err
  end.
