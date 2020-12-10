-module(util).
%-compile(export_all).
-export([list2set/1, shuffle/1, list2string/1, slist_tointlist/1, writelist/2, readlist/1,
  sortliste/1, resortliste/1, randomliste/1, randomliste/3,
  attachStamp/2, attachStamp/1, attachEnding/2, to_String/1,
  float_to_int/1, atom_to_integer/1, floor/1, ceil/1, fib/1, even/1, odd/1, type_is/1,
  counting1/1, counting/2, countread/1, countreset/1, countstop/1,
  globalvar/1, setglobalvar/2, getglobalvar/1, globalvarreset/1, globalvarstop/1,
  logging/2, logstop/0, timeMilliSecond/0]).

-define(MILL, 1000000).
-define(ZERO, integer_to_list(0)).

%% -------------------------------------------
% entfernt Duplikate in der Liste
%
list2set([]) -> [];
list2set([H | T]) -> [H | [X || X <- list2set(T), X /= H]].

%% Mischt eine Liste
% Beispielaufruf: NeueListe = shuffle([a,b,c]),
shuffle(List) -> PList = [{rand:uniform(), Elem} || Elem <- List],
  [Elem || {_, Elem} <- lists:keysort(1, PList)].

%% Wandelt Liste in eine Zeichenketten Liste um
% Beispiel: util:list2string([1,2,3,4,5]). --> "1 2 3 4 5 \n"
list2string([]) -> "\n";
list2string([H | T]) -> lists:concat([H, " ", list2string(T)]).
% transformiert die Liste von Zeichenketten in eine Liste von Integer	
% Beispiel: util:slist_tointlist(["1","2","3","4","5"]). --> [1,2,3,4,5]					
slist_tointlist([]) -> [];
slist_tointlist([SInt | STail]) ->
  {IntS, Case} = string:to_integer(SInt),
  case IntS of
    error ->
      io:format("in slist_tointlist: Fehler beim transformieren: ~p\n", [Case]);
    _Any -> [IntS | slist_tointlist(STail)]
  end.

% Schreibt eine Liste von Zahlen in eine Datei
%writelist(List,Filename) -> file:write_file(Filename,io_lib:format("~w",[List])).
writelist([H | T], Filename) ->
  {ok, IODevice} = file:open(Filename, write),
  ok = io:format(IODevice, "[~.10B", [H]),
  write_list(IODevice, T),
  file:close(IODevice).
write_list(IODevice, [H | T]) ->
  ok = io:format(IODevice, ",~.10B", [H]),
  write_list(IODevice, T);
write_list(IODevice, []) ->
  ok = io:format(IODevice, "]", []).

% Liest eine solche Liste von Zahlen aus einer Datei
readlist(Filename) -> {Sign, ListBinary} = file:read_file(Filename),
  case Sign of
    ok -> slist_tointlist(string:tokens(binary_to_list(ListBinary), "[],"));
    error ->
      io:format("in readlist: Fehler beim Lesen von ~p: ~p\n", [Sign, ListBinary])
  end.

%% -------------------------------------------
% Erzeugt eine sortierte Liste mit Num Zahlen
% beginnend bei 1 bis einschließlich Num
%
sortliste(Num) ->
  lists:seq(1, Num).
% Erzeugt eine umgekehrt sortierte Liste mit Num Zahlen 
% beginnend bei Num bis einschließlich 1
resortliste(Num) ->
  lists:reverse(lists:seq(1, Num)).
% Erzeugt eine unsortierte Liste mit Num Zufallszahlen im Bereich 1 bis Num
% ohne Duplikate 
randomliste(Num) ->
  shuffle([X || X <- lists:seq(1, Num)]).
% Erzeugt eine unsortierte Liste mit Num Zufallszahlen im Bereich Min bis Max
% Duplikate sind möglich 
randomliste(Num, Min, Max) ->
  RangeInt = Max - Min,
  lists:flatten([rand:uniform(RangeInt + 1) + Min - 1 || _ <- lists:seq(1, Num)]).

%% -------------------------------------------
% setzt einen Zeitstempel an einen Namen und verbindet dies mit der Endung.
% Beispielaufruf: util:attachStamp(name,svg). --> 'name163025.svg'
%                 util:attachStamp('Name','JPG'). --> 'Name177902.JPG'
attachStamp(AtomName, FileEndung) ->
  {_MegaSecs, _Secs, MicroSecs} = erlang:timestamp(),
  list_to_atom(lists:concat([AtomName, to_String(MicroSecs), ".", FileEndung])).
% setzt einen Zeitstempel an einen Namen.
% Beispielaufruf: util:attachStamp(name). --> name334000
%                 util:attachStamp('Name'). --> 'Name991000'
attachStamp(AtomName) ->
  {_MegaSecs, _Secs, MicroSecs} = erlang:timestamp(),
  list_to_atom(lists:concat([AtomName, to_String(MicroSecs)])).
% setzt Endung an einen Namen ohne Zeitstempel.
% Beispielaufruf: util:attachEnding(name,svg). --> 'name.svg'
%                 util:attachEnding('Name','JPG'). --> 'Name.JPG'
attachEnding(AtomName, FileEndung) ->
  list_to_atom(lists:concat([AtomName, ".", FileEndung])).

% Wandelt in eine Zeichenkette um
% Beispielaufruf: to_String(Something),
%
to_String(Etwas) ->
  lists:flatten(io_lib:format("~p", [Etwas])).

%% Transformiert float nach int
% gerundet wird kaufmänisch: a.44 bzw. a.444 ergibt a, a.45 bzw. a.445 ergibt a+1
%
float_to_int(Float) -> list_to_integer(float_to_list(Float, [{decimals, 0}])).

%% Transformiert atom Zahl nach Integer
% Bsp: atom_to_integer('42') --> 42
%
atom_to_integer(X) -> list_to_integer(atom_to_list(X)).

% rundet die Zahl ab
% -a.999999999999999 wird auf -(a+1) gerundet
% a.999999999999999 wird auf a gerundet
floor(X) ->
  T = erlang:trunc(X),
  case (X - T) of
    Neg when Neg < 0 -> T - 1;
    Pos when Pos > 0 -> T;
    _ -> T
  end.

% rundet die Zahl auf
% -a.999999999999999 wird auf -a gerundet
% a.999999999999999 wird auf a+1 gerundet
ceil(X) ->
  T = erlang:trunc(X),
  case (X - T) of
    Neg when Neg < 0 -> T;
    Pos when Pos > 0 -> T + 1;
    _ -> T
  end.

%% Fibonacci 2-ter Ordnung
fib(N) -> fib_iter(N, 0, 1).
% iterative Implementierung
fib_iter(0, Result, _Next) -> Result;
fib_iter(Iter, Result, Next) when Iter > 0 ->
  fib_iter(Iter - 1, Next, Result + Next).

%% bestimmt, ob die Zahl gerade oder ungerade ist
even(Integer) -> (Integer >= 0) and (Integer rem 2 =:= 0).
odd(Integer) -> (Integer >= 1) and (Integer rem 2 =/= 0).

% Ermittelt den Typ
% Beispielaufruf: type_is(Something),
%
type_is(Something) ->
  if is_atom(Something) -> atom;
    is_binary(Something) -> binary;
    is_bitstring(Something) -> bitstring;
    is_boolean(Something) -> boolean;
    is_float(Something) -> float;
    is_function(Something) -> function;
    is_integer(Something) -> integer;
    is_list(Something) -> list;
    is_number(Something) -> number;
    is_pid(Something) -> pid;
    is_port(Something) -> port;
    is_reference(Something) -> reference;
    is_tuple(Something) -> tuple
  end.

%% -------------------------------------------
% Ein globaler Zähler
%
%% Addiert 1
counting1(Counter) -> counting(Counter, 1).

%% Addiert Step						 
counting(Counter, Step) -> %Known = erlang:whereis(Counter),
  % case Known of
  %	undefined -> PIDcountklc = spawn(fun() -> countloop(0) end),
  %				 erlang:register(Counter,PIDcountklc);
  %	_NotUndef -> ok
  % end,
  % Counter ! {count,Step},
  % ok.
  setglobalvar(Counter, max(getglobalvar(Counter), 0) + Step).
%% Auslesen des Wertes
countread(Counter) -> %Known = erlang:whereis(Counter),
  %case Known of
  %	undefined -> 0;
  %	_NotUndef ->
  %		Counter ! {get,self()},
  %		receive
  %			{current,Num} -> Num;
  %			_SomethingElse -> 0
  %		end
  %end.
  max(getglobalvar(Counter), 0).
%% Setzt Wert auf 0
countreset(Counter) ->  %Known = erlang:whereis(Counter),
  %case Known of
  %	undefined -> PIDcountklc = spawn(fun() -> countloop(0) end),
  %				erlang:register(Counter,PIDcountklc);
  %	_NotUndef -> Counter ! reset, true
  %end.
  setglobalvar(Counter, 0).
%% Beendet den Zählprozess
countstop(Counter) ->  %Known = erlang:whereis(Counter),
  %case Known of
  %	undefined -> false;
  %	_NotUndef -> Counter ! kill,
  %				erlang:unregister(Counter),
  %				true
  %end.
  globalvarstop(Counter).
%% Der nebenläufige Prozess					
%countloop(Count) -> receive
%						{count,Num} -> countloop(Count + Num);
%						{get,PID} -> PID ! {current,Count},
%									countloop(Count);
%						reset -> countloop(0);
%						kill -> true
%					end.

%% -------------------------------------------
%% Eine globale Variable
%
% startet Prozess und setzt die Variable auf den Wert nil
globalvar(VariableName) -> Known = erlang:whereis(VariableName),
  case Known of
    undefined -> PIDcountklc = spawn(fun() -> glvarloop(nil) end),
      erlang:register(VariableName, PIDcountklc);
    _NotUndef -> ok
  end,
  ok.
% Setzt die Variable auf einen Wert
setglobalvar(VariableName, Value) -> Known = erlang:whereis(VariableName),
  case Known of
    undefined -> PIDcountklc = spawn(fun() -> glvarloop(nil) end),
      erlang:register(VariableName, PIDcountklc);
    _NotUndef -> ok
  end,
  VariableName ! {writevar, Value},
  ok.
% Liest den Wert aus der Variablen aus
getglobalvar(VariableName) -> Known = erlang:whereis(VariableName),
  case Known of
    undefined -> nil;
    _NotUndef ->
      VariableName ! {get, self()},
      receive
        {current, Value} -> Value;
        _SomethingElse -> nil
      end
  end.
% Setzt den Wert der Variablen auf nil
globalvarreset(VariableName) -> Known = erlang:whereis(VariableName),
  case Known of
    undefined -> PIDcountklc = spawn(fun() -> glvarloop(nil) end),
      erlang:register(VariableName, PIDcountklc);
    _NotUndef -> VariableName ! reset
  end,
  true.
% Beendet den nebenläufigen Prozess 
globalvarstop(VariableName) -> Known = erlang:whereis(VariableName),
  case Known of
    undefined -> false;
    _NotUndef -> VariableName ! kill,
      erlang:unregister(VariableName),
      true
  end.

glvarloop(Value) -> receive
                      {writevar, NewValue} -> glvarloop(NewValue);
                      {get, PID} -> PID ! {current, Value},
                        glvarloop(Value);
                      reset -> glvarloop(nil);
                      kill -> true
                    end.

%% -------------------------------------------
% Schreibt ggf auf den Bildschirm und in eine Datei
% nebenläufig zur Beschleunigung
% Beispielaufruf: logging('FileName.log',"Textinhalt"),
%
% logging(_Datei,_Inhalt) -> ok;
% Schreibt Inhal in Datei, nebenläufig!
logging(Datei, Inhalt) -> Known = erlang:whereis(logklc),
  case Known of
    undefined -> PIDlogklc = spawn(fun() -> logloop(0) end),
      erlang:register(logklc, PIDlogklc);
    _NotUndef -> ok
  end,
  logklc ! {Datei, Inhalt},
  ok.
% Beendet den nebenläufigen Prozess
logstop() -> Known = erlang:whereis(logklc),
  case Known of
    undefined -> false;
    _NotUndef -> logklc ! kill,
      erlang:unregister(logklc),
      true
  end.
% der nebenläufige Prozess					
logloop(Y) -> receive
                {Datei, Inhalt} -> shell:strings(false),
                  %io:format(Inhalt),
                  file:write_file(Datei, Inhalt, [append]),
                  logloop(Y + 1);
                kill -> true
              end.

%% Zeitstempel: 'MM.DD HH:MM:SS,SSS'
% Beispielaufruf: Text = lists:concat([Clientname," Startzeit: ",timeMilliSecond()]),
%
timeMilliSecond() ->
  {_Year, Month, Day} = date(),
  {Hour, Minute, Second} = time(),
  Tag = lists:concat([klebe(Day, ""), ".", klebe(Month, ""), " ", klebe(Hour, ""), ":"]),
  {_, _, MicroSecs} = erlang:timestamp(),
  Tag ++ concat([Minute, Second], ":") ++ "," ++ toMilliSeconds(MicroSecs) ++ "|".
% Hilfsfunktionen
toMilliSeconds(MicroSecs) ->
  Seconds = MicroSecs / ?MILL,
  %% Korrektur, da string:substr( float_to_list(0.234567), 3, 3). 345 ergibt
  if (Seconds < 1) -> CorSeconds = Seconds + 1;
    (Seconds >= 1) -> CorSeconds = Seconds
  end,
  string:substr(float_to_list(CorSeconds), 3, 3).
concat(List, Between) -> concat(List, Between, "").
concat([], _, Text) -> Text;
concat([First | []], _, Text) ->
  concat([], "", klebe(First, Text));
concat([First | List], Between, Text) ->
  concat(List, Between, string:concat(klebe(First, Text), Between)).
klebe(First, Text) ->
  NumberList = integer_to_list(First),
  string:concat(Text, minTwo(NumberList)).
minTwo(List) ->
  case {length(List)} of
    {0} -> ?ZERO ++ ?ZERO;
    {1} -> ?ZERO ++ List;
    _ -> List
  end.


