
%=====DECYZJE PODJETE PRZY IMPLEMENTACJI NIEDOSPECYFIKOWANYCH REGUL=====
%
% - Operacje wyboru bitow:
%   1)Dla pojedynczego bitu:
%      *Proba dostepu poza tablica generuje blad wykonaia
%
%
%   2)Dla wielu bitow
%      *Gdy poczatek lub koniec poza tablica - blad wykonania
%      *Gdy poczatek > koniec - blad wykonania
%
% - Wywolanie funkcji
%   1)Wywolwyna jest pierwsza funckcja ktorej nazwa unifikuje sie z
%   nazwa definiji np. dla programu:
%
%   def f(x,y) = 3
%   def f(x) = 2
%
%   zawsze bedzie nastepowala proba wywolania definicji: def f(x,y) = 3
%   interpreter nie obsluguje przeciazania funcji zatem w tym przypadku
%   wywolanie f(4) zakonczy sie bledem wykonania
%
%   2) W przypadku braku definicji o rzadanej nazwie - blad wykonania
%
% - Rozszerzanie srdodwiska
%   *) W przypadku gdy chcemy rozszerzyc srodowisko o zmienna x, a
%   zawiera ono juz wartosc x, zostaje stworzony nowy lokalny
%   zakres dla nazwy x np. wartoscia wyrazenia:
%
%   let x = 5 in (let x = 2 in x) + x
%
%   bedzie 7




:- module(konrad_werblinski_eval, [run/5]).
%odkomentowac!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%

:- op(200, fx, ~).
:- op(500, xfy, v).

% =================Inicjacja i uruchomienie predykatu eval==========

run(Program, FName, Arg, Value, Clauses) :-
  eval(Program, [(initial_input, Arg)], call(init, FName, var(init, initial_input)), Value, Clauses).

%====================Predykaty pomocnicze oglone==================

isList([]).
isList([_|_]).

join([], A, A).     %laczy listy olewajac kolejnosc
join([H|T], A, W) :-
  join(T, [H|A], W).

%====================Predykaty pomocnicze interpretacji============

%value_from_env(Env, Var, Val)
%Wyszukuje w srodowisku wartosc Val zmiennej Var

value_from_env([], var(Pos, Name), _) :- throw(runtime_error(('Uninitialized variable', Name), Pos)).
value_from_env([(Name, Value)|_], var(_, Name), Value) :- !.
value_from_env([_|T], Var, Value) :- value_from_env(T, Var, Value).


%seek_func(Prog, Name, Def, Pos)
%Wyszukuje w programie definicje dla funkcji o zadanej nazwie
seek_func([], Name, _, Pos) :-
  throw(runtime_error(('No definition for function: ', Name), Pos)).
seek_func([def(Name, Pat, Body)|_], Name, def(Name, Pat, Body), _) :- !.
seek_func([_|T], Name, Def, Pos) :- seek_func(T, Name, Def, Pos).

%match_pattern(P, Val, Env1, Env2, Pos).
%dopasowuje wzorzec i rozszerza podane srodowisko Env1 do Env2

match_pattern(wildcard(_), _, Env, Env, _) :- !.
match_pattern(var(_, Name), Val, Env, [(Name, Val)|Env], _) :- !.
match_pattern(pair(PosP, P1, P2), Val, Env, Env2, Pos) :-
  (
    (	Val = (Val1, Val2) , ! );
    (	throw(runtime_error(('Expected pair as a value, found: ', Val), PosP)) )
  ),
  match_pattern(P1, Val1, Env, Env1, Pos),
  match_pattern(P2, Val2, Env1, Env2, Pos), !.

match_pattern(Pat, Val, _, _, Pos) :-
  throw(runtime_error(('Cannot match value ', Val, ' with pattern: ', Pat), Pos)).

%elem_at_index(Vector, Index, Value, Pos)
%Pobiera z listy element z zadanej pozycji

elem_at_index(Vector, Index, Value, Pos) :- elem_at_index(Vector, 0, Index, Value, Pos).

elem_at_index([], Len, Index, _, Pos) :-
  throw(runtime_error(('Index out of range, try to access position: ', Index, ' in array of length: ', Len), Pos)).

elem_at_index([H|_], Index, Index, H, _) :- !.
elem_at_index([_|T], I, Index, Val, Pos) :-
  I2 is I + 1,
  elem_at_index(T, I2, Index, Val, Pos).

%range_from_array(Array, BeginI, EndI, Range, Pos)
%Pobiera z listy elementy w zadanym przedziale

range_from_array(_, BeginI, EndI, _, Pos) :-
  BeginI > EndI,
  throw(runtime_error((('Begin index: ', BeginI , ' bigger than end index: ', EndI, ' in bit selection'), Pos)) ) .


range_from_array(Array, BeginI, EndI, Range, Pos) :-
  range_from_array(Array, 0, BeginI, EndI, A-A, Range, Pos).

range_from_array([], Len, BeginI, EndI, Range-[], Range, Pos) :-
  (
    (
       (   (Len =< BeginI) ; (0 > BeginI) ),
       throw(runtime_error((('Index out of range, try to access position: ', BeginI, ' in array of length: ', Len), Pos)) )
    );
    (
       (   (Len =< EndI) ; ( 0 > EndI) ),
       throw(runtime_error((('Index out of range, try to access position: ', EndI, ' in array of length: ', Len), Pos)) )
    );
    (	true )
  ).

range_from_array([H|T], I, BeginI, EndI, A-X, Range, Pos) :-
  (
    (	I >= BeginI, I =< EndI, X = [H|X2], ! );
    (	X2 = X )
  ),
  I1 is I + 1,
  range_from_array(T, I1,  BeginI, EndI, A-X2, Range, Pos).

%Cztery ponizsze funkcje realizuja operacje bitowe bit po bicie
%Generuja wartosc oraz zbior efektow ubocznych

%perform_not(In, Out, Cons)

perform_not(In, Out, Cons) :-
  perform_not(In, AO-AO, Out, [], Cons).

perform_not([], Out-[], Out, Cons, Cons).

perform_not([H|T], AO-X, Out, AC, Cons) :-
  gensym(wire, Sig),
  !,
  X = [Sig | X2],
  perform_not(T, AO-X2, Out, [Sig v H ,~Sig v ~H|AC], Cons).


%perform_and(In1, In2, Out, Cons)

perform_and(In1, In2, Out, Cons) :-
  perform_and(In1, In2, AO-AO, Out, [], Cons).

perform_and([], [], Out-[], Out, Cons, Cons).
perform_and([B|T1], [C|T2], AO-X, Out, AC, Cons) :-
  gensym(wire, Sig),
  !,
  X = [Sig| X2],
  perform_and(T1, T2, AO-X2, Out, [Sig v ~B v ~C, ~Sig v B, ~Sig v C|AC], Cons).

%perform_or(In1, In2, Out, Cons)

perform_or(In1, In2, Out, Cons) :-
  perform_or(In1, In2, AO-AO, Out, [], Cons).

perform_or([], [], Out-[], Out, Cons, Cons).
perform_or([B|T1], [C|T2], AO-X, Out, AC, Cons) :-
  gensym(wire, Sig),
  !,
  X = [Sig| X2],
  perform_or(T1, T2, AO-X2, Out, [~Sig v B v C, Sig v ~B, Sig v ~C|AC], Cons).

%perform_xor(In1, In2, Out, Cons)

perform_xor(In1, In2, Out, Cons) :-
  perform_xor(In1, In2, AO-AO, Out, [], Cons).

perform_xor([], [], Out-[], Out, Cons, Cons).
perform_xor([B|T1], [C|T2], AO-X, Out, AC, Cons) :-
  gensym(wire, Sig),
  !,
  X = [Sig| X2],
  perform_xor(T1, T2, AO-X2, Out, [Sig v B v ~C, Sig v ~B v C, ~Sig v ~B v ~C, ~Sig v B v C|AC], Cons).



%===============================eval===================================
%Relacja eval w postaci
%eval(Prog, Env, Exp, Val, Cons).
%
% Zdecydowalem sie na implementacje bez DCG poniewaz wydaje mi sie ona
% bardziej intuicyjna i czytelna oraz blizsza naturalnej definicji tej
% relacji
%
%Prog zbiorem abstrakcyjnych drzew rozbioru wygenerowanych przez parser
%Env jest srodowiskiem w postaci listy par (Nazwa, Wartosc)
%Exp jest aktualnie ewaluowanym wyrazeniem
%Val jest wartoscia Exp
%Cons jest zbiorem klauzul opisujacych uklad


eval(_, _, num(_, N), N, []) :- !. %liczba
eval(_, _, empty(_), [], []) :- !. %pusty wektor

eval(_, Env, Var, Val, []) :- value_from_env(Env, Var, Val), !. %zmienna

eval(Prog, Env, bit(Pos, E), [A], Cons) :- %pojedynczy bit
  eval(Prog, Env, E, V1, C),
  (
      (	integer(V1), V1 =:= 1, !, gensym(wire, A), Cons =  [A|C] );
      (	integer(V1), V1 =:= 0, !, gensym(wire, A), Cons = [~A|C] );
      ( throw(runtime_error(('Expected 0 or 1, found: ', V1), Pos)) )
  ),
  !.

eval(Prog, Env, bitsel(Pos, E1, E2), [Bit], Cons) :- %wybor pojedynczego bitu
  eval(Prog, Env, E1, V1, C1),
  eval(Prog, Env, E2, V2, C2),
  (
      (	  isList(V1), integer(V2), ! );
      (	  \+ isList(V1), throw(runtime_error(('Expected vector, found: ', V1), Pos)) );
      (	  \+ integer(V2), throw(runtime_error(('Expected integer, found: ', V2), Pos))  )
  ),
  elem_at_index(V1, V2, Bit, Pos),
  join(C1, C2, Cons),
  !.

eval(Prog, Env, bitsel(Pos, E1, E2, E3), Val , Cons) :- %wybor wielu bitow
  eval(Prog, Env, E1, V1, C1),
  eval(Prog, Env, E2, V2, C2),
  eval(Prog, Env, E3, V3, C3),
  (
      (	  isList(V1), integer(V2), integer(V3),  ! );
      (	  \+ isList(V1), throw(runtime_error(('Expected vector, found: ', V1), Pos)) );
      (	  \+ integer(V2), throw(runtime_error(('Expected integer, found: ', V2), Pos))  );
      (	  \+ integer(V3), throw(runtime_error(('Expected integer, found: ', V3), Pos))  )
  ),
  range_from_array(V1, V3, V2, Val, Pos),
  join(C1, C2, C12),
  join(C12, C3, Cons),
  !.


eval(P, Env, pair(_, E1, E2), (V1, V2), Cons) :- %para jako wyrazenie
  eval(P, Env, E1, V1, C1),
  eval(P, Env, E2, V2, C2),
  join(C1, C2, Cons),
  !.

eval(P, Env, op(Pos, '#', E1), Val, Cons) :- %dlugosc wektora
  eval(P, Env, E1, V1, Cons),
  (
      (	 isList(V1), ! );
      (	 throw(runtime_error(('Expected vector, found: ', V1), Pos) ) )
  ),
  length(V1, Val),
  !.

eval(P, Env, op(Pos, '@', Lt, Rt), Val, Cons) :- %konkatenacja
  eval(P, Env, Lt, ValL, C1),
  eval(P, Env, Rt, ValR, C2),
  (
      (	 isList(ValL) , isList(ValR), ! );
      (	 \+ isList(ValL), throw(runtime_error(('Expected vector, found: ', ValL), Pos)) );
      (	 \+ isList(ValR), throw(runtime_error(('Expected vector, found: ', ValR), Pos)) )
  ),
  append(ValR, ValL, Val),
  join(C1, C2, Cons),
  !.

eval(P, Env, op(Pos, '~', E1), Val, Cons) :- %negacja
  eval(P, Env, E1, V1, C1),
  (
      (	 isList(V1), ! );
      (	 throw(runtime_error(('Expected vector, found: ', V1), Pos) ) )
  ),
  perform_not(V1, Val, C2),
  join(C1, C2, Cons),
  !.

eval(P, Env, op(Pos, Op, E1, E2), Val, Cons) :- %opereatory logiczne $ | ^
  member(Op, ['|', '^', '&']),
  eval(P, Env, E1, V1, C1),
  eval(P, Env, E2, V2, C2),
  (
      (	 isList(V1) , isList(V2), ! );
      (	 \+ isList(V1), throw(runtime_error(('Expected vector, found: ', V1), Pos)) );
      (	 \+ isList(V2), throw(runtime_error(('Expected vector, found: ', V2), Pos)) )
  ),
  (
      (
          length(V1, L1),
          length(V2, L2),
          L1 =\= L2,
          throw(runtime_error(('Vectors: ', V1, V2, ' have diffrent lengths'), Pos))
      );
      (	  Op = '&', !, perform_and(V1, V2, Val, C3) );
      (	  Op = '|', !, perform_or(V1, V2, Val, C3) );
      (	  Op = '^', !, perform_xor(V1, V2, Val, C3) )
  ),
  join(C1, C2, C12),
  join(C12, C3, Cons),
  !.


eval(P, Env, op(Pos, '-', E1), Val, Cons) :- % '-' jedno argumentowy
  eval(P, Env, E1, V1, Cons),
  (
      (	 integer(V1), ! ) ;
      (	 throw(runtime_error(('Expected integer, found: ', V1), Pos)) )
  ),
  Val is -V1,
  !.

eval(P, Env, op(Pos, Op, Lt, Rt), Val, Cons) :- %wyrazenia arytmetyczne
  member(Op, ['+', '-', '*', '/', '%']),
  eval(P, Env, Lt, V1, C1),
  eval(P, Env, Rt, V2, C2),
  (
      (	integer(V1), integer(V2), !);
      (	\+ integer(V1), throw(runtime_error(('Expected integer, found: ', V1), Pos))  );
      (	\+ integer(V2), throw(runtime_error(('Expected integer, found: ', V2), Pos))  )
  ),
  (
      (	 member(Op, ['+', '-', '*']), ! , Exp =.. [Op, V1, V2]   );
      (	 Op = '/', !, ((V2 = 0,	throw(runtime_error('Division by 0', Pos)) ) ; (Exp = V1 // V2)) );
      (	 Op = '%', !, ((V2 = 0, throw(runtime_error('Division by 0', Pos)) ) ;(Exp = V1 mod V2)) )
  ),
  Val is Exp,
  join(C1, C2, Cons).


eval(P, Env, op(Pos, Op, Lt, Rt), Val, Cons) :- %porownania
  member(Op, ['=', '<>', '<', '>', '<=', '>=']),
  eval(P, Env, Lt, V1, C1),
  eval(P, Env, Rt, V2, C2),
  (
      (	integer(V1), integer(V2), !);
      (	\+ integer(V1), throw(runtime_error(('Expected integer, found: ', V1), Pos))  );
      (	\+ integer(V2), throw(runtime_error(('Expected integer, found: ', V2), Pos))  )
  ),
  (
      (	 Op = '=', !, ((V1 =:= V2, !, Val = 1); (Val = 0)) );
      (	 Op = '<>', !, ((V1 =\= V2, !, Val = 1); (Val = 0)) );
      (	 Op = '<', !, ((V1 < V2, !, Val = 1); (Val = 0)) );
      (	 Op = '>', !, ((V1 > V2, !, Val = 1); (Val = 0)) );
      (	 Op = '<=', !, ((V1 =< V2, !, Val = 1); (Val = 0)) );
      (	 Op = '>=', !, ((V1 >= V2, !, Val = 1); (Val = 0)) )
  ),
  join(C1, C2, Cons).


eval(P, Env, if(Pos, E1, E2, E3), Val, Cons) :- %if else then
  eval(P, Env, E1, V1, C1),
  (
      (	 integer(V1), !  );
      (	 throw(runtime_error(('Expected integer, found: ', V1), Pos))  )
  ),
  (
      (	V1 =\= 0, !,  eval(P, Env, E2, Val, C2) );
      ( eval(P, Env, E3, Val, C2 ))
  ),
  join(C1, C2, Cons).


% Przy ewaluacji wyrazenia "let P = E1 in E2" tworzony jest lokalny
% zakres dla zniennych z wzorca P. Jest to zrealizowane poprzez
% dodawanie wartosciowan na poczatek srodowiska, funkcja value_from_env
% zwraca pierwsze dopasowanie.
eval(Prog, Env, let(Pos, Pat, E1, E2), Val, Cons) :- %let in
  eval(Prog, Env, E1, V1, C1),
  match_pattern(Pat, V1, Env, Env1, Pos),
  eval(Prog, Env1, E2, Val, C2),
  join(C1, C2, Cons),
  !.

eval(Prog, Env, call(Pos, Name, E), V, Cons) :- %wywolanie funkcji
  eval(Prog, Env, E, V1, C1),
  seek_func(Prog, Name, def(_, Pat, Body), Pos),
  match_pattern(Pat, V1, [], FunEnv, Pos),
  eval(Prog, FunEnv, Body, V, C2),
  join(C1, C2, Cons),
  !.

