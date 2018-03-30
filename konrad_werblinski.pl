% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
%
%
:- module(konrad_werblinski, [parse/3]).
%mam nadzeje ze odkomentowalem
%




% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.

% =================================Lexer===================================
% Zapis tokenow tok(token, pos(Line, LinePos, Pos, Len)).
% Lexer rzuca wyjatek gdy natrafi na nieznany token

lexer(Tokens, Line, LinePos, Pos, Path) -->
  garbage(Pos, Line, LinePos, NPos, NLine, NLinePos),
  (
    ( `(`, !, {Len = 1, Token = tok(lParen, pos(NLine, NLinePos, NPos, Len))});
    ( `)`, !, {Len = 1, Token = tok(rParen, pos(NLine, NLinePos, NPos, Len) )});
    ( `[`, !, {Len = 1, Token = tok(lSParen, pos(NLine, NLinePos, NPos, Len))});
    ( `]`, !, {Len = 1, Token = tok(rSParen, pos(NLine, NLinePos, NPos, Len))});
    ( `..`, !, {Len = 2, Token = tok(doubleDot, pos(NLine, NLinePos, NPos, Len))});
    ( `,`, !, {Len = 1, Token = tok(coma, pos(NLine, NLinePos, NPos, Len))});
    ( `<=`, !, {Len = 2, Token = tok(lessOrEqual, pos(NLine, NLinePos, NPos, Len))});
    ( `>=`, !, {Len = 2, Token = tok(moreOrEqual, pos(NLine, NLinePos, NPos, Len))});
    ( `=`, !, {Len = 1, Token = tok(assgn, pos(NLine, NLinePos, NPos, Len))});
    ( `<>`, !, {Len = 2, Token = tok(notEqual, pos(NLine, NLinePos, NPos, Len))});
    ( `<`, !, {Len = 1, Token = tok(less, pos(NLine, NLinePos, NPos, Len))});
    ( `>`, !, {Len = 1, Token = tok(more, pos(NLine, NLinePos, NPos, Len))});
    ( `^`, !, {Len = 1, Token = tok(xor, pos(NLine, NLinePos, NPos, Len))});
    ( `|`, !, {Len = 1, Token = tok(or, pos(NLine, NLinePos, NPos, Len))});
    ( `+`, !, {Len = 1, Token = tok(plus, pos(NLine, NLinePos, NPos, Len))});
    ( `-`, !, {Len = 1, Token = tok(minus, pos(NLine, NLinePos, NPos, Len))});
    ( `&`, !, {Len = 1, Token = tok(and, pos(NLine, NLinePos, NPos, Len))});
    ( `*`, !, {Len = 1, Token = tok(mult, pos(NLine, NLinePos, NPos, Len))});
    ( `/`, !, {Len = 1, Token = tok(div, pos(NLine, NLinePos, NPos, Len))});
    ( `%`, !, {Len = 1, Token = tok(mod, pos(NLine, NLinePos, NPos, Len))});
    ( `@`, !, {Len = 1, Token = tok(at,	pos(NLine, NLinePos, NPos, Len))});
    ( `#`, !, {Len = 1, Token = tok(hash, pos(NLine, NLinePos, NPos, Len))});
    ( `~`, !, {Len = 1, Token = tok(not, pos(NLine, NLinePos, NPos, Len))});
    ( num(0, Val, 0, Len), !, {Token = tok(num(Val), pos(NLine, NLinePos, NPos, Len))});
    ( id(X-X, Id, 0, Len), !,
      (
	  {
	     member((Id, Token),
		[
		     (`def`,  tok(def, pos(NLine, NLinePos, NPos, Len)) ),
		     (`else`,  tok(else, pos(NLine, NLinePos, NPos, Len)) ),
		     (`if`,  tok(if, pos(NLine, NLinePos, NPos, Len)) ),
		     (`in`,  tok(in, pos(NLine, NLinePos, NPos, Len)) ),
		     (`let`,  tok(let, pos(NLine, NLinePos, NPos, Len)) ),
		     (`then`,  tok(then, pos(NLine, NLinePos, NPos, Len)) ),
		     (`_`,  tok('_', pos(NLine, NLinePos, NPos, Len)) )
		 ])
	  , !}
      ;
      {Token = tok(identifier(Id), pos(NLine, NLinePos, NPos, Len)), !})
    );

    (
	[_|_],
	{
	    Position = file(Path, NLine, NLinePos, NPos),
	    throw(syntax_error('Found unknown token', Position))
	}
    )

    ),
  !,
  {NPos1 is NPos + Len, NLinePos1 is NLinePos + Len},
  lexer(TokRest, NLine, NLinePos1, NPos1, Path),
  {Tokens = [Token|TokRest]}.


lexer([], Line, LinePos, Pos, _)  --> garbage(Pos, Line, LinePos, _, _, _), !.

garbage(Pos, Line, LinePos, NPos, NLine, NLinePos) -->
  comment(Pos, Line, LinePos, Pos1, Line1, LinePos1) , !,
  garbage(Pos1, Line1, LinePos1, NPos, NLine, NLinePos).


garbage(Pos, Line, LinePos, NPos, NLine, NLinePos) -->
  white_space(Pos, Line, LinePos, Pos1, Line1, LinePos1) , !,
  garbage(Pos1, Line1, LinePos1, NPos, NLine, NLinePos)	.


garbage(NPos, NLine, NLinePos, NPos, NLine, NLinePos) --> [].

%objescie problemu zapetlania sie w predykaie garbage,
%wymuszam aby ciag bialych znakow byl niepusty, poprzez wymuszone przejsie
%przez predykat white_space.

white_space(Pos, Line, _, NPos, NLine, NLinePos) -->
  `\n`,
  !,
  {Pos1 is Pos + 1, Line1 is Line + 1},
  skip_white_space(Pos1, Line1, 1, NPos, NLine, NLinePos).

white_space(Pos, Line, LinePos, NPos, NLine, NLinePos) -->
  [Char], { code_type(Char, space) }, !,
  {Pos1 is Pos + 1,  LinePos1 is LinePos + 1},
  skip_white_space(Pos1, Line, LinePos1, NPos, NLine, NLinePos).

skip_white_space(Pos, Line, _, NPos, NLine, NLinePos) -->
  `\n`,
  !,
  {Pos1 is Pos + 1, Line1 is Line + 1},
  skip_white_space(Pos1, Line1, 1, NPos, NLine, NLinePos).

skip_white_space(Pos, Line, LinePos, NPos, NLine, NLinePos) -->
  [Char], { code_type(Char, space) }, !,
  {Pos1 is Pos + 1,  LinePos1 is LinePos + 1},
  skip_white_space(Pos1, Line, LinePos1, NPos, NLine, NLinePos).

skip_white_space(NPos, NLine, NLinePos, NPos, NLine, NLinePos) --> [].


comment(Pos, Line, LinePos, NPos, NLine, NLinePos) --> `(*`,
  {Pos1 is Pos + 2, LinePos1 is LinePos + 2},
  skip_comment(1, Pos1, Line, LinePos1, NPos, NLine, NLinePos), !.

skip_comment(Depth, Pos, Line, LinePos, NPos, Line, NLinePos) -->
  `*)`,
  {Depth is 1},
  !,
  {NPos is Pos + 2, NLinePos is LinePos + 2}.

skip_comment(Depth, Pos, Line, LinePos, NPos, NLine, NLinePos) -->
  `*)`,
  {Depth1 is Depth - 1},
  !,
  {Pos1 is Pos + 2, LinePos1 is LinePos + 2},
  skip_comment(Depth1, Pos1, Line, LinePos1, NPos, NLine, NLinePos).


skip_comment(Depth, Pos, Line, LinePos, NPos, NLine, NLinePos) -->
  `(*`,
  {Depth1 is Depth + 1},
  !,
  {Pos1 is Pos + 2, LinePos1 is LinePos + 2},
  skip_comment(Depth1, Pos1, Line, LinePos1, NPos, NLine, NLinePos).


skip_comment(Depth, Pos, Line, _,  NPos, NLine, NLinePos) -->
  `\n`,
  !,
  {Pos1 is Pos + 1, Line1 is Line + 1},
  skip_comment(Depth, Pos1, Line1, 1, NPos, NLine, NLinePos).

skip_comment(Depth, Pos, Line, LinePos,  NPos, NLine, NLinePos) -->
  [_],
  {Pos1 is Pos + 1, LinePos1 is LinePos + 1},
  skip_comment(Depth, Pos1, Line, LinePos1, NPos, NLine, NLinePos), !.

%rozbicie na dwa predykaty aby wymusic niepustosc literalu

num(ANum, Num, AL, L) -->
  (
    (`1`, !, {D = 1});
    (`2`, !, {D = 2});
    (`3`, !, {D = 3});
    (`4`, !, {D = 4});
    (`5`, !, {D = 5});
    (`6`, !, {D = 6});
    (`7`, !, {D = 7});
    (`8`, !, {D = 8});
    (`9`, !, {D = 9});
    (`0`, {D = 0})
  ),
  {ANum1 is 10 * ANum + D, AL1 is AL +1},
  num_rest(ANum1, Num, AL1, L).

num_rest(ANum, Num, AL, L) -->
   num(ANum, Num, AL, L).

num_rest(ANum, ANum, AL, AL) --> [], !.

%rozbicie na dwa predykaty aby wymusic niepustosc
%i odpowiedni poczatek identyfikatora

id(AId-E, Id, AL, L) -->
  `_`,
  !,
  {[C|_] = `_`, E = [C|E2], AL1 is AL + 1},
  id_rest(AId-E2, Id, AL1, L).

id(AId-E, Id, AL, L) -->
  [C],
  {code_type(C, alpha)},
  !,
  {E = [C|E2], AL1 is AL + 1},
  id_rest(AId-E2, Id, AL1, L).


id_rest(AId-E, Id, AL, L) -->
  `_`,
  !,
  {[C|_] = `_` , E = [C|E2], AL1 is AL + 1},
  id_rest(AId-E2, Id, AL1, L).

id_rest(AId-E, Id, AL, L) -->
  `'`,
  !,
  {[C|_] = `'`, E = [C|E2], AL1 is AL + 1},
  id_rest(AId-E2, Id, AL1, L).

id_rest(AId-E, Id, AL, L) -->
  [C],
  {code_type(C, alnum)},
  !,
  {E = [C|E2], AL1 is AL + 1},
  id_rest(AId-E2, Id, AL1, L).

id_rest(AId-[], AId, AL, AL)  --> [].

% ========================Predykaty pomocnicze==========================

makePos(pos(Line, LinePos, CharNo, Len),  Path, file(Path, Line, LinePos, CharNo, Len)).

%================================Parser==================================
% Parser zg�asza dwa rodzaje wyjatkow I dla niepoprawnej deficji, II dla
% niespodziewanego tokenu, na danej pozycji w kodzie.

definitions(Path, AST) -->
  definition(Path,Def),
  !,
  definitions(Path, AST2),
  {AST = [Def|AST2]}.

definitions(_, []) --> [].


definition(Path, AST) -->
  [tok(def, _)],
  [tok(identifier(Id) , _)],
  [tok(lParen, _)],
  comp_pattern(Path, ASTP, _),
  [tok(rParen, _)],
  [tok(assgn, _)],
  expression(Path, ASTE, _),
  {atom_codes(Name, Id), AST =  def(Name, ASTP, ASTE)}, !.

definition(Path, _) --> %exception typu I
  [tok(def, pos(NLine, NLinePos, NPos, _))],
  !,
  {Pos = file(Path, NLine, NLinePos, NPos),
  throw(syntax_error('Invalid definition', Pos))
  }.

definition(Path, _) --> %exception typu II
  [tok(T,  pos(NLine, NLinePos, NPos, _))],
  {Pos = file(Path, NLine, NLinePos, NPos),
  throw(syntax_error(('Found unexpected token: ', T), Pos))
  }.



comp_pattern(Path, AST, Pos) --> %Pattern1, Pattern2
  pattern(Path, AST1, Pos1),
  [tok(coma, _)],
  !,
  comp_pattern(Path, AST2, Pos2),
  {
  Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
  Pos2 = file(Path, _, _, CharNo2, Len2),
  Len is Len2 + CharNo2 - CharNo1,
  Pos =  file(Path, Line1, LinePos1, CharNo1, Len),
  AST = pair(Pos, AST1, AST2)
  },
  !.
comp_pattern(Path, AST, Pos) --> pattern(Path, AST, Pos).

pattern(Path, AST, Pos) --> variable(Path, AST, Pos), !. %zmienna
pattern(Path, AST, Pos1) -->
  [tok(lParen, pos(LPLine, LPLinePos, LPCharNo, _))],
  !,
  comp_pattern(Path, AST, _),
  [tok(rParen, pos(_, _, RPCharNo, _))],
  {
  Len1 is RPCharNo -  LPCharNo + 1,
  Pos1 = file(Path, LPLine, LPLinePos, LPCharNo, Len1)
  },
  !.

pattern(Path, wildcard(Pos), Pos) --> %wild card
  [tok('_', TokPos)],
  !,
  {makePos(TokPos, Path, Pos)}.

%=============Wyrazenia============================

expression(Path, AST, Pos) --> %if else then
  [tok(if, pos(IfLine, IfLinePos, IfCharNo, _))],
  !,
  expression(Path, AST1, _), !,
  [tok(then, _)],
  expression(Path, AST2, _), !,
  [tok(else, _)],
  expression(Path, AST3, Pos3), !,
  {
  Pos3 = file(Path, _, _, CharNo3, Len3),
  Len is CharNo3 - IfCharNo + Len3,
  Pos = file(Path, IfLine, IfLinePos, IfCharNo, Len),
  AST = if(Pos, AST1, AST2, AST3)
  }.

expression(Path, AST, Pos) --> %let in
  [tok(let, pos(LetLine, LetLinePos, LetCharNo, _))],
  !,
  comp_pattern(Path, AST1, _), !,
  [tok(assgn, _)],
  expression(Path, AST2, _), !,
  [tok(in, _)],
  expression(Path, AST3, Pos3), !,
  {
  Pos3 = file(Path, _, _, CharNo3, Len3),
  Len is CharNo3 - LetCharNo + Len3,
  Pos = file(Path, LetLine, LetLinePos, LetCharNo, Len),
  AST = let(Pos, AST1, AST2, AST3)
  }.

expression(Path, AST, Pos) --> op_expression(Path, AST, Pos), !.

%===============Wyrazenia z operatorami=============

op_expression(Path, AST, Pos) -->    %para E1,E2  (prawostronnie laczne)
  comparison(Path, AST1, Pos1),
  (
      (
          [tok(coma,_)],
          !,
          op_expression(Path, AST2, Pos2), !,
          {
	  Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
	  Pos2 = file(Path, _, _, CharNo2, Len2),
	  Len is CharNo2 - CharNo1 + Len2,
	  Pos = file(Path, Line1, LinePos1, CharNo1, Len),
	  AST = pair(Pos, AST1, AST2)
          }
      );
      (	  {AST = AST1, Pos = Pos1, ! })
  ).

comparison(Path, AST, Pos) -->  %porownania (nielaczne)
  concat(Path, AST1, Pos1),
  (
      (
          (
              ( [tok(lessOrEqual, _)],!, { Op = '<=' } );
              ( [tok(moreOrEqual, _)], !, { Op = '>=' } );
              ( [tok(assgn, _)], !, { Op = '=' });
              ( [tok(notEqual, _)], !, { Op = '<>' } );
              ( [tok(less, _)], !, { Op = '<' } );
              ( [tok(more, _)], !, { Op = '>' } )
          ),
          concat(Path, AST2, Pos2), !,
          {
	  Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
	  Pos2 = file(Path, _, _, CharNo2, Len2),
	  Len is CharNo2 - CharNo1 + Len2,
	  Pos = file(Path, Line1, LinePos1, CharNo1, Len),
	  AST = op(Pos, Op, AST1, AST2)
          }
      );
      (  {AST = AST1, Pos = Pos1, !} )
  ).

concat(Path, AST, Pos) -->   %konkatenacja (prawostronnie laczne)
  sum(Path, AST1, Pos1),
  (
      (
          [tok(at, _)],
          !,
	  concat(Path, AST2, Pos2), !,
          {
	  Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
	  Pos2 = file(Path, _, _, CharNo2, Len2),
	  Len is CharNo2 - CharNo1 + Len2,
	  Pos = file(Path, Line1, LinePos1, CharNo1, Len),
	  AST = op(Pos, '@', AST1, AST2)
          }
      );
      (	 {AST = AST1, Pos = Pos1, !} )
  ).

sum(Path, AST, Pos) -->    %addatywne (lewostronnie laczne)- ominiecie lewostronnej rekursji przez akumulator
   mult(Path, AST1, Pos1), sum(Path, AST, Pos, AST1, Pos1), !.

sum(Path, AST, Pos, AST1, Pos1) -->
  (
    ( [tok(xor, _)], !, { Op = '^' } );
    ( [tok(or, _)], !, { Op = '|' } );
    ( [tok(plus, _)], !, { Op = '+' } );
    ( [tok(minus,_)], !, { Op = '-' } )
  ),
  mult(Path, AST2, Pos2), !,
  {
  Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
  Pos2 = file(Path, _, _, CharNo2, Len2),
  Len is CharNo2 - CharNo1 + Len2,
  PosAcc = file(Path, Line1, LinePos1, CharNo1, Len),
  ASTAcc = op(PosAcc, Op, AST1, AST2)
  },
  sum(Path, AST, Pos, ASTAcc, PosAcc).

sum(_, AST, Pos, AST, Pos) --> [].

mult(Path, AST, Pos) -->  %multiplikatywne (lewostronnie laczne) - ominiecie lewostronnej rekursji przez akumulator
  unary(Path, AST1, Pos1), mult(Path, AST, Pos, AST1, Pos1), !.

mult(Path, AST, Pos, AST1, Pos1) -->
  (
    ( [tok(and, _)], !, { Op = '&' } );
    ( [tok(mult, _)], !, { Op = '*' } );
    ( [tok(div, _)], !, { Op = '/' } );
    ( [tok(mod,_)], !, { Op = '%' } )
  ),
  unary(Path, AST2, Pos2), !,
  {
  Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
  Pos2 = file(Path, _, _, CharNo2, Len2),
  Len is CharNo2 - CharNo1 + Len2,
  PosAcc = file(Path, Line1, LinePos1, CharNo1, Len),
  ASTAcc = op(PosAcc, Op, AST1, AST2)
  },
  mult(Path, AST, Pos, ASTAcc, PosAcc).

mult(_, AST, Pos, AST, Pos) --> [].



unary(Path, AST, Pos) --> %unarne
  (
    ( [tok(hash, TokPos)], !, { Op = '#' } );
    ( [tok(not, TokPos)], !, { Op = '~' } );
    ( [tok(minus, TokPos)], !, { Op = '-' } )
  ),
  unary(Path, AST2, Pos2), !,
  {
      TokPos = pos(Line1, LinePos1, CharNo1, _),
      Pos2 = file(Path, _, _, CharNo2, Len2),
      Len is CharNo2 - CharNo1 + Len2,
      Pos = file(Path, Line1, LinePos1, CharNo1, Len),
      AST = op(Pos, Op, AST2)
  }.

unary(Path, AST, Pos)  --> simple_expression(Path, AST, Pos), !.

%===============Wyrazenia proste===============

expression_in_paren(Path, AST, Pos) -->  %(expression)
  [tok(lParen, pos(LPLine, LPLinePos, LPCharNo, _))],
  !,
  expression(Path, AST, _), !,
  [tok(rParen, pos(_, _, RPCharNo, _))],
  {
  Len is RPCharNo -  LPCharNo + 1,
  Pos = file(Path, LPLine, LPLinePos, LPCharNo, Len)
  }.

simple_expression(Path, AST, Pos) --> select_bit(Path, AST, Pos).

%wybor bitow, obsuguje wszysktie reguly z
%simple_expression  co zwieksza wydajnosc (i zmiejsza czytlnosc)
%ominiecie lewostronnej rekursji za pomoca akumulatora

select_bit(Path, AST, Pos) -->
  expression_in_paren(Path, AST1, Pos1), !, select_bit(Path, AST, Pos, AST1, Pos1).

select_bit(Path, AST, Pos) -->
  atomic_expression(Path, AST1, Pos1), select_bit(Path, AST, Pos, AST1, Pos1).

select_bit(Path, AST, Pos, AST1, Pos1) -->
  [tok(lSParen, _)],
  expression(Path, AST2, _),
  (
      (
          [tok(rSParen, pos(_, _, RSPCharNo, _))], %wybor 1 bitu
          !,
          {
	      Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
	      Len is RSPCharNo - CharNo1 + 1,
	      PosAcc = file(Path, Line1, LinePos1, CharNo1, Len),
	      ASTAcc = bitsel(PosAcc, AST1, AST2)
          }
      );
      (
	  [tok(doubleDot , _)],  %wybor wielu bitow
	  !,
	  expression(Path, AST3, _),
	  [tok(rSParen, pos(_, _, RSPCharNo, _))],
	  {
	      Pos1 = file(Path, Line1, LinePos1, CharNo1, _),
	      Len is RSPCharNo - CharNo1 + 1,
	      PosAcc = file(Path, Line1, LinePos1, CharNo1, Len),
	      ASTAcc = bitsel(PosAcc, AST1, AST2, AST3)
	  }
      )

  ),
  select_bit(Path, AST, Pos, ASTAcc, PosAcc).

select_bit(_, AST, Pos, AST, Pos) --> [].
%===============Wyrazenia atomowe===============

atomic_expression(Path, AST, Pos) --> function_call(Path, AST, Pos), !.
atomic_expression(Path, AST, Pos) --> variable(Path, AST, Pos), !.
atomic_expression(Path, AST, Pos) --> num_literal(Path, AST, Pos), !.
atomic_expression(Path, AST, Pos) --> empty_vect(Path, AST, Pos), !.
atomic_expression(Path, AST, Pos) --> single_bit(Path, AST, Pos), !.


variable(Path, var(Pos, Name), Pos) --> %zmienna
  [tok(identifier(X), TokPos)],
  !,
  {
  makePos(TokPos, Path, Pos),
  atom_codes(Name, X)
  }.

num_literal(Path, num(Pos, N), Pos) --> %liczba
  [tok(num(N), TokPos)],
  !,
  {makePos(TokPos, Path, Pos)}.

function_call(Path, AST, Pos) -->
  [tok(identifier(X), TokPos)], %wywolanie funckji
  [tok(lParen, _)],
  !,
  expression(Path, AST1, _), !,
  [tok(rParen, pos(_, _, RPCharNo, _))],
  {
  TokPos = pos(Line1, LinePos1, CharNo1, _),
  Len is RPCharNo - CharNo1 + 1,
  Pos = file(Path, Line1, LinePos1, CharNo1, Len),
  atom_codes(Name, X),
  AST = call(Pos, Name, AST1)
  }.

empty_vect(Path, AST, Pos) --> %pusty wektor
  [tok(lSParen,  pos(LSPLine, LSPLinePos, LSPCharNo, _))],
  [tok(rSParen, pos(_, _, RSPCharNo, _))],
  !,
  {
  Len is RSPCharNo - LSPCharNo + 1,
  Pos = file(Path, LSPLine, LSPLinePos, LSPCharNo, Len),
  AST = empty(Pos)
  }.


single_bit(Path, AST, Pos) --> %pojedynczy bit
  [tok(lSParen,  pos(LSPLine, LSPLinePos, LSPCharNo, _))],
  expression(Path, AST1,_), !,
  [tok(rSParen, pos(_, _, RSPCharNo, _))],
  {
  Len is RSPCharNo - LSPCharNo + 1,
  Pos = file(Path, LSPLine, LSPLinePos, LSPCharNo, Len),
  AST = bit(Pos, AST1)
  }.



parse(Path, Codes, Program) :-
   phrase(lexer(Tokens, 1, 1, 0, Path), Codes),
   phrase(definitions(Path, Program), Tokens).
