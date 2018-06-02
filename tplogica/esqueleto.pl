symbol(a).
symbol(b).
symbol(c).

% Algunas regex de ejemplo

regexEj(1, a). % a
regexEj(2, or(a, b)). % a|b
regexEj(3, concat(E1, E2)) :- regexEj(1, E1), regexEj(2, E2). % a(a|b)
regexEj(4, star(E2)) :- regexEj(2, E2). % (a|b)*
regexEj(5, or(star(E1), E4)) :- regexEj(1, E1), regexEj(4, E4). % (a*|(a(a|b))*)
regexEj(6, star(or(a, ab))). %(a|ab)*
regexEj(7, concat(or(a, concat(a,b)), or(b, empty))). %(a|ab)(b|)
regexEj(8, concat(star(a), star(b))). %a*b*
regexEj(9, star(or(star(a), star(b)))).


% Ejercicio 1: tieneEstrella(+RegEx)

tieneEstrella(concat(X,_)) :- tieneEstrella(X).
tieneEstrella(concat(_,Y)) :- tieneEstrella(Y).
tieneEstrella(or(X,_)) :- tieneEstrella(X).
tieneEstrella(or(_,Y)) :- tieneEstrella(Y).
tieneEstrella(star(_)).


% Ejercicio 2: longitudMaxima(+RegEx, -Length)

longitudMaxima(empty, 0).
longitudMaxima(Cadena,LONG) :- symbol(Cadena), LONG is 1.
longitudMaxima(or(X,Y), LONG) :- not(tieneEstrella(or(X,Y))), longitudMaxima(X, LONG1), longitudMaxima(Y,LONG2), LONG is max(LONG1,LONG2).
longitudMaxima(concat(X,Y), LONG) :- not(tieneEstrella(concat(X,Y))), longitudMaxima(X, LONG1), longitudMaxima(Y, LONG2), LONG is LONG1 + LONG2.

% Ejercicio 3: cadena(?Cadena)

cadena([]).
cadena([X | XS]):- cadena(XS), symbol(X).

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([], empty).
match_inst([X], X) :- symbol(X).
match_inst(CADENA, or(X,_)) :- match_inst(CADENA, X).
match_inst(CADENA, or(_,Y)) :- match_inst(CADENA, Y).
match_inst(CADENA, concat(Y,Z)) :- append(C1, C2, CADENA), match_inst(C1,Y), match_inst(C2, Z).
match_inst([], star(_)). %0 apariciones.
%match_inst(CADENA, star(star(Y))) :- match_inst(CADENA, Y). %Esto esta mal!!! simplificarlo seria star(Y).
match_inst(CADENA, star(Y)) :- append(C1, C2, CADENA), not(length(C1,0)), match_inst(C1, Y), match_inst(C2, star(Y)).

% Ejercicio 5: match(?Cadena, +RegEx)

match(L, E) :- cadena(L), match_inst(L, E).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(L, Exp1, Exp2) :- match(L, Exp1), not(match(L,Exp2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)
prefijoMaximo(PRE, CADENA, Exp) :- append(PRE,_,CADENA), match(PRE, Exp), length(PRE, T), not(hayPrefijoMayor(CADENA,Exp,T)).

%hayPrefijoMayor(+L,+E,+T)
hayPrefijoMayor(CADENA, Exp, T):- append(PRE1,_,CADENA), length(PRE1, TI), TI > T, match(PRE1,Exp).

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar([], _, _, []).

%si no tengo un prefijo que matchee, busco en la cola de la cadena.
reemplazar([X | XS], Exp, Sust, [X | Rec]) :- 
    not(prefijoMaximo(_, [X | XS], Exp)), 
    reemplazar(XS, Exp, Sust, Rec).

%Si el prefijo maximo es el vacio, lo saltamos.
reemplazar([X | XS], Exp, Sust, [X | Rec]) :- 
    prefijoMaximo([], [X | XS], Exp), 
    reemplazar(XS, Exp, Sust, Rec).

%Si hay un prefijo maximo bueno.
reemplazar(Cadena, Exp, Sust, Res) :- 
    prefijoMaximo(P, Cadena, Exp), 
    length(P,TamP), 
    TamP > 0, 
    append(P, D, Cadena), 
    reemplazar(D, Exp, Sust, Rec), 
    append(Sust, Rec, Res).



%reemplazar([X | XS], E, R, [X | Rec]) :- 
%    prefijoMaximo(P1, [X | XS], E), 
%    prefijoMaximo(P2, XS, E), 
%    length(P1,TamP1), 
%    length(P2,TamP2), 
%    TamP1 > TamP2, 
%    reemplazar(XS, E, R, Rec).

% Habia pensado esto pero vamos con lo tuyo guido.
% reemplazar(Cadena, Exp, In, Out) :-
%    subcadenaLongMax(Cadena, Exp, S),
%    triappend(Pre, S, Suf, Cadena),
%    reemplazar(Pre, Exp, In, R1),
%    reemplazar(Suf, Exp, In, R2),
%    triappend(R1, S, R2, Out).


