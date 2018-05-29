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
match_inst([], star(_)). %0 apariciones
match_inst([X], X) :- symbol(X).
match_inst(CADENA, or(X,_)) :- match_inst(CADENA, X).
match_inst(CADENA, or(_,Y)) :- match_inst(CADENA, Y).
match_inst(CADENA, concat(Y,Z)) :- append(C1, C2, CADENA), match_inst(C2, Z), match_inst(C1,Y).
match_inst(CADENA, star(Y)) :- append(C1, C2, CADENA), match_inst(C1, Y), match_inst(C2, star(Y)).

%para concat deberia usar append??? y, si.

% Ejercicio 5: match(?Cadena, +RegEx)

% Como cadena no se sabe si esta instanciada me fijo la validez y luego macheo falta machearla cadena

match(L, E) :-  match_inst(L, E), cadena(L).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(L, Exp1, Exp2) :- match(L, Exp1), not(match(L,Exp2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)


prefijoMaximo(PRE, CADENA, Exp) :- append(PRE, _, CADENA), match(PRE, Exp), prefijos(CADENA, Exp, P1), length(PRE) >= length(P1).
prefijos(CADENA, Exp, P1) :- append(P1, _, CADENA), match_inst(P1, Exp).

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar(_, _, _, _) :- fail.
