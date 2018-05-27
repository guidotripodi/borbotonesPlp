symbol(a).
symbol(b).
symbol(c).

% Algunas regex de ejemplo

regexEj(1, a). % a
regexEj(2, or(a, b)). % a|b
regexEj(3, concat(E1, E2)) :- regexEj(1, E1), regexEj(2, E2). % a(a|b)
regexEj(4, star(E2)) :- regexEj(2, E2). % (a(a|b))*
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

longitudMaxima(_, _) :- fail.

% Ejercicio 3: cadena(?Cadena)

cadena([]).
cadena([X | XS]):- cadena(XS), symbol(X).

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst(_, _) :- fail.

% Ejercicio 5: match(?Cadena, +RegEx)

% Como cadena no se sabe si esta instanciada me fijo la valides y luego macheo falta machearla cadena

match(L, E) :- cadena(L), match_inst(L, E).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(L, Exp1, Exp2) :- match(L, Exp1), not(match(L,Exp2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(_, _, _) :- fail.

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar(_, _, _, _) :- fail.
