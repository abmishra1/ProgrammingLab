decode(_, D1, _, [], D1).

decode(_, _, LC, [C|_], X) :-
    C = 48,
    LC \= 49,
    LC \= 50,
    X is 0.

decode(D2, D1, LC, [C|L], X) :-
    (C = 48 , decode(0,D2,C,L,X));
    ((C > 48, C < 58),
        Y is D1 + D2,
        (((LC = 49);((LC = 50),(C >= 48, C =< 55))) -> decode(D1,Y,C,L,X); decode(D1,D1,C,L,X))
    ).

wrapper(X, S) :-
    string_codes(S, L),
    writeln(L),
    decode(1,1,0,L,X).