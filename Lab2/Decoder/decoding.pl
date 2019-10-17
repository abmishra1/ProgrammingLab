decode(1, []).

decode(X, L) :-
    onedigit(Y, L),
    twodigit(Z, L),
    X is Y + Z.

onedigit(X, [C | L]) :-
    ((C > 48, C < 58) -> decode(X, L) ; X is 0).

twodigit(X, L) :-
    X is 0,
    length(L, N),
    N == 0.

twodigit(X, L) :-
    X is 0,
    length(L, N),
    N == 1.

twodigit(X, [C1, C2 | L]) :-
    ((C1 == 49 ; (C1 == 50, C2 >= 48, C2 =< 55)) -> decode(X, L) ; X is 0).

wrapper(X, S) :-
    string_codes(S, L),
    % writeln(L),
    decode(X, L), !.