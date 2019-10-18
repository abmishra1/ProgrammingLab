menu(hungry, 1, 1, 1).
menu(not_so_hungry, 1, 1, 0).
menu(not_so_hungry, 0, 1, 1).
menu(diet, 1, 0, 0).
menu(diet, 0, 1, 0).
menu(diet, 0, 0, 1).

starter('Corn Tikki', 30, 0).
starter('Tomato Soup', 30, 1).
starter('Chilli Paneer', 30, 2).
starter('Crispy Chicken', 40, 3).
starter('Papdi Chaat', 20, 4).
starter('Cold Drink', 20, 5).

main_dish("Kadhai Paneer with Butter/Plain Naan", 50).
main_dish("Veg Korma with Butter/Plain Naan", 40).
main_dish("Murgh Lababdar with Butter/Plain Naan", 50).
main_dish("Veg Dum Biryani with Dal Tadka", 50).
main_dish("Steam Rice with Dal Tadka", 40).

desert("Ice-cream", 20, 0).
desert("Malai Sandwich", 30, 1).
desert("Rasmalai", 10, 2).

take_starter(_, 6, [], _).
take_starter(_, 6, CL, PL) :-
    CL \= [],
    append(PL, CL, TL),
    writeln(TL),
    fail.

take_starter(N, I, CL, PL) :-
    starter(Name, NV, I),
    N >= NV,
    N1 is N - NV,
    take_starter(N1, I, [Name|CL], PL).
    
take_starter(N, I, CL, PL) :-
    I < 6,
    J is I + 1,
    take_starter(N, J, CL, PL).    

select_starter(N, PL) :- take_starter(N, 0, [], PL).

take_desert(_, 3, [], _).
take_desert(_, 3, CL, PL) :-
    CL \= [],
    append(PL, CL, TL),
    writeln(TL),
    fail.

take_desert(N, I, CL, PL) :-
    desert(Name, NV, I),
    N >= NV,
    N1 is N - NV,
    take_desert(N1, I, [Name|CL], PL).
    
take_desert(N, I, CL, PL) :-
    I < 3,
    J is I + 1,
    take_desert(N, J, CL, PL).    

select_desert(N, PL) :- take_desert(N, 0, [], PL).

satisfy_hungry() :-
    starter(S, _, _),
    main_dish(M, _),
    desert(D, _, _),
    L = [S, M, D],
    writeln(L),
    fail.

satisfy_not_so_hungry(X) :-
    % writeln("inhere"),
    main_dish(Name, NV),
    N1 is 80 - NV,
    writeln(Name),
    writeln(N1),
    % select_starter(N1,[Name]),
    ( X = 1 -> select_starter(N1,[Name]) ; select_desert(N1,[Name])),
    fail.

satisfy_diet(X,Y,Z):-
    (X = 1, satisfy_starter_diet());
    (Y = 1, satisfy_main_dish_diet());
    (Z = 1, satisfy_desert_diet()).

satisfy_starter_diet() :-
    select_starter(40, []),
    fail.

satisfy_desert_diet() :-
    select_desert(40, []),
    fail.

satisfy_main_dish_diet() :-
    main_dish(Name, N),
    N =< 40,
    writeln([Name]),
    fail.

find_items(Status, X, Y, Z) :-
    menu(Status, X, Y, Z),
    ((Status = hungry, satisfy_hungry());
    (Status = not_so_hungry, satisfy_not_so_hungry(X));
    (Status = diet, satisfy_diet(X,Y,Z))).
