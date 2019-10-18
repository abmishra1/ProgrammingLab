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
take_starter(_, 6, CL, TL) :-
    append(CL, TL, CL),
    writeln(CL),
    fail.

take_starter(N, I, CL, TL) :-
    starter(Name, NV, I),
    N >= NV,
    N1 is N - NV,
    take_starter(N1, I, [Name|CL], TL).
    
take_starter(N, I, CL, TL) :-
    I < 6,
    J is I + 1,
    take_starter(N, J, CL, TL).    

select_starter(N, TL) :- take_starter(N, 0, [], TL).

take_desert(_, 3, [], _).
take_desert(_, 3, CL, TL) :-
    append(CL, TL, TL),
    writeln(CL),
    !,fail.

take_desert(N, I, CL, TL) :-
    desert(Name, NV, I),
    N >= NV,
    N1 is N - NV,
    take_desert(N1, I, [Name|CL], TL).
    
take_desert(N, I, CL, TL) :-
    I < 3,
    J is I + 1,
    take_desert(N, J, CL, TL).    

select_desert(N, TL) :- take_desert(N, 0, [], TL).

satisfy_hungry() :-
    starter(S, _, _),
    main_dish(M, _),
    desert(D, _, _),
    L = [S, M, D],
    writeln(L),
    fail.

satisfy_not_so_hungry(X) :-
    main_dish(Name, NV),
    N1 is 80 - NV,
    ( X = 1 -> select_starter(N1,[Name]) ; select_desert(N1,[Name])),
    fail.
    
find_items(Status, X, Y, Z) :-
    menu(Status, X, Y, Z),
    (Status = hungry, satisfy_hungry());
    (Status = not_so_hungry, satisfy_not_so_hungry(X)).


