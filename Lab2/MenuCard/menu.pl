/*Database for possible state and their corresponding 
    dish type values*/
menu(hungry, 1, 1, 1).
menu(not_so_hungry, 1, 1, 0).
menu(not_so_hungry, 0, 1, 1).
menu(diet, 1, 0, 0).
menu(diet, 0, 1, 0).
menu(diet, 0, 0, 1).

/*Database for possible starters and their corresponding 
    values*/
starter("Corn Tikki", 30, 0).
starter("Tomato Soup", 20, 1).
starter("Chilli Paneer", 40, 2).
starter("Crispy Chicken", 40, 3).
starter("Papdi Chaat", 20, 4).
starter("Cold Drink", 20, 5).

/*Database for possible main dish and their corresponding 
    values*/
main_dish("Kadhai Paneer with Butter/Plain Naan", 50).
main_dish("Veg Korma with Butter/Plain Naan", 40).
main_dish("Murgh Lababdar with Butter/Plain Naan", 50).
main_dish("Veg Dum Biryani with Dal Tadka", 50).
main_dish("Steam Rice with Dal Tadka", 40).

/*Database for possible desert and their corresponding 
    values*/
desert("Ice-cream", 20, 0).
desert("Malai Sandwich", 30, 1).
desert("Rasmalai", 10, 2).

/*Base case: When we have exahausted all possible starters 
    and have not selected any item*/
take_starter(_, 6, [], _).

/*Base case: When we have exahausted all possible starters 
            and have selected a list of starters we output the total list*/
take_starter(_, 6, CurrentList, PreviousList) :-
    CurrentList \= [],
    append(PreviousList, CurrentList, TotalList),
    writeln(TotalList).

/*  Case 1: For the current indexed starter we check if it is possible
            to take it in our menu if yes add it in list and call recursive
            function with updated left nutrient value*/
take_starter(NutrientValue, Index, CurrentList, PreviousList) :-
    starter(SelectedName, SelectedNutrientValue, Index),
    NutrientValue >= SelectedNutrientValue,
    RemainingNutrientValue is NutrientValue - SelectedNutrientValue,
    take_starter(RemainingNutrientValue, Index, [SelectedName|CurrentList], PreviousList).

/*  Case 2: For the current indexed starter we leave the starter and 
            move to the next starter*/ 
take_starter(NutrientValue, Index, CurrentList, PreviousList) :-
    Index < 6,
    NextIndex is Index + 1,
    take_starter(NutrientValue, NextIndex, CurrentList, PreviousList).    

 /*Wrapper function to generate all possible combinations of starters given
    a nutrient value*/
select_starter(NutrientValue, PreviousList) :- take_starter(NutrientValue, 0, [], PreviousList).


/*Base case: When we have exahausted all possible desert 
    and have not selected any item*/
take_desert(_, 3, [], _).

/*Base case: When we have exahausted all possible desert 
            and have selected a list of desert we output the total list*/
take_desert(_, 3, CurrentList, PreviousList) :-
    CurrentList \= [],
    append(PreviousList, CurrentList, TotalList),
    writeln(TotalList).

/*  Case 1: For the current indexed desert we check if it is possible
            to take it in our menu if yes add it in list and call recursive
            function with updated left nutrient value*/
take_desert(NutrientValue, Index, CurrentList, PreviousList) :-
    desert(SelectedName, SelectedNutrientValue, Index),
    NutrientValue >= SelectedNutrientValue,
    RemainingNutrientValue is NutrientValue - SelectedNutrientValue,
    take_desert(RemainingNutrientValue, Index, [SelectedName|CurrentList], PreviousList).

/*  Case 2: For the current indexed desert we leave the desert and 
            move to the next desert*/ 
take_desert(NutrientValue, Index, CurrentList, PreviousList) :-
    Index < 3,
    NextIndex is Index + 1,
    take_desert(NutrientValue, NextIndex, CurrentList, PreviousList).    

 /*Wrapper function to generate all possible combinations of desert given
    a nutrient value*/
select_desert(NutrientValue, PreviousList) :- take_desert(NutrientValue, 0, [], PreviousList).

satisfy_hungry() :-
    starter(StarterName, _, _),
    main_dish(MainDishName, _),
    desert(DesertName, _, _),
    DishCombinationList = [StarterName, MainDishName, DesertName],
    write(DishCombinationList).

/*This function generates all possible items combination by choosing a main dish
 and then chossing a starter or desert satisfying the nutrient condition as per 
    the combination requested*/
satisfy_not_so_hungry(Starter) :-

    /*Select a main dish and caluclate left nutrient value*/
    main_dish(DishName, NutrientValue),
    RemainingNutrientValue is 80 - NutrientValue,
    ( Starter = 1 -> 
         /*If starter is 1 we choose starters satisfyng the nutrient constraint*/
        (
            starter(SelectedStarterName,SelectedStarterValue,_),
            SelectedStarterValue =< RemainingNutrientValue,
            DishCombinationList = [DishName,SelectedStarterName],
            write(DishCombinationList)
        ); 
         /*If starter is 1 we choose deserts satisfyng the nutrient constraint*/
        (
            desert(SelectedDesertName,SelectedDesertValue,_),
            SelectedDesertValue =< RemainingNutrientValue,
            DishCombinationList = [DishName,SelectedDesertName],
            write(DishCombinationList)
        )
        
    ).

/*This functions selects a main dish satisyfying the nutrient constraint*/
satisfy_main_dish_diet() :-
    main_dish(DishName, NutrientValue),
    NutrientValue =< 40,
    write([DishName]).

/*This is a wrapper function to select the corresponding item type as requested for diet*/
satisfy_diet(Starter, MainDish, Desert):-
    Starter = 1, forall(select_starter(40, []), nl);
    MainDish = 1, forall(satisfy_main_dish_diet(), nl);
    Desert = 1, forall(select_desert(40, []), nl).

/*This is wrapper function to check if the requested combination is possible and if possible
    generate all the combinations */
find_items(Status, Starter, MainDish, Desert) :-
    menu(Status, Starter, MainDish, Desert),
    (
        Status = hungry, forall(satisfy_hungry(), nl);
        Status = not_so_hungry, forall(satisfy_not_so_hungry(Starter), nl);
        Status = diet, satisfy_diet(Starter, MainDish, Desert)
    ).
