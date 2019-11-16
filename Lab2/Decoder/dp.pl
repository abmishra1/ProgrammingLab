/* Base case when all string has been consumed we
unify the answer of number of ways with calculated value*/
decode_dp(_, PreviousNumberOfWays, _, [], PreviousNumberOfWays).

/*Case 1: When current character is 0 and previous character
 is not 1 or 2 , this means no possible solution exists*/
decode_dp(_, _, LastChar, [CurrentChar|_], NumberOfWays) :-
    CurrentChar = 48,
    LastChar \= 49,
    LastChar \= 50,
    NumberOfWays is 0,
    !, fail.

/*Case 2: This consist of two seperate cases:
    Case 2a: When current character is 0 we have already checked
            that if current character is 0 our previous character
             is 1 or 2
    Case 2b: When current character is not equal to 0 but is some
             number between 1 to 9*/
decode_dp(PrePreviousNumberOfWays, PreviousNumberOfWays, LastChar, [CurrentChar|CodedList], NumberOfWays) :-

    /*Case 2a:  In this case we must combine the previous 1 or 2 
                with this 0 to create a possible decoding therfore 
                the number of ways till now is equal to number of 
                ways of string with last 2 character removed (OldPreviousNumberOfWays)*/
    (CurrentChar = 48, decode_dp(0, PrePreviousNumberOfWays, CurrentChar, CodedList, NumberOfWays));

    /*Case 2b:  In this case current character is between 1 to 9.
                Therefore it can be independently taken to represent
                a letter between 'A' to 'I'. However if the previous 
                character was 1 or the previous character was 2 and
                current character is between 1 to 6 these last two 
                chracters can be combined to represent a letter*/
    (
        (CurrentChar > 48, CurrentChar < 58),
        SumOfNumberOfWays is PreviousNumberOfWays + PrePreviousNumberOfWays,
        (
            ((LastChar = 49);((LastChar = 50),(CurrentChar >= 48, CurrentChar < 55))) -> 
            decode_dp(PreviousNumberOfWays, SumOfNumberOfWays, CurrentChar, CodedList, NumberOfWays); 
            decode_dp(PreviousNumberOfWays, PreviousNumberOfWays, CurrentChar, CodedList, NumberOfWays)
        )
    ).

/*Wrapper Function to answer the total number of ways any given string
    can be decoded.*/
decode(NumberOfWays, InputString) :-
    string_codes(InputString, CodedList),
    % writeln(CodedList),
    decode_dp(1, 1, 0, CodedList, NumberOfWays).