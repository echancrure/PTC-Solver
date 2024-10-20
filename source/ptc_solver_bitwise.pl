%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 19/06/02
% Eclipse 7.0 program
% bitwise constraints
% part of the solver module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The solver works on decimals only (no binary, octal or hexadecimals)
%  therefore the binary encoding must be specified : bit length, signed/unsigned
%  2's complement is assumed for signed encoding
% Eclipse 's binary representation cannot be used as it deletes leading zeros
%  and therefore the length of the encoding cannot be detected
%%%
%L must be a list of integer decimal number including, possibly, a minus sign for negative numbers
%Len must be one of : 8, 16, 32 or 64
%Sign must be one of : signed or unsigned
bitwise_check(L, Len, Sign) :-  %todo: is this necessary?
        ((Len == 8, Sign == signed) ->
                all(L, -127, 127)
        ;
        (Len == 8, Sign == unsigned) ->
                all(L, 0, 255)
        ;
        (Len == 16, Sign == signed) ->
                all(L, -32767, 32767)
        ;
        (Len == 16, Sign == unsigned) ->
                all(L, 0, 65535)
        ;
        (Len == 32 , Sign == signed) ->
                all(L, -inf, inf)
        ;
        (Len == 32, Sign == unsigned) ->
                all(L, 0, inf)
        ;
        (Len == 64, Sign == signed) ->
                all(L, -inf, inf)
        ;
        (Len == 64, Sign == unsigned) ->
                all(L, 0, inf)
        ),
        !.
bitwise_check(_, Len, Sign) :-
    ptc_solver__verbose("Invalid Length or Sign in bitwise_check: ", [Len, Sign]).

all(List, Min, Max) :-
    List #:: Min..Max.
%%%
%convert a decimal coded integer into its binary equivalent of length Len using Sign coding
%I must be ground, Sign is signed or unsigned
convert(I, Len, Sign, Ib) :-
    (Sign == 'unsigned' ->
            convert2(I, Len, Ib)
    ;
     Sign == 'signed' ->
        (Len2 is Len - 1,
         convert2(I, Len2, I2), %with one less bit
         (I > 0 ->
            Ib = [0|I2]
         ;
            twoscomplement(I2, Ib)
         )
        )
    ),
    !.

convert2(I, Len, L) :-
    I2 is abs(I),
    convert3(I2, Len, L),
    !.

convert3(0, 0, []) :-
    !.
convert3(_, 0, _) :-
    %should never happen due to contraints in bitwise_check
    ptc_solver__verbose("Overflow in binary conversion", []).
convert3(I, Len, L) :-
    !,
    I2 is I // 2,
    R is I mod 2,
    Len2 is Len -1,
    append(L2, [R], L),
    convert3(I2, Len2, L2).

twoscomplement(I2, Ib) :-
    list_negate([0|I2], I3),
    add1(I3, 1, Ib, _).

add_bits(1, 1, 0, 1).
add_bits(1, 0, 1, 0).
add_bits(0, 1, 1, 0).
add_bits(0, 0, 0, 0).

add1([R], A, [R2], N) :-   %a single element
    !,
    add_bits(R, A, R2, N).
add1([B|R], A, [B2|R2], N2) :-
    add1(R, A, R2, N),
    add_bits(B, N, B2, N2).

%%%
to_decimal(L, Sign, Z) :-
    (Sign == signed ->
        (L = [S|R],
         to_decimal2(R, _, R2),
         (S == 1 ->
            Z is -1*R2
         ;
          S == 0 ->
            Z is R2
         )
        )
    ;
     Sign == unsigned ->
        to_decimal2(L, _, Z)
    ).

to_decimal2([D], 1, R) :-
    !,
    R is D*2^0.
to_decimal2([D|L], N1, R) :-
    to_decimal2(L, N, R2),
    R is R2 + D*2^N,
    N1 is N + 1.

%%%
neg_bit(0, 1).
neg_bit(1, 0).

list_negate([], []).
list_negate([B|R], [B2|R2]) :-
    neg_bit(B, B2),
    list_negate(R, R2).

%%%
and_bit(0, 0, 0).
and_bit(0, 1, 0).
and_bit(1, 0, 0).
and_bit(1, 1, 1).

list_and([], [], []).
list_and([B|R], [C|S], [D|T]) :-
    and_bit(B, C, D),
    list_and(R, S, T).

%%%
or_bit(0, 0, 0).
or_bit(0, 1, 1).
or_bit(1, 0, 1).
or_bit(1, 1, 1).

list_or([], [], []).
list_or([B|R], [C|S], [D|T]) :-
    or_bit(B, C, D),
    list_or(R, S, T).
%%%
xor_bit(0, 0, 0).
xor_bit(0, 1, 1).
xor_bit(1, 0, 1).
xor_bit(1, 1, 0).

list_xor([], [], []).
list_xor([B|R], [C|S], [D|T]) :-
    xor_bit(B, C, D),
    list_xor(R, S, T).

%%%
%There is a lot of repeat code below refactoring should be performed
%%%
%Bitwise and, binary C operator &
s_bwand(X, Y, Len, Sign, Z) :-
    Xeval #= X+0,
    Yeval #= Y+0,
    bitwise_check([Xeval, Yeval], Len, Sign),
    s_bwand2(Xeval, Yeval, Len, Sign, Z).

s_bwand2(X, Y, Len, Sign, Z) :-
    ((not nonground(X), not nonground(Y)) ->    %could use ECLiPSe default bitwise and if it has the correct semantics
        (convert(X, Len, Sign, X2),
         convert(Y, Len, Sign, Y2),
         list_and(X2, Y2, Z2),
         to_decimal(Z2, Sign, Z)
        )
    ;           %this could be so much clever: X bwand2 Y: the result can only be smaller at least for unsigned
        suspend(s_bwand2(X, Y, Len, Sign, Z), 3, [X, Y, Z]->inst)
    ).

%%%
%Bitwise or, binary C operator |
s_bwor(X, Y, Len, Sign, Z) :-
    Xeval #= X+0,
    Yeval #= Y+0,
    bitwise_check([Xeval, Yeval], Len, Sign),
    s_bwor2(Xeval, Yeval, Len, Sign, Z).

s_bwor2(X, Y, Len, Sign, Z) :-
    ((not nonground(X), not nonground(Y)) ->
        (convert(X, Len, Sign, X2),
         convert(Y, Len, Sign, Y2),
         list_or(X2, Y2, Z2),
         to_decimal(Z2, Sign, Z)
        )
    ;
        suspend(s_bwor2(X, Y, Len, Sign, Z), 3, [X, Y, Z]->inst)
    ).

%%%
%Bitwise xor, binary C operator ^
s_bwxor(X, Y, Len, Sign, Z) :-
    Xeval #= X+0,
    Yeval #= Y+0,
    bitwise_check([Xeval, Yeval], Len, Sign),
    s_bwxor2(Xeval, Yeval, Len, Sign, Z).

s_bwxor2(X, Y, Len, Sign, Z) :-
    ((not nonground(X), not nonground(Y)) ->
        (convert(X, Len, Sign, X2),
         convert(Y, Len, Sign, Y2),
         list_xor(X2, Y2, Z2),
         to_decimal(Z2, Sign, Z)
        )
    ;
     (not nonground(X), not nonground(Z)) ->
        s_bwxor(X, Z, Len, Sign, Y)
    ;
     (not nonground(Y), not nonground(Z)) ->
        s_bwxor(Y, Z, Len, Sign, X)
    ;
        suspend(s_bwxor2(X, Y, Len, Sign, Z), 3, [X, Y, Z]->inst)
    ).

%%%
%left shift, S is the amount of shifting, binary C operator <<
s_left_shift(Le, S, Len, _Sign, R) :-
    S #>= 0,	            %todo what if not? It is undefined but what does this mean in gcc?
    S #< Len, 
    Mult #= 2 ^ S,
    R #= Le * Mult.     %todo check for overflow
%%%
%right shift, S is the amount of shifting, binary C operator >>
s_right_shift(Le, S, Len, Sign, R) :-
    S #>= 0,	            %todo what if not? It is undefined but what does this mean in gcc?
    S #< Len, 
    (Sign = 'unsigned' ->
        R #= Le // (2 ^ S)	%same as div by 2 power amount of right shift
    ;
        (Is_positive #= (Le #>= 0), %using reified constraint
         (Is_positive == 1 ->
            R #= Le // (2 ^ S)	    %same as div by 2 power amount of right shift
         ;
          Is_positive == 0 ->	    %the operand is negative: should use floor division: todo (is the number is wholly divisible that's fine if not, reduce by -1)
            R #= (Le // (2 ^ S)) - 1	%unsound if number is wholly divisible to fix
         ;
            suspend(s_right_shift(Le, S, Len, Sign, R), 3, [Le, R]->inst)	%probably more constraints e.g. R and Le have the same sign
         )
        )
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%