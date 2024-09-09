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
bitwise_check(L, Len, Sign) :-
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
        (Len == 32 , Sign == signed) ->         %!!!loss %todo review these limitations
                all(L, -65535, 65535)
        ;
        (Len == 32, Sign == unsigned) ->        %!!!loss
                all(L, 0, 65535)
        ;
        (Len == 64, Sign == signed) ->          %!!!loss
                all(L, -65535, 65535)
        ;
        (Len == 64, Sign == unsigned) ->        %!!!loss
                all(L, 0, 65535)
        ),
        !.
bitwise_check(_, Len, Sign) :-
    ptc_solver__verbose("Invalid Length or Sign in bitwise_check: ", [Len, Sign]).

all([], _, _) :-
        !.
all([V|R], Min, Max) :-
    V #:: Min..Max,
    all(R, Min, Max).
%%%
%convert a decimal coded integer into its binary equivalent of length Len using Sign coding
%I must be ground, Sign is signed or unsigned
convert(I, Len, Sign, Ib) :-
    (Sign == unsigned ->
            convert2(I, Len, Ib)
    ;
     Sign == signed ->
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
build_list(0, Z, Z, []).
build_list(N, [_|Z], Z2, [0|R]) :-
    N =\= 0,
    N1 is N-1,
    build_list(N1, Z, Z2, R).

list_left_shift(Z2, S, Z3) :-
    build_list(S, Z2, Old_bits, New_bits),
    append(Old_bits, New_bits, Z3).

%%%
build_list_right(0, _, []) :-
    !.
build_list_right(N, V, [V|R]) :-
    N =\= 0,
    N1 is N-1,
    build_list_right(N1, V, R).

simple_right_shift(0, _, []) :-
    !.
simple_right_shift(N, [Z1|ZL], [Z1|R]) :-
    N =\= 0,
    N1 is N-1,
    simple_right_shift(N1, ZL, R).

list_right_shift(Z2, S, V, Result) :-
    build_list_right(S, V, New_bits),
    append(New_bits, Z2, Z3),
    length(Z2, S1),
    simple_right_shift(S1, Z3, Result).

%%%
%Bitwise negation, unary C operator ~
s_bwnot(X, Len, Sign, Z) :-
    Xeval #= X+0,
    bitwise_check([Xeval], Len, Sign),
    s_bwnot2(Xeval, Len, Sign, Z).

s_bwnot2(X, Len, Sign, Z) :-
    (not nonground(X) ->
        (convert(X, Len, Sign, Z2),
         list_negate(Z2, Z3),
         to_decimal(Z3, Sign, Z)
        )
    ;
     not nonground(Z) ->
        s_bwnot(Z, Len, Sign, X)
    ;
        suspend(s_bwnot2(X, Len, Sign, Z), 3, [X, Z]->inst)
    ).
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
    ((not nonground(X), not nonground(Y)) ->
        (convert(X, Len, Sign, X2),
         convert(Y, Len, Sign, Y2),
         list_and(X2, Y2, Z2),
         to_decimal(Z2, Sign, Z)
        )
    ;
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
%new bits coming from the right are zeros
s_left_shift(X, S, Len, Sign, Z) :-
    Xeval #= X+0,
    Seval #= S+0,
    Seval #>= 0,
    Seval #=< Len,
    bitwise_check([Xeval], Len, Sign),
    s_left_shift2(Xeval, Seval, Len, Sign, Z).

s_left_shift2(X, S, Len, Sign, Z) :-
    ((not nonground(X), not nonground(S)) ->
        (convert(X, Len, Sign, Z2),
         list_left_shift(Z2, S, Z3),
         to_decimal(Z3, Sign, Z)
        )
    ;
        suspend(s_left_shift2(X, S, Len, Sign, Z), 3, [X, S]->inst)
    ).

%%%
%right shift, S is the amount of shifting, binary C operator >>
%new bits coming from the right are zeros if unsigned or the sign bit otherwise if signed
s_right_shift(X, S, Len, Sign, Z) :-
    Xeval #= X+0,
    Seval #= S+0,
    Seval #>= 0,
    Seval #=< Len,
    bitwise_check([Xeval], Len, Sign),
    s_right_shift2(Xeval, Seval, Len, Sign, Z).

s_right_shift2(X, S, Len, Sign, Z) :-
    ((not nonground(X), not nonground(S)) ->
        (convert(X, Len, Sign, Z2),
         (Sign == unsigned ->
            list_right_shift(Z2, S, 0, Z3)
         ;
          Sign == signed ->
            (Z2 = [V|_],
             list_right_shift(Z2, S, V, Z3)
            )
         ),
         to_decimal(Z3, Sign, Z)
        )
    ;
        suspend(s_right_shift2(X, S, Len, Sign, Z), 3, [X, S]->inst)
    ).
%%%%%%%%%%%%% THE END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%