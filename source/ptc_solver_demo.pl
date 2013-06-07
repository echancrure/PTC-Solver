%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 08/02/01
% Eclipse 5.8 program
% solver_demo.pl
% demo of ATGen's solver capabilities 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ptc_solver).                                
%:- [ptc_solver].		%the solver is compiled and imported
:- [util__post_precedence].                           %
:- import print__symbolic/1, print__constraints/1, print__list/1, print__ranges/2, print__effects/2, print__solutions/2 from print.
:- import ptc_solver__label_integers/1, ptc_solver__label_enums/1, ptc_solver__label_reals/1 from ptc_solver.
:- import ptc_solver__get_frame/3, ptc_solver__sdl/1, ptc_solver__clean_up/0, ptc_solver__default_declarations/0 from ptc_solver.
:- import ptc_solver__type/2, ptc_solver__type/3, ptc_solver__type/4, ptc_solver__subtype/2, ptc_solver__subtype/3 from ptc_solver.
:- import ptc_solver__variable/2 from ptc_solver.
:- import ptc_solver__is_enum/1, ptc_solver__is_record/1, ptc_solver__is_array/1 from ptc_solver.
:- import ptc_solver__is_integer/1, ptc_solver__is_real/1 from ptc_solver.
:- import ptc_solver__create_record_from_agg/3 from ptc_solver.
:- import ptc_solver__create_array_from_agg/3 from ptc_solver.
:- import ptc_solver__submit_string/1, ptc_solver__get_all_variables/1 from ptc_solver.
:- import ptc_solver__set_flag/2 from ptc_solver.
:- import ptc_solver__is_rational/1, ptc_solver__rational_to_decimal/2 from ptc_solver.

eileenlarge210205([I, R]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([R], float),
        ptc_solver__sdl(R = 1000001+1).

eileenorelse270105([X3,X2,Y4,Z5,Z1,T1,Z7,Z8, R1]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(myinteger, integer, range_bounds(-10000, 10000)),
        ptc_solver__variable([X3,X2,Y4,Z5,Z1,T1,Z7,Z8], myinteger),
        ptc_solver__type(reif_type, integer, range_bounds(0, 1)),
        ptc_solver__variable([R1, R2, R3, R4], reif_type),
        ptc_solver__sdl(Z7 = 0),
        ptc_solver__sdl(T1 = 42),
        ptc_solver__sdl(reif(Z1 = 0 or_else T1 = 99, R1)),
        ptc_solver__sdl(R1 = 1).

%david 28/06/04
order280604([X3,X2,Y4,Z5,T1,Z7,Z8]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(myinteger, integer, range_bounds(-10000, 10000)),
        ptc_solver__variable([X3,X2,Y4,Z5,Z1,T1,Z7,Z8], myinteger),
        ptc_solver__sdl(Y4<=X3),
        ptc_solver__sdl(45=Z7),
        ptc_solver__sdl(Z7 = T1-100),
        ptc_solver__sdl(T1 = Y4-X3), 
        ptc_solver__label_integers([X3,X2,Y4,Z5,T1,Z7,Z8]).
inter280604([X3,X2,Y4,Z5,T1,Z7,Z8]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(myinteger, integer, range_bounds(-10000, 10000)),
        ptc_solver__variable([X3,X2,Y4,Z5,Z1,T1,Z7,Z8], myinteger),
        ptc_solver__sdl(Y4<=X3 and T1 = Y4-X3 and Z7 = T1-100 and 45=Z7),       %this should fail directly and it does
        ptc_solver__label_integers([X3,X2,Y4,Z5,T1,Z7,Z8]).
david280604([X3,X2,Y4,Z5,T1,Z7,Z8]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(myinteger, integer, range_bounds(-10000, 10000)),
        ptc_solver__variable([X3,X2,Y4,Z5,Z1,T1,Z7,Z8], myinteger),
        ptc_solver__sdl(X3=X2-42 and ((Y4>X3 and Z5 = Y4-X3 and Z1 = Z5) or (Y4<=X3 and T1 = Y4-X3 and Z7 = T1-100 and Z1=Z7)) and Z8 = Z1-45 and Z8 = 0),
        ptc_solver__label_integers([X3,X2,Y4,Z5,T1,Z7,Z8]).

%Eileen problem 12/05/04
pb120504([A,B,C]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(myinteger, integer, range_bounds(0, 10000)),
        ptc_solver__variable([A,B,C], myinteger),

        ptc_solver__sdl(A<=B),                          %30F   
        ptc_solver__sdl(A<>B and B<>C),                 %27F
        ptc_solver__sdl(A<>C and B<>C and A<>B),        %33F
        ptc_solver__sdl(A*A+B*B-C*C< 1.0001*1.0001),    %39T
        ptc_solver__sdl(A*A+B*B>=C*C),                  %42F
        %ptc_solver__sdl(C=134),
        %ptc_solver__sdl(B=2991),
        %ptc_solver__sdl(A=2994).
        ptc_solver__label_integers([A,B,C]).

%Eileen problem 11/05/04
pb110504([M]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ten, integer, range_bounds(0, 2)),
        ptc_solver__type(reif_type, integer, range_bounds(0, 1)),
        ptc_solver__type(char_type, integer, range_bounds(0, 255)),
        ptc_solver__variable([R1, R2, R3, R4], reif_type),
        ptc_solver__type(m_type, array, [ten], char_type),
        ptc_solver__variable([M], m_type),
        ptc_solver__sdl(reif(element(M, [0]) <> 32, R1)),
        ptc_solver__sdl(reif(element(M, [0]) <> 10, R2)), 
        ptc_solver__sdl(reif(R1<>0 and_then R2<>0, R3)),
        ptc_solver__sdl(reif(element(M, [0]) <> 9, R4)). %should really conclude earlier see 11/05/04 Eileen MSc diary


%Eileen problem with reif 30/04/04
pb300404([M0, M1, M2]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ten, integer, range_bounds(0, 2)),
        ptc_solver__type(reif_type, integer, range_bounds(0, 1)),
        ptc_solver__type(char_type, integer, range_bounds(0, 255)),
        ptc_solver__variable([R01, R02, R03, R11, R12, R13, R21, R22, R23], reif_type),
        ptc_solver__type(m_type, array, [ten], char_type),
        ptc_solver__variable([M], m_type),
        ptc_solver__variable([M0, M1, M2], char_type),
        ptc_solver__sdl(M = up_arr(M, [2], 0)), % the 3rd element is 0 [?,?,0]
	%%%
        ptc_solver__sdl(reif(element(M, [0]) = 32, R01)),
        ptc_solver__sdl(reif(element(M, [0]) = 10, R02)), 
        ptc_solver__sdl(reif(R01<>0 or_else R02<>0, R03)),
        ptc_solver__sdl(R03 = 1),
        %%%
        ptc_solver__sdl(reif(element(M, [1]) = 32, R11)),
        ptc_solver__sdl(reif(element(M, [1]) = 10, R12)), 
        ptc_solver__sdl(reif(R11<>0 or_else R12<>0, R13)),
        ptc_solver__sdl(R13 = 1),
        %%%
        ptc_solver__sdl(element(M, [0]) = M0),
        ptc_solver__sdl(element(M, [1]) = M1),
        ptc_solver__sdl(element(M, [2]) = M2),

        ptc_solver__label_integers([M0, M1, M2]).


%Eileen problem with reif 28/04/04
pb280404([M, R1, R2, R3, A, B]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ten, integer, range_bounds(0, 4)),
        ptc_solver__type(reif_type, integer, range_bounds(0, 1)),
        ptc_solver__variable([R1, R2, R3], reif_type),
        ptc_solver__type(m_type, array, [ten], integer),
        ptc_solver__variable([M], m_type),
        ptc_solver__variable([A, B], integer),
        ptc_solver__sdl(M = up_arr(M, [4], 0)), % the 5th element is 0 [?,?,?,?,0]
	ptc_solver__sdl(reif(element(M, [4]) = 32, R1)), %R1 should 0 now
        ptc_solver__sdl(reif(A>B, R2)), %R2 should be unknown
        ptc_solver__sdl(reif(R1<>0 or_else R2<>0, R3)), %R3 should be unknown
        ptc_solver__sdl(R3 = 1). %should deduce R2 be 1

e(A,B,C,D,E, X):-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
	ptc_solver__variable([A,B,C,D,E], integer),
	ptc_solver__variable([X], float),
        ptc_solver__sdl(eq_cast(A, X)),
        ptc_solver__sdl(X = 2.6),
        ptc_solver__sdl(B = 2.0),
	ptc_solver__sdl(C= conversion(integer, 2.6)).

	

d(A,B) :-	ptc_solver__clean_up,
        ptc_solver__default_declarations,
	ptc_solver__type(float, real, range_bounds(-3.4*(10.0**38), 3.4*(10.0**38))),
	ptc_solver__set_flag(enumeration_start, 50),
	ptc_solver__type( name_t, enumeration, [ mon, tue, wed, thu, fri, sat, sun] ),
	ptc_solver__set_flag(enumeration_start,-999),
	ptc_solver__type( nom_t, enumeration, [ lun, mar, mer, jeu, ven, sam, dim] ),
	ptc_solver__subtype(week_t, name_t, range_bounds(mon, fri)),
	ptc_solver__variable([A,B], integer),
	ptc_solver__sdl(A=pos(name_t, first(name_t)) and B=pos(nom_t, mer)).


c(A) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__sdl(conversion(float, 60)>55.2).

g(A, B) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ten, integer, range_bounds(-10, 10)),
        ptc_solver__variable([A], ten),
        ptc_solver__variable([B], float),
        ptc_solver__sdl(B = 1.6),
        ptc_solver__sdl(A = B),
        ptc_solver__label_integers([A]),
        ptc_solver__label_reals([B]).

jc([A,B,C]) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ten, integer, range_bounds(1, 10000)),
        ptc_solver__variable([A,B,C], ten),
        ptc_solver__sdl(not(A*A+B*B-C*C<0.0001*0.0001)),
        ptc_solver__label_integers([A, B, C]).

arr(A) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ten, integer, range_bounds(0, 9)),
        ptc_solver__type(m_type, array, [ten], integer),
        ptc_solver__variable([M], m_type),
        ptc_solver__sdl(element(up_arr(M, [0], 32+10), [0])>32),
	ptc_solver__variable([M_out], m_type),
	ptc_solver__sdl(M_out = up_arr(M, [0], 32+10)).
        
cmp(A, B, Z) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([A,B,C], float),
        ptc_solver__variable([Z], integer),
        ptc_solver__sdl(cmp(A, B, Z) and C=5),
        ptc_solver__label_reals([A,B,C]).

bitwise(A) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([A,B,C,R], integer),
        ptc_solver__sdl(bw_or(69, 113, 8, signed) = A),
        ptc_solver__label_integers([]).

new_or :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([A,B,C,R], integer),
        ptc_solver__sdl(A=5 or B=4),
        ptc_solver__sdl(A=2 and B=9).

reif :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([A,B,C,R], integer),
        ptc_solver__sdl(reif(A>B, R)),
        ptc_solver__sdl(A = 8),
        ptc_solver__sdl(B = 5).

        
martin30 :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__type(ran, integer, range_bounds(1,10)),
        ptc_solver__type(arr, array, [ran], integer),
        ptc_solver__variable([Test], arr),
        ptc_solver__sdl(element(Test, [2]) = 10).
        
rem(Year) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([Year, I], integer),
        ptc_solver__sdl(I rem 5 <> 0),
        ptc_solver__sdl(I = 5).

mod(Year) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([Year, I], integer),
        ptc_solver__sdl(I > 0),
        ptc_solver__sdl(I mod 5 <> 0),
        ptc_solver__sdl(I = 4).

not_leap(Year) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([Year], integer),
        ptc_solver__sdl(not((Year mod 4 = 0 and Year mod 100 <> 0) or Year mod 400 = 0)),
        ptc_solver__label_integers([Year]).

or_martin :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
     %   ptc_solver__set_flag(or_constraint_behaviour, choice),
        ptc_solver__type( name_t, enumeration, [ mon, tue, wed, thu, fri, sat, sun] ),
        ptc_solver__type( day_t, integer, range_bounds( 1, 31 ) ),
        ptc_solver__type( month_t, enumeration, [ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec] ),
        ptc_solver__type( year_t, integer, range_bounds( 1900, 3000 ) ),
        ptc_solver__type( date_t, record, [ ([name], name_t), ([day], day_t), ([month], month_t), ([year], year_t) ] ),
        ptc_solver__variable( [ VSE_14_next_date_out, VSE_14_date_out ], date_t),
        ptc_solver__variable( [ VSE_14_next_date, VSE_14_date ], date_t),
        ptc_solver__variable( [ VSE_8_year ], year_t),
        ptc_solver__sdl(field(VSE_14_date,name) = sun),
        ptc_solver__sdl(not( (field(VSE_14_date,month) = dec and field(VSE_14_date,day) = 31) )),
        ptc_solver__sdl(not( (field(VSE_14_date,day) = 28 and field(VSE_14_date,month) = feb) )),
        ptc_solver__sdl((field(VSE_14_date,day) = 31 or (field(VSE_14_date,day) = 29 and field(VSE_14_date,month) = feb) or (field(VSE_14_date,day) = 30 and (field(VSE_14_date,month) = apr or field(VSE_14_date,month) = jun or field(VSE_14_date,month) = sep or field(VSE_14_date,month) = nov)))),
        ptc_solver__sdl(VSE_14_next_date_out=up_rec(up_rec(up_rec(VSE_14_date,name,mon),day,1),month,succ(field(VSE_14_date,month)))),
        ptc_solver__sdl(field(VSE_14_date, month) = jan),
        ptc_solver__sdl(field(VSE_14_date, year) = 1967),
        ptc_solver__sdl(field(VSE_14_date, day) = 12).

m(D) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([A], float),
        ptc_solver__label_reals([A]),
        ptc_solver__is_rational(A) ->
        (        ptc_solver__rational_to_decimal(A, D)
        ;
                fail
        ).

try :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__variable([A, B, C], integer),
        ptc_solver__set_flag(or_constraint_behaviour, pure),
        ptc_solver__sdl(A=1 or B=2),
        (ptc_solver__sdl(C=1)
        ;
         ptc_solver__sdl(C=2)
        ),
        label_integers([A,B,C]),
        printf("A:%Gw\n", A),
        printf("B:%Gw\n", B),
        printf("C:%Gw\n", C),
        fail.

label_integers(List) :-        
        ptc_solver__label_integers(List),
        !.


example_label_floats(X, Y) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
	ptc_solver__variable([X, Y, Z], float),
	ptc_solver__sdl(X>2 and X<8 and Y>X and X=Z*Z+2),
	ptc_solver__label_reals([X, Y, Z]).

example(A, B) :-
        ptc_solver__clean_up,                       %to start in a clean environment        
        ptc_solver__default_declarations,           %solver initialisations
        ptc_solver__variable([A, B], integer),      %A and B are declared as integers
        ptc_solver__sdl(A>45 and B-5=A*A),          %constraints are imposed
        ptc_solver__label_integers([A,B]).          %a unique random solution is generated    


test(X, Y) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
	ptc_solver__variable([X, Y], integer),
	ptc_solver__sdl(X=2 or X =3),
	ptc_solver__label_integers([X, Y]).

go :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
        ptc_solver__submit_string("ptc_solver__variable([Z1, Z2], float)"),
        ptc_solver__variable([TF1], float),
        ptc_solver__is_record(TF1),
        ptc_solver__submit_string("ptc_solver__sdl(Z1 = 1.5)"),
	ptc_solver__submit_string("ptc_solver__sdl(Z2 = Z1/3)"),
        ptc_solver__submit_string("ptc_solver__variable([X, Y], integer)"),
        ptc_solver__submit_string("ptc_solver__sdl(X>Y*Y+Y and Y<5)"),
        ptc_solver__submit_string("ptc_solver__variable([A, A1], integer)"),
        ptc_solver__submit_string("ptc_solver__sdl(A=1 or A= 3)"),
        ptc_solver__submit_string("ptc_solver__sdl(Y = 2)"),
        fail.

martin(Next_date) :-
        ptc_solver__clean_up,
        ptc_solver__default_declarations,
	ptc_solver__type(name_t, enumeration, [mon, tue, wed, thu, fri, sat, sun]),
	ptc_solver__type(day_t, integer, range_bounds(1, 31)),
	ptc_solver__type(month_t, enumeration, [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]),
        ptc_solver__type(year_t, integer, range_bounds(1900, 3000)),
        ptc_solver__type(date_t, record, [([name], name_t), ([day], day_t), ([month], month_t), ([year], year_t)]),
        ptc_solver__variable([Next_date], date_t),
        ptc_solver__variable([T], integer),
        ptc_solver__sdl(field(Next_date, year) < 1940),
                ptc_solver__sdl(field(Next_date, name) = last(name_t)),

                ptc_solver__sdl(field(Next_date, year) = T),
                ptc_solver__label_integers([T]).

        
m2 :-
        ptc_solver__submit_string("ptc_solver__clean_up, ptc_solver__default_declarations"),
	ptc_solver__submit_string("ptc_solver__type(name_t, enumeration, [mon, tue])"),
	ptc_solver__submit_string("ptc_solver__type(date_t, record, [([name], name_t)])"),
	ptc_solver__submit_string("ptc_solver__variable([Try], name_t)"),
	ptc_solver__get_all_variables(All_vars),
	All_vars = [(_, T)],
	(ptc_solver__is_record(T) ->
	        true
        ;
	        ptc_solver__is_enum(T)
        ).

martin2109 :-
        ptc_solver__submit_string("ptc_solver__clean_up, ptc_solver__default_declarations"),
        ptc_solver__type( name_t, enumeration, [ mon, tue, wed, thu, fri, sat, sun] ),
        ptc_solver__type( day_t, integer, range_bounds( 1, 31 ) ),
        ptc_solver__type( month_t, enumeration, [ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec] ),
        ptc_solver__type( year_t, integer, range_bounds( 1900, 3000 ) ),
        ptc_solver__type( date_t, record, [ ([name], name_t), ([day], day_t), ([month], month_t), ([year], year_t) ] ),
        ptc_solver__variable( [ VSE_440_next_date, VSE_451_date ], date_t),
        ptc_solver__variable( [ VSE_159_year ], year_t),
        p1(VSE_440_next_date, VSE_451_date, VSE_159_year),
        p2(VSE_440_next_date, VSE_451_date, VSE_159_year),
        p3(VSE_440_next_date, VSE_451_date, VSE_159_year),
        p4(VSE_440_next_date, VSE_451_date, VSE_159_year),
        p5(VSE_440_next_date, VSE_451_date, VSE_159_year),
        p6(VSE_440_next_date, VSE_451_date, VSE_159_year),
        p7(VSE_440_next_date, VSE_451_date, VSE_159_year).

p1(VSE_440_next_date, VSE_451_date, VSE_159_year):-
        (S1= 1 , ptc_solver__sdl(true) ; S1 = 0).
p2(VSE_440_next_date, VSE_451_date, VSE_159_year):-
        (S2= 1 , ptc_solver__sdl(field(VSE_451_date,name) = sun) ; S2 = 0).
p3(VSE_440_next_date, VSE_451_date, VSE_159_year):-
        (S3= 1 , ptc_solver__sdl(true) ; S3 = 0).
p4(VSE_440_next_date, VSE_451_date, VSE_159_year):-        
        (S4= 1 , ptc_solver__sdl((field(VSE_451_date,month) = dec and field(VSE_451_date,day) = 31)) ; write(4)).
p5(VSE_440_next_date, VSE_451_date, VSE_159_year):-
        (S5= 1 , ptc_solver__sdl(true) ; write(5)).
p6(VSE_440_next_date, VSE_451_date, VSE_159_year):-        
        fail.
p7(VSE_440_next_date, VSE_451_date, VSE_159_year):-        
        fail.

session3(X, Y) :-
	ptc_solver__submit_string("ptc_solver__clean_up, ptc_solver__default_declarations"),
	ptc_solver__variable([X, Y], integer),
	call(q1(X, Y)),
	call(q2(X, Y)),
	call(q4(X, Y)),
	call(q5(X, Y)),
	call(q6(X, Y)),
	call(q7(X, Y)),
	call(q8(X, Y)),
	call(q9(X, Y)),
	call(q10(X, Y)),
	call(q11(X, Y)),
	call(q12(X, Y)),
	call(q13(X, Y)).

q1(X, Y) :-	
	(S1 = 1, ptc_solver__sdl(X>2 or Y>2) ; write(1)).
q2(X, Y) :-
	(S2 = 1, ptc_solver__sdl(X<2) ; write(2)).

q4(X, Y) :-
	fail.
q5(X, Y) :-
	(S3 = 1, ptc_solver__sdl(not(X<2)) ; write(3)).
q6(X, Y) :-
	true,
	!.
q7(X, Y) :-
	fail.
q8(X, Y) :-
	fail.
q9(X, Y) :-
	(S4 = 1, ptc_solver__sdl(not(X>2 or Y>2)) ; writew(4)).
q10(X, Y) :-
	(S5 = 1, ptc_solver__sdl(X<2) ; write(5)).
q11(X, Y) :-
	true,
	!.
q12(X, Y) :-
	fail.
q13(X, Y) :-
	(S6 = 1, ptc_solver__sdl(not(X<2)); S6 = 0).

pb(Date1, Date2) :-
        ptc_solver__submit_string("ptc_solver__clean_up, ptc_solver__default_declarations"),
        ptc_solver__type( name_t, enumeration, [ mon, tue, wed, thu, fri, sat, sun] ),
        ptc_solver__type( day_t, integer, range_bounds( 1, 31 ) ),
        ptc_solver__type( month_t, enumeration, [ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec] ),
        ptc_solver__type( year_t, integer, range_bounds( 1900, 3000 ) ),
        ptc_solver__type( date_t, record, [ ([name], name_t), ([day], day_t), ([month], month_t), ([year], year_t) ] ),
	ptc_solver__type( date_3, record, [([n1, n2, n3], date_t)]),
	ptc_solver__variable( [ Date1, Date2], date_3),
        %ptc_solver__sdl(Date2 = up_rec(Date1, n2, up_rec(field(Date1, n2), name, sun))).
	ptc_solver__sdl(Date2 = up_rec(field(Date1, n2), name, sun)).
        