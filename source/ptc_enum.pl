%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chris Meudec
% Eclipse 7.0 program
% module enum for enumeration meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%an enum variable E is of the form: E{ptc_enum(Basetype, Subtype, Position)}, a ground enumeration variable is represented by its literal
%08/12/2022 remark: literals would be easier to handle as variables as Basetype and Subtype would always be available and fewer special case 
%                   (type testing) handling
:- module(ptc_enum).

:- export ptc_enum__clean_up/0, ptc_enum__record_enum/2, ptc_enum__is_enum/1, ptc_enum__is_enum_type/1, ptc_enum__create_enum/5,
	  ptc_enum__sample/1, ptc_enum__get_position/3, ptc_enum__get_position/2, ptc_enum__get_literal/3, ptc_enum__get_literal/2,
	  ptc_enum__get_basetype/2, ptc_enum__succ/2, ptc_enum__pred/2, ptc_enum__pos/3, ptc_enum__val/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(ic)).

:- meta_attribute(ptc_enum, [unify: unify_enum/2, print:print_enum/2]).

:- dynamic ptc_enum/2.

:- import ptc_solver__get_frame/3, ptc_solver__first/2, ptc_solver__last/2, ptc_solver__arithmetic/3 from ptc_solver.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%remark using suspend.pl template for metaterm handlers

%unify_enum(Value, Attribute)
unify_enum(_, Attr) :-
	/*** ANY + VAR ***/
	var(Attr).           % Ignore if no attribute for this extension
unify_enum(Term, Attr) :-
	compound(Attr),
	unify_term_enum(Term, Attr).

unify_term_enum(Literal, ptc_enum(Basetype, _, Position)) :-
	nonvar(Literal),          % The metaterm was instantiated
	/*** NONVAR + META ***/
	ptc_enum(Basetype, Enum_list),
	!,
	member((Literal, Position), Enum_list), %check if the literal is in the list
	!.
unify_term_enum(Y{AttrY}, AttrX) :-
	-?->
	unify_enum_enum(Y, AttrX, AttrY).

unify_enum_enum(_, AttrX, AttrY) :-
	var(AttrY),               % no attribute for this extension
	/*** VAR + META ***/
	AttrX = AttrY.            %share the attribute
unify_enum_enum(Y, ptc_enum(Basetype, _, Position), AttrY) :-
	nonvar(AttrY),
	/*** META + META ***/
	AttrY = ptc_enum(Basetype, _, Position),
	(not nonground(Position) ->          %only one possible literal
	    (ptc_enum(Basetype, Enum_list),
	     member((Literal, Position), Enum_list), %find the literal
	     Y = Literal
	    )
	;
	    true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%only for debug
print_enum(_{ptc_enum(Basetype, Subtype, Position)}, Print) :-
	-?->
	Print = ptc_enum(Basetype, Subtype, Position).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptc_enum__clean_up :-
	retractall(ptc_enum(_, _)).

%record the list of literals of a base enumeration type
ptc_enum__record_enum(Enum_type, Enum_list) :-
	assert(ptc_enum(Enum_type, Enum_list)).

%check if a variable is an enumeration variable
ptc_enum__is_enum(_{Attr}) :-
	-?->
	compound(Attr),
	Attr = ptc_enum(_, _, _).

%check if a type is an enumeration type
ptc_enum__is_enum_type(Enum_type) :-
	ptc_enum(Enum_type, _).

%create an enumeration variable or a literal if position is ground
ptc_enum__create_enum(Basetype, Subtype, First_pos, Last_pos,  E) :-
	Position #:: First_pos..Last_pos, %constrain the position to an integer IC variable
	(not nonground(Position) ->
	    ptc_enum__get_literal(Basetype, Position, E)
	;
	    add_attribute(E, ptc_enum(Basetype, Subtype, Position))
	).

%sample an enumeration variable
%ptc_enum__sample(E{Attr}) :-
%	-?->
%	Attr = ptc_enum(Base_type, _, Position),
%	!,
%	ptc_enum(Base_type, Enum_list),     %find the possible values for E
%	!,
%a trick to ensure that awaken goals are given the literal and not ptc_enum(Basetype, _, Position)
%	copy_term(Position, Position2),
%	length(Enum_list, L),
%	random(N),
%	mod(N, L, Start),
%	length(Start_list, Start),
%	append(Start_list, [(Literal, Position)], Dummy_list),
%	N2 is L-Start-1,
%	length(End_list, N2),
%	append(Dummy_list, End_list, List),
%	List = Enum_list,
%	E = Literal.

%sample an enumeration variable
ptc_enum__sample(E{Attr}) :-
	-?->
	Attr = ptc_enum(Base_type, _, Position),
	!,
	ptc_enum(Base_type, Enum_list),     %find the possible values for E
	!,
	%a trick to ensure that awaken goals are given the literal and not ptc_enum(Basetype, _, Position)
	copy_term(Position, Position2),
	member((Literal, Position2), Enum_list),  %always picks the first one: to change!
	E = Literal.

%get the position of a literal or of an enumeration variable of a given type
ptc_enum__get_position(Enum_type, Literal, Position) :-
	nonvar(Literal),
	ptc_solver__arithmetic(Literal, R, e),
	!,
	ptc_enum(Enum_type, Enum_list),             %find the enumeration list
	!,
	member((R, Position), Enum_list), %can fail
	!.
ptc_enum__get_position(_, _{Attr}, Position) :-
	-?->
	Attr = ptc_enum(_, _, Position).

%get the position of a literal or of an enumeration variable not knowing its type
ptc_enum__get_position(Literal, Position) :-
	nonvar(Literal),
	ptc_solver__arithmetic(Literal, R, e),	%this does very little
	!,
	ptc_enum(_, Enum_list),           %find an enumeration list
	member((R, Position), Enum_list), %can fail but what if R, the literal, appears in several enumeration list but in different position: which is the correct position?
	!.
ptc_enum__get_position(_{Attr}, Position) :-
	-?->
	Attr = ptc_enum(_, _, Position).

%get the literal corresponding to a ground position of a given type
ptc_enum__get_literal(Enum_type, Position, Literal) :-
	Position_eval $= eval(Position), %was Position #= Position_eval + 0,          %evaluate the position
	!,
	ptc_enum(Enum_type, Enum_list),             %find the enumeration list
	!,
	member((Literal, Position_eval), Enum_list), %can fail
	!.

%get the literal corresponding to a position not knowing its type
ptc_enum__get_literal(Position, Literal) :-
	Position_eval $= eval(Position),          %evaluate the position
	!,
	ptc_enum(_, Enum_list),                     %find an enumeration list
	member((Literal, Position_eval), Enum_list), %can fail
	!.

%get the basetype of an enumeration variable
ptc_enum__get_basetype(_{Attr}, Basetype) :-
	-?->
	Attr = ptc_enum(Basetype, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%apply the constraint 'R is the predecessor of X'
ptc_enum__pred(X, R) :-
	ptc_enum__succ(R, X).         %as simple as that

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apply the constraint 'R is the successor of X'
ptc_enum__succ(X, R) :-
	not nonground(X),                      %X is a literal
	!,
	ptc_enum(_, Enum_list),                %find ANY?? enumeration list
	sublist([(X, _), (R, _)], Enum_list),  %success or failure (backtracks)
	!.

ptc_enum__succ(X, R) :-
	not nonground(R),                      %R is a literal
	!,
	ptc_enum(_, Enum_list),                %find ANY?? enumeration list
	sublist([(X, _), (R, _)], Enum_list),  %success or failure (backtracks)
	!.

%X is an enumeration variable
%R is a simple variable (only happens in first call of an original succ)
ptc_enum__succ(X{ptc_enum(BasetypeX, _, _)}, R) :-
	-?->
	free(R),                               %R is a free variable, not a metaterm
	%we create, as a first approximation, an enumeration variable R with frame the basetype of X
	ptc_solver__get_frame(BasetypeX, base_enumeration, R),
	continue_init_succ(X, R).

%X is a simple variable (only happens in first call of an original pred)
%R is an enumeration variable
ptc_enum__succ(X, R{ptc_enum(BasetypeR, _, _)}) :-
	-?->
	free(X),                               %X is a free variable, not a metaterm
	%we create, as a first approximation, an enumeration variable X with frame the basetype of R
	ptc_solver__get_frame(BasetypeR, base_enumeration, X),
	continue_init_succ(X, R).

%X and R are two enumeration variables with X not the last of an enumeration type
ptc_enum__succ(X{ptc_enum(BasetypeX, _, PositionX)}, R{ptc_enum(_, _, PositionR)}) :-
	-?->
	%we ensure that R is greater than X
	ptc_enum(BasetypeX, Enum_listX),
	get_integer_bounds(PositionX, MinX, MaxX),
	sublist([(_, MinX), (_, MinR)], Enum_listX),
	sublist([(_, MaxX), (_, MaxR)], Enum_listX),
	PositionR #:: MinR..MaxR,
	(nonground(PositionR) ->
	    (%we ensure that X is less than R
		 get_integer_bounds(PositionR, New_minR, New_maxR),
	     sublist([(_, New_minX), (_, New_minR)], Enum_listX),
	     sublist([(_, New_maxX), (_, New_maxR)], Enum_listX),
	     PositionX #:: New_minX..New_maxX,
	     (nonground(PositionX) ->
			suspend(ptc_enum__succ(X, R), 3, [PositionX, PositionR]->ic:[min,max,hole,type])
	     ;
		 	sublist([(X, PositionX), (R, _)], Enum_listX) %X and R become ground
         )
	    )
    ;  
	    sublist([(X, _), (R, PositionR)], Enum_listX)  %X and R become ground
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
continue_init_succ(X{ptc_enum(BasetypeX, _, PositionX)}, R) :-
	-?->
	%we ensure that X cannot be the last of an enumeration type
	ptc_solver__last(BasetypeX, Sup_litX),
	ptc_enum(BasetypeX, Enum_list),
	sublist([(Max_litX, Max_posX), (Sup_litX, _)], Enum_list), %find the maximum position allowed for X
	PositionX #=< Max_posX,
	(nonground(PositionX) ->
        ptc_enum__succ(X, R)                  %and we apply the constraint on the two enumeration variables
	;                                      %PositionX can become ground
	    (X = Max_litX,
	     R = Sup_litX                      %X and R become ground
        )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptc_enum__val(Basetype, X, R) :-
	ptc_enum__pos(Basetype, R, X).             %as simple as that

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X is a literal, an enumeration variable or a typeless variable
%R is an integer, an integer variable or a typeless variable
ptc_enum__pos(Basetype, X, R) :-
	(not nonground(X) ; not nonground(R)),         %X is a literal, or R is an integer
	ptc_enum(Basetype, Enum_list),
	!,
	member((X, R), Enum_list),
	!.

ptc_enum__pos(Basetype, X, R) :-
	free(X),
	ptc_solver__get_frame(Basetype, base_enumeration, X),           %as a first approximation
	ptc_enum__pos(Basetype, X, R).                     %we apply the constraint

ptc_enum__pos(Basetype, X{ptc_enum(_, _, Position)}, R) :-
	-?->
	free(R),                    %R is a typeless variable (only happens in first call)
	!,
	R = Position,
	suspend(ptc_enum__pos(Basetype, X, R), 2, [R]->ic:[min,max,hole,type]).

%X is an enumeration variable
%R is an integer variable
ptc_enum__pos(Basetype, X{ptc_enum(_, _, Position)}, R) :-
	-?->
	!,
	R = Position,                                  %actually performs the intersection of the domains
	get_integer_bounds(R, MinR, MaxR),
	ptc_enum(Basetype, Enum_list),
	(member((_, MinR), Enum_list) ->               %the minimum of R has a literal equivalent
		New_minR = MinR
	;
	    (!,
		 find_min_pos(Enum_list, MinR, New_minR) %find the supremum of MinR
	    )
	),
	(member((_, MaxR), Enum_list) ->               %the maximum of R has a literal equivalent
		New_maxR = MaxR
	;
		(!,
		 find_max_pos(Enum_list, MaxR, New_maxR) %find the infinimum of MaxR
	    )
	),
	(New_minR = New_maxR ->                        %only one position allowed for R
	    (R = New_minR,                             %R becomes ground
	     member((X, R), Enum_list)                 %X becomes ground
	    )
	;
	    (R #:: New_minR..New_maxR,
		 suspend(ptc_enum__pos(Basetype, X, R), 2, [R]->ic:[min,max,hole,type])
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%New_min_pos is the supremum of Min_pos
find_min_pos([], _, _) :-
	fail.
find_min_pos([(_, Pos)|Rest], Min_pos, New_min_pos) :-
	Min_pos < Pos ->
	    New_min_pos = Pos
	;
	    find_min_pos(Rest, Min_pos, New_min_pos)
	.

%New_max_pos is the infimum of Max_pos
find_max_pos([(_, Pos)], Max_pos, New_max_pos) :-
	Pos < Max_pos ->
	    New_max_pos = Pos
	;
	    fail
	.
find_max_pos([(_, Pos), Next|Rest], Max_pos, New_max_pos) :-
	find_max_pos([Next|Rest], Max_pos, New_max_pos) ->
	    true
	;
	Pos < Max_pos ->
	    New_max_pos = Pos
	;
	    fail
	. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Succeeds if Sublist is a sublist of List
sublist(_, []) :-
	!,
	fail.

sublist(Sublist, List) :-
	truesublist(Sublist, List) ->
	    true
	;
	    (List = [_|Rest],
	     sublist(Sublist, Rest)
        )
	.

truesublist([], _).
truesublist([X|L], [X|S]) :-
	truesublist(L, S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%