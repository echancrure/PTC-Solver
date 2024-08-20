%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec -  - started 08/10/99
% Eclipse 7.0
% part of the ptc_solver module
% inconsistencies detection and constraints applications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%can fail
%Rel is the relation
%L is the left operand of Op
%R is the right operand of Op
%process L and R, then apply the constraint L Rel R throught apply_relation/7
relation(Rel, L, R):-
	arithmetic(L, X, XT), %process left operand giving X of type XT
	arithmetic(R, Y, YT), %process right operand giving Y of type YT
	!,
	apply_relation(L, X, XT, Rel, R, Y, YT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from relation/3 by apply_relation(L, X, XT, Rel, R, Y, YT)
%can fail
%L is the original left operand
%X is the variable of SDL expression representing L
%XT is the type of X
%Rel is the relational operator
%R is the original right operand
%Y is the variable of SDL expression representing R
%YT is the type of Y
%apply the appropriate constraint
apply_relation(_, X, array, =, _, Y, array) :-
	!,
	X = Y.
apply_relation(_, X, record, =, _, Y, record) :-
	!,
	X = Y.
apply_relation(_, X, XT, Fun, _, Y, YT) :-
	get_integer_real_relation(Fun, Rel_int, Rel_rea), %get correct relation syntax
	((XT = e, YT = e) ->               %a constraint on enumeration variables
		(Fun = = ->
		    (!,        %do not under any circumstances remove this 27-09-01
             X = Y
		    )
		;
		 Fun = > ->
		 	(ptc_enum__succ(Y, SuccY),
		  	 ptc_enum__get_position(SuccY, Pos_succY),
		  	 ptc_enum__get_position(X, PosX),
		  	 !,
		  	 PosX #>= Pos_succY
		 	)
		;
		 Fun = < ->
		    (ptc_enum__pred(Y, PredY),
		     ptc_enum__get_position(PredY, Pos_predY),
		     ptc_enum__get_position(X, PosX),
		     !,
			 PosX #=< Pos_predY
		 	)
		;
		    (ptc_enum__get_position(X, PosX),
		     ptc_enum__get_position(Y, PosY),
		     Constraint =.. [Rel_int, PosX, PosY],
		     !,
		     Constraint
		    )
		)
	;	%can only involve integer or real expressions from here
	 (evalWrap(X, Xeval),
	  evalWrap(Y, Yeval),
	  ((XT == i, YT == i) ->          % a pure integer constraint
			Constraint =.. [Rel_int, Xeval, Yeval]    	% build the constraint
	   ;
			Constraint =.. [Rel_rea, Xeval, Yeval] 		% build the constraint
	  ), 
	  !,
	  Constraint                    %finally apply the constraint
	 )
	), 
	!.

%Fun is a SDL relational operator
%Rel_int is the out fd (integer) equivalent
%Rel_rea is the out clpq (real) equivalent
get_integer_real_relation(=, #=, $=).
get_integer_real_relation(<>, #\=, $\=).
get_integer_real_relation(<, #<, $<).
get_integer_real_relation(>, #>, $>).
get_integer_real_relation(<=, #=<, $=<).
get_integer_real_relation(>=, #>=, $>=).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\
% January 2023
% evalWrap wraps a dynamically created ic expressions (integer or real) with eval/1 to keep the IC library happy.
% This is was not necessary previously because we systematically used intermediate ic variables everywhere (see diary)
% This way of doing things should be more efficient (fewer variables means, fewer delayed goals and fewer wakes) and also immediate detections
%   of failure in some cases(e.g. X = X+1 )
% Should only be called on integer or real IC expressions
evalWrap(X, Xeval) :-
	(var(X) ->
		Xeval = X
	;
		Xeval = eval(X)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is recursive; can fail
% X is an arithmetic expression in Ada syntax (without relational operators which are treated by relation/3)
%  to transform into constraints
% R denotes the result of the operation represented by X
% T denotes r or i indicating a real or an integer constraint
% check for numbers
arithmetic(X, X, T):-
	number(X),
	!,
	(integer(X) ->
	    T = i
	;
	    T = r
	).

% check for variables
arithmetic(X, X, T):-
	var(X),
	!,
	(ic:is_solver_var(X) ->
	    (ic:get_solver_type(X, Type),
		 (Type == integer ->
			T = i
	     ;
		  Type == real ->
			T = r
		 )
	    )
	 ;
	  ptc_enum__is_enum(X) ->
	    T = e
	 ;
	  ptc_record__is_record(X) ->
	    T = record
	 ;
	  ptc_array__is_array(X) ->
	    T = array
	).

% arithmetic/3 for operators
% check operands and apply the correct operator depending on whether dealing with an integer expression or a potentially mixed real expression
arithmetic(*(Le, Ri), X * Y, T):-
	!,
	arithmetic(Le, X, XT),
	arithmetic(Ri, Y, YT),
	((XT == i, YT == i) ->
	    T = i
    ;
	    T = r
	).

arithmetic(+(Le, Ri), X + Y, T):-
	!,
	arithmetic(Le, X, XT),
	arithmetic(Ri, Y, YT),
	((XT == i, YT == i) ->
	    T = i
	;
	    T = r
	).

arithmetic(-(Le, Ri), X - Y, T):-
	!,
	arithmetic(Le, X, XT),
	arithmetic(Ri, Y, YT),
	((XT == i, YT == i) ->
	    T = i
	;
	    T = r
	).

arithmetic(-(Ri), -Y, T) :-
	!,
	arithmetic(Ri, Y, T).

arithmetic(abs(Ri), abs(X), T) :-
	!,
	arithmetic(Ri, X, T).

%removed my custom power operator 
arithmetic(**(Le, Ri), X^Y, T):-
	!,
	arithmetic(Le, X, XT),
	arithmetic(Ri, Y, YT),
	((XT == i, YT == i) ->	%so even 1^(0.1) will be treated as a real expression even though 1 is the result 
	    T = i
    ;
	    T = r
	).

%'/' can apply to real or integer operands
arithmetic(/(Le, Ri), R, T) :-
	!,
	arithmetic(Le, X, XT),
	arithmetic(Ri, Y, YT),
	((XT == i, YT == i) ->
	    (T = i,
		 R = X // Y
	    )
	;
	    (T = r,
	     R = X / Y
	    )
	).

%mod can only apply to integer operands and the resulting type is always integer
arithmetic(mod(Le, Ri), R, i) :-
	!,
	arithmetic(Le, X, i),
	arithmetic(Ri, Y, i),
	s_mod(X, Y, R). 	%custom defined operator for modulo: no equivalent in ic

%rem can only apply to integer operands and the resulting type is always integer
arithmetic(rem(Le, Ri), X rem Y, i) :-
	!,
	arithmetic(Le, X, i),
	arithmetic(Ri, Y, i).

%type conversion of an expression
%float and integer conversions only
%does not deal when the new range is greater than the previous one
arithmetic(conversion(Type_mark, From_exp), R, To_type) :-
    !,
	arithmetic(From_exp, From_exp_eval, From_type),
	arithmetic(first(Type_mark), Min, To_type),
	arithmetic(last(Type_mark), Max, To_type),
	(apply_relation(From_exp, From_exp_eval, From_type, >=, first(Type_mark), Min, To_type) ->
	    true
    ;
        ptc_solver__error("Failed type conversion: systematic run-time error in your code")
    ),
	(apply_relation(From_exp, From_exp_eval, From_type, <=, last(Type_mark), Max, To_type) ->
	    true
    ;
        ptc_solver__error("Failed type conversion: systematic run-time error in your code")
    ),
	((From_type == r, To_type == i) ->
        s_cast_to_int(From_exp_eval, R)
    ;
     (From_type == i, To_type == r) ->
		R $= eval(From_exp_eval)
    ;
        R = From_exp_eval     %ie. From_type = To_type no conversion necessary
    ).

%round can only apply to a real and the resulting type is always integer
arithmetic(round(Ri), R, i) :-
	!,
	arithmetic(Ri, X, r),
	s_cast_to_int(X, R).

%the type of the result is the type of the element accessed
%arithmetic is not called on Array here because simplifications are performed within ptc_array__get_element
arithmetic(element(Array, Index), R, T) :-
	!,
	ptc_array__get_element(element(Array, Index), R, T).

arithmetic(agg(Type, AsgL), R, record) :-
    ptc_solver__get_frame(Type, record, _),
	!,
	ptc_record__create_record_from_agg(Type, AsgL, R).

arithmetic(agg(Type, AsgL), R, array) :-
    ptc_solver__get_frame(Type, array(_), _),
	!,
	ptc_array__create_array_from_agg(Type, AsgL, R).

% 19-05-2000 added call to arithmetic on Exp
arithmetic(up_arr(Array, Index, Exp), R, array) :-
	!,
	arithmetic(Array, Up_array, array),
    arithmetic(Exp, R_exp, T),
	(T == i ->
     	Reval #= eval(R_exp)
	;
	 T == r ->
		Reval $= eval(R_exp)
	;
		Reval = R_exp 
	),
	ptc_array__up_array(Up_array, Index, Reval, R).

%the type of the result is the type of the field
%arithmetic is not called on Record here because simplifications are performed within ptc_record__get_field
arithmetic(field(Record, Field), R, T) :-
	!,
	ptc_record__get_field(field(Record, Field), R, T).

% 19-05-2000 added call to arithmetic on Exp
arithmetic(up_rec(Record, Field, Exp), R, record) :-
	!,
	arithmetic(Record, Up_record, record),
	arithmetic(Exp, R_exp, T),
	(T == i ->
     	Reval #= eval(R_exp)
	;
	 T == r ->
		Reval $= eval(R_exp)
	;
		Reval = R_exp 
	),
	ptc_record__up_record(Up_record, Field, Reval, R).

arithmetic(first(Discrete_type), R, T) :-
	!,
	ptc_solver__first(Discrete_type, Tmp_R),
	arithmetic(Tmp_R, R, T).

arithmetic(last(Discrete_type), R, T) :-
	!,
	ptc_solver__last(Discrete_type, Tmp_R),
	arithmetic(Tmp_R, R, T).

%succ works on the base type of X
arithmetic(succ(X), R, T) :-
	!,
	arithmetic(X, X_exp, T),
	(T == i ->
		R = X_exp + 1
	;                   %X is an enumeration variable or a literal (i.e. T = e)
	    ptc_enum__succ(X_exp, R)
	).

%pred works on the base type of X
arithmetic(pred(X), R, T) :-
	!,
	arithmetic(X, X_exp, T),
	(T == i ->
		R #= eval(X_exp - 1)
	;                   %X is an enumeration variable or a literal (i.e. T = e)
	    ptc_enum__pred(X_exp, R)
	).

%the basetype is always returned by the examiner
arithmetic(pos(Basetype, Ri), R, i) :-
	!,
	arithmetic(Ri, X, T),
	(T == i ->         %X is an integer
	    R = X
	;                 %X is a literal or an enumeration variable (i.e. XT = e)
	    ptc_enum__pos(Basetype, X, R)
	).

%the basetype is always returned by the examiner
arithmetic(val(Basetype, Ri), R, T) :-
	!,
	arithmetic(Ri, X, i),           %must always be an integer
	(ptc_enum__is_enum_type(Basetype) ->      %Basetype is an enumeration type
	    (T = e,
	     Xeval #= eval(X),            %evaluate X
	     ptc_enum__val(Basetype, Xeval, R)
	    )
	;                                %Basetype must be an integer type
	    (T = i,
	     R = X
	    )
	).

%%%bitwise
arithmetic(bw_not(Le, Len, Sign), R, i) :-
    !,
    s_bwnot(Le, Len, Sign, R).

arithmetic(bw_and(Le, Ri, Len, Sign), R, i) :-
    !,
    s_bwand(Le, Ri, Len, Sign, R).

arithmetic(bw_or(Le, Ri, Len, Sign), R, i) :-
    !,
    s_bwor(Le, Ri, Len, Sign, R).

arithmetic(bw_xor(Le, Ri, Len, Sign), R, i) :-
    !,
    s_bwxor(Le, Ri, Len, Sign, R).

arithmetic(left_shift(Le, S, Len, Sign), R, i) :-
    !,
    s_left_shift(Le, S, Len, Sign, R).

arithmetic(right_shift(Le, S, Len, Sign), R, i) :-
    !,
    s_right_shift(Le, S, Len, Sign, R).

%not a variable, not a number, not a compound, must be an atom i.e. a literal e.g. 'monday'
arithmetic(Literal, Literal, e) :-
	atom(Literal),
	!.

%%!!!a boolean expression here
%will occur whenever a constaint is applied on a boolean output via apply_assign_using_sdl
arithmetic(Boolean_exp, R, e) :-
    (ptc_solver__sdl(Boolean_exp) -> %this is correct only if the solver does not delay
        R = true
    ;
        R = false
    ).
%%%
%see ATGen notebook 02/02/04 and Eileen's diary same period
%to accomodate C assignments that may imply an implicit casting
%in C = is replaced by eq_cast
%in C == is replaced by =
%makes no difference for int to real conversion eq_cast(f, 42) is the same as f = 42
% but for eq_cast(i, 7.9) i becomes 7 while i = 7.9 fails
eq_cast(Le, Ri) :-
    arithmetic(Le, L_eval, L_type),
    arithmetic(Ri, R_eval, R_type),
    (L_type == R_type ->
        apply_relation(Le, L_eval, L_type, =, Ri, R_eval, R_type)
    ;
     (L_type == i, R_type == r) ->    	%from real to int
        s_cast_to_int(R_eval, L_eval)  	%as is done in conversion constraint
    ;
     (L_type == r, R_type == i) ->    	%from int to real
		L_eval $= eval(R_eval)
    ;
        ptc_solver__error("Failed eq_cast/2 in ptc_solver_engine1 file the types are different and not real nor integer")
    ).
%%%%%%%%%%%%%%%%%%%%%sdl/3, boolean/3, negate/3%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A single, simpler, predicate can be (and originally was) written to replace the three given below,
% but for efficiency reasons it has been rewritten.
% 2 style of simpler implementations involving only one predicate can be given:
%   *one with many overlapping clauses
%   *one using if the else
%  both are less efficient than what is given below.
% The clauses below are exclusive and detected as such by the compiler, thus less testing is
%  necessary and less fruitless backtracking occurs. The only backtracking which occurs is intended
%  and only arise from the (necessary) choice points.
% This version when profiled (using profile/1) proved twice as quick than the previous version,
%  it is also a simpler version to trace.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sdl/3 called from process_traversal_cond/4 by sdl(Cond, [], ConsL) and also by boolean/3 and negate/3
% indirectly recursif; choice points; can fail
% Cond is an SDL expression represent a path traversal condition
% the second argument is the current list of applied constraints
% apply constraints according to Cond and generate ConsL by calling boolean/3
% backtracking generate different classes throught the choice points
% failure denotes overall inconsistency or no more classes possible
% boolean constraints are processed first then arithmetics through relation/3
sdl(Cond, CLi, CLo) :-
	(var(Cond) ->                    %Cond must be a boolean variable that must be true
        boolean(=(Cond, true), CLi, CLo)
	;
	 Cond = element(_, _) ->         %an array element which is a boolean
	    (ptc_array__get_element(Cond, Elem, e),	%booleans are enumerations
	     sdl(Elem, CLi, CLo)
	    )
	;
	 Cond = field(_, _) ->
		(ptc_record__get_field(Cond, Elem, e),	%booleans are enumerations
		 sdl(Elem, CLi, CLo)
		)
	;
	 Cond = reif(Constraint, R) ->
	    s_reif(Constraint, R)   %see ptc_solver_boolean
	;
	 Cond = cmp(Exp1, Exp2, R) ->
	    s_cmp(Exp1, Exp2, R)    %see ptc_solver_boolean
    ;
     Cond = eq_cast(Exp1, Exp2) ->
        eq_cast(Exp1, Exp2)     %see above
	;
		boolean(Cond, CLi, CLo)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from sdl/3 by boolean(Cond, CLi, CLo) and also by negate/3
%recursive; choice points; can fail
boolean(true, Cli, Cli).
boolean(false, Cli, Cli) :-
	fail.
boolean(and(L, R), CLi, CLo) :-
	random(2, R2),
    (R2 == 0 ->
        (sdl(L, CLi, CLo1),
         sdl(R, CLo1, CLo)
        )
    ;
	    (sdl(R, CLi, CLo1),
	     sdl(L, CLo1, CLo)
        )
    ).

boolean(and_then(L, R), CLi, CLo) :-
	sdl(L, CLi, CLo1),
    sdl(R, CLo1, CLo).

boolean(or(L, R), _, _) :-
    s_or(L, R).             %see ptc_solver_boolean

boolean(or_else(L, R), _, _) :-
    s_or_else(L, R).        %see ptc_solver_boolean

boolean(xor(L, R), CLi, CLo) :-
    boolean(or(and(L, not(R)), and(not(L), R)), CLi, CLo).

%not can be over an array of Booleans
%see initial call to sdl as it is similar
boolean(not(Bool), CLi, CLo) :-
	(var(Bool) ->            %Cond must be a boolean variable that must be true
        boolean(=(Bool, false), CLi, CLo)
	;
	 Bool = element(_, _) ->
	    (ptc_array__get_element(Bool, Elem, e),	%booleans are enumerations
	     boolean(not(Elem), CLi, CLo)
	    )
	;
	 Bool = field(_, _) ->
		(ptc_record__get_field(Bool, Elem, e),	%booleans are enumerations
		 boolean(not(Elem), CLi, CLo)
		)
	;
	    negate(Bool, CLi, CLo)
	).

boolean(=(X, Y), CLi, [=(X, Y)|CLi]) :-
	relation(=, X, Y).
boolean(<>(X, Y), CLi, [<>(X, Y)|CLi]) :-
	relation(<>, X, Y).
boolean(<(X, Y), CLi, [<(X, Y)|CLi]) :-
	relation(<, X, Y).
boolean(>(X, Y), CLi, [>(X, Y)|CLi]) :-
	relation(>, X, Y).
boolean(<=(X, Y), CLi, [<=(X, Y)|CLi]) :-
	relation(<=, X, Y).
boolean(>=(X, Y), CLi, [>=(X, Y)|CLi]) :-
	relation(>=, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from boolean/3 by negate(Bool, Cli, Clo)
%indirectly recursif; choice point; can fail
%negate the boolean expression Bool by calling
%  * relation/3 if an arithmetic relation
%  * boolean /3 if a boolean expression which is not a boolean variable
%  * sdl/3 if a boolean expression which could be a boolean variable
negate(false, _, _).        %not false
negate(true, _, _) :-       %not true
	fail.
negate(not(Rel), CLi, CLo) :-                 %not not Rel == Rel
	sdl(Rel, CLi, CLo).
negate(=(X, Y), CLi, [<>(X, Y)|CLi]) :-
	relation(<>, X, Y).
negate(<>(X, Y), CLi, [=(X, Y)|CLi]) :-
	relation(=, X, Y).
negate(<(X, Y), CLi, [>=(X, Y)|CLi]) :-
	relation(>=, X, Y).
negate(>(X, Y), CLi, [<=(X, Y)|CLi]) :-
	relation(<=, X, Y).
negate(<=(X, Y), CLi, [>(X, Y)|CLi]) :-
	relation(>, X, Y).
negate(>=(X, Y), CLi, [<(X, Y)|CLi]) :-
	relation(<, X, Y).
negate(and(X, Y), CLi, CLo) :-
	boolean(or(not(X), not(Y)), CLi, CLo).
negate(and_then(X, Y), CLi, CLo) :-
	boolean(or_else(not(X), not(Y)), CLi, CLo).
negate(or(X, Y), CLi, CLo) :-
	boolean(and(not(X), not(Y)), CLi, CLo).
negate(or_else(X, Y), CLi, CLo) :-
	boolean(and_then(not(X), not(Y)), CLi, CLo).
negate(xor(X, Y), CLi, CLo) :-
    boolean(or(and(X, Y), and(not(X), not(Y))), CLi, CLo).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%