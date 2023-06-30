%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 19/12/00
% Eclipse 7.0 program
% Boolean constraints
% part of the solver module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%The question of the 'or' constraint is a difficult one. For a detailed discussion see
% the ATGen project diary.
% Basically we have two main ways of dealing with the 'or' constraint:
%       - The best way would be not to introduce choice points but instead implement
%          a truly clp constraint. Unfortunately we have been unable to implement
%          such a constraint in the general case. Instead the current version delays
%          most of the time: only when a decision is fully known that part of the
%          constraint is applied. In practice, we usually have to wait to labeling time
%          for that to occur. Thus the current implementation of this solution is very
%          inneficient. [Wed. 28th Nov. 2001]
%       - The second solution is to create choice points in the 'or' constraint.
%          Thus this is not a proper clp implementation but is quite efficient at the expense
%          of having to deal with sometimes unwanted backtracking however. In practice
%          this version will give all combimations of the decisions submitted: this can
%          lead to a very large number of combinations potentially (excluding logical
%          contradictions) 2 power N for a pure or constraints with N leaf decisions.
%          [non-pure constraints are also affected:
%           e.g. A or (B and C) leads to 5 combinations:         A and ~B and ~C
%                                                                A and ~B and  C
%                                                                A and  B and ~C
%                                                                A and  B and  C
%                                                               ~A and  B and  C
%          ]
% we give here both constraints controlled by an asserted fact

s_or(A, B) :-
    (or_constraint_behaviour(V) ->
        true
    ;
        V = pure
    ),
    (V = pure ->
        s_or_pure(A, B)
    ;
     V = choice ->
        s_or_choice(A, B)
    ).

s_or_else(A, B) :-
    (or_constraint_behaviour(V) ->
        true
    ;
        V = pure
    ),
    (V = pure ->
        s_or_else_pure(A, B)
    ;
     V = choice ->
        s_or_else_choice(A, B)
    ).

%%%%%%%%%%%%%%%%%%%%%%%  CHOICE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%truly random!
s_or_choice(A, B) :-
    frandom(N),
    N6 is fix(N*6),
	(N6 = 0 ->
	    (   sdl(and(A, not(B)), _, _)
	    ;
	        sdl(and(A, B), _, _)
        ;
	        sdl(and(not(A), B), _, _)
	    )
    ;
	 N6 = 1 ->
	    (   sdl(and(A, not(B)), _, _)
	    ;
	        sdl(and(not(A), B), _, _)
        ;
	        sdl(and(A, B), _, _)
	    )
    ;
     N6 = 2 ->
        (   sdl(and(not(A), B), _, _)
	    ;
	        sdl(and(A, not(B)), _, _)
        ;
	        sdl(and(A, B), _, _)
	    )
    ;
     N6 = 3 ->
        (   sdl(and(not(A), B), _, _)
	    ;
	        sdl(and(A, B), _, _)
        ;
	        sdl(and(A, not(B)), _, _)
	    )
    ;
     N6 = 4 ->
        (   sdl(and(A, B), _, _)
        ;
            sdl(and(not(A), B), _, _)
	    ;
	        sdl(and(A, not(B)), _, _)
	    )
    ;
     N6 = 5 ->
        (   sdl(and(A, B), _, _)
        ;
            sdl(and(A, not(B)), _, _)
	    ;
            sdl(and(not(A), B), _, _)
	    )
    ).

s_or_else_choice(A, B) :-
    sdl(A, _, _),
    frandom(N),
    N2 is fix(N*2),
    (N2 = 0 ->
        (   sdl(B, _ , _)
        ;
            boolean(not(B), _, _)
        )
    ;
     N2 = 1 ->
        (   sdl(not(B), _, _)
        ;
            sdl(B, _ , _)
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%  PURE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s_or_pure(A, B) :-
    known(A, KCA),
    (KCA == no ->
        (known(B, KCB),
         (KCB == no ->
            (term_variables(dummy(A), VarsA),
             get_atomics(VarsA, AtomicsA),
             term_variables(dummy(B), VarsB),
             get_atomics(VarsB, AtomicsB),
             suspend(s_or(A, B), 4, [AtomicsA, AtomicsB]->inst) %priority has been lowered from 3 to 4 (David's diary 29/06/04) to be lower than the clpfd constraints before the fail within success/2
	        )
         ;
            KCB == true ->
                sdl(B, _, _)            %should never fail
         ;
            KCB == false ->
                (sdl(not(B), _, _),     %not really necessary but should never fail
                 sdl(A, _, _)           %determine the outcome of the  'A or B'
                )
         )
        )
        ;
         KCA == true ->
            sdl(A, _, _)                    %should never fail
        ;
         KCA == false ->
            (sdl(not(A), _, _),             %not really necessary but should never fail
             sdl(B, _, _)                   %determine the outcome of the  'A or B'
            )
        ).

%27/01/05
%we are changing the pure or_else constraint to be a bit more clever
% in A or_else B, if A is unknown but if B is known and is false then we can conclude that A has to be true
s_or_else_pure(A, B) :-
    known(A, KCA),
    (KCA == no ->
        (known(B, KCB),
         (KCB == no ->  %we will have to delay
            (term_variables(dummy(A), VarsA),
             get_atomics(VarsA, AtomicsA),
             term_variables(dummy(B), VarsB),
             get_atomics(VarsB, AtomicsB),
             suspend(s_or_else_pure(A, B), 3, [AtomicsA, AtomicsB]->inst)
	        )
         ;
          KCB == true ->        %unsure here, in pure logic we should sdl(B, _, _) but what about A? in C code this will only happen if A is false
                                % but in ordinary logic the value of A does not matter ...                                    
                                % so for safety we delay just as above
            (term_variables(dummy(A), VarsA),
             get_atomics(VarsA, AtomicsA),
             term_variables(dummy(B), VarsB),
             get_atomics(VarsB, AtomicsB),
             suspend(s_or_else_pure(A, B), 3, [AtomicsA, AtomicsB]->inst)
	        )
         ;
          KCB == false ->        %here A must be true
            (sdl(not(B), _, _),     %not really necessary but should never fail
             sdl(A, _, _)           %determine the outcome of the  'A or B'
            )
         )
        )
    ;
     KCA == true ->
        sdl(A, _, _)    %should never fail
    ;
     KCA == false ->
        (sdl(not(A), _, _),     %should never fail
         sdl(B, _, _)
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%to handle C Boolean expressions which return an int
% which can be part of expressions such as x = (y<z)
%see my Eileen's MSc diary
% pretty much as a reified constraint

%R must be an fd constraint between with domain 0..1 only
s_reif(Constraint, R) :-
	(R == 1 ->
		sdl(Constraint, _, _)
	;
     R == 0 ->
		sdl(not(Constraint), _, _)
	;                               %R in unknown
	 (known(Constraint, KC),
        (KC == no ->           %the constraint is unknown : we must delay
            (term_variables(dummy(Constraint), Vars_constraint),
             get_atomics(Vars_constraint, Atomics_constraint),
             suspend(s_reif(Constraint, R), 3, [Atomics_constraint, R]->inst)
	        )
        ;
         KC == true ->         %the constraint can only be true
            (sdl(Constraint, _, _) ->         %should never fail
                R #= 1
            ;
                ptc_solver__error("Constraint can only be true but fails: systematic error (from s_reif/2 in ptc_solver_boolean.pl)", [])
            )
        ;
         KC == false ->        %the constraint can only be false
            (sdl(not(Constraint), _, _) ->    %should never fail
                R #= 0
            ;
                ptc_solver__error("Constraint can only be false but fails: systematic error (from s_reif/2 in ptc_solver_boolean.pl)", [])
            )
        )
     )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%to handle Javabyte code dcmp and lcmp
% 1st and 2nd arguments are real expressions
% R is the result:      -1 if 1st < 2nd
%                       0  if 1st = 2nd
%                       1  if 1st > 2nd
%see my Jacinta's MSc diary
s_cmp(Exp1, Exp2, R) :-
    arithmetic(Exp1, D1, _),
    arithmetic(Exp2, D2, _),
    arithmetic(R, Z, i),
    Z #>= -1,
    Z #=< 1,
    s_cmp2(D1, D2, Z).

s_cmp2(D1, D2, R) :-
    (ground(R) ->
        (R == -1 ->
            sdl(D1 < D2, _, _)
        ;
         R == 0 ->
            sdl(D1 = D2, _, _)
        ;
         R == 1 ->
            sdl(D1 > D2, _, _)
        )
    ;
     %crude constraint here only
     %could be much much better by checking domains
     (ground(D1), ground(D2)) ->
        (D1 == D2 ->
            R #= 0
        ;
         D1 > D2 ->
            R #= 1
        ;
         D1 < D2 ->
            R #= -1
        )
    ;   %we must delay
        suspend(s_cmp2(D1, D2, R), 3, [D1, D2, R]->inst)
    ).

%%%%
successT(Constraint) :-
    sdl(Constraint, _, _),
    getval(entail_stack, [es(no, F)|ES]),
    setval(entail_stack, [es(yes, F)|ES]),  %Constraint can be true
    fail.
successF(Constraint) :-
    sdl(not(Constraint), _, _),
    getval(entail_stack, [es(T, no)|ES]),
    setval(entail_stack, [es(T, yes)|ES]),  %not Constraint can be true
    fail.

known(Constraint, KC) :-
    getval(entail_stack, ES),
    setval(entail_stack, [es(no, no)|ES]),          %initial element is pushed on the stack

    not successT(Constraint),

    not successF(Constraint),
    getval(entail_stack, [es(ST, SF)|ES]),
    setval(entail_stack, ES),                       %top of the stack is removed: job done
    ((ST == yes, SF == yes) ->
        KC = no                 %cannot be decided
    ;
     (ST == yes, SF == no) ->
        KC = true               %the constraint can only be true
    ;
     (ST == no, SF == yes) ->
        KC = false              %the constraint can only be false
    ;
     (ST == no, SF == no) ->
        ptc_solver__error("Constraint cannot be true nor false: systematic error (from known/2 in ptc_solver_boolean.pl)", [])
    ).

%%%
%for reif we need to suspend on all variables (see 30/04/04 Eileen's diary)
get_atomics([], []).
get_atomics([V|Rest], List) :-
    ((ptc_solver__is_enum(V) ; ptc_solver__is_integer(V) ; ptc_solver__is_real(V))->
        List = [V|List1]
    ;
     ptc_solver__is_array(V) ->
        (partition_array_vars(V, List2),
         append(List2, List1, List)
        )
	;
	 ptc_solver__is_record(V) ->
	    (partition_record_vars(V, List2),
         append(List2, List1, List)
        )
    ),
	get_atomics(Rest, List1).

partition_array_vars(V, List) :-
	ptc_solver__get_array_index_elements(V, Indice_elements),
	term_variables(Indice_elements, Vars),             %obtain all the variables in the array
	partition_component_vars(Vars, List).

% called from partition_vars/5 by partition_record_vars(Value, IL1, RL1, EL1)
% Vars is a list of record variables, which can belong to an array or not
partition_record_vars(V, List) :-
	ptc_solver__get_record_field_values(V, Field_values),    % obtain all the fields and values of V
	term_variables(Field_values, Vars),
	partition_component_vars(Vars, List).    % partition all field variables

%called from partition_record_vars/5 and partition_array_vars and recursively by partition_component_vars(Vars, IL1, RL1, BL1, EL1)
partition_component_vars([], []).
partition_component_vars([V|Rest], List) :-
	(ptc_solver__is_enum(V) ->                                     % an enumeration
	    List = [V|List1]
	;
	 ptc_solver__is_integer(V) ->                                   % an integer
	    List = [V|List1]
	;
	 ptc_solver__is_real(V) ->	                                   % a real
	    List = [V|List1]
	;
	 ptc_solver__is_record(V) ->                           % a record
	    (partition_record_vars(V, List2), % need to partition the variables recorded as fields in all Vars
             append(List2, List1, List)
            )
	;
	 ptc_solver__is_array(V) ->                             % an array
	    (partition_array_vars(V, List2),
	     append(List2, List1, List)
            )
	),
	partition_component_vars(Rest, List1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%