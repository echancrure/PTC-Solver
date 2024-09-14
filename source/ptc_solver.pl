%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 23/06/00
% Eclipse 7.1 program
% ptc_solver.pl
% ptc_solver module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%This module is a little bit messy. Its function is to provide an interface for the solver.
%It could be simplified via syntactic rationalisation of the solver per se.
%%%
:- get_flag(version, '7.1').    %check for valid ECLiPSe version: issue warning only if not 
%%%
:- module(ptc_solver).
mytrace.            %call this to start debugging
:- spy mytrace/0.
:- export ptc_solver__version/1.
:- export ptc_solver__clean_up/0, ptc_solver__default_declarations/1.
:- export ptc_solver__sdl/1, ptc_solver__arithmetic/3, ptc_solver__relation/3.
:- export ptc_solver__type/2, ptc_solver__type/3, ptc_solver__type/4, ptc_solver__subtype/2, ptc_solver__subtype/3.
:- export ptc_solver__variable/2.
:- export ptc_solver__is_enum/1, ptc_solver__is_record/1, ptc_solver__is_array/1.
:- export ptc_solver__sample_enum/1.
:- export ptc_solver__first/2, ptc_solver__last/2, ptc_solver__size/2, ptc_solver__basetype/2.
:- export ptc_solver__error/1.
:- export ptc_solver__get_frame/3.
:- export ptc_solver__enum_get_literal/3, ptc_solver__enum_get_position/2, ptc_solver__enum_get_basetype/2.
:- export ptc_solver__get_record_field_values/2, ptc_solver__get_array_index_elements/2.
:- export ptc_solver__integer_range/3, ptc_solver__is_integer/1, ptc_solver__is_real/1.
:- export ptc_solver__label_integers/1, ptc_solver__label_enums/1, ptc_solver__label_reals/2, ptc_solver__label_reals/1.
:- export ptc_solver__create_record_from_agg/3, ptc_solver__create_array_from_agg/3.
:- export ptc_solver__set_flag/2.
:- export ptc_solver__perform_cast/3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(ic)).
:- use_module(library(ic_kernel)).

:- include([ptc_solver_memory_model_ilp32, ptc_solver_memory_model_ilp64]).
:- include([ptc_solver_types, ptc_solver_engine1, ptc_solver_boolean, ptc_solver_extensions]).
:- include([ptc_solver_bitwise, ptc_labeling]).

:- use_module([ptc_array, ptc_record, ptc_enum]).

:- import ptc_enum__clean_up/0, ptc_enum__record_enum/2, ptc_enum__create_enum/5, ptc_enum__get_position/2, ptc_enum__get_position/3, ptc_enum__succ/2 from ptc_enum.
:- import ptc_enum__pred/2, ptc_enum__is_enum/1, ptc_enum__pos/3, ptc_enum__is_enum_type/1, ptc_enum__get_literal/3, ptc_enum__get_basetype/2 from ptc_enum.
:- import ptc_enum__val/3, ptc_enum__sample/1 from ptc_enum.
:- import ptc_record__create_record/2, ptc_record__get_field/3, ptc_record__is_record/1, ptc_record__create_record_from_agg/3 from ptc_record.
:- import ptc_record__up_record/4, ptc_record__get_all_field_values/2 from ptc_record.
:- import ptc_array__create_array/4, ptc_array__get_element/3, ptc_array__is_array/1, ptc_array__create_array_from_agg/3 from ptc_array.
:- import ptc_array__up_array/4, ptc_array__get_all_index_elements/2 from ptc_array.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% undoing the operators precedence of the solver prior to compilation
:- include([util__pre_precedence]).

%%%
:- dynamic enumeration_start/1.
:- dynamic float_to_int_convention/1.
:- dynamic debug_mode/1.
%%%
ptc_solver__version("C 2.1").

ptc_solver__error(Message) :-
    printf(user_error, "***PTC Solver Fatal Error***\n", []),
    printf(user_error, "%w\n\n\n", Message),
    abort.
ptc_solver__error(Message, Term) :-
    printf(user_error, "***PTC Solver Fatal Error***\n", []),
    printf(user_error, "%w: %w\n\n\n", [Message, Term]),
    abort.

ptc_solver__verbose(Message, Term) :-
    (debug_mode('on') ->
        ((Term = [] ->
            printf(user_error, "%w\n", Message)
        ;
            printf(user_error, "%w: %w\n", [Message, Term])
         ),
         flush(user_error)
        )
    ;
        true
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptc_solver__perform_cast(cast(Same_type, Same_type), Symbolic_expression, Symbolic_expression) :-   %identical do nothing
    !.
ptc_solver__perform_cast(cast(long_double, double), Symbolic_expression, Symbolic_expression) :-    %less precise: do nothing
    !.
ptc_solver__perform_cast(cast(long_double, float), Symbolic_expression, Symbolic_expression) :-     %less precise: do nothing
    !.
ptc_solver__perform_cast(cast(double, float), Symbolic_expression, Symbolic_expression) :-          %less precise: do nothing
    !.
ptc_solver__perform_cast(cast(To_type, From_type), Symbolic_expression, Casted) :-
    !,
    my_eval(Symbolic_expression, Symbolic_eval),
    ptc_solver__basetype(To_type, To_basetype),
    ptc_solver__basetype(From_type, From_basetype),
    (To_basetype == floating_point ->
        (From_basetype == integer ->    %an integer is casted to a floating point, will always fit
            Casted $= Symbolic_eval + 0.0
        ;
            (%more precise floating point conversion
             ptc_solver__variable([Casted], To_type),
             Casted $= Symbolic_eval                    %will prevent most undefined behaviour
            )
        )
    ;
        (From_basetype == integer ->    %an integer is casted to an integer
            perform_integral_cast(To_type, From_type, Symbolic_eval, Casted)
        ;
            (%a floating point is casted to an integer, truncation necessary
             ptc_solver__variable([Casted], To_type),
             Casted $= truncate(Symbolic_eval)   %note the odd use of $= here; truncate is new: see ECLiPSe 7.1 release notes
            )
        )
    ).
ptc_solver__perform_cast(cast(To_type, From_type), Symbolic_expression, _Casted) :-
    !,
    ptc_solver__error("Casting from %w to %w failed: %w probably outside of range", [From_type, To_type, Symbolic_expression]).
%%%
    perform_integral_cast(To_type, From_type, Symbolic_Eval, Casted) :-
        (To_type = unsigned(_) ->
            (ptc_solver__last(To_type, Last),
             (From_type = unsigned(_) ->
                Casted #= Symbolic_Eval rem (Last +1)    %from unsigned to unsigned: may wrap if To_type is smaller
             ;
                (%casting from a signed type into an unsigned type
                 %when ground and within the bounds, we could perhaps bypass the call to rem if it is expensive
                 get_bounds(Symbolic_Eval, Lo, Hi),
                 (Hi < 0 ->     %casting a negative integer to an unsigned type
                    Casted #= (Symbolic_Eval rem (Last+1)) + Last + 1    %e.g.cast(unsigned(char), _, -300, 212) because (-300 rem 256) + 256 == 212
                 ;
                  Lo >= 0 ->    %casting a positive integer to an unsigned type
                    Casted #= Symbolic_Eval rem (Last +1)                %e.g.cast(unsigned(char), _, 300, 212) because (300 rem 256) == 44
                 ;
                    (ptc_solver__variable([Casted], To_type),
                     suspend(perform_integral_cast(To_type, From_type, Symbolic_Eval, Casted), 3, Symbolic_Eval->inst)  %unclear what could be gained from waken this if Casted becomes ground
                    )
                 )
                )
             )
            )
        ;
            (%casting to signed, integer, type
             ptc_solver__variable([Casted], To_type),
             Casted #= Symbolic_Eval
            )
        ).
%%%
%to improve runtime 
my_eval(X, Y) :-
    eval(X, Y).  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptc_solver__clean_up :-
	retractall(enumeration_start(_)),
    retractall(float_to_int_convention(_)),
    retractall(debug_mode(_)),
	assert(enumeration_start(1)),
    assert(float_to_int_convention(truncate)),
    assert(debug_mode('off')),
	ptc_enum__clean_up.

ptc_solver__label_integers(IL) :-
    ptc_labeling__integers(IL).

ptc_solver__label_enums(IL) :-
    ptc_labeling__enums(IL).

ptc_solver__label_reals(IL) :-  %legacy
    ptc_solver__label_reals(IL, _FL). 
ptc_solver__label_reals(IL, FL) :-
    ptc_labeling__reals(IL, FL).

ptc_solver__is_real(Var) :-
    var(Var),
    ic:get_solver_type(Var, 'real').

ptc_solver__is_integer(Var) :-
    var(Var),
    ic:get_solver_type(Var, 'integer').

ptc_solver__integer_range(Integer_var, Min, Max) :-
    ic:get_bounds(Integer_var, Min, Max).

ptc_solver__is_enum(Var) :-
    ptc_enum__is_enum(Var).

ptc_solver__is_record(Var) :-
    ptc_record__is_record(Var).

ptc_solver__is_array(Var) :-
    ptc_array__is_array(Var).

ptc_solver__sample_enum(Var) :-
    ptc_enum__sample(Var).

ptc_solver__enum_get_literal(Enum_type, Position, Literal) :-
    ptc_enum__get_literal(Enum_type, Position, Literal).

ptc_solver__enum_get_position(Enum, Position) :-
    ptc_enum__get_position(Enum, Position).

ptc_solver__enum_get_basetype(Enum, Basetype) :-
    ptc_enum__get_basetype(Enum, Basetype).

ptc_solver__get_record_field_values(Var, Field_values) :-
    ptc_record__get_all_field_values(Var, Field_values).

ptc_solver__get_array_index_elements(Var, Index_elements) :-
    ptc_array__get_all_index_elements(Var, Index_elements).

%%%
ptc_solver__set_flag(Flag, Value) :-
    (Flag == enumeration_start ->
		(integer(Value) ->
			(retractall(enumeration_start(_)),
                         assert(enumeration_start(Value))
			)
		;
			ptc_solver__verbose("In ptc_solver__set_flag/2, the enumeration_start flag can be an integer but the following is not allowed", Value)
		)
    ;
     Flag == float_to_int_convention ->
        (float_to_int_convention(_) ->
            ptc_solver__error("In ptc_solver_set_flag/2, the float_to_int_convention can only be set once")
        ;
         ((Value = truncate ; Value = nearest) ->
            (retractall(float_to_int_convention(_)),
             assert(float_to_int_convention(Value))
            )
         ;
            ptc_solver__verbose("In ptc_solver__set_flag/2, the float_to_int_convention flag can be 'truncate' or 'nearest' but the following is not allowed", Value)
         )
        )
    ;
     Flag == debug_mode ->
        ((Value == 'on' ; Value == 'off') ->
            (retractall(debug_mode(_)),
             assert(debug_mode(Value))
            )
         ;
            ptc_solver__verbose("In ptc_solver__set_flag/2, the debug_mode flag can be 'on' or 'off' but the following is not allowed", Value)
        )
    ;
        ptc_solver__verbose("In ptc_solver__set_flag/2, the following flag does not exist", Flag)
    ).

%submits a constraint
ptc_solver__sdl(Cond) :-
    var(Cond),
    !,
    Cond #\= 0.	%i.e. true in C
ptc_solver__sdl(0) :-
    !,
    fail.
ptc_solver__sdl(N) :-
	integer(N),
	!.
ptc_solver__sdl(Cond) :-
    sdl(Cond).

ptc_solver__arithmetic(Exp, Eval, T) :-
    arithmetic(Exp, Eval, T).

ptc_solver__relation(Comparator, Left, Right) :-
    relation(Comparator, Left, Right).

%%%%
ptc_solver__create_record_from_agg(A,B,C) :-
    ptc_record__create_record_from_agg(A,B,C).
%%%%
ptc_solver__create_array_from_agg(A,B,C) :-
    ptc_array__create_array_from_agg(A,B,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defining the operators precedence of the solver
:- include([util__post_precedence]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%