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
:- export ptc_solver__clean_up/0, ptc_solver__default_declarations/2.
:- export ptc_solver__sdl/1, ptc_solver__arithmetic/3, ptc_solver__relation/3.
:- export ptc_solver__type/2, ptc_solver__type/3, ptc_solver__type/4, ptc_solver__subtype/2, ptc_solver__subtype/3.
:- export ptc_solver__variable/2.
:- export ptc_solver__is_enum/1, ptc_solver__is_record/1, ptc_solver__is_array/1.
:- export ptc_solver__sample_enum/1.
:- dynamic ptc_solver__first/2, ptc_solver__last/2.
:- export ptc_solver__first/2, ptc_solver__last/2.
:- export ptc_solver__error/1.
:- export ptc_solver__get_frame/3.
:- export ptc_solver__enum_get_literal/3, ptc_solver__enum_get_position/2, ptc_solver__enum_get_basetype/2.
:- export ptc_solver__get_record_field_values/2, ptc_solver__get_array_index_elements/2.
:- export ptc_solver__integer_range/3, ptc_solver__is_integer/1, ptc_solver__is_real/1.
:- export ptc_solver__label_integers/1, ptc_solver__label_enums/1, ptc_solver__label_reals/2, ptc_solver__label_reals/1.
:- export ptc_solver__create_record_from_agg/3, ptc_solver__create_array_from_agg/3.
:- export ptc_solver__set_flag/2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(ic)).

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

:- local variable(entail_stack).        %used for entail check within reif in ptc_solver_boolean.pl file
%%%
:- dynamic or_constraint_behaviour/1.
:- dynamic enumeration_start/1.
:- dynamic float_to_int_convention/1.
:- dynamic debug_mode/1.
%%%
ptc_solver__version("C 2.1").

ptc_solver__error(Message) :-
    writeln(user_error, "***PTC Solver Fatal Error***"),
    writeln(user_error, Message),
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
%2 important clauses that record the entire type system
%Type_mark is direct from the source code
%Subtype_mark is the base type of the type : integer, real, base_enumeration, record, array(Component_type_mark), or Parents_type for an enumeration subtype
ptc_solver__set_frame(Type_mark, Subtype_mark, Value) :-
    record(frame, f(Type_mark, Subtype_mark, Value)).

ptc_solver__get_frame(Type_mark, Subtype_mark, Value) :-
    recorded(frame, f(Type_mark, Subtype_mark, Value)).     %extremely slow!!! see 16-05-2001

ptc_solver__clean_up :-
    setval(entail_stack, []),    %initialisation (see ptc_solver_boolean.pl file)
	erase_all(frame),
	retractall(or_constraint_behaviour(_)),
	retractall(enumeration_start(_)),
    retractall(float_to_int_convention(_)),
    retractall(debug_mode(_)),
	assert(enumeration_start(1)),
    assert(float_to_int_convention(truncate)),
    assert(debug_mode('off')),
	retractall(ptc_solver__first(_, _)),
	retractall(ptc_solver__last(_, _)),
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
    (Flag == or_constraint_behaviour ->
        (or_constraint_behaviour(_) ->
            ptc_solver__error("In ptc_solver_set_flag/2, the or_constraint_behaviour can only be set once")
         ;
            ((Value = pure ; Value = choice) ->
                (retractall(or_constraint_behaviour(_)),
                 assert(or_constraint_behaviour(Value))
                )
             ;
                ptc_solver__verbose("In ptc_solver__set_flag/2, the or_constraint_behaviour flag can be 'pure' or 'choice' but the following is not allowed", Value)
            )
        )
	;
     Flag == enumeration_start ->
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