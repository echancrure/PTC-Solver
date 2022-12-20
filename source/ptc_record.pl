%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chris Meudec
% Eclipse 7.0
% module record for record meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a record variable R is of the form: R{ptc_record(Field_values)}, where Field_values is a list of (Field_name, Value)
% a record meta variable never becomes ground, even if a record is fully known it is still represented as a meta record variable
:- module(ptc_record).

:- meta_attribute(ptc_record, [unify:unify_record/2, print:print_record/2]).
:- export ptc_record__is_record/1, ptc_record__create_record/2, ptc_record__get_all_field_values/2.
:- export ptc_record__get_field/3, ptc_record__raw_get_field/2, ptc_record__up_record/4, ptc_record__create_record_from_agg/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- import ptc_solver__get_frame/3 from ptc_solver.
:- import ptc_solver__arithmetic/3 from ptc_solver.
:- import ptc_solver__sdl/1 from ptc_solver.

%unify_record(Value, Attribute) : only a false call and META + nonvar(Attr) will ever arise

unify_record(_, Attr) :-
	var(Attr).                       %ANY+META (false call)
unify_record(Term, Attr) :-
	compound(Attr),
	unify_term_record(Term, Attr).

unify_term_record(_{AttrY}, AttrX) :-   %META + META
	-?->
	AttrX = AttrY.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%print a record variable (only for debug)
%could be used to print range of fields when non ground, or the values when ground, to be used in conjunction of array printing, real,
%  integer, enumeration etc
print_record(_{ptc_record(Field_values)}, Print) :-
	-?->
	Print = (Field_values).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is a record variable
ptc_record__is_record(_{Attr}) :-
	-?->
	compound(Attr),
	Attr = ptc_record(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create a record variable
ptc_record__create_record(Field_values,  R) :-
	add_attribute(R, ptc_record(Field_values)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptc_record__get_all_field_values(_{Attr}, Field_values) :-
	-?->
	Attr = ptc_record(Field_values).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get the expression of a field of an array, X is of the form field(Record_exp, Field)
ptc_record__get_field(X, R, T) :-
	ptc_record__raw_get_field(X, Exp),
	ptc_solver__arithmetic(Exp, R, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get the value of a field of a meta record variable
ptc_record__raw_get_field(field(Record, Field), Exp) :-
	ptc_record__is_record(Record),                            %Record is a meta record variable
	!,
	ptc_record__get_all_field_values(Record, Field_values),
	member((Field, Exp), Field_values),
	!. %added 07-11-2000

%get the value of a field of an aggregate
ptc_record__raw_get_field(field(agg(Type_mark, AsgL), Field), Exp) :-
	!,
    ptc_solver__get_frame(Type_mark, record, _),            %making sure we are dealing with a record aggregate
    ptc_record__create_record_from_agg(Type_mark, AsgL, R),
	ptc_record__raw_get_field(field(R, Field), Exp).

%get the value of a field of a update record, simplify if possible.
ptc_record__raw_get_field(field(up_rec(Record, Field_upf, Exp_upf), Field), Exp) :-
	!,
	(Field_upf = Field ->
	    Exp_upf = Exp
	;
	    ptc_record__raw_get_field(field(Record, Field), Exp)
	).

%get the value of a field of a field expression
ptc_record__raw_get_field(field(field(Record, Field1), Field2), Exp2) :-
	!,
	ptc_record__raw_get_field(field(Record, Field1), Exp1),%get the value of the internal field expression (which must be a record expression)
	ptc_record__raw_get_field(field(Exp1, Field2), Exp2).

%anything else (i.e. Record is not an explicit record expression) must be an array expression which evaluate to a record
ptc_record__raw_get_field(field(Array_exp, Field), Exp) :-
	!,
	ptc_solver__arithmetic(Array_exp, Record, _),          %the type returned is either a record or is unknown (because of possible delays)
	ptc_record__raw_get_field(field(Record, Field), Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%return an anonymous record variable of an updated meta record variable
ptc_record__up_record(Record, Field, Exp, New_record) :-
	ptc_record__is_record(Record),                            %Record is a meta record variable(should always be the case)
	ptc_record__get_all_field_values(Record, Field_values),
	append(Start, [(Field, _)|Rest], Field_values),       %find the position of Field in the field value list of record
	append(Start, [(Field, Exp)|Rest], New_field_values),
	ptc_record__create_record(New_field_values, New_record).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%AsgL is of the form: for SPARK ada
%                       [exp, ..., exp]
%          or           [([field], exp), ..., ([field], exp)]
% [but why [field] in the above? (see parsing?)]
ptc_record__create_record_from_agg(Type, AsgL, Record) :-
	ptc_solver__get_frame(Type, record, Anon_record),                      %obtain the frame for records of this type
	ptc_record__get_all_field_values(Anon_record, Field_values),
	init_field_values(AsgL, Field_values),
	ptc_record__create_record(Field_values, Record).

init_field_values(AsgL, Field_values) :-
    (AsgL = [(_, _)|_] ->
        init_field_values_named(AsgL, Field_values)
    ;
        init_field_values_positional(AsgL, Field_values)
    ).

init_field_values_named([], _).
init_field_values_named([([Field], Value)|Rest], Field_values) :-
	member((Field, Matched), Field_values),
	!,  %added 12/04/01
	ptc_solver__sdl(=(Value, Matched)),
	init_field_values_named(Rest, Field_values).

init_field_values_positional([], _).
init_field_values_positional([Exp|Rest], [(_, Exp)|Rest2]) :-
    init_field_values_positional(Rest, Rest2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%