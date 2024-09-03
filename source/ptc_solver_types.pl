%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec -  - started 22/10/99
% ECLiPSe 7.1
% part of the ptc_solver module : variables declaration matters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic c_type_declaration/5, c_type_declaration/3.

ptc_solver__default_declarations(Solver_install_dir, Memory_model) :-
	concat_string([Solver_install_dir, "ptc_solver_memory_model_", Memory_model], Default_type_file),
	(compile([Default_type_file]) ->
		ptc_solver__verbose("Compiled default C declarations from file", Default_type_file)
	;	
		(concat_string(["Memory Model in file: ", Default_type_file, " is not valid"], Error_message),
		 ptc_solver__error(Error_message)
		)
	),
	create_all_types.

create_all_types :-
	c_type_declaration(Type_name, Base_type, _Size, First, Last),	%todo Size is not currently recorded
	(Base_type == 'integer' ->
		(%I #:: First..Last,
		 integers([I]),		%integer with infinite domain
		 ptc_solver__set_frame(Type_name, 'integer', I)
		)
	;
	 Base_type == 'floating_point' ->
	 	(FP $:: First..Last,		%unsound? very wide floating point numbers (e.g. of long double type with upper bound to 1.18973e+4932) will have their bounds 'approximated' to infinity by the IC library
		 ptc_solver__set_frame(Type_name, 'real', FP)
	   )
	;
		(concat_string(["Base type: ", Base_type, " is not valid"], Error_message),
		 ptc_solver__error(Error_message)
		)
	),
	asserta(ptc_solver__first(Type_name, First)),
	asserta(ptc_solver__last(Type_name, Last)),
	fail.
create_all_types :-
	c_type_declaration(_Type_name, _Base_type, _Size),	%todo Size is not currently recorded
	fail.
create_all_types :-
	!.
%%%%%%%%%%
%a real type without range constraint
ptc_solver__type(Type_mark, real) :-
	ptc_solver__verbose("*START Real Type", Type_mark),
	ptc_solver__get_frame(float, real, R),            %obtain the default frame of type float
	ptc_solver__set_frame(Type_mark, real, R),
	ptc_solver__first(float, Min),
	ptc_solver__last(float, Max),
	asserta(ptc_solver__first(Type_mark, Min)),
	asserta(ptc_solver__last(Type_mark, Max)),
	ptc_solver__verbose("*END Type", Type_mark).

%an enumeration type
ptc_solver__type(Type_mark, enumeration, LiteralL) :-
	ptc_solver__verbose("*START Enumeration Type", Type_mark),
	enumeration_start(Value),			    %can be user set via call to ptc_solver__set_flag(enumeration_start, Value)
	representation(LiteralL, Enum_list, Value),         %assign default positions to literals
	ptc_enum__record_enum(Type_mark, Enum_list),        %internal enum
	Enum_list = [(First_lit, First_pos)|_],         %obtain the first of the enumeration list
	length(Enum_list, L),
	L1 is L-1,
	length(Mask, L1),
	append(Mask, [(Last_lit, Last_pos)], Enum_list), %obtain the last of the enumeration list
	asserta(ptc_solver__first(Type_mark, First_lit)),
	asserta(ptc_solver__last(Type_mark, Last_lit)),
	ptc_enum__create_enum(Type_mark, Type_mark, First_pos, Last_pos, E), %create a dummy enumeration variable E
	ptc_solver__set_frame(Type_mark, base_enumeration, E),
	ptc_solver__verbose("*END Enumeration Type", Type_mark).

%a record type
%Flat_field_values is of the form a list of (Field_Name, Value)
ptc_solver__type(Type_mark, record, Field_list) :-
	ptc_solver__verbose("*START Record Type", Type_mark),
	extract_fields(Field_list, Field_values),       %replace the type marks with values
	flatten(Field_values, Flat_field_values),
	ptc_record__create_record(Flat_field_values, R),    %create a record variable R simply by adding new attribute
	ptc_solver__set_frame(Type_mark, record, R),           %record a frame R of type Type_mark
	ptc_solver__verbose("*END Record Type", Type_mark).

%a real type with range constraint
ptc_solver__type(Type_mark, real, range_bounds(Min, Max)) :-
	ptc_solver__verbose("*START Real Type", Type_mark),
	ptc_solver__get_frame(float, real, R),            %obtain the default frame of type float
	relation(>=, R, Min),
	relation(<=, R, Max),
	ptc_solver__set_frame(Type_mark, real, R),
	asserta(ptc_solver__first(Type_mark, Min)),
	asserta(ptc_solver__last(Type_mark, Max)),
	ptc_solver__verbose("*END Real Type", Type_mark).

%an integer type
ptc_solver__type(Type_mark, integer, range_bounds(Min, Max)) :-
	ptc_solver__verbose("*START Integer Type", Type_mark),
	ptc_solver__get_frame(integer, integer, I),
	relation(>=, I, Min),
	relation(<=, I, Max),
	ptc_solver__set_frame(Type_mark, integer, I),
	asserta(ptc_solver__first(Type_mark, Min)),
	asserta(ptc_solver__last(Type_mark, Max)),
	ptc_solver__verbose("*END Integer Type", Type_mark).

%a constrained array
ptc_solver__type(Type_mark, array, Index_list, Component_type_mark) :-
%!!!DEBUG!!!
%(Type_mark = limittabletype ->
%	set_flag(extract_index/2, spy, on)
%;
%	true
%),
	ptc_solver__verbose("*START Array Type", Type_mark),
	extract_index(Index_list, Eval_index_list),    %extract the list of index of the form a list of (First, Last)
	ptc_array__create_array(Type_mark, Component_type_mark, Eval_index_list, A),   %generate the array
	ptc_solver__set_frame(Type_mark, array(Component_type_mark), A), %record a frame A of type Type_mark
	%actually there seems to be no need to record the type of the component in the above
	ptc_solver__verbose("*END Array Type", Type_mark).

ptc_solver__subtype(Subtype_mark, Type_mark) :-
	ptc_solver__verbose("*START Subtype", Subtype_mark),
	ptc_solver__get_frame(Type_mark, Base_type, R),                 %obtain the frame R of type Type_mark
	ptc_solver__first(Type_mark, Min),
	ptc_solver__last(Type_mark, Max),
	ptc_solver__set_frame(Subtype_mark, Base_type, R),              %record the subtype
	asserta(ptc_solver__first(Subtype_mark, Min)),
	asserta(ptc_solver__last(Subtype_mark, Max)),
	ptc_solver__verbose("*END Subtype", Subtype_mark).

%a discrete subtype
ptc_solver__subtype(Subtype_mark, Type_mark, range_bounds(Min, Max)) :-
	ptc_solver__verbose("*START Subtype", Subtype_mark),
	ptc_solver__get_frame(Type_mark, Base_type, R),
	(Base_type = base_enumeration ->      %Type_mark is a base enumeration type
		(relation(>=, R, Min),
		 relation(<=, R, Max),
		 ptc_enum__get_position(Type_mark, Min, Min_pos), %obtain the position of Min [todo CHECK correctness of range]
		 ptc_enum__get_position(Type_mark, Max, Max_pos), %obtain the position of Max [todo CHECK correctness of range]
		 !,
		 asserta(ptc_solver__first(Subtype_mark, Min)), %record the first literal
		 asserta(ptc_solver__last(Subtype_mark, Max)),   %record the last literal
		 ptc_enum__create_enum(Type_mark, Subtype_mark, Min_pos, Max_pos, E),
		 ptc_solver__set_frame(Subtype_mark, Type_mark, E) %record the subtype, along with its base type (trick)
														   %07/05/01 after checking the above I am not convinced that it works
														   %it also breaks the pattern that a type is recorded with it
														   % base_type always
		)
	;
	 Base_type = integer ->		%is the subtype of an integer type
		(relation(>=, R, Min),
		 relation(<=, R, Max),
		 ptc_solver__set_frame(Subtype_mark, integer, R), %record the subtype
		 asserta(ptc_solver__first(Subtype_mark, Min)),  %record the first integer
		 asserta(ptc_solver__last(Subtype_mark, Max))    %record the last integer
		)
	;
	 Base_type = real ->
		(relation(>=, R, Min),
		 relation(<=, R, Max),
		 ptc_solver__set_frame(Subtype_mark, real, R),  %record the subtype
		 asserta(ptc_solver__first(Subtype_mark, Min)), %record the first integer
		 asserta(ptc_solver__last(Subtype_mark, Max))	%record the last integer
		)
	; %the subtype of a enumeration subtype
		ptc_solver__subtype(Subtype_mark, Base_type, range_bounds(Min, Max))
	),
	ptc_solver__verbose("*END Subtype", Subtype_mark).

ptc_solver__subtype(_Subtype_mark, _Type_mark, range(_)) :-           %todo
	ptc_solver__error("Subtype with range not yet implemented").
ptc_solver__subtype(_Subtype_mark, _Type_mark, range(_, _)) :-        %todo
	ptc_solver__error("Subtype with range not yet implemented").

%variable declarations
ptc_solver__variable(Identifier_list, Type_mark) :-
	ptc_solver__verbose("*START Variables", Identifier_list),
	init_variables(Identifier_list, Type_mark),
	ptc_solver__verbose("*END Variables", Identifier_list).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for enumeration types
representation([], [], _).
representation([Literal|RestL], [(Literal, N)|Rest_enum], N) :-
	N1 is N+1,
	representation(RestL, Rest_enum, N1).

% called from variable/2 by init_variables(Identifier_list, Type_mark)
% is recursif
% Identifier_list is the in list of variable identifiers
% Type_mark is the in type mark of the identifiers in Identifier_list
% initialise the var predicate for all identifiers in Identifier_list
init_variables([], _).
init_variables([Identifier|Rest], Type_mark) :-
	ptc_solver__get_frame(Type_mark, _, Identifier),
	!,                                              %added 02/02/2000
	init_variables(Rest, Type_mark).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% called from initialise_declarations/1 by extract_index(Index_list, Eval_index_list)
% is recursive
% Index_list is the in list of type_marks
% Eval_index_list is the out list of indexes of the form [(First0, Last0) ... (FirstN, LastN)] corresponding to Index_list
extract_index([], []).
extract_index([Type_mark|Rest_index_list], [(First, Last)|Rest_eval_index_list]) :-
	!,
	ptc_solver__first(Type_mark, First),
	!,
	ptc_solver__last(Type_mark, Last),
	extract_index(Rest_index_list, Rest_eval_index_list).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% called from initialise_declarations/1 by extract_fields(Field_list, Field_values)
% is recursive
% Field_list is the in list of ([Inner_field_list], Type_mark)
% Field_values is the out list of (Field, Value) where Value is a variable of Type_mark corresponding to Index_list
extract_fields([], []).
extract_fields([(Inner_field_list, Type_mark)|Rest], [Inner_field_values|Rest_field_values]) :-
	extract_inner_fields(Inner_field_list, Type_mark, Inner_field_values),
	extract_fields(Rest, Rest_field_values).

extract_inner_fields([], _, []).
extract_inner_fields([Field|Rest_field_list], Type_mark, [(Field, Value)|Rest_field_values]) :-
	!,
	ptc_solver__get_frame(Type_mark, _, Value),        %initialise field value
	!,
	extract_inner_fields(Rest_field_list, Type_mark, Rest_field_values).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%