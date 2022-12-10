%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chris Meudec
% Eclipse 7.0 program
% ptc_array.pl
% module array for array meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% an array variable R is of the form: R{ptc_array(Type, Component_type, Indice_Elements)},
%  where Indice_Elements is a list of (Indice, Element) and Type the type of the array and Component_type the type
%  of the elements of the array
%  a multidimensional array is represented as [([1, 1], el), [1, 2], el) etc.]
%  an array of array is represented as [([1], array), ([2], array) etc.]
%  so the internal representation follows the logical structures of arrays
% an array meta variable never becomes ground, even if an array is fully known it is still represented as a meta array variable
% remark: using suspend.pl template for metaterm handlers and the fact that only META + META will ever arise (and false calls)

:- module(ptc_array).

:- meta_attribute(ptc_array, [unify:unify_array/2, print:print_array/2]).
:- export ptc_array__is_array/1, ptc_array__create_array/4, ptc_array__get_all_index_elements/2.
:- export ptc_array__get_element/3, ptc_array__up_array/4, ptc_array__create_array_from_agg/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(ic)).
%:- ensure_loaded(ptc_solver).
:- import ptc_solver__arithmetic/3 from ptc_solver.
:- import ptc_solver__relation/3 from ptc_solver.
:- import ptc_solver__get_frame/3 from ptc_solver.
:- import ptc_solver__error/1 from ptc_solver.
%:- ensure_loaded(ptc_enum).
:- import ptc_enum__get_position/2 from ptc_enum.
%:- ensure_loaded(ptc_record).
:- import ptc_record__raw_get_field/2 from ptc_record.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%unify_array(Value, Attribute) : only a false call and META + nonvar(Attr) will ever arise
unify_array(_, Attr) :-
	var(Attr).                %ANY+VAR (false call)
unify_array(Term, Attr) :-
	compound(Attr),
	unify_term_array(Term, Attr).

unify_term_array(_{AttrY}, AttrX) :-  %META+META
	-?->
	AttrX = AttrY.

%print an array variable (only for debug atm)
%could be used to print elemenst range when non ground, or the elements when ground, to be used in conjunction of record printing, real,
%  integer, enumeration etc
print_array(_{ptc_array(Type, Element_type, Indice_elements)}, Print) :-
	-?->
	Print = ptc_array(Type, Element_type, Indice_elements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is an array variable
ptc_array__is_array(_{Attr}) :-
	-?->
	compound(Attr),
	Attr = ptc_array(_, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create an array variable
ptc_array__create_array(Type, Component_type, IndexL, R) :-
	ptc_solver__get_frame(Component_type, _, Element_frame),
	record(tmp_term, Element_frame),                %to make a perfect copy
	generate_array(IndexL, Indice_elements),
	erase_all(tmp_term),
	add_attribute(R, ptc_array(Type, Component_type, Indice_elements)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Indice_list is the list of indices of the form [(First0, Last0) ... (FirstN, LastN)]
% Element is the frame for the element of the array
%Indice_elements for an array of the form for a 1..3, 1..2 array is:
% _[([1,1], el1.1), ([1,2], el1.2), ([2,1], el2.1), ([2,2], el2.2), ([3,1], el3.1), ([3,2], el3.2)]
%should be more efficient than a backtracking version (e.g. using findall and member)
generate_array(Index_list, Combinations) :-
	gen1(Index_list, [[]], Combinations).

gen1([], [], []).
gen1([], [Index|Index_list], [(Index, Element)|Index_element_list]) :-
	recorded(tmp_term, Element),
	gen1([], Index_list, Index_element_list).
gen1([(Min, Max)|Rest], In, Out) :-
	gen_single_list(Min, Max, Single_list),
	combine(In, Single_list, Out1),
	gen1(Rest, Out1, Out).

%generate a list of indices between Min and Max
%gen_single_list(1, 4, [1, 2, 3, 4]).
gen_single_list(Max, Max, [Max]).
gen_single_list(Min, Max, [Min|Rest]) :-
	ptc_solver__arithmetic(succ(Min), Next, _),
	gen_single_list(Next, Max, Rest).

%combine a list of index with a list of indices
combine([], _, []).
combine([Index|Index_list], Indice_L, New_Index_list) :-
	subcombine(Indice_L, Index, Sub_Index_list),
	combine(Index_list, Indice_L, Rest_Index_list),
	append(Sub_Index_list, Rest_Index_list, New_Index_list).

%combine a list of indices with an index
%subcombine([1, 2], [1, 1, 1], [[1, 1, 1, 1], [1, 1, 1, 2]]).
subcombine([], _, []).
subcombine([Indice|Indice_L], Index, [New_Index|Rest]) :-
	append(Index, [Indice], New_Index),
	subcombine(Indice_L, Index, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ptc_array__get_all_index_elements(_{Attr}, Index_elements) :-
	-?->
	Attr = ptc_array(_, _, Index_elements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X is element(Array, Index) where Array is an array expression and Index is a list of index expressions
%when an index which needs to be evaluated (not always necessary) is unknown delays are reported in get_element
% throught the delayed list of variables Delayed, in which case if the type is unknown or
% if the result is not a meta variable(i.e. a frame) we get the type and frame
ptc_array__get_element(X, R, T) :-
	get_element(X, Simplified_X, Delayed),	%a lot of syntactic simplifications can be performed here
	(nonvar(Delayed) ->      			  	%the index is unknown because of the variables in Delayed
	    (Simplified_X = element(Array, _),
	     get_element_type(Array, Component_type_mark),      %extracts the type of the elements of Array
	     ptc_solver__get_frame(Component_type_mark, Component_base_type, R), %return a frame of type Component_type_mark i.e. of type the type of the elements of the array
	     (Component_base_type = integer ->                       %USE THE SAME TYPES INSTEAD OF THIS TRANSFORMATION (EITHER I OR INTEGER)
		 	T = i
	     ;
	      Component_base_type = real ->
	        T = r
	     ;
	      Component_base_type = base_enumeration ->
	        T = e
	     ;
	      Component_base_type = record ->
	        T = record
	     ;
	      Component_base_type = array(_) ->
	        T = array
	     ;
	        T = e	%must be an enumeration subtype
	     ),
		 suspend(ptc_solver__relation(=, Simplified_X, R), 3, Delayed->inst)	%Delayed may contain ic and/or enum variables
	    )
	;                                      % no delays
	    ptc_solver__arithmetic(Simplified_X, R, T)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get the element of a meta array variable while performing as much simplification as possible.
%Delayed : if the element is unknown, Delayed contains the variables on which the index depends
get_element(X, Simp_exp, Delayed) :-
	((X = element(Array, Index), ptc_array__is_array(Array)) ->
	    (eval_index(Index, Eval_index, Ground),
	     (Ground = true ->
		 	(ptc_array__get_all_index_elements(Array, Indice_elements),
		  	 member((Eval_index, Simp_exp), Indice_elements)
	        )
	     ;
	        (Simp_exp = element(Array, Eval_index),
			 Delayed = Eval_index
	        )
	     )
	    )
	;
	 X = element(agg(Type, AsgL), Index) ->
	    (ptc_array__create_array_from_agg(Type, AsgL, Array),
	     get_element(element(Array, Index), Simp_exp, Delayed)
	    )

%below is more efiicient than creating the array as above
%but below is incomplete as Find_AsgL does not work for positional aggregates
%	    eval_index(Index, Eval_index, Ground),
%	     (Ground = true ->
%		 find_AsgL(AsgL, Eval_index, Simp_exp)
%	     ;
%	         (Simp_exp = element(agg(Type, AsgL), Eval_index),
%		  Delayed = Eval_index
%	         )
%	     )

	;
	 X = element(up_arr(Array, Index_upa, Exp_upa), Index) ->
	    (eval_index(Index, Eval_index, Ground),
	     (Ground = true ->
		 	(eval_index(Index_upa, Eval_index_upa, Ground_upa),
		  	 (Ground_upa = true ->
		      	(Eval_index = Eval_index_upa ->
			  	 	Simp_exp = Exp_upa
		        ;
		          	get_element(element(Array, Eval_index), Simp_exp, Delayed)
		      	)
		  	 ;
		      	(Simp_exp = element(up_arr(Array, Eval_index_upa, Exp_upa), Eval_index),
			  	 Delayed = Eval_index_upa
		      	)
		  	 )
	        )
	     ;
	        (Simp_exp = element(up_arr(Array, Index_upa, Exp_upa), Eval_index),
			 Delayed = Eval_index
	        )
	     )
	    )
	;
	 X = element(element(Array, Index1), Index2) ->
	    (get_element(element(Array, Index1), Simp_exp1, D1),
	     (nonvar(D1) ->
	    	(Simp_exp = element(Simp_exp1, Index2),
			 Delayed = D1
	        )
	     ;
	        get_element(element(Simp_exp1, Index2), Simp_exp, Delayed)
	     )
	    )
	;                         
	 X = element(field(Record_exp, Field), Index) ->	%anything else must be a field record access
	    (ptc_solver__arithmetic(field(Record_exp, Field), Array, _),  %we obtain an array expression
	     get_element(element(Array, Index), Simp_exp, Delayed)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%obtain the type of the elements of an array expression
% a meta array variable
get_element_type(_{Attr}, Type) :-
	-?->
	Attr = ptc_array(_, Type, _).

%aggregate
get_element_type(agg(Array_type, _), Type) :-
	ptc_solver__get_frame(Array_type, array(Type), _).

get_element_type(element(Array, _), Type) :-
	get_element_type(Array, Array_type),
	ptc_solver__get_frame(Array_type, array(Type), _).

get_element_type(up_arr(Array, _, _), Type) :-
	get_element_type(Array, Type).

%anything else can only be field access of a record expression
%difficult, the easiest way is to evaluate the field access and obtain the type of the expression obtained
% a bit messy and superflous work
get_element_type(field(Record, Field), Type) :-
	ptc_record__raw_get_field(field(Record, Field), Array_exp),
	get_element_type(Array_exp, Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_index(Index, Eval_index, Ground) :-
	eval_index2(Index, Eval_index),
	(not nonground(Eval_index) ->
	    Ground = true
	;
	    Ground = false
	).

eval_index2([], []).
eval_index2([Index|Rest_indexes], [I|Rest_I]) :-
	ptc_solver__arithmetic(Index, Ieval, T),	%can be an integer or an enumeration
	(T = i ->
        Ieval #= I
	;
	    Ieval = I	%an enum
    ),
	eval_index2(Rest_indexes, Rest_I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%find the correct element corresponding to index in an aggregate (represented as a list of pairs (Indexes, Exp))
%can fail
find_AsgL(AsgL, [I], R) :-
    !,
    find_AsgL2(AsgL, I, R).
find_AsgL(AsgL, [I|Rest], R) :-   %more than one indice : multidimentional array
    !,
    find_AsgL2(AsgL, I, Exp),    %Exp can be an unqualified aggregate i.e. a pure AsgL
    ((nonvar(Exp), Exp = [_|_]) ->                %then Exp is an AsgL itself
        find_AsgL(Exp, Rest, R)
    ;
        R = Exp
    ).

find_AsgL2([], _, _) :-
	!,
	fail.
find_AsgL2([([others], Exp)], _, Exp) :-
    !.
find_AsgL2([(Indexes, Exp)|Next], I, R) :-
	(in_index(Indexes, I) ->
	    R = Exp
	;
	    find_AsgL2(Next, I, R)
	). 

in_index([], _) :-
	!,
	fail.
in_index([First|Next], I) :-
    (eq_index(First, I) ->
	    true
	;
	    in_index(Next, I)
	).

%all the different choices available
%can fail if doesn't match
eq_index(range_bounds(Min, Max), I) :-
	!,
	ptc_solver__relation(<=, Min, I),
	ptc_solver__relation(<=, I, Max).
eq_index(range(_), _) :-          %range attribute
    !,
    ptc_solver__error("range attribute not dealt with yet").
eq_index(A, I) :-                   %exp
	!,
	ptc_solver__relation(=, A, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%return an anonymous array variable of an updated meta array variable
%this is weird, looks like we may re-write the New_array variable several times (for a multidimential Index we
% may re-awake the up_array several times add do ptc_solver__get_frame several times)
% [this probably works via the unification handler for arrays that is defined at the top of this module]
% and finally whenever Index becomes ground, the array attribute is added to a var which already has
% an array attribute [again this probably works via the array unification handler]
% as well being very complex this looks like a lot of superflous work
% also there is no need to evaluate the index every time
ptc_array__up_array(Array, Index, Exp, New_array) :-
	ptc_array__is_array(Array),                  %Array is a meta array variable(should always be the case)
	ptc_array__get_all_index_elements(Array, Indice_elements),
	eval_index(Index, Eval_index, Ground),
	(Ground = true ->
	    (append(Start, [(Eval_index, _)|Rest], Indice_elements),
	     append(Start, [(Eval_index, Exp)|Rest], New_indice_elements),  %why do we do this?
	     get_meta_array_type(Array, Type),
	     create_array_from_indice_elements(Type, New_indice_elements, New_array)
	    )
	;
	    (get_meta_array_type(Array, Type),
	     ptc_solver__get_frame(Type, array(_), New_array),
		 suspend(ptc_array__up_array(Array, Eval_index, Exp, New_array), 3, Eval_index->inst)	%Eval_index may contain ic and/or enum variables
	    )
	).

get_meta_array_type(_{Attr}, Type) :-
	-?->
	Attr = ptc_array(Type, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%AsgL is of the form: for SPARK Ada
%                       [exp, ..., exp]                                                 --positional
%          or           [exp, ..., exp, ([others], exp)]                                --positional
%          or           [([others], exp)]                                               --named
%          or           [(Indices, exp), ..., ([Indices], exp)]                       --named
%          or           [(Indices, exp), ..., ([Indices], exp), ([others], exp)]      --named
%where Indices is of the form: for SPARK Ada
%                       [I1, ..., In]
%where I is of the form: for SPARK Ada
%                       exp
%          or           range_bounds(min, max)
%          or           range(typemark) --not yet implemented

ptc_array__create_array_from_agg(Type, AsgL, Array) :-
    ptc_solver__get_frame(Type, array(_), Anon_array),
	ptc_array__get_all_index_elements(Anon_array, Indice_elements),
    init_indice_elements(AsgL, Indice_elements),
    create_array_from_indice_elements(Type, Indice_elements, Array).

init_indice_elements(AsgL, Indice_elements) :-
    (AsgL = [(_, _)|_] ->
        init_indice_elements_named(AsgL, Indice_elements)
    ;
        init_indice_elements_positional(AsgL, Indice_elements)
    ).

%%%
init_indice_elements_positional([], _).
init_indice_elements_positional([([others], _)], []).
init_indice_elements_positional([([others], Exp_asg)], [(_, Value)|Rest2]) :-
        ptc_solver__relation(=, Exp_asg, Value),
        init_indice_elements_positional([([others], Exp_asg)], Rest2).
init_indice_elements_positional([Exp_asg|Rest], [(_, Value)|Rest2]) :-
        ptc_solver__relation(=, Exp_asg, Value),
        init_indice_elements(Rest, Rest2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%care is needed: Index is always a single index but Value can be arrays (for multi dimentional arrays)
% and AsgL can contain multi indexes
init_indice_elements_named(_, []).
init_indice_elements_named(AsgL, [(Index, Value)|Rest]) :-
        find_AsgL(AsgL, Index, Exp),
        ptc_solver__relation(=, Value, Exp),             %a simple '=' does not work because Value might be an aggregate
        init_indice_elements_named(AsgL, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_array_from_indice_elements(Type, Indice_elements, Array) :-
	ptc_solver__get_frame(Type, array(Component_type), _),
	add_attribute(Array, ptc_array(Type, Component_type, Indice_elements)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%