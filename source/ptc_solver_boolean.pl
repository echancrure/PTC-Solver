%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 19/12/00
% Eclipse 7.0 program
% Boolean constraints
% part of the solver module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
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