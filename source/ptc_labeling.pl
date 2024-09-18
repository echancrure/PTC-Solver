%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec
% Eclipse 7.1 program
% takes care of the labelling of variables in the solver's format
% various strategy could be implemented including the use of local search algorithms (Hill climbing, Tabu Search etc.)
%  see Eclipse sample programs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- import ptc_enum__sample/1 from ptc_enum.
:- import ptc_solver__first/2, ptc_solver__last/2 from ptc_solver.
:- lib('ic').
:- lib('ic_kernel').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%make take a long time to complete on integers with very large intervals: may need a timeout
ptc_labeling__integers(L) :-
	%mytrace,
	constrain_to_finite_domain(L, L_out),	%the integer variables must not have infite domains
	ic:search(L_out, 0, most_constrained, 'indomain_random', bbs(5), []).	%aborts if one of the bound is infinite

constrain_to_finite_domain([], []).
constrain_to_finite_domain([verif(Type, Var)|R], [Var|Rest_vars]) :-
	get_bounds(Var, Lo, Hi),    %labeling will not work if the domain is infinite
	(Lo == -1.0Inf ->
		(ptc_solver__first(Type, First),
		 impose_min(Var, First) 	%from ic_kernel, will not trigger propagation, but will restrict the domain
		)
	;
	  	true
	),
	(Hi == 1.0Inf ->
		(ptc_solver__last(Type, Last),
		 impose_max(Var, Last)		%from ic_kernel, will not trigger propagation, but will restrict the domain
		)
	;
	  	true
	),
	constrain_to_finite_domain(R, Rest_vars).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%can fail, leave choice points
%EL is the in list of original enumeration variables
%remark: EL can contain ground terms
ptc_labeling__enums([]).
ptc_labeling__enums([E|EL]) :-
	(nonground(E) ->
	    ptc_enum__sample(E)		%take a sample, can be resatisfied
	;
	    true
	),
	ptc_labeling__enums(EL).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%can fail, leave choice points
%VL is the list of IC vars (or breal numbers) to label
%returns as FL a list corresponding floats as approximation
%make take a long time to complete on reals with very large intervals: may need a timeout
%The default threshold is 1e-8.
ptc_labeling__reals(VL, FL) :-	%the bounds of the real varaibles must not be infinite at this stage
	%mytrace,
	%ic:locate(VL, VL, 0.001, log),	%reduce the intervals of the vars in L, down to less than the precision given; the outcome is a list of breals or IC real vars: both representations are interval based
	%ic:locate(VL, VL, 1000.0, log),
	force_instantiation(VL, FL).

%force the breals and IC Vars to become ground and returns a list corresponding floats as approximation
force_instantiation([], []).
force_instantiation([V|VL], [F|FL]) :-
	sample_ICVar(V),
	F is float(V),
	force_instantiation(VL, FL).

%can fail
%R is the real variable to sample
%constrain R to a value or fail
%the sampling is not exhaustive i.e. a failure does not indicate an absence of solution
% (contrast with sample_integer/1)
%remark: bias towards 0.0;
% with a domain size of 2000 about 18 samples are taken (3+5+5+5)
sample_ICVar(R) :-
	ic:get_float_bounds(R, InfR, SupR),
	SizeR is SupR - InfR,               %the size of the domaim of R
	frandom(N),
    Mid is InfR + (SizeR/2)*(0.95+N/10),%mid is chosen to be around +- 5 percent of the true middle
	(	R $= Mid
    ;	% choice point
	    R $= InfR
	;	% choice point
	    R $= SupR
	;	% choice point
	    sample_real_around(mid, R, Mid, SizeR, 0.1) %trying different samples around Mid
    ;	% choice point
	    sample_real_around(inf, R, InfR, SizeR, 0.1) %trying different samples around Inf
	;	% choice point
	    sample_real_around(sup, R, SupR, SizeR, 0.1) %trying different samples around Sup
	).

%can fail; choice points
%R is the real variable to sample
%Point is a point of the domain of R
%SizeR is the size of the domain of R
%the last argument, N, is a float which is increased after each recursion
% it is used to stop the recursion as well as to increase the gap between the mid point
% and the sample value generated after each recursion
sample_real_around(Region, R, Point, SizeR, Limit) :-
	(Limit > SizeR -> %arbritrary stop of the recursion
		(!,
	     fail
	    )
    ;
		(random(N1),
	     random(N2),
	     (N1 > N2 ->         %generate a standard deviation
			Deviation is N2/N1
	     ;
	        (Region == mid ->
                Deviation is -N1/N2
            ;
                Deviation is N1/N2
        	)
	    ),
%generate a sample value around the mid point, with limit increasing after each recursion
	    (Region == sup ->
            Sample is Point - Limit*Deviation
        ;
            Sample is Limit*Deviation + Point
        ),
	    (   R $= Sample            %try the sample
	    ;	% choice point
	    	(Next_limit is Limit*10, %increase the limit
		 	 sample_real_around(Region, R, Point, SizeR, Next_limit) %try again
	        )
	    )
	   )
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%