%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec
% Eclipse 7.0 program
% takes care of the labeling of variables in the solver's format
% various strategy could be implemented including the use of local search algorithms (Hill climbing, Tabu Search etc.)
%  see Eclipse sample programs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(ptc_labeling).

:- export ptc_labeling__integers/1, ptc_labeling__enums/1, ptc_labeling__reals/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- lib(clpq), lib(fd).
:- import ptc_enum__sample/1 from ptc_enum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from ... by ptc_labeling__integers(IL)
%recursif; can fail
%IL is the list of original integer variables from AVL
%! IL can contain ground integer terms
ptc_labeling__integers([]).
ptc_labeling__integers(IL) :-
	IL \= [],
	term_variables(IL, IL_nonground),
	(IL_nonground = [] ->
	    true
	;
	    (deleteffc(I, IL_nonground, Rest), % select the variable, I, in IL with the smallest domain, Rest is IL minus I
% generate a value for it, thus awaking dependent constraints possibly inducing backtracking if inconsistent
% deleteffc selects the most constrained variable amongst those with the smallest domain;
% deleteff the variable with the smallest domain
% could use maxdelayed as defined for the labeling of reals, because no way to chose the most constrained first.
% but the underlying problem in fd as opposed to clpqr is that nonlinear constraints are not idenfied as such and are
% not differentiated from ordinary delayed constraints
	     sample_integer(I),               %can fail
	     ptc_labeling__integers(Rest)
	    )
	).


%called from ptc_labeling__integers/1 by sample_integer(I)
%recursif; can fail; choice points
%Var is an integer variable
%I is given a system of constraint wide consistent integer value
%fail only when the entire original domain has been sampled
%remark: bias towards 0 (was true when the mid point was the middle of the domain)
sample_integer(I) :-
	dvar_domain(I, DomI),
	dom_range(DomI, MinI, MaxI),
%	Mid is (MaxI - MinI)//2 + MinI,
% Mid is the mid point of the domain of I
% another way to chose Mid to avoid results which look the same is to choose a cutting point at random
% frandom returns a float in [0, 1]
        frandom(N),
        Mid is fix((MaxI - MinI) * N) + MinI,
%1st choice point
	(
	    I #= Mid                    %try the mid point
	;
%on failure or backtracking the domain is split in two
	    (
%2nd choice point
	     (
		I #< Mid                %choose the left subdomain
	     ;
%on failure or backtracking the other subdomain is sampled
	        I #> Mid                %choose the right subdomain
	     ),
	     sample_integer(I)          %re sample according to chosen subdomain
	    )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from ... by ptc_labeling__enums(EL)
%recursif; can fail
%EL is the in list of original enumeration variables from AVL
%remark: EL can contain ground terms
ptc_labeling__enums([]).
ptc_labeling__enums([E|EL]) :-
	(nonground(E) ->
	    ptc_enum__sample(E) %was ptc_solver__sample_enum(E)            %take a sample, can be resastisfied
	;
	    true
	),
	ptc_labeling__enums(EL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from ... by ptc_labeling__reals(RL)
%recursif; can fail
%RL is the in list of original real variables from AVL
%remark: RL can contain ground real terms
ptc_labeling__reals([]).
ptc_labeling__reals(RL) :-
	term_variables(RL, VarsL),      %eliminate all grounds
        (VarsL == [] ->                 %do not take away : needs to be checked every time incl. first call
                true    %finished
        ;
                select_r(R, VarsL, Rest),  %all combinations are tried (for n : can be very large: n!)
                                %see select_r second  ';' clause where the search is cut
                                %whenever a list of the most constrained variables cannot be sampled
                                %linerect2.pl is a good exampel to try it on (it is very long because Det0 which is not one of the most
                                %constrained variable cannot be sampled
	        sample_real(R),         %many samples are taken
	        ptc_labeling__reals(Rest)
        ).

%called from ptc_labeling__reals/1 by select(R, RL_delayed, Rest_delayed)
%choice point
%R is an out real variable from RL_delayed
%RL_delayed is a list of real variables with associated delayed constraints
%Rest_delayed is out and is equivalent to RL_delayed minus R
%select a variable R from RL_delayed according to the maximum delayed first principle
% on backtraking take a random variable using delete
select_r(_, [], _) :-
        !,
        fail.
select_r(R, RL, Rest) :-
	maxdelayed(RL, -999, [], Low_delay_out, [], High_delay_out),
	(
            (delete(R, High_delay_out, Rest_High),  %random select from highly delayed variables
             append(Low_delay_out, Rest_High, Rest)
            )
        ;               %choice point
			(delete(R, Low_delay_out, Rest_Low),  %random select from lowly delayed variables
			 append(High_delay_out, Rest_Low, Rest)
   			)
	).

%initial call: maxdelayed(RL, -999, [], Low_delay_out, [], High_delay_out)
maxdelayed([], _, Low_delay_in, Low_delay_in, High_delay_in, High_delay_in) :- !.
maxdelayed([R|RL], Max_delay_nb, Low_delay_in, Low_delay_out, High_delay_in, High_delay_out) :-
	delayed_goals_number(R, Max_delays_R),
	(Max_delays_R > Max_delay_nb ->        %best solution so far needs updating
	                (New_max_delay = Max_delays_R,
	                 append(Low_delay_in, High_delay_in, New_low_delay_in),
                         New_high_delay_in = [R]
                        )
	;
                 Max_delays_R = Max_delay_nb ->
	                (New_max_delay = Max_delay_nb,      %no change
	                 New_low_delay_in = Low_delay_in,   %no change
                         New_high_delay_in = [R|High_delay_in]
                        )
                ;
	                (New_max_delay = Max_delay_nb,              %no change
                         New_low_delay_in = [R|Low_delay_in],
	                 New_high_delay_in = High_delay_in          %no change
	                )
        ),
	maxdelayed(RL, New_max_delay, New_low_delay_in, Low_delay_out, New_high_delay_in, High_delay_out).

%called from ptc_labeling__reals/1 by sample_real(R)
%can fail
%R is the real variable to sample
%constrain R to a value or fail
%the sampling is not exhaustive i.e. a failure does not indicate an absence of solution
% (contrast with sample_integer/1)
%remark: bias towards 0.0; a more efficient version would do the sampling using rationals
% with a domain size of 2000 about 18 samples are taken (3+5+5+5)
sample_real(R) :-
	inf(R, InfR),
	sup(R, SupR),
	SizeR is SupR - InfR,               %the size of the domaim of R
%	Mid is (SupR - InfR) / 2 + InfR,    %Mid is the mid point of the domain of R
%same as for integers: to introduce a bit of randomness in the result generated
%mid is chosen to be around +- 5 percent of the true middle
	frandom(N),
        Mid is InfR + (SizeR/2)*(0.95+N/10),
%	Mid is (SizeR/2 + InfR)*(0.95+N/20), --problems as Mid then is not always>inf
%        Mid is (N+4.5)*SizeR/10 + InfR,
	(   {R = Mid}
        ;
	    {R = InfR}
	;
	    {R = SupR}
	;
	    sample_real_around(mid, R, Mid, SizeR, 0.1) %trying different samples around Mid
        ;
	    sample_real_around(inf, R, InfR, SizeR, 0.1) %trying different samples around Inf
	;
	    sample_real_around(sup, R, SupR, SizeR, 0.1) %trying different samples around Sup
	).

%called from sample_real/1 by sample_real_around_mid(R, Mid, SizeR, 0.0)
%is recursif; can fail; choice points
%R is the real variable to sample
%Point is a point of the domain of R
%SizeR is the size of the domain of R
%the last argument, N, is a float which is increased after each recursion
% it is used to stop the recursion as well as to increase the gap between the mid point
% and the sample value generated after each recursion
sample_real_around(Region, R, Point, SizeR, Limit) :-
	Limit > SizeR -> %arbritrary stop of the recursion
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
	    (   {R = Sample}            %try the sample
% choice point
	    ;
	        (Next_limit is Limit*10, %increase the limit
		 sample_real_around(Region, R, Point, SizeR, Next_limit) %try again
	        )
	    )
	   )
       .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%           END           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%