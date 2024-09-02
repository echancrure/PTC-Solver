%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 08/10/99
% Eclipse 7.1 program
% user defined constraints
% part of the ptc_solver module
%%%%%%%%%%%%%%%%%%%%%%%   s_cast_to_int/2   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%R is a real variable
%I is an integer expression
%custom defined casting constraint using C convention of truncating
s_cast_to_int(R, I) :-
	Ieval #= eval(I),
	s_cast_to_int2(R, Ieval).

%called from s_round/2 by s_round2(R, Ieval)
%R is a real variable
%I is an integer variable
s_cast_to_int2(R, I) :-
	(ground(R) ->
	    (get_min(R, Min),
		 I is fix(Min)	%I think that should always work 
		)
	;
	 ground(I) ->
	    round_inverse(I, I, R)       %implies R in an interval
	;                                    %nor R nor I are ground
	    (%updating the real according to the integer
		 get_integer_bounds(I, MinI, MaxI),
	     round_inverse(MinI, MaxI, R),
	     %updating the integer according to the real
		 get_float_bounds(R, Inf, Sup),
		 Min is fix(Inf),
		 Max is fix(Sup),
		 I #:: Min .. Max,
	     (ground(I) ->
		 	true                        %no further improvements
	     ;
			suspend(s_cast_to_int2(R, I), 3, [R, I]->ic:[min,max])
	     )
	    )
	).

%MinI is the minimum of the integer I
%MaxI is the maximum of the integer I
%R is a real variable
%constrain R to an interval according to MinI and MaxI
%when MinI = MaxI = I then behaves as the inverse of round
round_inverse(MinI, MaxI, R) :-
	(MinI > 0 ->
		R $>= MinI
	;		
		R $> MinI - 1
	),
	(MaxI > 0 ->
	    R $< MaxI + 1
	;
	    R $=< MaxI
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%