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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   S_MOD/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X, Y and R are integer expressions
%constraint: R = X mod Y  'modulo'
s_mod(X, Y, Z) :-
	Xeval #= eval(X),                  %evaluate X
	Yeval #= eval(Y),                  %evaluate Y
	Zeval #= eval(Z),                  %evaluate Z
	cons_same_sign(Yeval, Zeval),  %constrain Y and Z to be of the same sign (always hold)
	s_mod2(Xeval, Yeval, Zeval).

%called from s_mod/3 by s_mod2(Xeval, Yeval, Zeval)
%X, Y and Z are integer variables
s_mod2(X, Y, Z) :-
	((extract_sign(X, SignX), extract_sign(Y, SignY)) ->    %the signs are known
	    (SignX = SignY ->                      %X and Y are of the same sign
			Z #= X rem Y                 %X mod Y = X rem Y
	    ;
	        (%below is only true if Z1 <> 0 otherwise the result is 0
	         Z1 #= X rem Y                   %X mod Y = X rem Y + Y
	         (ground(Z1) ->
	            (Z1 == 0 ->
	                Z #= 0
                ;
                    Z #= Z1 + Y                      %X and Y are of opposite sign
		        )
	         ;
				suspend(s_mod2(X, Y, Z), 3, [X, Y, Z]->ic:[min,max,hole])
             )
	        )
	    )
	;                                          %the signs are unknown
		suspend(s_mod2(X, Y, Z), 3, [X, Y, Z]->ic:[min,max,hole])
	).

%called from s_mod/3 by cons_same_sign(Yeval, Zeval)
%X and Y are integer variables
%constrain X and Y to be of the same sign
cons_same_sign(X, Y) :-
	(extract_sign(X, SignX) ->  %the sign of X is known
	    (SignX = 1 ->
			Y #>= 0
	    ;
	     SignX = -1 ->
	        Y #=< 0
	    )
	;                          %the sign of X is unknown
	 extract_sign(Y, SignY) ->  %the sign of Y is known
	    (SignY = 1 ->
			X #>= 0
	    ;
	     SignY = -1 ->
	        X #=< 0
	    )
	;                          %the sign of X and the sign of Y are both unknown
		suspend(cons_same_sign(X, Y), 3, [X,Y]->ic:[min,max,hole])
	).

%called from s_mod2/3 twice and from cons_same_sign/2 twice
%can fail
%X is an integer variable
%SignX is the out sign of X, 1 denote that X is positive, -1 negative
%fail when the sign is unknown
extract_sign(X, SignX) :-
	get_integer_bounds(X, MinX, MaxX),
	(MinX >= 0 ->
	    SignX = 1
	;
	 MaxX =< 0 ->
	    SignX = -1
	;
		fail
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%