%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 08/10/99
% Eclipse 7.0 program
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   S_DIV/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X, Y and Z are integer expressions
%user defined integer div constraint, remark: X div Y = truncate(X / Y)
s_div(X, Y, Z) :-
	Xeval #= eval(X),                %evaluate X
	Yeval #= eval(Y),                %evaluate Y
	Zeval #= eval(Z),                %evaluate Z
	Yeval #\= 0,                  %Y \= 0 always hold
	s_div2(Xeval, Yeval, Zeval).

%called from s_div/3 by s_div2(Xeval, Yeval, Zeval)
%X, Y and Z are integer variables
s_div2(X, Y, Z) :-
	((ground(X), ground(Y)) -> 	%covers the case when X, Y and Z are ground
	    Z is X // Y
	;
	 X == 0 ->                 	%special case, 0 div Y = 0 always hold
	    Z #= 0                  %Y is free
	;
	 Z == 0 ->                  %special case, abs(Y) > abs(X) holds
	    (AbsY #> AbsX,          %could be improved
	     Y #= abs(AbsY),        %as we are not exactly interested in the absolute value of Y
	     X #= abs(AbsX)         %idem
	    )
	;
	 Z == 1 ->                  %special case, X div X = 1 always hold
	    X #= Y
	;
	 Z == -1 ->                 %special case, X div -X = -1 always hold
	    X #= -Y
	;
	 Y == 1 ->                  %special case, X div 1 = X always hold
	    X #= Z
	;
	 Y == -1 ->                 %special case, X div -1 = -X always hold
	    X #= -Z
	;
	    (%calculating the bounds of X according to Y and Z
		 get_integer_bounds(X, MinX, MaxX),
		 get_integer_bounds(Y, MinY, MaxY),
		 get_integer_bounds(Z, MinZ, MaxZ),
	     X1 is MinY*(MinZ + sgn(MinZ)) - sgn(MinY)*sgn(MinZ), %X bounds similar to Y*Z
	     X11 is MinY*MinZ,
	     X2 is MinY*(MaxZ + sgn(MaxZ)) - sgn(MinY)*sgn(MaxZ),
	     X22 is MinY*MaxZ,
	     X3 is MaxY*(MinZ + sgn(MinZ)) - sgn(MaxY)*sgn(MinZ),
	     X33 is MaxY*MinZ,
	     X4 is MaxY*(MaxZ + sgn(MaxZ)) - sgn(MaxY)*sgn(MaxZ),
	     X44 is MaxY*MaxZ,
	     mum(>, [X1, X11, X2, X22, X3, X33, X4, X44], _, New_maxX),
	     mum(<, [X1, X11, X2, X22, X3, X33, X4, X44], _, New_minX),
		 X #:: New_minX .. New_maxX,
	     %calculate the bounds of Z according to X and Y
	     bounds_XdivY(Y, MinX, MaxX, MinY, MaxY, New_minZ, New_maxZ),
	     Z #:: New_minZ .. New_maxZ,
	     %calculate the bounds of Y according to X and Z
	     (is_in_domain(0, Z) ->
		 	true                  %Y is nearly free
	     ;
	        (bounds_XdivY(Z, MinX, MaxX, MinZ, MaxZ, New_minY, New_maxY),
		  	 Y #:: New_minY .. New_maxY
	        )
	     ),
	     term_variables((X, Y, Z), Vars),
	     (Vars = [] ->
		 	true                  %all the variables are ground
	     ;
			suspend(s_div2(X, Y, Z), 3, Vars->ic:[min,max,hole])
	     )
	    )
	).

%called by s_div2/3 and bound_XdivY/7
%Op is either '>' or '<'
%the second argument is a list of numbers or 'nc' terms
%the third argument is the result so far
%the fourth argument is the out maximum (if Op is '>') or minimum (if Op is '<') of the list of numbers
mum(_, [], M, M).
mum(Op, [I|IL], M, Res) :-
	((ground(M), M \= nc) ->
		(Op = > ->
	       	((I \= nc, I > M) ->
		   		mum(Op, IL, I, Res)
	        ;
	           	mum(Op, IL, M, Res)
	       	)
	    ;
	     Op = < ->
	       	((I \= nc, I < M) ->
		   		mum(Op, IL, I, Res)
	        ;
	           	mum(Op, IL, M, Res)
	        )
	   )
    ;
		mum(Op, IL, I, Res)
	).

%called from s_div2/3 by bounds_XdivY(Y, MinX, MaxX, MinY, MaxY, New_minZ, New_maxZ) etc
%Y is an integer variable
%MinX, MaxX, MinY, MaxY are the minimum and maximum of X and Y respectively
%New_minZ, New_maxZ are the out bounds of X div Y
%calculate the bound of X div Y
bounds_XdivY(Y, MinX, MaxX, MinY, MaxY, New_minZ, New_maxZ) :-
	dom_near_0(Y, Max_negY, Min_posY), %obtain the closest numbers around 0
	(ground(Min_posY) ->
	    (Z1 is MaxX // Min_posY,
	     Z11 is MinX // Min_posY
	    )
	;
	    (Z1 = nc,                     %nc indicates that the bound are unknown
	     Z11 = nc
	    )
	),
	(ground(Max_negY) ->
	    (Z2 is MinX // Max_negY,
	     Z12 is MaxX // Max_negY
	    )
	;
	    (Z2 = nc,
	     Z12 = nc
	    )
	),
	Z3 is MaxX // MinY,
	Z4 is MinX // MaxY,
	mum(>, [Z1, Z2, Z3, Z4], _, New_maxZ), %the maximum of the list
	mum(<, [Z11, Z12, Z3, Z4],_, New_minZ). %the minimum of the list

%called from bounds_XdivY/7 by dom_near_0(Y, Max_negY, Min_posY)
%Var is an integer variable
%Max_neg is the maximum negative of Var
%Min_pos is the minimum positive of Var
dom_near_0(Var, Max_neg, Min_pos) :-
	get_domain(Var, Dom),
	[V2, V3] #:: Dom,
	(V2 #> 0 ->                     %V2 is similar to Var but positive
	    get_min(V2, Min_pos)
	;
	    true
	),
	(V3 #< 0 ->                     %V3 is similar to Var but negative
		get_max(V3, Max_neg)
	;
	    true
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
			s_rem(X, Y, Z)                  %X mod Y = X rem Y
	    ;
	        (%below is only true if Z1 <> 0 otherwise the result is 0
	         s_rem(X, Y, Z1),                   %X mod Y = X rem Y + Y
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

%called from s_mod/3 by cons_same_sign(Yeval, Zeval) and from s_rem/3 by cons_same_sign(X, Z)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   S_REM/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%X, Y and R are integer expressions
%constraint: R = X rem Y  'remainder'
%also called from s_mod2/3 twice
%constraint:  X rem Y = Z is equivalent to X - (X div Y)*Y = Z
s_rem(X, Y, Z) :-
	Xeval #= eval(X),                  %evaluate X
	Yeval #= eval(Y),                  %evaluate Y
	Zeval #= eval(Z),                  %evaluate Z
	cons_same_sign(Xeval, Zeval),  %constrain X and Z to be of the same sign (always hold)
	s_div(Xeval, Yeval, XDivY),
	Zeval #= Xeval - XDivY*Yeval.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%