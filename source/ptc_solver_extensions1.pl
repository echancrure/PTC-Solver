%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 08/10/99
% Eclipse 7.0 program
% ptc_solver_extensions1.pl
% user defined constraints
% part of the ptc_solver module
s_cast_to_real(I, R) :-
        (ground(I) ->
                {R = I}         %there is no need to evaluate I: alows integers outside of FD's range to be converted to reals
                                %modified 22/02/05 (see eileenlarge210205([I, R]) predicate is ptc_solver_demo.pl)
        ;
                Ieval #= I+0                        %evaluate I
        ),
	s_cast_to_real2(Ieval, R).

s_cast_to_real2(I, R) :-
        ground(I) ->
                {R = I}         %I is unique no further constraint
        ;
        ground(R) ->
                (fix(R, R_fix),         %truncate towards 0
                 ({R_fix-R = 0} ->      %R must be an integer
                        I #= R_fix
                  ;
                        fail            %yes it is a failure (e.g. no int could ever be casted into, say, 6.2)
                 )
                )
        ;
        %updating the real according to the integer
                (dvar_domain(I, DomI),
	         dom_range(DomI, MinI, MaxI),
                 {R >= MinI},           %e.g. MinI = 5 -> [5.0 .. X and MinI = -5 -> ]-6.0 .. X (but must be an integer so [-5.0 ..X
                 {R =< MaxI},           %e.g. MaxI = 8 -> X .. 9.0[ (but must be an integer so X .. 8.0] and MaxI = -8 -> X .. -8.0]
                 %updating the integer according to the real
                 %REMEMBER : R must be an integer
	         inf(R, Inf),
                 fix(Inf, Inf_fix),
                 ({Inf_fix-Inf = 0} ->  %i.e. the inf is an integer
                        I #>= Inf_fix
                 ;
                  Inf > 0 ->            %e.g. Inf = 3.2 ->  I >= 4 (and R >= 4.0)
                        (I #>= Inf_fix+1,
                         {R >= Inf_fix+1}
                        )
                 ;                      %e.g. Inf = -3.2 -> I >= -3 (and R >= -3.0)
                        (I #>= Inf_fix,
                         {R >= Inf_fix}
                        )
                 ),
                 sup(R, Sup),
                 fix(Sup, Sup_fix),
                 ({Sup_fix-Sup = 0} ->  %i.e. the sup is an integer
                        I #<= Sup_fix
                 ;
                  Sup >= 0 ->            %e.g. Sup = 10.7 -> I <= 10 (and R <= 10.0)
                        (I #<= Sup_fix,
                         {R =< Sup_fix}
                        )
                 ;                      %e.g. Sup = -10.7 -> I <= -11 (and R <= -11.0)
                        (I #<= Sup_fix-1,
                         {R =< Sup_fix-1}
                        )
                 ),
                 (ground(I) ->
		        true                        %no further improvements
	         ;
	                (make_suspension(s_cast_to_real2(I, R), 1, Susp), %delay on R and I
	                 insert_suspension(R, Susp, inst of suspend, suspend),
	                 insert_suspension(I, Susp, any of fd, fd)
	                )
	         )
                )
        .

%to be changed to s_cast_to_int
s_cast_to_int(R, I) :-
        Ieval #= I+0,                        %evaluate I
	{R >= -1000000000, R =< 1000000000}, %otherwise conversion will fail
	s_cast_to_int2(R, Ieval).

s_cast_to_int2(R, I) :-
        ground(R) ->
                (fix(R, R_fix),  %truncate towards 0
                 I #= R_fix             %I is unique no further constraint
                )
        ;
        ground(I) ->
                (I > 0 ->                       %I = 5
                        {I =< R, R < I+1}       %R : [5.0 .. 6.0[, no further constraint
                ;
                 I < 0 ->                       %I = -5
                        {I-1 < R, R =< I}       %R : ]-6.0 .. -5.0], no further constraint
                ;
                 I = 0 ->                       %I = 0
                        {-1 < R, R < 1}            %R : ]-1.0 .. 1.0[, no further constraint
                )
        ;
        %updating the real according to the integer
                (dvar_domain(I, DomI),
	         dom_range(DomI, MinI, MaxI),
                 (MinI > 0 ->
                        {R >= MinI}             % 3..X -> [3.0..X'
                 ;
                        {R > MinI - 1}          % -3..X -> ]-4..X' (covers 0)
                 ),
                 (MaxI >= 0 ->
                        {R < MaxI + 1}          % X..3 -> X'..4[ (covers 0)
                 ;
                        {R =< MaxI}             % X..-3 -> X'..-3]
                 ),
                 %updating the integer according to the real
	         inf(R, Inf),                   %Inf = 3.2 -> I >= 3
                 fix(Inf, Inf_fix),             %Inf = -3.2 -> I >= -3
                 sup(R, Sup),                   %Sup = 8.2 -> I <= 8
                 fix(Sup, Sup_fix),             %Sup = -8.2 -> I <= -8
                 I #>= Inf_fix,
                 I #<= Sup_fix,
                 (ground(I) ->
		        true                        %no further improvements
	         ;
	                (make_suspension(s_cast_to_int2(R, I), 1, Susp), %delay on R and I
	                 insert_suspension(R, Susp, inst of suspend, suspend),
	                 insert_suspension(I, Susp, any of fd, fd)
	                )
	         )
                )
        .
%%%%%%%%%%%%%%%%%%%%%%%   S_ROUND/2   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from arithmetic/3 by s_round(X, R)
%R is a real variable
%I is an integer expression
%user defined round constraint
s_round(R, I) :-
	Ieval #= I+0,                        %evaluate I
	{R >= -1000000000, R =< 1000000000}, %otherwise conversion will fail
	s_round2(R, Ieval).


%called from s_round/2 by s_round2(R, Ieval)
%R is a real variable
%I is an integer variable
s_round2(R, I) :-
	ground(R) ->
	    ada_round(R, I)                  %implies I is ground
	;
	ground(I) ->
	    ada_round_inverse(I, I, R)       %implies R in an interval
	;                                    %nor R nor I are ground
	    (%updating the real according to the integer
	     dvar_domain(I, DomI),
	     dom_range(DomI, MinI, MaxI),
	     ada_round_inverse(MinI, MaxI, R),
	     %updating the integer according to the real
	     inf(R, Inf),
	     ada_round(Inf, Inf_round),
	     ((1 is denominator(Inf - 1_2),  %decimal part is 0.5
	       Inf < 0,                      %Inf is negative
	       entailed(R=\= Inf)            %Inf is not taken
	      ) ->
	          Min is Inf_round + 1       %special case when Inf in {-0.5, -1.5, -2.5 ...} and R \= Inf
	      ;
	          Min is Inf_round
	     ),
	     sup(R, Sup),
	     ada_round(Sup, Sup_round),
	     ((1 is denominator(Sup - 1_2),  %decimal part is 0.5
	       Sup > 0,                      %Sup is positive
	       entailed(R =\= Sup)           %Sup is not taken
	      ) ->
	          Max is Sup_round - 1       %special case when Sup in {0.5, 1.5, 2.5 ...} and R \= Sup
	      ;
	          Max is Sup_round
	     ),
	     I #>= Min,
	     I #<= Max,
	     (ground(I) ->
		 true                        %no further improvements
	      ;
	         (make_suspension(s_round2(R, I), 3, Susp), %delay on R and I
	          insert_suspension(R, Susp, inst of suspend, suspend),
	          insert_suspension(I, Susp, any of fd, fd)
	         )
	     )
	    )
	.

%called from s_round2/2 by ada_round(R, I), ada_round(Inf, Min -1) etc
%R is a real and is ground (preferably a rational)
%I is an integer
%calculate round(R) according to Ada95 semantics
ada_round(R, I) :-
	FixR is fix(R),
	(abs(FixR - R) >= 1_2 ->
	    I is FixR + sgn(R)               %round away from 0 when decimal part >= 0.5
	;
	    I is FixR
	).


%called from s_round2/2 by ada_round_inverse(I, I, R) and ada_round_inverse(MinI, MaxI, R)
%MinI is the minimum of the integer I
%MaxI is the maximum of the integer I
%R is a real variable
%constrain R to an interval according to MinI and MaxI
%when MinI = MaxI = I then behaves as the inverse of round
ada_round_inverse(MinI, MaxI, R) :-
	(MinI > 0 ->
	    {R >= MinI - 1_2}
	;
	    {R > MinI - 1_2}          %when MinI <= 0
	),
	(MaxI < 0 ->
	    {R =< MaxI + 1_2}
	;
	    {R < MaxI + 1_2}          %when MaxI >= 0
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   S_DIV/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from arithmetic/3 by s_div(X, Y, Z)
%X, Y and Z are integer expressions
%user defined integer div constraint, remark: X div Y = truncate(X / Y)
s_div(X, Y, Z) :-
	Xeval #= X+0,                %evaluate X
	Yeval #= Y+0,                %evaluate Y
	Zeval #= Z+0,                %evaluate Z
	Yeval #= 0,                  %Y \= 0 always hold
	s_div2(Xeval, Yeval, Zeval).

%called from s_div/3 by s_div2(Xeval, Yeval, Zeval)
%X, Y and Z are integer variables
s_div2(X, Y, Z) :-
	((ground(X), ground(Y)) ->    %covers the case when X, Y and Z are ground
	    Z is X // Y
	;
	 X == 0 ->                    %special case, 0 div Y = 0 always hold
	    Z #= 0                   %Y is free
	;
	 Z == 0 ->                    %special case, abs(Y) > abs(X) holds
	    (AbsY #> AbsX,           %could be improved
	     s_abs(Y, AbsY),         %as we are not exactly interested in the absolute value of Y
	     s_abs(X, AbsX)          %idem
	    )
	;
	 Z == 1 ->                    %special case, X div X = 1 always hold
	    X #= Y
	;
	 Z == -1 ->                   %special case, X div -X = -1 always hold
	    X #= -Y
	;
	 Y == 1 ->                    %special case, X div 1 = X always hold
	    X #= Z
	;
	 Y == -1 ->                   %special case, X div -1 = -X always hold
	    X #= -Z
	;
	    (%calculating the bounds of X according to Y and Z
	     dvar_domain(X, DomX),
	     dom_range(DomX, MinX, MaxX),
	     dvar_domain(Y, DomY),
	     dom_range(DomY, MinY, MaxY),
	     dvar_domain(Z, DomZ),
	     dom_range(DomZ, MinZ, MaxZ),
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
	     (abs(New_minX) < 1000000000  ->
			X #>= New_minX
	     ;
	        true                 %to avoid overflow
	     ),
	     (abs(New_maxX) < 1000000000 ->
	        X #<= New_maxX
	     ;
	          true                 %to avoid overflow
	     ),
	     %calculate the bounds of Z according to X and Y
	     bounds_XdivY(Y, MinX, MaxX, MinY, MaxY, New_minZ, New_maxZ),
	     Z :: New_minZ .. New_maxZ,
	     %calculate the bounds of Y according to X and Z
	     (dom_member(0, DomZ) ->
		 	true                  %Y is nearly free
	     ;
	        (bounds_XdivY(Z, MinX, MaxX, MinZ, MaxZ, New_minY, New_maxY),
		  	 Y :: New_minY .. New_maxY
	        )
	     ),
	     term_variables((X, Y, Z), Vars),
	     (Vars = [] ->
		 	true                  %all the variables are ground
	     ;
	    	(make_suspension(s_div2(X, Y, Z), 3, Susp),
		  	 insert_suspension(Vars, Susp, any of fd, fd)
	        )
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
	(ground(M), M \= nc) ->
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
       .



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
	dvar_domain(Var, Dom),
	var_fd(V2, Dom),
	var_fd(V3, Dom),
	(V2 #> 0 ->                     %V2 is similar to Var but positif
	    (dvar_domain(V2, DomV2),
	     dom_range(DomV2, Min_pos, _)
	    )
	;
	    true
	),
	(V3 #< 0 ->                     %V3 is similar to Var but negatif
	    (dvar_domain(V3, DomV3),
	     dom_range(DomV3, _, Max_neg)
	    )
	;
	    true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   S_MOD/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from arithmetic/3 by s_mod(X, Y, R)
%X, Y and R are integer expressions
%constraint: R = X mod Y  'modulo'
s_mod(X, Y, Z) :-
	Xeval #= X+0,                  %evaluate X
	Yeval #= Y+0,                  %evaluate Y
	Zeval #= Z+0,                  %evaluate Z
	cons_same_sign(Yeval, Zeval),  %constrain Y and Z to be of the same sign (always hold)
	s_mod2(Xeval, Yeval, Zeval).


%called from s_mod/3 by s_mod2(Xeval, Yeval, Zeval)
%X, Y and Z are integer variables
s_mod2(X, Y, Z) :-
	(extract_sign(X, SignX), extract_sign(Y, SignY)) ->    %the signs are known
	    (SignX = SignY ->                      %X and Y are of the same sign
		s_rem(X, Y, Z)                  %X mod Y = X rem Y
	    ;
	        (%below is only true if Z1 <> 0 otherwise the result is 0
	         s_rem(X, Y, Z1),                   %X mod Y = X rem Y + Y
	         (ground(Z1) ->
	                (Z1 = 0 ->
	                        Z #= 0
                        ;
                                Z #= Z1 + Y                      %X and Y are of opposite sign
		        )
	         ;
	                (make_suspension(s_mod2(X, Y, Z), 3, Susp),
	                 insert_suspension((Z1), Susp, inst of suspend, suspend)
	                )
                 )
	        )
	    )
	;                                          %the signs are unknown
	    (make_suspension(s_mod2(X, Y, Z), 3, Susp),
	     insert_suspension((X, Y, Z), Susp, any of fd, fd)
	    )
	.


%called from s_mod/3 by cons_same_sign(Yeval, Zeval) and from s_rem/3 by cons_same_sign(X, Z)
%X and Y are integer variables
%constrain X and Y to be of the same sign
cons_same_sign(X, Y) :-
	extract_sign(X, SignX) ->  %the sign of X is known
	    (SignX = 1 ->
		Y #>= 0
	    ;
	     SignX = -1 ->
	        Y #<= 0
	    )
	;                          %the sign of X is unknown
	extract_sign(Y, SignY) ->  %the sign of Y is known
	    (SignY = 1 ->
		X #>= 0
	    ;
	     SignY = -1 ->
	        X #<= 0
	    )
	;                          %the sign of X and the sign of Y are both unknown
	    (make_suspension(cons_same_sign(X, Y), 3, Susp),
	     insert_suspension((X, Y), Susp, any of fd, fd)
	    )
	.

%called from s_mod2/3 twice and from cons_same_sign/2 twice
%can fail
%X is an integer variable
%SignX is the out sign of X, 1 denote that X is positive, -1 negative
%fail when the sign is unknown
extract_sign(X, SignX) :-
	dvar_domain(X, DomX),
	dom_range(DomX, MinX, MaxX),
	(MinX >= 0 ->
	    SignX = 1
	;
	 MaxX =< 0 ->
	    SignX = -1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   S_REM/3   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%called from arithmetic/3 by s_rem(X, Y, R)
%X, Y and R are integer expressions
%constraint: R = X rem Y  'remainder'
%also called from s_mod2/3 twice
%constraint:  X rem Y = Z is equivalent to X - (X div Y)*Y = Z
s_rem(X, Y, Z) :-
	Xeval #= X+0,                  %evaluate X
	Yeval #= Y+0,                  %evaluate Y
	Zeval #= Z+0,                  %evaluate Z
	cons_same_sign(Xeval, Zeval),  %constrain x and Z to be of the same sign (always hold)
	s_div(Xeval, Yeval, XDivY),
	Xeval - XDivY*Yeval #= Zeval.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%