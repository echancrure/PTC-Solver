%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec
% Eclipse 6.0 program
% util__pre_precedence.pl
% undoing the operators precedence of the solver prior to compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%'necessary' in debug mode only (when recompiling several times)
% interferences between '->' in prolog and '->' in  sdl when reloading (possible interference with other operators
%  so they are also included below)
% also the default handler for dynamic (see dynamic +SpecList) does not seem to be working.
%  so we only declare dynamic predicate once at first loading. And retract them all at all other time.
abolish_list_op([], _).
abolish_list_op([O|OL], Mode) :-
	abolish_op(O, Mode),    %deprecated
	abolish_list_op(OL, Mode).


:- current_op(80, xfy, xor) ->
	(abolish_list_op([-, +, not], fy),
	 abolish_list_op([**, abs, *, /, mod, rem, +, -, &], yfx),
	 abolish_list_op([and, and_then, or, or_else, xor], xfy),
	 abolish_list_op([=, <>, <, >, <=, >=], xfx)
	)
	;
	true
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%