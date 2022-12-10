%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec
% Eclipse 7.0 program
% undoing the operators precedence of the solver prior to compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%'necessary' in debug mode only (when recompiling several times)
% interferences between '->' in prolog and '->' in  sdl when reloading (possible interference with other operators
%  so they are also included below)
% also the default handler for dynamic (see dynamic +SpecList) does not seem to be working.
%  so we only declare dynamic predicate once at first loading. And retract them all at all other time.
:- current_op(80, xfy, xor) ->
		(export op(0, fy, [-, +, not]),
	 	 export op(0, yfx, [**, abs, *, /, mod, rem, +, -, &]),
	 	 export op(0, xfy, [and, and_then, or, or_else, xor]),
	 	 export op(0, xfx, [=, <>, <, >, <=, >=])
		)
	;
		true
	. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%