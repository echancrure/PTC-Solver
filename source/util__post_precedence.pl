%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec
% Eclipse 7.0 program
% defining the operators precedence of the solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% operator precedence as defined in Ada
% put at the end to avoid interferences with other Prolog operators
% not too sure if xfy or yfx corresponding to (right to left and left to right respectively)
% should be used, right to left seem more efficient but is different from Ada semantics

:- export op(30, fy, [not]).                    %must be fy for 'not not a' expressions
:- export op(30, yfx, [**, abs]).               %must be left to right for 'a** b ** c' expressions
:- export op(40, yfx, [*, /, mod, rem]).        %must be left to right for a*b mod c
:- export op(50, fy, [+, -]).                   %must be fy for '- -a' expressions
:- export op(60, yfx, [+, -, &]).                  %could be xfy, does not matter
:- export op(70, xfx, [=, <>, <, >, <=, >=]).   %can be anything, relations never repeated
:- export op(80, xfy, [and, and_then, or, or_else, xor]).          %xfy for speed up

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
