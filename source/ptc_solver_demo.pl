:- use_module([ptc_solver]).	            %the solver is compiled and imported
example(A, B) :-
    ptc_solver__clean_up,                   %to start in a clean environment
    ptc_solver__default_declarations('ILP32'),       %solver initialisations
    ptc_solver__variable([A, B], integer),  %A and B are declared as integers
    ptc_solver__sdl(A>45 and B-5=A*A),      %constraints are imposed
    ptc_solver__label_integers([A,B]).      %a unique random solution is generated