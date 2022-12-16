%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec
% ECLiPSe 7.0 program
% basic script for the recompilation of the ptc solver modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%IMPORTANT: compile this file in a new ECLiPSe instance (you may get extra warnings otherwise)
%IMPORTANT: remove the existing ptc_*.eco files from lib_public folder in your ECLiPSe installation: otherwise fcompile gets confused
%IMPORTANT: in ptc_solver.pl you should use lib() of the modules rather than compile them there and then
%IMPORTANT: therefore your code should be changed prior to execution of new_version
%IMPORTANT: change the source path below
%IMPORTANT: change the version number ptc_solver__version("2") in ptc_solver.pl
%IMPORTANT: since Eclipse 5.7 some warnings are issued regarding non-existing modules during new_version, I think they can be ignored (over zealous module checking?)

new_version :-  %have you read the IMPORTANT note above?
        set_flag(debug_compile, off),                           %for efficiency and privacy purposes
        set_flag(cwd, "//C/Users/Chris2/GoogleDrive/ATGen/ptcSolver/source/"), %location of the source files
        get_flag(cwd, CWD),
	append_strings(CWD, "../lib_public", Output_dir),       %target location of pre-compiled module output
        Options = [outdir:Output_dir, output:eco],
	compile(ptc_array, Options),	      %ptc_array module
	compile(ptc_enum, Options),           %ptc_enum module
        compile(ptc_record, Options),         %ptc_record module
        compile(ptc_labeling, Options),       %ptc_labeling module
        compile(ptc_solver, Options).         %ptc_solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%