%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 09/02/01 - updated 12/06/13
% ECLiPSe 7.0 program
% ptc_update.pl
% basic script for the recompilation of the ptc solver modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%IMPORTANT: compile this file in a new ECLiPSe instance (you may get extra warnings otherwise)
%IMPORTANT: remove the existing ptc_*.eco files from lib_public folder in your ECLiPSe installation: otherwise fcompile gets confused
%IMPORTANT: in ptc_solver.pl you should do lib() of the modules rather than compile them there and then
%IMPORTANT: therefore the code should be changed prior to execution of new_version
%IMPORTANT: change the source path below
%IMPORTANT: change the version number ptc_solver__version("1.7") in ptc_solver.pl
%IMPORTANT: since Eclipse 5.7 some warnings are issued during new_version, I think they can be ignored (over zealous module checking?)
:- lib(fcompile).
new_version :-  %have you read the IMPORTANT note above?
        set_flag(debug_compile, off),                           %for efficiency and privacy purposes
        set_flag(cwd, "//C/Users/Chris2/GoogleDrive/ATGen/ptcSolver/source/"), %location of the source files
        get_flag(cwd, CWD),
	append_strings(CWD, "../lib_public", Output_dir),       %target location of pre-compiled module output
	fcompile(ptc_array, [outdir:Output_dir]),	        %ptc_array module
	fcompile(ptc_enum, [outdir:Output_dir]),                %ptc_enum module
        fcompile(ptc_record, [outdir:Output_dir]),              %ptc_record module
        fcompile(ptc_labeling, [outdir:Output_dir]),            %ptc_labeling module
        fcompile(ptc_solver, [outdir:Output_dir]).              %ptc_solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%