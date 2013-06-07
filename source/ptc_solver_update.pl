%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Christophe Meudec - started 09/02/01
% ECLiPSe 5.8 program
% ptc_update.pl
% basic script for the recompilation of the ptc solver modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%IMPORTANT: in ptc_solver you should do lib() of the modules rather than compile them there and then
%IMPORTANT: therefore the code should be changed prior to execution of new_version
%IMPORTANT: change  the paths below
%IMPORTANT: change the version number in ptc_solver.pl
%IMPORTANT: since Eclipse 5.7 some warnings are issued during new_version, I think they can be ignored (over zealous module checking?)  
:- lib(fcompile).
new_version :-  %have you read the IMPORTANT note above?
        set_flag(debug_compile, off),                           %for efficiency and privacy purposes
        set_flag(cwd, "//D/bck ATGen/ptc solver released/1.5.3/source/"), %location of the source files
        get_flag(cwd, CWD),
	append_strings(CWD, "../lib_public", Output_dir),     %location of ECLiPSe installation
	fcompile(ptc_array, [outdir:Output_dir]),	        %ptc_array module
	fcompile(ptc_enum, [outdir:Output_dir]),                %ptc_enum module
        fcompile(ptc_record, [outdir:Output_dir]),              %ptc_record module
        fcompile(ptc_labeling, [outdir:Output_dir]),            %ptc_labeling module
        fcompile(ptc_solver, [outdir:Output_dir]).              %ptc_solver

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
