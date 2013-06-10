***********************************************************
PTC Solver Version 1.5.3
February 2005, Christophe Meudec, meudecc@itcarlow.ie
***********************************************************

This file contains last minute information and additional
helpful information about the PTC Solver.

TABLE OF CONTENTS
===========================================================

  1. WHERE TO FIND INFORMATION
  2. WHAT'S NEW IN THIS RELEASE
  3. INSTALLATION DIRECTORY OVERVIEW
  4. DOCUMENTATION
  5. KNOWN ISSUES
  6. BUG REPORT
  7. RELEASES HISTORY
  8. CONTACT


1. WHERE TO FIND INFORMATION
===========================================================

Apart from this file, ptc_readme.txt, the main source of
information on the PTC Solver is its user manual given in
the file ptc_user_manual.pdf .

The ATGen project website,
http://glasnost.itcarlow.ie/~meudecc/atgen/ , is the source for
latest minute information on the PTC solver, including news,
upgrades, patches, applications and session examples. 

The author and contact person for the PTC solver is
Christophe Meudec meudecc@itcarlow.ie

2. WHAT'S NEW IN THIS RELEASE
===========================================================
Version 1.5.3 is a minor update of the ptc_solver. The conversion 
of integral numerical values into reals has been improved.

Version 1.5.2 is a minor update of the ptc_solver. The pure 
version of the or_else constraint have been improved.

Version 1.5.1 is a minor update of the ptc_solver. The reif and 
pure versions of the or and or_else constraints have been improved.

Version 1.5 is a major update of the ptc_solver. Many issues 
relating to mixed constraints have been re-addressed. 

Version 1.4 is a major update of the PTC solver. Mixed constraint
arithmetic handling has been overhauled. The documentation has been
improved.

Version 1.3.6 is a minor update of the PTC solver. The bitwise
xor constraint has been fixed. The parsing speed improvement 
implemented in version 1.3.3 has been reversed. The starting point 
for numbering enumeration literals can now be specified by the user.

Version 1.3.5 is a minor update of the PTC solver. The solver
has been ported to ECLiPSe 5.6.

Version 1.3.4 is a minor update of the PTC solver. A bug in
the handling of the first and last value of the float type
has been corrected.

Version 1.3.3 is a minor update of the PTC solver. The parsing
speed of very large constraints has been improved.

Version 1.3.2 is a minor update of the PTC solver. A new
constraint 'cmp' has been added.

Version 1.3.1 is a minor update of the PTC solver. A bug in
the modulo constraint has been corrected. A bug in the rem
constraint has also been corrected. Labeling of integers
has changed.

Version 1.3 is a major update of the PTC solver. New predicates
for bitwise negation, or, and and xor as well as left and right
shifting have been added.

Version 1.2.3 is a minor update of the PTC solver. The pure
'or' constraint has been improved. And a new constraint is
available.

Version 1.2.2 is a patch release of the PTC solver. The /
(division operator) was broken.

Version 1.2.1 is a patch release of the PTC solver. The pure
'or' constraint was broken.

Version 1.2 is a major update of the PTC solver. Two versions
of the 'or' constraint are now provided.

Version 1.1.1 is a minor update of the PTC solver. Improvement
in the labeling of non-linear real constraints.

Version 1.1 is a patch release of the PTC solver. A major
problem with equality between enumeration expressions has
been fixed. 

Version 1.0.1 is a minor update of the PTC solver. Boolean
variable should now be labelled as enumeration variables.

Version 1.0.0 is the first public release of the PTC Solver.
It is a fully functional constraints solver over path
traversal conditions. See the user manual in the
ptc_user_manual.pdf file for more details. 

3. INSTALLATION OVERVIEW
===========================================================

You should have a working version of ECLiPSe 5.8 installed
on your machine before proceeding further. ECLiPSe can be
obtained from http://www.icparc.ic.ac.uk/eclipse/ .

The PTC Solver installation zip file should be extracted in
your ECLiPSe installation directory. The PTC Solver installs
itself as a public library of ECLiPSe in the lib_public
subdirectory of ECLiPSe.

The released PTC Solver zip file contains the following files:


doc\ptc_embed_sessions.cpp  %Examples of sessions for PTC solver embedding
doc\ptc_embed_solver.cpp    %Auxillary C functions for PTC solver embedding
doc\ptc_license.txt         %PTC Solver user license
doc\ptc_readme.txt          %This file
doc\ptc_user_manual.pdf     %The PTC Solver user manual
lib_public\ptc_array.eco           %Solver library to deal with array types
lib_public\ptc_enum.eco            %Solver library to deal with enumeration 
types
lib_public\ptc_labeling.eco        %Solver library for labeling strategies      
  
lib_public\ptc_record.eco          %Solver library to deal with record types
lib_public\ptc_solver.eco          %The PTC Solver interface 


4. DOCUMENTATION
===========================================================  

The documentation for the PTC Solver is contained in the
ptc_user_manual.pdf file. 

5. KNOWN ISSUES
===========================================================

The solver limitations are described in the PTC Solver user
manual.

6. BUG REPORT
===========================================================  

You can send a bug report to the contact person given below.
Make sure to give as many details as possible including:
PTC Solver version, platform, ECLiPSE version, interfacing
method (Prolog, C/C++ ...), script of the constraints
posted, description of the problem, full error messages issued
(including provenance: OS, Interface, ECLiPSe or PTC Solver).

Your problem will be addressed as soon as possible.

7. RELEASES HISTORY
===========================================================  
***** Version 1.5.3 *****
February 2005 (internal release only)
  Minor update
   - In the very specific case of the conversion of an integral
     numerical value into a real, numeric values outside of the 
     range of integer variables defined by the PTC solver are 
     allowed. Note that limitations on the range of integer 
     expressions and variables remain.  

***** Version 1.5.2 *****
January 2005 (internal release only)
  Minor update
   - The pure version of the or_else constraint delays less
     often. This should improve efficiency.
   - The solver now works with ECLiPSe 5.8

***** Version 1.5.1 *****
April 2004 (internal release only)
  Minor update
   - The reif and the pure version of the or and or_else 
     constraints delay less often. This should improve efficiency.
   - The previous efficiency difference between the 'pure' and 'choice'
     versions of the or and or_else constraint should now be negligeable.
     Mentions of it in the user manual have been removed.

***** Version 1.5 *****
February 2004 (internal release only)
  Major update
   - Mixed arithmetic constraints has been completely overhauled.
     The conversion constraints between integer and real was
     incomplete and could lead to wrong results. Implicit type
     conversions are now working properly (in a truncate C fashion
     for reals to integers). In general all these constraints are now 
     much tighter.
   - A new constraint 'eq_cast' has been introduced to mimic 
     assignments in C and allow implicit type conversion to take 
     place from a real to an int (e.g. in C, i = 7.9, i becomes 7 
     while in i == 7.9 is actually false).
   - A new flag for customising type conversions from real to int
     is available. The default is the C convention of always 
     truncating towards 0. The Ada convention of rounding to 
     the nearest integer is also available. 
   - Labeling of real numbers has been improved. It was sometimes
     attempting sampling outside of the allowed range. Variable
     ordering has also been improved.

***** Version 1.4 *****
October 2003 (internal release only)
  Major update
   - Mixed arithmetic constraints has been overhauled. It now
     behaves in a C fashion since other languages usually requires
     explicit type conversion and disallow mixed arithmetic 
     expressions. Basically, integers involved in mixed arithmetic 
     constraints are handled as reals. Whenever a real variable is
     made equal to an integer variable the real value is truncated 
     rather than rounded. See user manual. 
   - The previously undocumented 'conversion' predicate has been 
     documented
   - The div operator in the documentation has been removed. / 
     performs integer division whenever both its operands are 
     integers otherwise floating point division is performed.
   - Labeling tips have been included in the user manual.

***** Version 1.3.6 *****
September 2003 (internal release only)
  Minor update
   - The bitwise 'xor' constraint now behaves properly. It was 
     previously behaving as the 'or' constraint.
   - The parsing improvement introduced in version 1.3.3 has been 
     reversed. The improvement was causing problems for updated 
     records or arrays containing arithmetic expressions (e.g.
     ptc_solver__sdl(M_out = up_arr(M, [0], 32+10)) did not work.
     Further work is necessary in this area, and the issue will
     be addressed again in the future.
   - A new flag for customising the enumeration start of enumeration 
     literals is available. The default numbering starts at 1. Before
     declaring a new enumeration type, the predicate 
     ptc_solver__set_flag(enumeration_start, Value) can be called to 
     change the default value. Note that Value must be a ground integer
     value. If wished, the flag can be set at different values for 
     different types.

***** Version 1.3.5 *****
June 2003 (internal release only)
  Minor update
   - The solver has been ported to ECLiPSe 5.6. The solver will not
     work with earlier versions ECLiPSe.

***** Version 1.3.4 *****
June 2003 (internal release only)
  Minor update
   - A bug in the handling of the first and last value of the float
     type has been corrected. This bug made the conversion constraint
     fail under some circumstances. 

***** Version 1.3.3 *****
May 2003 (internal release only)
  Minor update
   - The parsing speed of very large constraints has been improved.
     The speed up obtained is hard to measure. On short constraints
     no significant improvement can be detected. On very large
     constraints (containing thousands of atomic constraints) speed
     ups of over 40% have been measured.

***** Version 1.3.2 *****
February 2003
  Minor update
   - A new constraint 'cmp' has been added. See user manual.
   
***** Version 1.3.1 *****
September 2002
  Minor update
   - A bug in the modulo constraint has been corrected. It was sometime
     giving an error message.
   - A bug in the rem constraint has been corrected. It was sometime
     giving an error message.
   - Labeling of integers has changed.  

***** Version 1.3 *****
September 2002
  Major update
   - New bitwise constraints added. They work on decimal numbers, and the
     encoding length must be indicated (8, 16, 32 or 64 bits) as well as
     the encoding scheme (unsigned or signed (2's complement)). See user
     manual for details and limitations.
   - Two new constraints for left and right shifting have been added. 

***** Version 1.2.3 *****
May 2002
  Minor update
   - The 'pure' version of the 'or' constraint has been improved in certain
     circumstances. If any of the operands is fully known (i.e. has no free
     variables) then the constraint will not delay. Previously, both operands
     had to be fully known
   - A new constraint 'reif' is available. It allows to reify constraints
     (i.e. to reflect or direct the outcome of a constraint via a 0/1 integer
      variable). See user manual for details.
   - A new predicate, ptc_solver__version/1, allows to query the solver for
     its version number.   

***** Version 1.2.2 *****
April 2002
  Minor update
   - The / operator was not working properly whenever both operands were
     variables.

***** Version 1.2.1 *****
December 2001
  Minor update
   - The 'pure' version of the 'or' constraint was not working properly since
     version 1.0.0 whenever arrays or records where involved. This is now
     fixed but this new version is much slower than before.
   - The 'mod' operator was giving the wrong result whenever its arguments were
     of different sign and the result was supposed to be zero. This is now
     patched.
   - The 'pure' version of the 'or else' constraint behaved as the 'or'
     constraint in certain circumstances.

***** Version 1.2 *****
December 2001
  Major update
   - Two versions of the 'or' constraint are now provided : a 'pure' version
     similar in all respects to previous versions of PTC solver release. You
     should keep using this one if you are happy with it (default version).
     A 'choice' version which is much more efficient but generates multiple
     solutions via backtacking (see user manual for details).
   - Additional ptc_solver__set_flag/2 predicate to control optional aspects
     of the solver.
   - The 'and' constraint is now properly random.
   - Four additional predicates to deal with floats as rationals especially
     useful for embedding the ptc solver in a non Prolog environment.
   - Major documentation improvements (issues regarding overflowing [properly
     this time], issues on floating point number precision etc.)  
   - Minor documentation corrections.
   
***** Version 1.1.1 *****
November 2001
  Minor update
   - Labeling of non-linear constraints over floats has been improved.
   - Various documentation corrections.
   - Various documentation improvements (issues regarding overflowing,
     priorities of logical connectors).
   - The default range for integers has been widened to -65535 ... +65535.

***** Version 1.1 *****
October 2001
  Major update.
   - Bug in equality between enumeration expressions has been fixed.
   - A bug in the 'or' constraint has been fixed.
   - Various documentation corrections (e.g. the predicates
     ptc_solver__number/2 and ptc_solver__constant/2 are not provided
     as was suggested by the documentation).
   - Additional ptc_solver__match_variable/2 predicate useful for
     embedding.

*****Version 1.0.1*****
May 2001
  Minor update.
   - Internally Booleans are now treated as enumeration
   variables. The only change at the interface level is that
   the labelling of Boolean variables should be performed
   using the enumeration variables labelling predicate.
   - Addition of the Boolean logical operators or_else and and_then. 
   - Various documentation corrections (e.g. sqr and odd
   functions are not provided as was suggested by the documentation,
   and aggregates were all wrongly documented)
   - Various small internal improvements and simplifications.

*****Version 1.0.0*****
March 2001
  Initial release


8. CONTACT
===========================================================  

Dr Christophe Meudec
Institute of Technology Carlow
CPM Department
Kilkenny Road
Carlow
Ireland

meudecc@itcarlow.ie
Tel +353 (0)503 76266
Fax +353 (0)503 70517

http://glasnost.itcarlow.ie/~meudecc/

========================== END ============================