***********************************************************
PTC Solver Version 2.1.1
June 2023, Dr Christophe Meudec, echancrure@gmail.com
***********************************************************

A Path Traversal Conditions solver to analyse imperative programs. It can solve constraints expressed using traditional programming language constructs. It is useful for symbolic execution, static analysis of code etc.

TABLE OF CONTENTS

  1. WHERE TO FIND INFORMATION
  2. RELEASES OVERVIEW
  3. INSTALLATION DIRECTORY OVERVIEW
  4. DOCUMENTATION
  5. KNOWN ISSUES
  6. BUG REPORT
  7. RELEASES HISTORY
  8. CONTACT

***********************************************************

1. WHERE TO FIND INFORMATION

Apart from this file the main source of information on the PTC Solver is its user manual given in the file ptc_user_manual.pdf in the doc folder.

The author and contact person for the PTC solver is Dr Christophe Meudec echancrure@gmail.com, South East Technological University, Ireland.

An informal brief introduction is available at See https://docs.google.com/document/d/121YIr-f-EwJ7aLwtxQoahjZ2XtOTJK6sdLUz0-SA2ig/edit?usp=sharing for further information where comments can be made.

The repository master branch contains the latest stable version. The dev branch is work in progress.

2. RELEASES OVERVIEW

The RELEASES HISTORY section further down in this document contains more details.

Version 2.1.1 is a minor update. 

Version 2.1 is a port update. The PTC Solver now requires ECLiPSE 7.1.

Version 2.0.2 is a minor update. The handling of arithmetic expressions should be faster.

Version 2.0.1 is a minor update. The labeling of reals has been modified.

Version 2.0 is a significant overhaul. The PTC Solver internal constraints
over integers and reals have been ported to the more powerful ECLiPSe
IC library.

Version 1.7 is a port update. The PTC Solver is now compatible
with Eclipse 7.0. Labeling of rationals is more thorough.

Version 1.6 is a port update. The PTC Solver is now compatible
with the latest version of Eclipse 6.0. The PTC Solver is now
open source.

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

Version 1.3.3 is a minor update of the PTC solver. The parsing speed of very large constraints has been improved.

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

Version 1.2.1 is a patch release of the PTC solver. The pure 'or' constraint was broken.

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

You should have a working version of ECLiPSe 7.1 (the Constraint Programming System, not the IDE...) installed on your machine before proceeding further. ECLiPSe can be obtained from https://eclipseclp.org/ .

4. DOCUMENTATION

The documentation for the PTC Solver is contained in the doc\ptc_user_manual.pdf file. 

5. KNOWN ISSUES

The solver limitations are described in the doc\ptc_user_manual.pdf file.

6. BUG REPORTS

You can send a bug report to the author.
Make sure to give as many details as possible including:
PTC Solver version, platform, ECLiPSe version, interfacing
method (Prolog, C/C++ ...), script of the constraints
posted, description of the problem, full error messages issued
(including provenance: OS, Interface, ECLiPSe or PTC Solver).

7. REQUESTS

The PTC Solver handles many typical constraints suitable for the analysis of software code. However if you feel it is missing something please contact the author with a request for extension.

8. RELEASES HISTORY

*****Version 2.1.1*****
June 2023
  Minor Update
   - The user manual has been modified:
      - the predicates ptc_solver__real_min/2 and ptc_solver__real_max/2 have been removed from the documentation as they are redundant and were not supported;
      - the predicate ptc_solver__integer_range/3 has been renamed ptc_solver__variable_range/3 as it works both on integer and real variables;
      - some spelling mistakes have been corrected.
   - The predicate ptc_solver__is_real/1 now only succeeds on real variables and fails otherwise.
   - The desired precision for the labelling of floats can be specified using ptc_solver__set_flag(epsilon, Value): to be used with care as described in the user manual.
   - The solver now issues more fatal error (especially when using improper Flag values) rather than ignoring them unless in debug mode. 

*****Version 2.1*****
February 2023
  Port update
   - The PTC Solver now requires ECLiPSE 7.1;
   - A warning is issued if the wrong version of ECLiPSe is used;
   - A new predicate for labelling integer variables using a random method is provided ptc_solver__label_integers/2;
   - Internally, the constraints integer division and remainder are now handled by the IC library: this may, or may not be an improvement.

*****Version 2.0.2*****
January 2023
  Minor update
   - Performance of handling of arithmetic expressions over integers and/or reals should be improved, especially in some circumstances where direct failure could not be detected previously without enumerating the entire domain of integer variables.  

*****Version 2.0.1*****
December 2022
  Minor update
   - The labeling of reals has been improved by generating tighter real intervals in some circumstances. In addition a new version of the ptc_labeling__reals predicate is available which also outputs approximate float solutions suitable for printing;
   - The ptc_readme.txt file has been removed: GitGub README.md is used instead;
   - From this version onwards, pre-compiled solver modules will no longer be released as they served little purpose: the folder lib_public has been removed.

*****Version 2.0*****
December 2022
  Significant overhaul
   - The PTC Solver internal constraints over integers and reals
     have been ported to the more powerful ECLiPSe IC library. 
     This has many important consequences, including:
       - As rationals are no longer used to represent reals, all 
         predicates and constraints relating to rationals have 
         been removed from the solver: 
           - ptc_solver__is_rational/1
           - ptc_solver__rational_to_decimal/2
           - ptc_solver__numerator/2
           - ptc_solver__denominator/2
       - This new version of the PTC Solver is estimated to be 
         on average at least 10 times faster than the previous versions.
         In particular labelling performance has been greatly improved;
       - Labelling is less random than in previous versions: the PTC Solver
         now often returns identical solutions on different runs;
       - The PTC Solver runtime performance variability has been at 
         least halved: the standard deviation is down from about 0.8 of 
         the mean to about 0.3;
       - The range of integers that can be safely handled
         has been increased by at least 2 orders of magnitude. This is
         work in progress so further widening may occur.
         For now, the integers range has been widened from 
         -65 535 .. +65 535 to -10 000 000 .. +10 000 000;
       - The range of reals has been slightly widened from
         -0.37*10^12 .. 0.37*10^12 to -10*10^12 .. 10*10^12. This is work
         in progress and further widening may happen;
       - Ada rounding constraints have been removed for now. The 
         float_to_int_convention flag has been deprecated. The PTC
         Solver now uses the C convention of truncating when rounding. 

*****Version 1.7*****
June 2022
  Major update
   - The solver has been ported to ECLiPSe 7.0;
   - The labeling of rationals is more thorough and will take  longer in infeasible cases, it is still unsastifactory  in many instances.

*****Version 1.6*****
June 2013
  Major update
   - The solver has been ported to ECLiPSe 6.0;
   - The solver is now open source.

*****Version 1.5.3*****
February 2005 (internal release only)
  Minor update
   - In the very specific case of the conversion of an integral
     numerical value into a real, numeric values outside of the 
     range of integer variables defined by the PTC solver are 
     allowed. Note that limitations on the range of integer 
     expressions and variables remain.  

*****Version 1.5.2*****
January 2005 (internal release only)
  Minor update
   - The pure version of the or_else constraint delays less
     often. This should improve efficiency.
   - The solver now works with ECLiPSe 5.8

*****Version 1.5.1*****
April 2004 (internal release only)
  Minor update
   - The reif and the pure version of the or and or_else 
     constraints delay less often. This should improve efficiency.
   - The previous efficiency difference between the 'pure' and 'choice'
     versions of the or and or_else constraint should now be negligeable.
     Mentions of it in the user manual have been removed.

*****Version 1.5*****
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

*****Version 1.4*****
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

*****Version 1.3.6*****
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

*****Version 1.3.5*****
June 2003 (internal release only)
  Minor update
   - The solver has been ported to ECLiPSe 5.6. The solver will not
     work with earlier versions ECLiPSe.

*****Version 1.3.4*****
June 2003 (internal release only)
  Minor update
   - A bug in the handling of the first and last value of the float
     type has been corrected. This bug made the conversion constraint
     fail under some circumstances. 

*****Version 1.3.3*****
May 2003 (internal release only)
  Minor update
   - The parsing speed of very large constraints has been improved.
     The speed up obtained is hard to measure. On short constraints
     no significant improvement can be detected. On very large
     constraints (containing thousands of atomic constraints) speed
     ups of over 40% have been measured.

*****Version 1.3.2*****
February 2003
  Minor update
   - A new constraint 'cmp' has been added. See user manual.
   
*****Version 1.3.1*****
September 2002
  Minor update
   - A bug in the modulo constraint has been corrected. It was sometime
     giving an error message.
   - A bug in the rem constraint has been corrected. It was sometime
     giving an error message.
   - Labeling of integers has changed.  

*****Version 1.3*****
September 2002
  Major update
   - New bitwise constraints added. They work on decimal numbers, and the
     encoding length must be indicated (8, 16, 32 or 64 bits) as well as
     the encoding scheme (unsigned or signed (2's complement)). See user
     manual for details and limitations.
   - Two new constraints for left and right shifting have been added. 

*****Version 1.2.3*****
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

*****Version 1.2.2*****
April 2002
  Minor update
   - The / operator was not working properly whenever both operands were
     variables.

*****Version 1.2.1*****
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

*****Version 1.2*****
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
   
*****Version 1.1.1*****
November 2001
  Minor update
   - Labeling of non-linear constraints over floats has been improved.
   - Various documentation corrections.
   - Various documentation improvements (issues regarding overflowing,
     priorities of logical connectors).
   - The default range for integers has been widened to -65535 ... +65535.

*****Version 1.1*****
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

9. CONTACT

Dr Christophe Meudec
South East Technological University
Computing Department
Kilkenny Road
Carlow
Ireland

echancrure@gmail.com

http://www.echancrure.eu/

============================= END ===============================
