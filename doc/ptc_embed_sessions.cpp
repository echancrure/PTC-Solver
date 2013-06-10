//demonstration sessions
void session1()
{
	quiet();
	fprintf(log, "\n___STARTING SESSION 1___\n");

	submit_string_checked("ptc_solver__variable([X, Y], integer)");

	submit_string_checked("ptc_solver__sdl(X>41)");

	submit_string_checked("ptc_solver__sdl(X<56)");

    submit_string("ptc_solver__integer_range(X, Min, Max)");

	submit_string("ptc_solver__variable([Z1, Z2], float)");
    submit_string("ptc_solver__sdl(Z1 = 1.5)");
	submit_string("ptc_solver__sdl(Z2 = Z1/3)");

    submit_string_checked("ptc_solver__sdl(2*X+1=Y)");

	submit_string_checked("ptc_solver__sdl(Y>0)");
				
	display_all_vars();
	
	submit_string("ptc_solver__label_integers([X, Y])");
 
	display_all_vars();

	submit_string("fail"); //display another sample

    display_all_vars();

	fprintf(log, "___END SESSION 1___\n");
}//session1


void session2()
{	
	fprintf(log, "\n___STARTING SESSION 2___\n");

	submit_string_checked("ptc_solver__variable([X, Y], integer)");

	submit_string_checked("ptc_solver__sdl(X>0)");

	submit_string_checked("ptc_solver__sdl(X = 5) ; true");

	submit_string("ptc_solver__label_integers([X, Y]), !");
	
	display_all_vars();

	submit_string("fail, ptc_solver__sdl(not X = 5)");
	submit_string("ptc_solver__label_integers([X, Y]), !");
	display_all_vars();

	fprintf(log, "___END SESSION 2___\n");
}//session2


//Demonstration of search in a control flow graph
//Corresponds roughly to the code:
// if X>2 or Y>2 then blah;		//condition 1
//				 else blah;
// if X<2 then blah;			//condition 2
//        else blah;
// where blah does not change the values of X and Y
// we check the paths in the following order:
//       1 true, 2 true
//       1 true, 2 false
//       1 false, 2 true
//       1 false, 2 false	 
void session3()
{	
	fprintf(log, "___STARTING SESSION 3___\n");
	submit_string("ptc_solver__variable([X, Y], integer)");				//X and Y are declared as integers

    submit_string("S1 = 1, ptc_solver__sdl(X>2 or Y>2) ; S1 = 0");	    //The general format for constraints submission that can backtrack is "Si = 1, Constraint ; Si = 0"
	submit_string("S2 = 1, ptc_solver__sdl(X<2) ; S2 = 0");				//The values of the variables Si indicate if the constraint has been successful (1) or not (0)  
	fprintf(log, "S2 is %i\n", check_success("S2"));						//check_success should return 1
	fprintf(log, "S10 is %i\n", check_success("S10"));						//check_success will return -2 as S10 does not exist	
	submit_string("ptc_solver__label_integers([X, Y]), !");				//A sample is generated for X and Y
	display_all_vars();												//The values of current variables are displayed. Should satisfy: X<2 and Y>2 
	
	submit_string("fail");											//We backtrack one level on purpose: the constraint X<=2 is undone (S2 will now be 0)
	fprintf(log, "S2 is %i\n", check_success("S2"));						//check_success should return 0 for S2
	submit_string("S3 = 1, ptc_solver__sdl(not(X<2)) ; S3 = 0");		 
	submit_string("ptc_solver__label_integers([X, Y]), !");
	display_all_vars();												//Should satisfy: X>2 or (X=2 and Y>2)

	submit_string("fail");											//We backtrack on purpose S3 = 0
	submit_string("fail");											//We backtrack on purpose S1 = 0
	fprintf(log, "S2 is %i\n", check_success("S2"));						//S2 has fallen out of scope: check_success returns -2
	submit_string("S4 = 1, ptc_solver__sdl(not(X>2 or Y>2)) ; S4 = 0"); 
	submit_string("S5 = 1, ptc_solver__sdl(X<2) ; S5 = 0"); 
	submit_string("ptc_solver__label_integers([X, Y]), !");				
	display_all_vars();												//Should satisfy: X<2 and Y<=2

	submit_string("fail");											//S5 = 0
	submit_string("S6 = 1, ptc_solver__sdl(not(X<2)); S6 = 0");		
	submit_string("ptc_solver__label_integers([X, Y]), !");				 
	display_all_vars();												//Should satisfy: X=2 and Y<=2
	
	fprintf(log, "___END SESSION 3___\n");
}//session3


//Demonstration of search in a control flow graph with failure of one of the path
//Corresponds roughly to the code:
// if X<=2 and Y<=2 then blah;
//					else blah;
// if X<=2 then blah;
//         else blah;
// where blah does not change the values of X and Y			 
void session4()
{		
	fprintf(log, "___STARTING SESSION 4___\n");
	submit_string("ptc_solver__variable([X, Y], integer)");			//X and Y are declared as integers

    submit_string("S1 = 1, ptc_solver__sdl(X<=2 and Y<=2) ; S1 = 0");	//The general format for constraints submission that can backtrack is "Si = 1, Constraint ; Si = 0"
	submit_string("S2 = 1, ptc_solver__sdl(X<=2) ; S2 = 0");			//The values of the variables Si indicate if the constraint has been successful (1) or not (0)  
	submit_string("ptc_solver__label_integers([X, Y]), !");				//A sample is generated for X and Y
	display_all_vars();												//The values of current variables are displayed. Should satisfy : X<=2 and Y<=2 
	
	submit_string("fail");											//We backtrack one level on purpose: the constraint X<=2 is undone (S2 will now be 0)
	submit_string("S3 = 1, ptc_solver__sdl(not(X<=2)) ; S3 = 0");		//This should fail as (X<=2 and Y<=2) and X>2 is a contradiction; X>2 could not be added indicated by S3 = 0 
	submit_string("ptc_solver__label_integers([X, Y]), !");
	display_all_vars();												//Should satisfy: X<=2 and Y<=2

	submit_string("fail");											//We backtrack on purpose S1 = 0
	submit_string("S4 = 1, ptc_solver__sdl(not (X<=2 and Y<=2)) ; S4 = 0"); 
	submit_string("S5 = 1, ptc_solver__sdl(X<=2) ; S5 = 0"); 
	submit_string("ptc_solver__label_integers([X, Y]), !");				
	display_all_vars();												//Should satisfy: X<=2 and Y>2

	submit_string("fail");											//S5 = 0
	submit_string("S6 = 1, ptc_solver__sdl(not(X<=2)); S6 = 0");		
	submit_string("ptc_solver__label_integers([X, Y]), !");				 
	display_all_vars();												//Should satisfy: X>2

	fprintf(log, "___END SESSION 4___\n");
}//session4

void session5()
{	
	fprintf(log, "___STARTING SESSION 5___\n");
    submit_string_checked("S1 = 1, X = 5 ; S1 = 0");
	submit_string_checked("S2 = 1, Y = 2 ; S2 = 0, true");
	display_all_vars();
	
	submit_string_checked("fail");
	submit_string_checked("S3 = 1, X = 0 ; S3 = 0");  //this should fail
	display_all_vars();

	submit_string("fail");
	submit_string("fail");
	submit_string_checked("S4 = 1, X = 10 ; S4 = 0");
	submit_string_checked("S5 = 1, Y = 4 ; S5 = 0"); 
	display_all_vars();

	submit_string_checked("fail");
	submit_string_checked("S6 = 1, Y = 5 ; S6 = 0");
	display_all_vars();

	fprintf(log, "___END SESSION 5___\n");
}//session5

void session6()
{		
	fprintf(log, "___STARTING SESSION 6___\n");
	submit_string("ptc_solver__variable([X, Y], integer)");			//X and Y are declared as integers

    submit_string("ptc_solver__sdl(X=2 or X=3)");
	submit_string("ptc_solver__label_integers([X, Y]), !");	
	display_all_vars();

	fprintf(log, "___END SESSION 6___\n");
}//session6