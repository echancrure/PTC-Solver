// Christophe Meudec
// Visual C++
// started 07/09/00
// Embedding ATGen's ptc_solver from C++ : demo
// 
#include <eclipseclass.h> //necessary for embedding ECLiPSe into C++
#include <stdio.h>

FILE *log; // log file for output of messages during a session

// A list of couple items composed of the variable name and its value
struct couple {
	char name[30];
	EC_word value;
    struct couple *next;
};


// adds a constraint in string format to the current store of constraints
// returns EC_succeed on success EC_fail otherwise
int submit_string(char *s)
{
	post_goal(term(EC_functor("ptc_solver__submit_string", 1), s));
    return EC_resume();
}//submit_string

// identical to submit_string(s) except that a checked message is also printed
int submit_string_checked(char *s)
{
	if (submit_string(s) == EC_succeed) 
		{fprintf(log, "CHECK TRUE: %s\n", s);
		 return EC_succeed;
		}
	else {fprintf(log, "CHECK FALSE: %s\n", s);
		  return EC_fail;
		 }
}//submit_string_checked

// the ptc_solver runs quietly : no error nor warning messages
void quiet()
{
	printf("Warning: ECLiPSe is running in quiet mode\n");
	post_goal(term(EC_functor("set_stream", 2), EC_atom("error"), EC_atom("null")));
	post_goal(term(EC_functor("set_stream", 2), EC_atom("warning_output"), EC_atom("null")));
	EC_resume();
}


// decompose the Prolog list of (name, value) to the C list of (name, value)
struct couple *decompose(EC_word Var_list) 
{
	struct couple *new_couple ;
    EC_word tail, Acouple ;
	EC_word atom1;
	EC_atom word1;
	struct couple *start, *couple_list = NULL;

    start = couple_list; 
	for (int i = 0; (EC_succeed == Var_list.is_list(Acouple, Var_list)); i++) //the list has a head, Acouple, and a tail Var_ist.
	{new_couple = (struct couple *) malloc (sizeof(struct couple));
     if (!new_couple) {printf("Out of memory.\n"); exit(1);};
//retrieving the variable name as a string
	 Acouple.arg(1, atom1);
	 atom1.is_atom(&word1);
	 strcpy(new_couple->name, word1.name());
//retrieving the variable value	 
	 Acouple.arg(2, new_couple->value);
//end of the list
	 new_couple->next = NULL;

	 if (!start) 
		start = new_couple;		//first couple
	 else 
		couple_list->next = new_couple;
	 
     couple_list = new_couple;
	} //for
	return start;
}


//display the C list of couples (name, value)
//to do : other types than integer, floating point 
void display(struct couple *list)
{
double d;
long i;
EC_atom did;
	fprintf(log, "*** Displaying current values\n");
	while (list)
	{fprintf(log, "variable %s\t", list->name);
	 if (EC_succeed == (list->value).is_long(&i))  //word-sized integer
	     fprintf(log, "is the integer %d\n", i);
	 else if (EC_succeed == (list->value).is_double(&d)) //floating point number
		 fprintf(log, "is the float %f\n", d);
     else
		 fprintf(log, "is not ground\n");
     list = list->next;
	}//while
	fprintf(log, "*** End of display\n");
}// display


// retrieve and display all the current variables
void display_all_vars()
{   
	struct couple *ref_list; //C list of couples (name, value) for displaying results
	EC_ref Var_list; //Prolog list of couples (name, value) for retrieving result

	post_goal(term(EC_functor("ptc_solver__get_all_variables", 1), Var_list));
	EC_resume(); 

	ref_list = decompose(Var_list); //returns the C list of variables couples (name, value)
	display(ref_list);
}//display_all_vars


// retrieve the value of a single instantiated integer variable
// used to check the success or otherwise of a submitted goal with a tell tale variable
// returns -1 if the variable is not instantiated
// returns -2 if teh variable does not exist (out of scope maybe)  
int check_success(char *s)
{
	EC_ref value;
	long i;

	post_goal(term(EC_functor("ptc_solver__get_single_variable", 2), EC_word(s), value));
	EC_resume();		//the above will always succeed and return -2 if the variable is unknown 
	if (EC_succeed == EC_word(value).is_long(&i))		//word-sized integer
		return i;	//a tell tale variable should return 0 for failure, 1 for success
	else return -1;	//not instantiated;
}//check_success


#include "sessions.cpp"

void main()
{  	
	// initialisation of ECLiPSe
	ec_set_option_ptr(EC_OPTION_DEFAULT_MODULE, "ptc_solver");
	ec_init();
    
	//quiet();

	// loading of the solver
	post_goal(term(EC_functor("lib", 1), "ptc_solver"));
	printf("ECLiPSe and the PTC solver are being loaded ...\n");
	if (EC_resume()==EC_succeed) printf("The PTC Solver has been loaded\n");
	else printf("Error: The PTC Solver did not load properly\n");

    // initialisation of the solver 
    submit_string("ptc_solver__clean_up, ptc_solver__default_declarations");
	printf("The PTC Solver is initialised\n");
	
	//output file
	log = fopen("latest run.log", "w");
	
	printf("The PTC Solver is running\n");

	//calling a demonstration session
	session3();

	printf("The PTC Solver has finished\n");

	//Unloading ECLiPSe and tidying up 
	ec_cleanup();

	printf("ECLiPSe and the PTC Solver have been unloaded\n\n");

	fclose(log);
}//main
