

/*#####################################################################################*/
/*                                 PRINT_VARS                                          */
/*
Produces a nice output of variables and their labels. Like proc contents but just the 
variable names.
%print_vars(sashelp.cars);
*/
%macro print_vars(dataset_name);
title Variables in &dataset_name ;
ods select Variables;
proc contents data=&dataset_name ;
run;
ods select default;
title;
%mend;


/*#####################################################################################*/
/*                                   LIST_VARS                                        */
/*
Returns a list of variables in a particular dataset. Useful if you are trying to 
manipulate the contents of a dataset, especially if used with %remove_word , %first_word, 
etc.

Optional extra: Set col=1 to also output the variables as a column. Handy if you want to
copy and paste variable names into Excel. 
%put Here are the variables in cars: %list_vars(sashelp.cars);
*/

%macro list_vars(dsn, col=0);
 %local varlist dsid i;

 %let dsid = %sysfunc(open(&dsn));

 %if &dsid %then %do;
 %do i=1 %to %sysfunc(attrn(&dsid,nvars)); 
 %if &col=1 %then %put %sysfunc(varname(&dsid,&i));
 %let varlist=&varlist %sysfunc(varname(&dsid,&i)); 
 %end;

 %let dsid = %sysfunc(close(&dsid));
 %end;
 &varlist 
%mend list_vars;

/*#####################################################################################*/
/*                                 VAR_TYPE                                            */
/*
This returns N or C depending on the type of variable involved.    
It is useful for processing a series of datasets where one variable changes type from one year to the next.
*/
%macro Var_Type(ds,var);
%local rc dsid result;
%let dsid=%sysfunc(open(&ds));
%if &dsid %then
   %do;
      %let result=%sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,&var))));
   %end;
%else %let result=ERR_&var;
  %let rc=%sysfunc(close(&dsid));
  &result
%mend Var_Type;





/*#####################################################################################*/
/*                                 VAR_LENGTH                                          */
/*
This returns the length of the variable. Like Var_type, it is useful if you are processing
many years of the same dataset which you later hope to combine. 
*/
%macro Var_Length(ds,var);
%local rc dsid result;
%let dsid=%sysfunc(open(&ds));
%if &dsid %then
   %do;
      %let result=%sysfunc(varlen(&dsid,
                       %sysfunc(varnum
                       (&dsid,&var))));
   %end;
%else %let result=ERR_&var;
  %let rc=%sysfunc(close(&dsid));
  &result
%mend ;


/*#####################################################################################*/
/*                                 VAR_EXIST                                          */
/*
This returns 1 if a variable exists in a dataset, and 0 otherwise. Useful if you are 
processing a range of dataset and new variables appear in most recent versions. 
*/
%macro Var_Exist(ds,var);
	%local rc dsid result;
	%let dsid=%sysfunc(open(&ds));
	%if %sysfunc(varnum(&dsid,&var)) > 0 %then %do;
		%let result=1;
		%put NOTE: Variable &var exists in &ds;
	%end;
	%else %do;
		%let result=0;
		%put NOTE: Variable &var does not exist in &ds;
	%end;
	%let rc=%sysfunc(close(&dsid));
	&result
%mend ;



/*#####################################################################################*/
/*                                 RENAME_VAR                                          */
/*
This renames a variable in a dataset. Uses proc datasets so is more efficient than 
using a stand-alone data step.

data something;
	set sashelp.cars;
run;
%rename_var(something , old_var=Make , new_var = Make_of_car );
*/
%macro rename_var(ds , old_var= , new_var=);
%if %Dataset_Exist(&ds.) = 0 %then %do;
	%put Error: Dataset &ds. does not exist.;
	%abort;
%end;
PROC DATASETS LIBRARY=work nolist;
MODIFY &ds ;
RENAME &old_var=&new_var;
RUN;
%mend;


/*#####################################################################################*/
/*                                   MAX_LENGTH                                        */
/*
Tells you what the longest length is for a particular variable in a dataset. Useful if
you are deciding to trim lengths to save memory or increase efficiency.

%max_length(sashelp.cars , model);
*/
%macro max_length(dataset_name , variable_name);
%local Largest_Variable_Length;
proc sql noprint;
select max(length(&variable_name)) into :Largest_Variable_Length from &dataset_name ;
quit;
%put Largest length of &variable_name: &Largest_Variable_Length;
%mend;
