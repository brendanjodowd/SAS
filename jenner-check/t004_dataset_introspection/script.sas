/* Bundle t004 — dataset-introspection macros from brendan_macros.sas
   Macros: %list_vars %var_exist %var_type %var_length
   Run against sashelp.cars, using the documented example calls. */

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

%macro Var_Exist(ds,var);
	%local rc dsid result;
	%let dsid=%sysfunc(open(&ds));
	%if %sysfunc(varnum(&dsid,&var)) > 0 %then %do;
		%let result=1;
	%end;
	%else %do;
		%let result=0;
	%end;
	%let rc=%sysfunc(close(&dsid));
	&result
%mend ;

%macro Var_Type(ds,var);
%local rc dsid result;
%let dsid=%sysfunc(open(&ds));
%if &dsid %then
   %do;
      %let result=%sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,&var))));
   %end;
%else %do;
	%let result=ERR_&var;
%end;
  %let rc=%sysfunc(close(&dsid));
  &result
%mend Var_Type;

%macro Var_Length(ds,var);
%local rc dsid result;
%let dsid=%sysfunc(open(&ds));
%if &dsid %then
   %do;
      %let result=%sysfunc(varlen(&dsid, %sysfunc(varnum(&dsid,&var))));
   %end;
%else %let result=ERR_&var;
  %let rc=%sysfunc(close(&dsid));
  &result
%mend ;

/* Documented examples from the macro headers */
%put LIST_VARS: %list_vars(sashelp.cars);
%put VAR_EXIST_model: %var_exist(sashelp.cars , model);
%put VAR_EXIST_sandwich: %var_exist(sashelp.cars , sandwich);
%put VAR_TYPE_origin: %var_type(sashelp.cars , origin);
%put VAR_LENGTH_Origin: %Var_Length(sashelp.cars , Origin);
