/* Bundle t005 — %PF, the abbreviated PROC FREQ macro from brendan_macros.sas
   Also exercises %is_blank and dictionary.titles / PROC SQL.
   Run against sashelp.cars, using the documented example calls. */

%macro is_blank(param);
  %sysevalf(%superq(param)=,boolean)
%mend;

%macro PF(dataset , variables);
%local Title_Var title_exists;
proc sql noprint;
select text  into :Title_Var from Dictionary.Titles;
quit;
%put title_var is &Title_Var;
%local pf_no_statement;
%let pf_no_statement = ;
%if %sysfunc(find(&variables , *)) ge 1 %then %do;
	%let pf_no_statement = / norow nocol nopercent;
%end;
%let title_exists = 1;
%if %is_blank(&Title_Var) %then %do;
%let title_exists = 0;
title &dataset ;
%end;
%put title_exists is &title_exists;
proc freq data= &dataset ;
	table &variables &pf_no_statement;
run;
%if &title_exists = 0 %then %do;
title;
%end;
%mend;

/* Documented examples from the macro header */
%pf(sashelp.cars , type);
%pf(sashelp.cars , origin*type);
