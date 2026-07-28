/* Bundle t006 — duplicate-checking macros from brendan_macros.sas
   Macros: %delete_dataset %check_duplicates %abort_if_duplicates
   Exercises nested PROC SQL aggregation + PROC DATASETS + DATA-step reporting.
   Run against sashelp data, using the documented example calls. */

%macro delete_dataset(list);
%local list;
proc datasets lib=work memtype=data nolist ;
	delete &list;
quit;
run;
%mend;

%macro check_duplicates(dataset , var_name);
%put Dataset: &dataset;
proc sql;
create table num_of_occurrences_ds as select count(Count) as Count, Count as Occurrences  from
	(select count(&var_name) as Count  from &dataset group by &var_name)
	group by Count
;
quit;

data num_of_occurrences_ds;
	set num_of_occurrences_ds nobs=total;
	if total=1 then do;
		if Occurrences = 1 then put "All " Count "unique entries appeared once only" ;
		else put  "All " Count "unique entries appeared on " Occurrences "occasions only" ;
	end;
	else if _n_ <=25 then do;
		if _n_ = 1 then put "Occurrences - Number";
		put Occurrences Count ;
	end;
	else do;
		put "There were more than 25 different frequencies for this variable.";
		stop;
	end;
run;
%delete_dataset(num_of_occurrences_ds);
%mend;

%macro abort_if_duplicates(dataset , var_name);
%local This_should_be_1;
proc sql noprint;
	select max(Count) into  :This_should_be_1 from
	(select count(&var_name) as Count from &dataset group by &var_name)
	;
quit;
%if %eval( &This_should_be_1 > 1) %then %do;
	%put Error: There are some duplicates of &var_name in &dataset;
%end;
%else %do;
	%put Report: There were no duplicates of &var_name in &dataset;
%end;
%mend;

/* Documented examples from the macro headers */
%check_duplicates(sashelp.iris , species);
%abort_if_duplicates(sashelp.iris , species);
