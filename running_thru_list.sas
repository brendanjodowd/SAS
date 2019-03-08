/*#####################################################################################*/
/*                                 RUNNING_THRU_LIST                                   */

/*This macro is needed for the output of running_thru_list. You might find it useful
otherwise*/
%macro print_in_mins_seconds(time);
%local time ;
%if %sysevalf(&time < 60 ) %then %do;
	%sysfunc(round(&time , 1)) seconds 
%end;
%else %if %sysevalf(&time < 120 ) %then %do;
	%sysfunc( floor( %sysevalf(&time/60) ) ) minute and %sysfunc(round(%sysfunc(mod(&time ,  60)))) seconds 
%end;
%else %do;
	%sysfunc( floor( %sysevalf(&time/60) ) ) minutes and %sysfunc(round(%sysfunc(mod(&time ,  60)))) seconds 
%end;
%mend;
%put The time is %print_in_mins_seconds(400) ;

/*
This is very useful for processing multiple datasets using the same macro, and as it works it outputs the 
progress and estimated time remaining. It requires a list of things to process, usually datasets, and the 
name of the macro to be applied. Your list can be the names of the datsets themselves, e.g.
SALES_2010 SALES_2011 SALES_2012 etc.
OR
You may just pass the list of years as the list and write your macro in such as way that it creates
the name of the dataset using the year. This way you can define the years of interest as a macro variable
in one part of your SAS program and pass this to each stage of your process. 
E.g. 
%macro my_macro(year);
data SALES_&year;
	set SALES_&year;
run;
%mend;

Here is an example where you can test the macro, but with words rather than datasets

%macro print_the_word(name_of_word);
%put &name_of_word;
%mend;

%running_thru_list(Max the dog likes walking , print_the_word);

*/

%macro running_thru_list(list, macro_name);

%local 
total_in_list_RTL /*number of elements in the list*/
step_counter_RTL /*counting through the list*/
steps_done_RTL /*the above minus 1*/
percentage_per_step_RTL /*100 divided by number of things in list*/
start_time_RTL /*time when nested macro is first applied*/
time_per_step_RTL /*Average length of time taken to apply nested macro*/
remaining_time_RTL /* How long until the process is finished */
finish_time_RTL /* When is the whole process going to be finished*/
total_time_RTL /*How long the whole process actually took*/
time_string_RTL /*A string that describes a duration in minutes and seconds*/
duration_of_this_step_RTL /*I just use this to find out if the process is getting slower or faster*/
;

%let total_in_list_RTL = %sysfunc(countw(&list));
%put number of elements in list is &total_in_list_RTL ; 
%let percentage_per_step_RTL = %sysevalf( 100/ &total_in_list_RTL );

%do step_counter_RTL  = 1 %to &total_in_list_RTL ;
	/* steps done (or completed) is one less than the counter */
	%let steps_done_RTL = %eval(&step_counter_RTL - 1);

	/* If we're on the first step we need to make note of the starting time */
	%if &step_counter_RTL = 1 %then %do; 
		%let start_time_RTL = %sysfunc(datetime());
		%put Applying macro for first time... ;
	%end;

	/*This value is set to the current time. After the macro is run the new current time will be subtracted from it*/
	%let duration_of_this_step_RTL =  %sysfunc(datetime());
	/* Apply the macro. */
	%&macro_name( %scan(&list, &step_counter_RTL) );
	%let duration_of_this_step_RTL = %sysevalf( %sysfunc(datetime()) - &duration_of_this_step_RTL );

	/* Find average time per step and remaining time to finish */
	%let time_per_step_RTL = %sysevalf( ( %sysfunc(datetime()) - &start_time_RTL) /  &step_counter_RTL 	);
	%let remaining_time_RTL = %sysevalf( &time_per_step_RTL * ( &total_in_list_RTL - &step_counter_RTL) );
	%let remaining_time_RTL = %sysfunc(round(  &remaining_time_RTL	, 1)) ;
	%let finish_time_RTL = %sysfunc(  sum( %sysfunc(inputn(%sysfunc(time()), 8.)) , &remaining_time_RTL  )   );

	%put Current time is %sysfunc(time(),time8.0);
	/*%put Current time is %sysfunc( PutN( %sysfunc(datetime()) , Time8 ) ) ;*/
	%put Remaining time is %print_in_mins_seconds(&remaining_time_RTL) , will be finished at %sysfunc( PutN( &finish_time_RTL , Time8 ) ) ;

	%if &step_counter_RTL ~= 1 %then %do; 
		%put Duration of last run was %sysfunc(round( %sysevalf(100*&duration_of_this_step_RTL / &time_per_step_RTL ) ) ) % of the average ;
	%end;
	

	/*Optional Progress Bar*/
	%if %sysevalf(&step_counter_RTL < &total_in_list_RTL) %then %do;
		%let progress_output_RTL = | %sysfunc(repeat(#, %sysfunc(round(%sysevalf(((&percentage_per_step_RTL*&step_counter_RTL)*0.5)-1), 1))))
		%sysfunc(repeat(-, %eval(49-%sysfunc(round(%sysevalf(((&percentage_per_step_RTL*&step_counter_RTL)*0.5)), 1))) ))
		|;
		%let progress_output_RTL = %sysfunc(compress(&progress_output_RTL));
		%if %length(%sysfunc(round(%sysevalf(&percentage_per_step_RTL*&step_counter_RTL),1))) <2 %then %do;
			%let progress_output_RTL  = 
			%substr(&progress_output_RTL, 1, 25) 
			%sysfunc(round(%sysevalf(&percentage_per_step_RTL*&step_counter_RTL))) % 
			%substr(&progress_output_RTL, 28);
		%end;
		%else %do;
			%let progress_output_RTL  = 
			%substr(&progress_output_RTL, 1,24) 
			%sysfunc(round(%sysevalf(&percentage_per_step_RTL*&step_counter_RTL))) %
			%substr(&progress_output_RTL, 28);
		%end;
	%end;
	%else %do;
		%let progress_output_RTL  =  | %sysfunc(repeat(#,22)) 100% %sysfunc(repeat(#,22)) |;
	%end;
	%let progress_output_RTL = %sysfunc(compress(&progress_output_RTL));
	%put &progress_output_RTL;


%end;

/*How much time the whole process took*/
%let total_time_RTL = %sysevalf(%sysfunc(datetime()) - &start_time_RTL) ;
%put Process took %print_in_mins_seconds(&total_time_RTL);

%mend;
