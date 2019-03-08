
/*#####################################################################################*/
/*                                 TELL_ME_ABOUT                                       */
/*
This macro is used for analysing a collection of datasets which are similar, but may have
differences, for example the P35 file is similar each year, but sometimes variables 
appear in more recent years and sometime the length of a variable changes from one year
to the next. 

Here I'll use "year" as the thing which distinguishes the datasets, but it could equally 
be month, county, local authority, etc. 

It produces three datasets which tell you about the NAMEs of variables in each year, the
TYPE of each variable in each year, and the LENGTH of each variable in each year.

*/
/*
The first bit makes NAME TYPE LENGTH NOBS columns for each year
*/
%macro tell_me_about(library= ,  filename= , list= , list_bit_after_library=YES , list_bit_after_filename=YES ,suffix= );
%local step_counter_TMA list_item library_part filename_part;

%put Number of elements in list is %sysfunc(countw(&list , %STR( )));

%do step_counter_TMA  = 1 %to %sysfunc(countw(&list , %STR( )));

	%let list_item = %scan(&list, &step_counter_TMA , " ") ;

	%put list_item is &list_item.;

	%if &library = %STR( ) %then %let library_part = ;
	%else %do ; 
		%let library_part = &library;
		%if &list_bit_after_library = YES %then %let library_part = &library_part.&list_item;
	%end;

	%let filename_part = &filename;
	%if &list_bit_after_filename = YES %then %let filename_part = &filename_part.&list_item;
	%if &suffix ~= %STR( ) %then %let filename_part = &filename_part.&suffix;

	%if &library = %STR( ) %then %let dataset_to_import = &filename_part.;
	%else %let dataset_to_import = &library_part..&filename_part.;

	%if %Dataset_Exist(&dataset_to_import.) = 0 %then %do;
		%put Error: Dataset &dataset_to_import. does not exist.;
		%put Check if list_bit_after_library or list_bit_after_filename should be set to "NO";
		%put If you are working out of the WORK library, you can run this macro without the library statement at all;
		%abort;
	%end;

	%put Will check dataset: &dataset_to_import.;

	proc contents data = &dataset_to_import. 
	out = format_record_&list_item. (keep=name type length nobs) 
	noprint;
	run; 
	data format_record_&list_item.;
		set format_record_&list_item.;
		name = lowcase(name);
		name_&list_item. = name;
		if type = 2 then type_&list_item. = "C";
		if type = 1 then type_&list_item. = "N";
		length_&list_item. = length;
		set = &list_item. ;
	run;
	proc sort data=format_record_&list_item. ; by name; run;
%end;


/*
This gets just the set and NOBS.
Each row is roughly the same, but as a precaution we take the maximum NOBS
The next few steps take the 
*/
data all_num_obs;
	set format_record_: (keep=set nobs);
run;
proc sort data=all_num_obs ; by set descending nobs; run;
data all_num_obs (drop=set nobs rename=(char_set = set char_obs=nobs)); 
	length char_set $32. char_obs $32.;
	set all_num_obs; by set descending nobs; if first.set; 
	char_set = strip(put(set, 8.));
	char_obs = strip(put(nobs, 8.));
run;
/*At this stage you just have columns of set and nobs*/
proc transpose data=all_num_obs out=all_num_obs (drop=set) name=set prefix=name_;
id set;
var nobs;
run;
data all_num_obs;
	length name $32.;
	set all_num_obs;
	name="_nobs";
run;
/*Now it's transposed as a row*/

/*Now we make the individual records. The first one has nobs also*/
data name_record ;
	merge all_num_obs format_record_: (keep=name name_:);
	by name;
run;
data type_record ;
	merge format_record_: (keep=name type_:);
	by name;
run;
data length_record ;
	merge format_record_: (keep=name length_:);
	by name;
run;
%delete_dataset(format_record_: all_num_obs);

%mend;
