/*#####################################################################################*/
/*                                   SEQ                                               */
/*
Generates an ascending sequence of numbers from x to y

%put %seq(10 , 15);
*/

%macro seq(x,y);
%local sequence_return;
%do %until (&x>&y);
  %let sequence_return = &sequence_return &x;
  %let x=%eval(&x+1);
%end;
&sequence_return
%mend;

/*#####################################################################################*/
/*                                   REP                                               */
/*
Repeats a string a number of times, separated by spaces

%put %rep( cat dog fish,  6);
%put %rep( cat dog fish,  each=6);

*/
%macro rep(string, times  , each=);
%local rep_counter return_string rep_counter_2;

%if %is_blank(&times) and %is_blank(&each) %then %do;
	%put ERROR: Both times and each values to rep macro are blank;
	%abort;
%end;
%if %is_blank(&times)=0 and %is_blank(&each)=0 %then %do;
	%put ERROR: Rep macro cannot take values for both times and each;
	%abort;
%end;

%if %is_blank(&times)=0 %then %do;
	%put in the times structure;
	%if %is_int(&times)=0 OR %eval(&times < 0) %then %do;
		%put ERROR: Problem with times value passed to REP macro: &times;
		%abort;
	%end;
	%let return_string =;
	%do rep_counter = 1 %to &times;
		%let return_string = &return_string &string;
	%end;
%end;
%else %do;
	%put in the each structure;
	%if %is_int(&each)=0 OR %eval(&each < 0) %then %do;
		%put ERROR: Problem with each value passed to REP macro: &each;
		%abort;
	%end;
	%let return_string =;
	%do rep_counter = 1 %to %num_words(&string);
		%do rep_counter_2 = 1 %to &each;
			%let return_string = &return_string %nth_word(&string , &rep_counter);
		%end;
	%end;
%end;

&return_string
%mend;
/*#####################################################################################*/
/*                               ADD_PREFIX / SUFFIX                                   */
/*
%put %add_prefix( %seq(1,4) , p_);
%put %add_prefix( 1 2 3 4 , p_);
%put %add_suffix( 1 2 3 4 , _p);
*/

%macro add_prefix(list , prefix , add_in = NO);
%local result add_prefix_counter list;
%let list = %cmpres(&list);
%if &add_in = NO %then %do;
	%let result = &prefix%sysfunc(tranwrd(&list , %STR( ) , %STR( &prefix)));
%end;
%else %do add_prefix_counter = 1 %to %sysfunc(countw(&list));
	%if &add_prefix_counter = 1 %then %let result = &prefix%scan(&list , &add_prefix_counter) (in=in_%scan(&list , &add_prefix_counter)) &prefix ;
	%else %if &add_prefix_counter = %sysfunc(countw(&list)) %then %let result = &result%scan(&list , &add_prefix_counter) (in=in_%scan(&list , &add_prefix_counter)) ;
	%else %let result = &result%scan(&list , &add_prefix_counter) (in=in_%scan(&list , &add_prefix_counter)) &prefix ;
%end;
&result
%mend;

%macro add_suffix(list , suffix );
%local result add_prefix_counter list;
%let list = %cmpres(&list);
%let result = %sysfunc(tranwrd(&list , %STR( ) , %STR(&suffix )))&suffix;
&result
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

%macro list_num_vars(dsn);
 %local varlist dsid i;
 %let dsid = %sysfunc(open(&dsn));
 %if &dsid %then %do;
 %do i=1 %to %sysfunc(attrn(&dsid,nvars)); 
 %if %var_type(&dsn , %sysfunc(varname(&dsid,&i)))=N %then %let varlist=&varlist %sysfunc(varname(&dsid,&i)); 
 %end;
 %let dsid = %sysfunc(close(&dsid));
 %end;
 &varlist 
%mend list_num_vars;

%macro list_char_vars(dsn);
 %local varlist dsid i;
 %let dsid = %sysfunc(open(&dsn));
 %if &dsid %then %do;
 %do i=1 %to %sysfunc(attrn(&dsid,nvars)); 
 %if %var_type(&dsn , %sysfunc(varname(&dsid,&i)))=C %then %let varlist=&varlist %sysfunc(varname(&dsid,&i)); 
 %end;
 %let dsid = %sysfunc(close(&dsid));
 %end;
 &varlist 
%mend list_char_vars;

/*#####################################################################################*/
/*                                 ADD_COMMAS                                          */
/*
%put here are the variables with commas: %add_commas(make type year cylinders );
*/
%macro add_commas(list  );
%local result list;
%let list = %cmpres(&list);
%let result = %sysfunc(tranwrd(&list , %STR( ) , %STR(, )));
&result
%mend;

/*#####################################################################################*/
/*                                 ADD_KEEP                                            */
/*
%put %add_keep( sales_2010 sales_2011 sales_2012, id job_title);
*/
%macro add_keep(list , keep_vars  );
%local result list;
%let list = %cmpres(&list);
%let result = %sysfunc(tranwrd(&list , %STR( ) , %STR( (keep= &keep_vars) ))) (keep= &keep_vars);
&result
%mend;

%macro add_drop(list , drop_vars  );
%local result list;
%let list = %cmpres(&list);
%let result = %sysfunc(tranwrd(&list , %STR( ) , %STR( (drop= &drop_vars) ))) (drop= &drop_vars);
&result
%mend;

/*#####################################################################################*/
/*                                UNION                                                */
/*
%put %union(dog cat rabbit horse pigeon cat dog cheetah);
%put %union(2001 2002 2003 2004 2005 2006 2003 2004 2008 2001 2002);
%put %union( tiger fox wolf hawk   Fox wolfhound goshawk );
%put %union( id JOB id age ID sex);
*/

%macro union(full_list);
%local full_list_counter current_word return_list;
%let return_list = ;
%do full_list_counter = 1 %to %sysfunc(countw(&full_list));
	%let current_word = %scan(&full_list , &full_list_counter );
	%if  %find_word(&return_list , &current_word)=0 %then %do;
		%let return_list = &return_list  &current_word;
	%end;
%end;
&return_list
%mend;

/*#####################################################################################*/
/*                                INTERSECT_LISTS                                      */
/*
%put %intersect_list(dog cat rabbit horse pigeon , cat dog cheetah);
%put %intersect_list(2001 2002 2003 2004 2005 2006 , 2003 2004 2008 2001 2002);
%put %intersect_list( tiger fox wolf hawk ,  Fox wolfhound goshawk );
*/

%macro intersect_lists(left_list , right_list);
%local interesect_list_counter current_word return_list;
%let return_list = ;
%do interesect_list_counter = 1 %to %sysfunc(countw(&right_list));
	%let current_word = %scan(&right_list , &interesect_list_counter );
	%if  %find_word(&left_list , &current_word) %then %do;
		%let return_list = &return_list  &current_word;
	%end;
%end;
&return_list
%mend;

/*#####################################################################################*/
/*                                LEFT_ANTI_JOIN                                       */
/*
%put %left_anti_join(dog cat rabbit horse pigeon , cat dog cheetah);
%put %left_anti_join(2001 2002 2003 2004 2005 2006 , 2003 2004 2008);
*/

%macro left_anti_join(left_list , right_list);
%local anti_join_counter current_word ;
%if %is_blank(&right_list)=0 %then %do anti_join_counter = 1 %to %num_words(&right_list);
	%let current_word = %scan(&right_list , &anti_join_counter , ,s );
	%if  %find_word(&left_list , &current_word) %then %do;
		%let left_list = %remove_word(&left_list , &current_word);
	%end;
%end;
&left_list
%mend;

%macro remove_words(left_list , right_list);
%local anti_join_counter current_word ;
%if %is_blank(&right_list)=0 %then %do anti_join_counter = 1 %to %num_words(&right_list);
	%let current_word = %scan(&right_list , &anti_join_counter , ,s);
	%if  %find_word(&left_list , &current_word) %then %do;
		%let left_list = %remove_word(&left_list , &current_word);
	%end;
%end;
&left_list
%mend;


/*#####################################################################################*/
/*                                PAIRWISE_JOIN                                        */
/*
%put %pairwise_join(hand foot tree , bag ball house);
%put %pairwise_join(library_2010 library_2012 , file_2010 file_2012 , sep=. );
%put %pairwise_join(cat dog bird , bed house cage , sep=space);
*/
%macro pairwise_join(list_1 , list_2 , sep= );
%local size_list match_counter;
%let return_sentence = ;
%if %upcase(&sep)=SPACE %then %let sep = %str( );
%let size_list = %sysfunc(countw(&list_1));
%if &size_list ~= %sysfunc(countw(&list_2)) %then %abort;
%do match_counter = 1 %to &size_list;
	%let return_sentence = &return_sentence %scan(&list_1, &match_counter)&sep%scan(&list_2, &match_counter);
%end;
&return_sentence
%mend;

/*#####################################################################################*/
/*                             AS_NUM  AS_CHAR  EXTRACT_NUM                            */

%macro as_num(number);
input(strip(&number) , 8.)
%mend;

%macro as_char(number);
strip(put(&number , 8.))
%mend;

%macro extract_num(number);
input(strip( compress(&number , , "dk")  ) , 8.)
%mend;

/*#####################################################################################*/
/*                                      SQUISH                                         */
%macro squish(string);
strip(compbl(&string))
%mend;

/*#####################################################################################*/
/*                                   LAST_N_CHARS                                      */
%macro last_n_chars(string , n);
substr(&string , length(&string)-&n+1  )
%mend;

/*#####################################################################################*/
/*                            CROP_LEFT AND CROP_RIGHT                                 */
/*
Like in Excel. Designed to work in data steps, not for macro variables. 

data example;
	a = "peter is the name of a cat";
	b = "name";
	C = %crop_right(a , b);
	D = %crop_left(a , "name");
run;
*/

%macro crop_right(sentence , phrase);
left(substr(&sentence , find(&sentence , &phrase) + length(&phrase)))
%mend;

%macro crop_left(sentence , phrase);
trimn(substr(&sentence , 1, find(&sentence , &phrase)-1))
%mend;

/*#####################################################################################*/
/*                                    NUM_WORDS                                        */
/*
Number of words in a string/list. Note that the only delimiter is some kind of space, 
so fullstops do not seperate words on their own. Therefore SAS datasets with library
are a single word
*/
%macro num_words(sentence);
%sysfunc(countw(&sentence , , s))
%mend;

/*#####################################################################################*/
/*                                    NTH_WORD                                         */
/*
Returns the nth word in a list. Space is only delimiter.
*/

%macro nth_word(sentence , number);
%if &number <1 OR &number >%num_words(&sentence) %then %do;
	%put ERROR: Number is out of range for this list;
	%abort;
%end;
%scan(&sentence , &number , ,s)
%mend;

/*#####################################################################################*/
/*                                    FIND_WORD                                        */
/*
This returns zero or a postive number, so can be used for logical operations.

FINDW can also be used, but it returns the character position instead of the word position.
This function tells you if the word you're looking for is the first, second third (etc.) word
in a sentence.

This only finds whole words and is case insensitive.

%put Find the word: %find_word(It has been a busy day for Mr. Bee , bee);
%put Find the word: %find_word(It has been a busy day for Mr. Bee , crow);
*/
%macro find_word(sentence , word ) ;
%local word_position_counter;
%let word_position_counter = 0;
%if %sysfunc(findw(%lowcase(&sentence ), %lowcase(&word) , ,s )) %then %do;
	%do word_position_counter = 1 %to %num_words(&sentence);
		%if %lowcase(&word) = %lowcase(%scan(&sentence , &word_position_counter , ,s)) %then %goto LEAVE;
		%else %if &word_position_counter = %num_words(&sentence) %then %return;
	%end;
%end;
%LEAVE: &word_position_counter
%mend;


/*#####################################################################################*/
/*                           FIRST_WORD , LAST_WORD                                    */
/*
%put The first word is: %first_word(The first word is);
%put The last word is: %last_word(The first word is);
*/
%macro first_word(sentence);
%scan(&sentence,1)
%mend;
%macro last_word(sentence);
%scan(&sentence,-1)
%mend;


/*#####################################################################################*/
/*                WORDS_BEGINNING_WITH     WORDS_ENDING_WITH                           */
/*

%put %words_beginning_with(The_shop there it goes Theodore , the);
%put %words_ending_with(Boing goes a SINGING thing , ing);
%put %words_beginning_with(%list_vars(sashelp.cars)  , mpg);
%put %words_containing( id ANNUAL_TAX income_tax_to_date  age , tax);

*/
%macro words_beginning_with(sentence , phrases);
%local begin_counter word_bit current_word return_sentence current_phrase;
%let return_sentence = ;
%do begin_counter = 1 %to %num_words(&sentence);
	%let current_word = %nth_word(&sentence , &begin_counter) ;
		%do phrase_counter = 1 %to %num_words(&phrases);
			%let current_phrase = %nth_word(&phrases , &phrase_counter);	
			%if %eval(%length(&current_phrase) <= %length(&current_word)) %then %do;
				%let word_bit = %substr(&current_word , 1 , %length(&current_phrase));
				%if %lowcase(&word_bit) = %lowcase(&current_phrase) %then %let return_sentence = &return_sentence &current_word;
			%end;
		%end;
%end;
&return_sentence 
%mend;


%macro words_ending_with(sentence , phrases);
%local begin_counter word_bit current_word return_sentence current_phrase;
%let return_sentence = ;
%do begin_counter = 1 %to %num_words(&sentence);
	%let current_word = %nth_word(&sentence , &begin_counter) ;
		%do phrase_counter = 1 %to %num_words(&phrases);
			%let current_phrase = %nth_word(&phrases , &phrase_counter);	
			%if %eval(%length(&current_phrase) <= %length(&current_word)) %then %do;
				%let word_bit = %substr(&current_word , %eval(%length(&current_word) - %length(&current_phrase) +1 )  );
				%if %lowcase(&word_bit) = %lowcase(&current_phrase) %then %let return_sentence = &return_sentence &current_word;
			%end;
		%end;
%end;
&return_sentence 
%mend;


%macro words_containing(sentence , phrases);
%local contain_counter word_bit current_word return_sentence current_phrase phrase_counter;
%let return_sentence = ;
%do begin_counter = 1 %to %num_words(&sentence);
	%let current_word = %nth_word(&sentence , &begin_counter) ;
		%do phrase_counter = 1 %to %num_words(&phrases);
			%let current_phrase = %nth_word(&phrases , &phrase_counter);	
			%if %eval(%length(&current_phrase) <= %length(&current_word)) %then %do;
				%if %sysfunc(find(  %lowcase(&current_word) , %lowcase(&current_phrase)  )) %then %let return_sentence = &return_sentence &current_word;
			%end;
		%end;
%end;
&return_sentence 
%mend;



/*#####################################################################################*/
/*                                 REMOVE_WORD                                         */
/*

Compare the following two outputs:
%put  %sysfunc(tranwrd(It has been a busy day for Mr. Bee as he goes about his bee business, bee , ));
%put  %remove_word(It has been a busy day for Mr. Bee as he goes about his bee business, bee  );

Potential replacement:
%macro remove_word(sentence , word);
%sysfunc(prxchange(s/\b&word\b//i ,-1 , &sentence))
%mend;
*/


%macro remove_word(sentence, word);
%local remove_counter return_sentence ;
%let return_sentence = ;
%do remove_counter = 1 %to %num_words(&sentence);
	%if %eval(%lowcase(&word) = %lowcase(%nth_word(&sentence , &remove_counter)) ) %then %do;	
	%end;
	%else %do;
		%let return_sentence = &return_sentence %nth_word(&sentence , &remove_counter);
	%end;
%end;
&return_sentence
%mend;

/*#####################################################################################*/
/*                            REMOVE_WORDS_CONTAINING                                  */


%macro remove_words_containing(list , phrase);
%remove_words(&list , %words_containing(&list , &phrase))
%mend;

/*#####################################################################################*/
/*                                 REMOVE_NTH_WORD                                     */
/*
Returns a sentence without the nth word. Assumes spaces between words. 
Hasn't been thoroughly tested.

%put %remove_nth_word(I have a dog And a cat , 5);
*/
%macro remove_nth_word(sentence , n);
%sysfunc(prxchange(s/%sysfunc(repeat((\w+)\s+, &n-1))/%add_prefix( %seq(1,%eval(&n-1)), $ ) /,1, &sentence))
%mend;

/*#####################################################################################*/
/*                                   REPLACE_WORD                                      */
/*

Compare the following two outputs:
%put  %sysfunc(tranwrd(It has been a busy day for Mister Bee as he goes about his bee business , bee , Squirrel));
%put  %replace_word(It has been a busy day for Mister Bee as he goes about his bee business , bee , Squirrel);
*/
%macro replace_word(sentence , old_word , new_word);
%local word_position_counter sentence_to_return;
%let sentence_to_return = ;
%do word_position_counter = 1 %to %sysfunc(countw(&sentence));
	%if %lowcase(&old_word) = %lowcase(%scan(&sentence , &word_position_counter)) %then 
		%let sentence_to_return = &sentence_to_return &new_word;
		%else %let sentence_to_return = &sentence_to_return %scan(&sentence , &word_position_counter);
%end;
&sentence_to_return 
%mend;


/*#####################################################################################*/
/*                                   LIST_SUBSTR                                       */
/*

Returns a list of substrings of each word in an original list, with each substring 
defined by a starting position and length.

%put %list_substr(file_2010_a file_2011_a file_2012_b , 6 , 4);
*/
%macro list_substr(list , start , length);
%local num_words substr_counter return_list;
%let num_words = %sysfunc(countw(&list , , s));
%let return_list = ;
/*
Need to check all words are long enough
*/
%do substr_counter = 1 %to &num_words;
	%let word_length  = %length(%scan(&list, &substr_counter , , s));
	%if %eval(&word_length < &start + &length -1 ) %then %do;
		%put One of the words in the list is not long enough: %scan(&list, &substr_counter , , s);
		%abort;
	%end;
%end;
%do substr_counter = 1 %to &num_words;
	%let return_list = &return_list %substr(%scan(&list, &substr_counter , , s) , &start , &length);
%end;
&return_list
%mend;


/*#####################################################################################*/
/*                                 IF_ELSE                                             */
/*
%put response is %if_else(2020 > 2010 , True , False);
Note you have to use EQ instead of = here or else SAS will think you are using a positional parameter. 
See also NE GT LT GE LE IN
*/
%macro if_else(statement , T , F ) ;
%local return_element;
%if &statement %then %let return_element = &T;
%else %let return_element = &F;
&return_element.
%mend;


/*#####################################################################################*/
/*                                 PRINT_VARS                                          */
/*
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
/*                                 VAR_EXIST                                          */
/*
%put %var_exist(sashelp.cars , model);
%put %var_exist(sashelp.cars , sandwich);
*/
%macro Var_Exist(ds,var);
	%local rc dsid result;

	%let dsid=%sysfunc(open(&ds));
	%if %sysfunc(varnum(&dsid,&var)) > 0 %then %do;
		%let result=1;
		/*%put NOTE: Variable &var exists in &ds;*/
	%end;
	%else %do;
		%let result=0;
		/*%put NOTE: Variable &var does not exist in &ds;*/
	%end;
	%let rc=%sysfunc(close(&dsid));
	&result
%mend ;

/*#####################################################################################*/
/*                                 VARS_EXIST                                          */
/*
%put %vars_exist(sashelp.cars , model model_name model_id);
*/
%macro vars_exist(dataset, list_of_vars);
%intersect_lists( %list_vars( &dataset ), &list_of_vars )
%mend;

/*#####################################################################################*/
/*                                 VAR_TYPE                                            */
/*
%put Type is %var_type(sashelp.cars , origin);
*/
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

/*#####################################################################################*/
/*                                 VAR_LENGTH                                          */
/*
%put Here is the length: %Var_Length(sashelp.cars  , Origin);
*/
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

/*#####################################################################################*/
/*                                  VAR_FMT                                            */
/*
%put %var_fmt(sashelp.cars , invoice);
*/
%macro Var_FMT(ds,var);
%local rc dsid result;
%let dsid=%sysfunc(open(&ds));
%if &dsid %then
   %do;
      %let result=%sysfunc(varfmt(&dsid,%sysfunc(varnum(&dsid,&var))));
   %end;
%else %let result=ERR_&var;
  %let rc=%sysfunc(close(&dsid));
  &result
%mend Var_FMT;

/*#####################################################################################*/
/*                                   RIGHT_CASE                                        */
/*
%put The correct case for model is %right_case(sashelp.cars , mODeL);
*/
%macro right_case(dataset_name , var_name);
%local right_case_counter;
%do right_case_counter = 1 %to %sysfunc(countw(%list_vars(&dataset_name)));
	%if %lowcase(&var_name ) = %lowcase(%scan(%list_vars(&dataset_name), &right_case_counter)) %then 
		%let var_name = %scan(%list_vars(&dataset_name), &right_case_counter);
%end;
&var_name
%mend;

/*#####################################################################################*/
/*                                   MAX_LENGTH                                        */
/*
%max_length(sashelp.cars , model);
*/
%macro max_length(dataset_name , variable_name);
%local Largest_Variable_Length;
proc sql noprint;
select max(length(&variable_name)) into :Largest_Variable_Length from &dataset_name ;
quit;
%put Largest length of &variable_name: &Largest_Variable_Length;
%mend;

/*#####################################################################################*/
/*                                 SHOW_DATASETS                                       */
/*
%show_datasets;
*/
%macro show_datasets;
proc sql ;
  create table datasets_memory as
  select memname as dataset label="Dataset Name" , filesize format=sizekmg., 
	modate as last_modified, nobs as rows format=comma20., nvar as columns 
  from dictionary.tables
  where libname = 'WORK'
  order by filesize desc;
quit ;
%mend;

/*#####################################################################################*/
/*                                 DATASET_EXIST                                       */
/*
Returns 1 or 0 depending on whether the dataset exists

%put %dataset_exist(sashelp.cars);
%put %dataset_exist(sashelp.boats);
*/
%macro Dataset_Exist(ds);
%local rc dsid result;
%let dsid=%sysfunc(open(&ds));

%if &dsid %then %do;
	%let result = 1;
%end;
%else %do;
	/*%put Dataset &ds does not exist;*/
	%let result = 0;	
%end;
%let rc=%sysfunc(close(&dsid));
&result
%mend ;

/*#####################################################################################*/
/*                                 DATASETS_EXIST                                       */
/*
Pass a list of datasets, returns only those which actually exist. 
Note that it returns a list and is not a logical test.

%put %datasets_exist(sashelp.cars somthing_else);
*/
%macro datasets_exist(list_of_datasets);
%local return_list;
%let return_list = ;
%do i = 1 %to %sysfunc(countw(&list_of_datasets , , s));
	%if %dataset_exist( %scan(&list_of_datasets ,&i, , s) ) %then %let return_list = &return_list. %scan(&list_of_datasets ,&i, , s);
%end;
&return_list.
%mend;

/*#####################################################################################*/
/*                                 LIST_DATASETS                                       */
/*
Pass this macro either a libname or a directory path (without quotes) and it will return
a list of all the SAS datasets within. 

E.g. 
%put %list_datasets(WORK);
*/

%macro list_datasets(dir );
	%local filrf rc did memcnt name i folder_name return_list_datasets lib_bit;
	%let lib_bit = ;
	%if %index(&dir , \) = 0 and %index(&dir , /)=0 %then
		%do;
			%let lib_bit = &dir..;
			%let dir = %sysfunc(pathname(&dir));
		%end;

	%let rc=%sysfunc(filename(filrf,&dir));
	%let did=%sysfunc(dopen(&filrf));

	%if &did eq 0 %then
		%do;
			%put Directory &dir cannot be opened or does not exist;

			%return;
		%end;

	%LET return_list_datasets =;

	%do i = 1 %to %sysfunc(dnum(&did));
		%let name=%qsysfunc(dread(&did,&i));

		%if %qupcase(%qscan(&name,-1,.)) = SAS7BDAT %then
			%do;
				%LET return_list_datasets = &return_list_datasets &lib_bit.%qscan(&name,1,.);
			%end;
	%end;

	%let rc=%sysfunc(dclose(&did));
	%let rc=%sysfunc(filename(filrf));
	&return_list_datasets
%mend list_datasets;




/*#####################################################################################*/
/*                                 DELETE_DATASET                                      */
/*
%delete_dataset(data_1 data_2 );
%delete_dataset( TEMP_: );
*/
%macro delete_dataset(list);
%local list;
proc datasets lib=work memtype=data nolist ;
	delete &list;
quit;
run;
%mend;


/*#####################################################################################*/
/*                                 RENAME_VAR                                          */
/*
data something;
	set sashelp.cars;
run;
%rename_var(something , old_name=Make , new_name = Make_of_car );

If the dataset doesn't exist then ABORT.
If warning is YES and the old_name doesn't exist then ERROR ABORT
ELSE If warning is YES and the new_name exists then ERROR ABORT
ELSE If warning is NO and the old_name doesn't exist then NOTE 
ELSE If warning is NO and the new_name exists then NOTE 
ELSE (old_name exists and new_name doesn't) RENAME

*/

%macro rename_var(ds , old_name= , new_name= , warn=YES);
%if %dataset_exist(&ds)=0 %then %do;
	%put ERROR: The dataset passed to the rename macro does not exist: &ds ;
	%abort;
%end;

%if &warn = YES and %var_exist(&ds , &new_name) %then %do;
	%put ERROR: The variable &new_name already exists in the dataset &ds; 
	%abort;
%end;
%else %if &warn = YES and %var_exist(&ds , &old_name)=0 %then %do;
	%put ERROR: The variable &old_name does not exist in the dataset &ds; 
	%abort;
%end;
%else %if &warn = NO  and %var_exist(&ds , &new_name) %then %do;
	%put NOTE: The variable &new_name already exists in the dataset &ds so nothing will be renamed; 
%end;
%else %if &warn = NO  and %var_exist(&ds , &old_name)=0 %then %do;
	%put NOTE: The variable &old_name does not exist in the dataset &ds so nothing will be renamed; 
%end;
%else %do;
	PROC DATASETS LIBRARY=work nolist;
	MODIFY &ds ;
	RENAME &old_name=&new_name;
	QUIT;
	RUN;
%end;
%mend; 


/*#####################################################################################*/
/*                                 ADD_LABEL                                           */
/*
data names_of_people;
	input name $;
	datalines;
Joe
Tim
Brendan
;
run;
%add_label( names_of_people , name , "Name of person"  );
%add_label( names_of_people , hat , "Name of person"  );
%add_label( names_of_people , hat , "Name of person" , warn=NO );
%print_vars(names_of_people);
*/
%macro add_label(ds_label , var_label , label , warn=YES);
%if &warn = YES | %Var_Exist(&ds_label  , &var_label ) %then %do;
PROC DATASETS LIBRARY=work nolist;
MODIFY &ds_label ;
label &var_label = &label;
RUN;
quit;
%end;
%mend;


/*#####################################################################################*/
/*                                 DROP_FORMAT                                         */
%macro drop_format(ds , var);
proc datasets lib=work memtype=data nolist;
   	modify &ds;
    attrib &var format=;
run;
%mend;

/*#####################################################################################*/
/*                                 SHOW_MACROS                                         */
/*
%show_macros;
*/
%macro show_macros;
proc sql;
  select * 
    from dictionary.catalogs
    where objtype='MACRO';
quit;
%mend;


/*#####################################################################################*/
/*                                   IS_BLANK                                          */
/*
%let var_1 = ;
%let var_2 = Hello;
%put %is_blank(&var_1);
%put %is_blank(&var_2);
*/
%macro is_blank(param);
  %sysevalf(%superq(param)=,boolean)
%mend;

/*#####################################################################################*/
/*                                   IS_INT                                            */
/*
With thanks to Lex Jansen
%put %is_int(2007);
%put %is_int(1E10);
%put %is_int(12345.4);
%put %is_int(123rabbit);
*/
%macro is_int(str);
%local test_string result;
%let test_string = %sysfunc(compress(&str ,, kd));
%if &test_string = &str %then %let result = 1;
%else %let result = 0;
&result
%mend; 

/*#####################################################################################*/
/*                        PF - ABBREVIATED PROC FREQ MACRO                             */
/*
A concise way of writing a proc freq statement, without any percentages for combination
proc freqs. 
Also checks if there is an existing title, and creates a new one if not.

%pf(sashelp.cars , type*cylinders);
%pf(sashelp.cars , type);

*/
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

/*#####################################################################################*/
/*                        PS - ABBREVIATED PROC SORT MACRO                             */
/*
data something;
	set sashelp.cars;
run;
%ps(something , type cylinders);
*/
%macro PS(dataset , variable);
proc sort data= &dataset ;
	by &variable;
run;
%mend;

/*#####################################################################################*/
/*                                  KEEP_FIRST                                         */

%macro keep_first(dataset_name, variable_list);
%local last_variable;
%let last_variable = %scan(&variable_list, -1);
data &dataset_name;
	set &dataset_name;
	by &variable_list;
	if first.&last_variable;
run;
%mend;

/*#####################################################################################*/
/*                                   UNIQUE                                            */
/*
I wanted something similar to the unique function in R, which quickly shows the unique 
entries for a particular variable. It also shows the number of each entry.
In some ways it is like a proc freq, but the output is to the log and it explicity 
states the number of unique entries. 
I find this handy for something like Male/Female, where you can't be sure initially if 
there are no missing values, or NA. 
It does not produce the breakdown with numbers for each entry type if there are more than
100 unique entries.

%unique(sashelp.cars , make);
*/

%macro unique(dataset , var);
%local nobs dsnid unique_counter;
%let dsnid = %sysfunc(open(&dataset));
%if &dsnid %then %do;
    %let rc  =%sysfunc(close(&dsnid));
	proc summary data= &dataset nway;
		class &var;
		output out=unique_vars (drop=_TYPE_ rename=(_FREQ_=count));
	run;
	%let dsnid = %sysfunc(open(unique_vars));
	%let nobs=%sysfunc(attrn(&dsnid,nlobs));
	%let rc  =%sysfunc(close(&dsnid));

	%put Number of unique entries: &nobs;
	%if &nobs<=100 %then %do;
		data unique_vars;
			set unique_vars;
			put &var count;
		run;
	%end;
	%delete_dataset(unique_vars);
%end;
%else %do; %put Unable to open &dsn - %sysfunc(sysmsg()); %end;
%mend;

/*#####################################################################################*/
/*                                CHECK_DUPLICATES                                     */
/*
This tells you the number of unique entries which have a particular number of appearances
in the dataset. Outputs to the log E.g. in the column "Bob Joe Andy Tim Paul Paul" the 
output would be:
Occurrences - Number
1 4
2 1
Because there were four unique entries which had only one occurrence, and one unique entry 
which had 2 occurrences. 

Have a look at the following examples:
%check_duplicates(sashelp.baseball , name);
%check_duplicates(sashelp.baseball , team);
%check_duplicates(sashelp.iris , species);
*/

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



/*#####################################################################################*/
/*                                ABORT_IF_DUPLICATES                                     */
/*
Checks for duplicates of a certain variable, returns error if so.

%abort_if_duplicates(sashelp.baseball , name);
%abort_if_duplicates(sashelp.baseball , team);
%abort_if_duplicates(sashelp.iris , species);
*/

%macro abort_if_duplicates(dataset , var_name);
%local This_should_be_1;
proc sql noprint;    
	select max(Count) into  :This_should_be_1 from
	(select count(&var_name) as Count from &dataset group by &var_name)
	;
quit;
%if %eval( &This_should_be_1 > 1) %then %do;
	%put Error: There are some duplicates of &var_name in &dataset;
	%abort;
%end;
%else %do;
	%put Report: There were no duplicates of &var_name in &dataset;
%end;
%mend;


/*#####################################################################################*/
/*                                ABORT_UNEQUAL_DUPLICATES                                     */
/*
Checks if there are differing number of duplicates of a certain variable, returns error if so.

%abort_unequal_duplicates(sashelp.baseball , name);
%abort_unequal_duplicates(sashelp.baseball , team);
%abort_unequal_duplicates(sashelp.iris , species);
*/

%macro abort_unequal_duplicates(dataset , var_name);
%local max_counter min_counter;
proc sql noprint;    
	select max(Count) , min(Count) into  :max_counter ,   :min_counter from
	(select count(&var_name) as Count from &dataset group by &var_name)
	;
quit;
%let max_counter = %CMPRES(&max_counter);
%let min_counter = %CMPRES(&min_counter);
%put min and max are: &min_counter &max_counter;
%if %eval( &max_counter ~= &min_counter) %then %do;
	%put Error: There are differing numbers of duplicates of &var_name in &dataset;
	%abort;
%end;
%else %do;
	%put Report: Variable &var_name is duplicated the same number of times (&max_counter) in &dataset;
%end;
%mend;


/*#####################################################################################*/
/*                                 RUNNING_THRU_LIST                                   */

/*This macro is needed for the output of running_thru_list. You might find it useful
otherwise
%put The time is %print_in_mins_seconds(400) ;
*/
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

%let total_in_list_RTL = %sysfunc(countw(&list , , s));
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
	%&macro_name( %scan(&list, &step_counter_RTL , , s) );
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



/*#####################################################################################*/
/*                                 TELL_ME_ABOUT                                       */
/*
This macro is used for analysing a series of inconsistent datasets

It produces three datasets which tell you about the NAMEs of variables in each year, the
TYPE of each variable in each year, and the LENGTH of each variable in each year.

If you pass it only one dataset, it combines these three datasets into just one dataset with
three columns.

EXAMPLE:

data sales_1;
	length name $ 5 sex $1  sales 8;
	input name $ sex $ sales ;
	datalines;
Max M 1
Ben M 2
Joe M 3
;
data sales_2;
	length name $ 8 sex $1 sales 8;
	input name $  sex $ sales ;
	datalines;
Brendan M 4
Maxine F 7
Joan F 5
;
data sales_3;
	length name $ 8 surname $8 sex 3 sales 8;
	input name $  surname $ sex sales ;
	datalines;
Jim Gavin 1 4
Michael Collins 1 7
Mary McAleese 2 5
Niall Horan 1 3
;
run;	

%tell_me_about(sales_1 sales_2 sales_3 );

*/
%macro tell_me_about( list );
%local step_counter_TMA list_item library_part list_length_TMA basis_for_names first_item_TMA second_item_TMA list_of_list_items;

%let list_length_TMA = %sysfunc(countw(&list , %STR( )));

%let basis_for_names = WORD;

%if %eval(&list_length_TMA. >1 )%then %do;
	%let first_item_TMA = %scan(&list, 1 , " ");
	%let second_item_TMA = %scan(&list, 2, " ");

	%if &first_item_TMA = &second_item_TMA %then %do;
		%put These datasets are the same; %abort;
	%end;

	%if  %sysfunc(find(&first_item_TMA , .)) %then %do;
		%if %scan(&first_item_TMA, 2 ) = %scan(&second_item_TMA, 2 ) %then %let basis_for_names = LIBRARY;
		%else %let basis_for_names = DATASET;
	%end;
%end;
%else %if %eval(&list_length_TMA. =1 )%then %do;
	%if  %sysfunc(find(&list , .)) %then %do;
		%if %scan(&first_item_TMA, 2 ) = %scan(&second_item_TMA, 2 ) %then %let basis_for_names = LIBRARY;
		%else %let basis_for_names = DATASET;
	%end;
%end;


%let list_of_list_items = ;

%do step_counter_TMA  = 1 %to &list_length_TMA.;
	%let dataset_to_import = %scan(&list, &step_counter_TMA , " ") ;
	%if &basis_for_names = WORD %then %let list_item = &dataset_to_import;
	%else %if &basis_for_names = LIBRARY %then %let list_item = %scan(&dataset_to_import, 1 );
	%else %if &basis_for_names = DATASET %then %let list_item = %scan(&dataset_to_import, 2 );
	%let list_of_list_items = &list_of_list_items &list_item;

	%if %Dataset_Exist(&dataset_to_import.) = 0 %then %do;
		%put Error: Dataset &dataset_to_import. does not exist.;		%abort;
	%end;

	proc contents data = &dataset_to_import. 
	out = fmts_&list_item. (keep=name type length nobs) 
	noprint	;
	run; 
	data fmts_&list_item.;
		length set $50;
		set fmts_&list_item.;
		name = lowcase(name);
		name_&list_item. = name;
		if type = 2 then type_&list_item. = "C";
		if type = 1 then type_&list_item. = "N";
		length_&list_item. = length;
		set = "&list_item." ;
	run;
	proc sort data=fmts_&list_item. ; by name; run;
%end;

/*
This gets just the set and NOBS.
Each row is roughly the same, but as a precaution we take the maximum NOBS
The next few steps take the 
*/
data all_num_obs;
	set %add_prefix(&list_of_list_items , fmts_ )  (keep=set nobs);
run;
proc sort data=all_num_obs ; by set descending nobs; run;


data all_num_obs (drop= nobs rename=( char_obs=nobs)); 
	length char_obs $32.;
	set all_num_obs; 
	by set descending nobs; 
	if first.set; 
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
	merge all_num_obs %add_keep( %add_prefix(&list_of_list_items , fmts_ ) , name name_:) ; 
	by name;
	array n_a {*} name_: ;
	*incontinuity = countw( catx(" " ,  of n_a[*])   )  ~= dim(n_a);
	incontinuity = cmiss(of n_a[*]) > 0;
run;

data type_record ;
	merge %add_keep( %add_prefix(&list_of_list_items , fmts_ ) , name type_:) ; 
	by name;
	array t_a {*} type_: ;
	incontinuity = min(whichc("N" , of t_a[*]), whichc("C" , of t_a[*]))>0;
run;

data length_record ;
	merge %add_keep( %add_prefix(&list_of_list_items , fmts_ ) , name length_:) ; 
	by name;
	array l_a {*} length_: ;
	incontinuity = range(of l_a[*]) >0;
run;
%delete_dataset(%add_prefix(&list_of_list_items , fmts_ ) all_num_obs);


%if %eval(&list_length_TMA. =1 )%then %do;
	data dataset_format (drop=incontinuity);
		merge 
				type_record (rename= %remove_words(%list_vars(type_record),name incontinuity)=Type) 
				length_record (rename= %remove_words(%list_vars(length_record),name incontinuity)=Length);
		by name;
	run;
	%delete_dataset(name_record type_record length_record);
	proc report data=dataset_format ;
	run;
	%delete_dataset(dataset_format);
run;
%end;
%else %do;
title Variable continuity;
proc report data=name_record ;
	column %list_Vars(name_record);
	define incontinuity / display noprint;
	compute incontinuity;
		if incontinuity =1 then call define(_row_,"style","style={background=orange}");
	endcomp;
run;
title Type continuity;
proc report data=type_record ;
	column %list_Vars(type_record);
	define incontinuity / display noprint;
	compute incontinuity;
		if incontinuity =1 then call define(_row_,"style","style={background=orange}");
	endcomp;
run;
title Length continuity;
proc report data=length_record ;
	column %list_Vars(length_record);
	define incontinuity / display noprint;
	compute incontinuity;
		if incontinuity =1 then call define(_row_,"style","style={background=orange}");
	endcomp;
run;
title;
%delete_dataset(name_record type_record length_record);
%end;
%mend;
