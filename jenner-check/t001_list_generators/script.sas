/* Bundle t001 — list-generation macros from brendan_macros.sas
   Macros: %is_blank %is_int %num_words %nth_word %seq %rep %add_prefix
           %add_suffix %add_commas
   Callers are the documented examples from the macro headers. */

%macro is_blank(param);
  %sysevalf(%superq(param)=,boolean)
%mend;

%macro is_int(str);
%local test_string result;
%let test_string = %sysfunc(compress(&str ,, kd));
%if &test_string = &str %then %let result = 1;
%else %let result = 0;
&result
%mend;

%macro num_words(sentence);
%sysfunc(countw(&sentence , , s))
%mend;

%macro nth_word(sentence , number);
%if &number <1 OR &number >%num_words(&sentence) %then %do;
	%put ERROR: Number is out of range for this list;
	%abort;
%end;
%scan(&sentence , &number , ,s)
%mend;

%macro seq(x,y);
%local sequence_return;
%do %until (&x>&y);
  %let sequence_return = &sequence_return &x;
  %let x=%eval(&x+1);
%end;
&sequence_return
%mend;

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
	%if %is_int(&times)=0 OR %eval(&times < 0) %then %do;
		%put ERROR: Problem with times value passed to REP macro: &times;
		%abort;
	%end;
	%let return_string =;
	%do rep_counter = 1 %to &times;
		%let return_string = &return_string &string;
	%end;
%end;
&return_string
%mend;

%macro add_prefix(list , prefix , add_in = NO);
%local result add_prefix_counter list;
%let list = %cmpres(&list);
%if &add_in = NO %then %do;
	%let result = &prefix%sysfunc(tranwrd(&list , %STR( ) , %STR( &prefix)));
%end;
&result
%mend;

%macro add_suffix(list , suffix );
%local result add_prefix_counter list;
%let list = %cmpres(&list);
%let result = %sysfunc(tranwrd(&list , %STR( ) , %STR(&suffix )))&suffix;
&result
%mend;

%macro add_commas(list  );
%local result list;
%let list = %cmpres(&list);
%let result = %sysfunc(tranwrd(&list , %STR( ) , %STR(, )));
&result
%mend;

/* Documented examples from the macro headers */
%put SEQ: %seq(10 , 15);
%put ADD_PREFIX: %add_prefix( %seq(1,4) , p_);
%put ADD_PREFIX_LIT: %add_prefix( 1 2 3 4 , p_);
%put ADD_SUFFIX: %add_suffix( 1 2 3 4 , _p);
%put ADD_COMMAS: %add_commas(make type year cylinders );
%put REP_TIMES: %rep( cat dog fish,  6);
%put IS_INT_2007: %is_int(2007);
%put IS_INT_123rabbit: %is_int(123rabbit);
%put IS_BLANK_EMPTY: %is_blank();
%put IS_BLANK_HELLO: %is_blank(Hello);
