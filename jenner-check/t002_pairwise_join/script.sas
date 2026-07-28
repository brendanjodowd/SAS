/* Bundle t002 — %pairwise_join, the pairwise list-joining macro from
   brendan_macros.sas. Zips two equal-length lists element by element,
   with an optional separator.
   Callers are the documented examples from the macro header. */

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

/* Documented examples from the macro header */
%put PAIRWISE: %pairwise_join(hand foot tree , bag ball house);
%put PAIRWISE_DOT: %pairwise_join(library_2010 library_2012 , file_2010 file_2012 , sep=. );
%put PAIRWISE_SPACE: %pairwise_join(cat dog bird , bed house cage , sep=space);
