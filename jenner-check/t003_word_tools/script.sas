/* Bundle t003 — word-manipulation macros from brendan_macros.sas
   Macros: %first_word %last_word %replace_word
   Callers are the documented examples from the macro headers. */

%macro first_word(sentence);
%scan(&sentence,1)
%mend;

%macro last_word(sentence);
%scan(&sentence,-1)
%mend;

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

/* Documented examples from the macro headers */
%put FIRST_WORD: %first_word(The first word is);
%put LAST_WORD: %last_word(The first word is);
%put REPLACE_WORD: %replace_word(It has been a busy day for Mister Bee as he goes about his bee business , bee , Squirrel);
