/*#####################################################################################*/
/*                           FIRST_WORD , LAST_WORD                                    */
/*
These both return a word.
%put The first word is: %first_word(The first word is);
%put The last word is: %last_word(The first word is);
*/
%macro first_word(sentence);
%scan(&sentence,1)
%mend;

%macro last_word(sentence);
%scan(&sentence,-1)
%mend;

%macro second_word(sentence);
%scan(&sentence,2)
%mend;

