## Preface
### How to import
```SAS
filename fileURL url 'https://raw.githubusercontent.com/brendanjodowd/SAS/master/brendan_macros.sas';
%include fileURL;
filename fileURL; 
```

### What get's returned?

Some macros produce datasets or html output as a standalone statement. These are introduced with a semicolon below to indicate that they work on their own, e.g. ```show_datatsets;```

Other macros return some kind of a string which is intended for use as code within some other kind of statement like a data step. These are not presented with a semicolon below, e.g. ```%list_vars() ```. You can usually check what they return by preceding with a ```%put``` statement.

### What do you mean by a 'list'?

A list here refers to a sequence of words that will be used as code. It could be a list of datasets, variables, years, etc. Macros designed for use with lists are useful when you building a series of datasets or when you're trying to write flexible code. They are not designed for working with character type variables, since they will probably pass the name of the variable rather than the string contained within it.

***
Table of Contents
=================

  * [Generating Lists](#generating-lists)
      * [seq](#seq)
      * [add_prefix](#add_prefix)
      * [list_vars](#list_vars)
      * [add_commas](#add_commas)
      * [add_keep](#add_keep)
  * [Joining Lists](#joining-lists)
      * [union](#union)
      * [intersect_lists](#intersect_lists)
      * [left_anti_join](#left_anti_join)
      * [pairwise_join](#pairwise_join)
  * [Manipulating Words in Lists](#manipulating-words-in-lists)
      * [find_word](#find_word)
      * [first_word, last_word](#first_word-last_word)
      * [words_beginning_with, -ending -containg](#words_beginning_with)
      * [remove_word](#remove_word)
      * [remove_nth_word](#remove_nth_word)
      * [replace_word](#replace_word)
  * [Manipulating String Variables](#manipulating-string-variables)
      * [as_num, as_char](#as_num-as_char)
      * [extract_num](#extract_num)
      * [crop_left, crop_right](#crop_left-crop_right)
  * [Checking variables in datasets](#checking-variables-in-datasets)
      * [print_vars](#print_vars)
      * [var_exist](#var_exist)
      * [var_type](#var_type)
      * [var_length](#var_length)
      * [var_fmt](#var_fmt)
      * [right_case](#right_case)
      * [max_length](#max_length)
  * [Managing Datasets and Variables](#managing-datasets-and-variables)
      * [show_datasets](#show_datasets)
      * [dataset_exist](#dataset_exist)
      * [delete_dataset](#delete_dataset)
      * [rename_var](#rename_var)
      * [rename_if_exist](#rename_if_exist)
      * [add_label](#add_label)
      * [drop_format](#drop_format)
  * [Managing macros and macro variables](#managing-macros-and-macro-variables)
      * [show_macros](#show_macros)
      * [is_int](#is_int)
      * [is_blank](#is_blank)
  * [Handy abbreviations](#handy-abbreviations)
      * [proc freq](#proc-freq)
      * [proc sort](#proc-sort)
  * [Checking counts and duplicates](#checking-counts-and-duplicates)
      * [unique](#unique)
      * [check_duplicates](#check_duplicates)
      * [abort_if_duplicates](#abort_if_duplicates)
      * [abort_unequal_duplicates](#abort_unequal_duplicates)
  * [running_thru_list](#running_thru_list)
  * [tell_me_about](#tell_me_about)
      


## Generating Lists
### seq
```SAS
%seq(x, y)
```
Generates an ascending list from one number to another.
### add_prefix
```SAS
%add_prefix(<some list here>, prefix)
```
Returns a list but with a string concatenated to start of each word. Useful for converting one kind of list into another.
E.g.
```SAS
%put %add_prefix(2010 2011 2012, accounts_);
```
... returns: ```accounts_2010 accounts_2011 accounts_2012```
### list_vars
```SAS
%list_vars(some_dataset)
%list_num_vars(some_dataset)
%list_char_vars(some_dataset)
```
Returns a list of variables in a dataset that you might use as part of your code. You might choose to manipulate the string that gets returned, for example by adding or removing certain words. Can be useful in certain proc sql commands where the full list of variables is required. 
### add_commas
```SAS
%add_commas(<some list here>)
```
Returns a list but with commas between each word. Handy for proc sql and certain other functions.
### add_keep
```SAS
%add_keep(<some list here> , var_1 var_2)
```
Returns a list but with ```(keep = var_1 var_2)``` after each word. Handy for when you're using ```set``` with a list of datasets. A bit more efficient than specifying these on the output dataset.
E.g.
```SAS
set %add_keep(team_1 team_2 team_3 , name age );
```
... becomes: ```set team_1 (keep = name age) team_2 (keep = name age) team_3 (keep = name age);```

## Joining lists
Boolean operations on multiple lists. These are especially powerful when used in combination with ```%list_vars()``` and other macros for modifying lists.
### union
```SAS
%union(<first list> <second list> <third list>)
```
Returns the combined set of lists with duplicates removed.
E.g.
```SAS
%put %union(id age sales id job name id name height);
```
... returns: ```id age sales job name height```
### intersect_lists
```SAS
%intersect_lists(<first list> , <second list>)
```
Returns elements which appear in both lists. Very useful for ```keep``` and ```drop``` statements, or where a variable has an inconsistent name across datasets. 
E.g.
```SAS
%put %intersect_lists(id age sales job name , id name height);
```
... returns: ```id name```
### left_anti_join
```SAS
%left_anti_join(<first list> , <second list>)
```
Returns the first list with the exclusion of any elements which appear in the second. Again, useful for ```keep```, ```drop``` and ```set``` statements. 
E.g.
```SAS
%put %left_anti_join(id age job name , id name height);
```
... returns: ```age job```

### pairwise_join
```SAS
%pairwise_join(<first list> , <second list> , sep=)
```
Returns the lists joined together in a pairwise fashion, with each pair separated by ```sep``` (null by default). Lists must be the same length. 
E.g. ```%pairwise_join(hand foot tree , bag ball house)``` returns ```handbag football treehouse```.

And ```%pairwise_join(library_2010 library_2012 , file_2010 file_2012 , sep=. )``` returns ```library_2010.file2010 library_2012.file_2012```.



## Manipulating Words in Lists
Tools for querying and adjusting lists.
### find_word
```SAS
%find_word( <some list> , word)
```
Returns ```n```, where ```word``` is the nth word in ```<some list>```, and 0 if ```word``` does not appear in ```<some list>```. Differs from ```FINDW()``` which returns character position. Finds whole words and is case insensitive.

### first_word last_word
```SAS
%first_word(<some list>)
%last_word(<some list>)
```
Simply returns the first or last word in a list. 
### words_beginning_with 
```SAS
%words_beginning_with(<some list> , phrase)
%words_ending_with(<some list> , phrase)
%words_containing(<some list> , phrase)
```
Returns all words from a list which begin/end/contain ```phrase```. Useful for dropping or keeping a raft of similar variables. 
#### remove_word
```SAS
%remove_word(<some list> , word)
```
Returns the list with ```word``` removed. Case insensitive and matches whole word only. 
### remove_nth_word
```SAS
%remove_nth_word(<some list> , n)
```
Returns the list with the nth word removed. 
### replace_word
```SAS
%replace_word(<some list> , old_word , new_word)
```
Returns the list with ```old_word``` replaced by ```new_word```. Case insensitive and matches whole word only.

## Manipulating String Variables
### as_num as_char
```SAS
%as_num( <some character variable> )
%as_char( <some numeric variable> )
```
A neat little expression for quickly making a new numeric/character variable using a character/numeric variable as input. Negates need for put/input in majority of cases. 
E.g.
```SAS
/*inside data step*/
    numeric_age = %as_num(character_age);
```
### extract_num
```SAS
%extract_num( <some string with a number in it> )
```
A stronger version of ```%as_num()```, extracting a number from among other characters in a variable. E.g.:
```SAS
 code = %extract_num( "(2) Arts and Humanities" )
```
...gives ```code``` a value of 2.

### crop_left crop_right
```SAS
%crop_left( string_variable , "phrase" )
%crop_right( string_variable , "phrase" )
```
Similar to LEFT() and RIGHT() in Excel. `crop_left` returns a string up to (and excluding) the first instance of `"phrase"` in `string_variable`. 
`crop_right` returns the string after the first appearance of `"phrase"`. 


## Checking variables in datasets

### print_vars
```SAS
%print_Vars(dataset_name);
```
Prints out a list of variables (inc. type and length) in a short, handy command.

### var_exist
```SAS
%var_exist(dataset_name , variable_name)
```
Returns '1' or '0' depending on whether `variable_name` appears in `dataset_name`. Useful for case-based handling of datasets. Also used in some other macros. 

### var_type
```SAS
%var_type(dataset_name , variable_name)
```
Returns 'C' or 'N' depending on whether `variable_name` is of type character or numeric. Useful for case-based handling of datasets.

### var_length
```SAS
%var_type(dataset_name , variable_name)
```
Returns (memory) length of `variable_name`. Useful for case-based handling of datasets.

### var_fmt
```SAS
%var_fmt(dataset_name , variable_name)
```
Returns the format of `variable_name` in `dataset_name`.

### right_case
```SAS
%right_case(dataset_name , variable_name)
```
Returns the word `variable_name` again, but with letters capitalised in the same way that they are in the dataset. Used within some other macros to ensure that case-insensitive inputs do not alter the capitalisation of variable names as they are stored. 

### max_length
```SAS
%max_length(dataset_name , some_char_variable);
```
This outputs a line to the log which tells you the maximum string length of the variable `some_char_variable`. Useful when you want to reduce the memory available to a variable, but you're not sure if doing so will crop strings. 

## Managing Datasets and Variables

### show_datasets
```SAS
%show_datasets;
```
Produces a handy table showing all the datasets in the WORK library, including their size, and numbers of rows and columns.
### dataset_exist
```SAS
%dataset_exist(dataset_name)
```
Logical test to see if a dataset exists. Used in many other macros. 

### delete_dataset
```SAS
%delete_dataset(dataset_name);
```
Deletes a dataset. You can also pass a list of datasets, or use in conjunction with colon (:) to delete datasets starting with a common string.

### rename_var
```SAS
%rename_var(dataset_name , old_var= , new_var=);
```
A handy macro for changing variable names. Uses ```proc datasets``` and so is fairly efficient.
### rename_if_exist
```SAS
%rename_if_exist(dataset_name , old_var= , new_var=);
```
Same as previous, but only renames if the old_var exists. Does not return an error if it doesn't exist. Handy if you're dealing with an inconsistent series of datasets. 
### add_label
```SAS
%add_label(dataset_name , variable_name , label , warn=YES);
```
Adds a label to a variable. WARN is set to yes by default, meaning that you get a warning in the log if the variable doesn't exist. Set it to NO if you don't want a warning (I.e., apply label in the case that the variable exists).
### drop_format 
```SAS
%drop_format(dataset_name , variable_name);
```
Removes a format from a variable. Handy if datasets come in with a weird format. 

## Managing macros and macro variables
### show_macros
```SAS
%show_macros;
```
Prints a list of all the macros that have been defined so far. 

### is_int
```SAS
%is_int(<macro variable>)
```
Logical test to check if a macro variable can be used as an integer. 
### is_blank
```SAS
%is_blank(<macro variable>)
```
Logical test to check if a macro variable is blank or not. Useful for creating checks within your own macros. I think I got this from Lex Jansen

## Handy abbreviations
### proc freq
```SAS
%pf(dataset_name , var_1);
%pf(dataset_name , var_1*var_2);
```
Abbreviation for `proc freq`. Displays only counts when a 2D table is requested (`nocol norow nopercent` options).
### proc sort
```SAS
%ps(dataset_name , var_1 var_2);
```
Abbreviation for `proc sort`. Pass any number of variables. 

## Checking counts and duplicates
### unique
```SAS
%unique(dataset_name , variable_name);
```
Based on the `unique()` function in R. Outputs frequency counts for each `variable_name` to the log. Also states number of unique entries for `variable_name`. Does not produce frequency counts if there more than 100 unique values. 

### check_duplicates
```SAS
%check_duplicates(dataset_name , variable_name);
```
This outputs a two-column table to log. It gives a frequency count OF a frequency count for `variable_name`. Why would you want this? Well you can see easily whether every value for `variable_name` appears once, for example, or you can see whether every value for `variable_name` appears ten times. I find it useful when applied to some kind of id variable, and I expect each id to appear exactly n times. 

### abort_if_duplicates
```SAS
%abort_if_duplicates(dataset_name , variable_name);
```
This returns `%abort();` if any value for `variable_name` appears more than once. A useful failsafe.

### abort_unequal_duplicates
```SAS
%abort_if_duplicates(dataset_name , variable_name);
```
This returns `%abort();` if a frequency count over `variable_name` returns more than one unique frequency. A useful failsafe if you're building a dataset that should have exactly ten entries (for example) for each unique value of `variable_name`. 


## running_thru_list
[rtl]:README.md#running_thru_list
```SAS
%running_thru_list(2010 2011 2012 2013 , some_macro);

/* ...is equivalent to... */

%some_macro(2010);
%some_macro(2011);
%some_macro(2012);
%some_macro(2013);
```
Repeats a macro over a list, and produces a progress indicator to the log which includes an estimate of the remaining time. 

Example of log output:
```SAS
Current time is  9:26:31
Remaining time is 3 minutes and 50 seconds , will be finished at  9:30:21
Duration of last run was 94 % of the average
|#######################75%############------------|
```
The list that you use could be the name of a series of datasets, it could be a list of years, names, months, etc. A good idea is to define the list as a macro variable so that you can operate a series of macros to a list that is defined in only one location. You can use a series of years in combination with `%add_prefix()`, for example, to operate on a series of datasets. E.g.

```SAS
%let years_to_process = 2010 2011 2012 2013;
%running_thru_list( %add_prefix( &years_to_process, SALES_ ) , some_macro);

/* ...is equivalent to... */

%some_macro(SALES_2010);
%some_macro(SALES_2011);
%some_macro(SALES_2012);
%some_macro(SALES_2013);
```

## tell_me_about
[tma]:README.md#tell_me_about
```SAS
%tell_me_about( <some list of datasets> );
```
This macro is useful for investigating a series of inconsistently structured datasets, e.g. the length or name of a variable changes from one month to the next. It produces three tables which compare the stucture of the series of datasets. 
* **name_record** - gives the variable names in each dataset.
* **type_record** - gives the type (C or N) of each variable in each dataset.
* **length_record** - gives the length of each variable in each dataset.

It will name the columns in each of these three tables according to the names of the datasets that you provide. It uses filenames only provided they are different, or library names otherwise. 

If you provide a single dataset, then only one table is created with all information in three columns. 
