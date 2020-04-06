
Table of Contents
=================

  * [List Macros](#ls)
    * [Idea](#jl)
    * [Features](#ml)
  * [Installation](#vm)


# SAS
Some of my SAS macros

**[List macros][list-macros]**

**[Joining Lists][jl]**

**[manipulating_macro_lists][ml]**

**[variable_macros][vm]**

**[running_thru_list][rtl]**

**[tell_me_about][tma]**


## List macros

Macros for generating lists.
***
```SAS
%seq(x, y)
```
Generates and ascending list from one number to another.
***
```SAS
%add_prefix(<some list here>, prefix)
```
Returns a list but with a string concatenated to start of each word. Useful for converting one kind of list into another.
E.g.
```SAS
%put %add_prefix(2010 2011 2012, accounts_);
```
... returns: ```accounts_2010 accounts_2011 accounts_2012```
***
```SAS
%list_vars(some_dataset)
```
Returns a list of variables in a dataset that you might use as part of your code. You might choose to manipulate the string that gets returned, for example by adding or removing certain words. Can be useful in certain proc sql commands where the full list of variables is required. 
***
```SAS
%add_commas(<some list here>)
```
Returns a list but with commas between each word. Handy for proc sql and certain other functions.
***
```SAS
%add_keep(<some list here> , var_1 var_2)
```
Returns a list but with ```(keep = var_1 var_2)``` after each word. Handy for when you're using ```set``` with a list of datasets. A bit more efficient than specifying these on the output dataset.
E.g.
```SAS
set %add_keep(team_1 team_2 team_3 , name age );
```
... becomes: ```set team_1 (keep = name age) team_2 (keep = name age) team_3 (keep = name age);```

## Joining Lists
[jl]:README.md#joining_lists
Boolean operations on multiple lists. These are especially powerful when used in combination with ```%list_vars()``` and other macros for modifying lists.
***
```SAS
%union(<first list> <second list> <third list>)
```
Returns the combined set of lists with duplicates removed.
E.g.
```SAS
%put %union(id age sales id job name id name height)
```
... returns: ```id age sales job name height```
***
```SAS
%intersect_lists(<first list> , <second list>)
```
Returns elements which appear in both lists. Very useful for ```keep``` and ```drop``` statements, or where a variable has an inconsistent name across datasets. 
E.g.
```SAS
%put %intersect_lists(id age sales job name , id name height)
```
... returns: ```id name```
***
```SAS
%left_anti_join(<first list> , <second list>)
```
Returns the first list with the exclusion of any elements which appear in the second. Again, useful for ```keep```, ```drop``` and ```set``` statements. 
E.g.
```SAS
%put %left_anti_join(id age job name , id name height)
```
... returns: ```age job```

## Manipulating Macro Lists
[ml]:README.md#manipulating_lists
Tools for querying and adjusting lists.
***
```SAS
%find_word( <some list> , word)
```
Returns ```n```, where ```word``` is the nth word in ```<some list>```, and 0 if ```word``` does not appear in ```<some list>```. Differs from ```FINDW()``` which returns character position. Finds whole words and is case insensitive.

***
```SAS
%first_word(<some list>)
%last_word(<some list>)
```
Simply returns the first or last word in a list. 
***
```SAS
%words_beginning_with(<some list> , phrase)
%words_ending_with(<some list> , phrase)
%words_containing(<some list> , phrase)
```
Returns all words from a list which begin/end/contain ```phrase```. Useful for dropping or keeping a raft of similar variables. 
***
```SAS
%remove_word(<some list> , word)
```
Returns the list with ```word``` removed. Case insensitive and matches whole word only. 
***
```SAS
%remove_nth_word(<some list> , n)
```
Returns the list with the nth word removed. 
***
```SAS
%replace_word(<some list> , old_word , new_word)
```
Returns the list with ```old_word``` replaced by ```new_word```. Case insensitive and matches whole word only.

## variable_macros
[vm]:README.md#variable_macros
Macros for dealing with variables in SAS datasets.
***
```SAS
%print_vars(some_dataset)
```
Provides just the variables part of a proc contents. 

***
```SAS
%var_exist(some_dataset , some_variable)
```
Returns 1 or 0. 
***
```SAS
%var_type(some_dataset , some_variable)
```
Returns N or C. 
***
```SAS
%var_length(some_dataset , some_variable)
```
Returns (memory) length of variable. 
***
```SAS
%max_length(some_dataset , some_variable)
```
Returns maximum actual length of a variable (not the length of the memory assigned for the variable). Useful for helping you to decide which variables can be safely cropped to save memory.
***

```SAS
%rename_var(some_dataset , old_var= , new_var=)
```
A handy macro for changing variable names. Uses ```proc datasets``` and so is efficient.

## running_thru_list
[rtl]:README.md#running_thru_list

Very often you have to apply a macro over and over again on a sequence of similar datasets. The macro ```running_thru_list``` does this for you, given a list and the name of the macro that you want to apply to that list. 

A nice feature is that it outputs a progress bar to the log and an estimation for what time the whole process will be finished at. 

Suppose you had a list of datasets called ```SALES_2010```, ```SALES_2011```, ```SALES_2012```, etc., and you wanted to pass them through a macro you have already created called ```process_sales```.

You could simply write:
```SAS
%process_sales(SALES_2010);
%process_sales(SALES_2011);
%process_sales(SALES_2012);
```
A problem here is that you're hard-coding the years in. If you have lots of macros in your process flow then you'll need to specify the years in lots of places, making updates tricky.

```running_thru_list``` allows you to replace this with:
```SAS
%running_thru_list(SALES_2010 SALES_2011 SALES_2012 , process_sales);
```

The main advantage of this is that it allows you to define the list in one location (like at the top of your process) rather than hard-coding the specific years throughout your code. E.g.
```SAS
%let list_of_sales_files = SALES_2010 SALES_2011 SALES_2012;

%running_thru_list(&list_of_sales_files , the_first_macro);
%running_thru_list(&list_of_sales_files , the_second_macro);
%running_thru_list(&list_of_sales_files , the_third_macro);
```
You could also just define the years and derive the list of files using ```%add_prefix()```:

```SAS
%let list_of_sales_years = 2010 2011 2012;

%running_thru_list(%add_prefix(&list_of_sales_years , _SALES) , the_first_macro);
%running_thru_list(%add_prefix(&list_of_sales_years , _SALES) , the_second_macro);
%running_thru_list(%add_prefix(&list_of_sales_years , _SALES) , the_third_macro);
```


## tell_me_about
[tma]:README.md#tell_me_about

This macro is useful for where you have a series of datasets that are similar, but may have small changes from one to the next. E.g. maybe the length of a variable changes, maybe the name of one variable changes slightly from one dataset to the next, maybe an indictor is of type 'char' in one dataset and type 'numeric' in another. 

This macro creates three output datasets which compare the stucture of a series of datasets. 
* **name_record** - gives the variable names in each dataset.
* **type_record** - gives the type (C or N) of each variable in each dataset.
* **length_record** - gives the length of each variable in each dataset.

