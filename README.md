# SAS
Some of my SAS macros

## variable_macros

Macros for dealing with variables in SAS datasets.
***
```
%print_vars(some_dataset)
```
Provides just the variables part of a proc contents. 
***
```
%list_vars(some_dataset)
```
Returns a list of variables in a dataset that you might use as part of your code. You might choose to manipulate the string that gets returned, for example by adding or removing certain words. Can be useful in certain proc sql commands where the full list of variables is required. 
***
```
%var_exist(some_dataset , some_variable)
```
Returns 1 or 0. 
***
```
%var_type(some_dataset , some_variable)
```
Returns N or C. 
***
```
%var_length(some_dataset , some_variable)
```
Returns (memory) length of variable. 
***
```
%max_length(some_dataset , some_variable)
```
Returns maximum actual length of a variable (not the length of the memory assigned for the variable). Useful for helping you to decide which variables can be safely cropped to save memory.
***

```
%rename_var(some_dataset , old_var= , new_var=)
```
A handy macro for changing variable names. Uses ```proc datasets``` and so is efficient.

## Running Thru List

This is a macro for carrying out another macro over and over again on a list of things (e.g. a sequence of similar datasets).
A nice feature is that it outputs a progress bar to the log as well as the estimated time remaining. 

Suppose you had a list of datasets called ```SALES_2010```, ```SALES_2011```, ```SALES_2012```, etc., and you wanted to pass them through a macro you have already created called ```process_sales```.

You could simply write:
```
%process_sales(SALES_2010);
%process_sales(SALES_2011);
%process_sales(SALES_2012);
```
A problem here is that you're hard-coding the years in. If you have lots of macros in your process flow then you'll need to specify the years in lots of places, making updates tricky.

```running_thru_list``` allows you to replace this with:
```
%running_thru_list(SALES_2010 SALES_2011 SALES_2012 , process_sales);
```

The main advantage of this is that it allows you to define the list in one location (like at the top of your process) rather than hard-coding the specific years throughout your code. E.g.
```
%let list_of_sales_files = SALES_2010 SALES_2011 SALES_2012;

%running_thru_list(&list_of_sales_files , the_first_macro);
%running_thru_list(&list_of_sales_files , the_second_macro);
%running_thru_list(&list_of_sales_files , the_third_macro);
```
You could also just define the years and derive the list of files using ```%add_prefix()```:

```
%let list_of_sales_year = 2010 2011 2012;

%running_thru_list(%add_prefix(&list_of_sales_year , _SALES) , the_first_macro);
%running_thru_list(%add_prefix(&list_of_sales_year , _SALES) , the_second_macro);
%running_thru_list(%add_prefix(&list_of_sales_year , _SALES) , the_third_macro);
```
