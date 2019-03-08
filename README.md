# SAS
Some of my SAS macros

[variable_macros][vm]


## variable_macros
[vm]:SAS#variable_macros
Macros for dealing with variables in SAS datasets.
***
```SAS
%print_vars(some_dataset)
```
Provides just the variables part of a proc contents. 
***
```SAS
%list_vars(some_dataset)
```
Returns a list of variables in a dataset that you might use as part of your code. You might choose to manipulate the string that gets returned, for example by adding or removing certain words. Can be useful in certain proc sql commands where the full list of variables is required. 
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

## Running Thru List

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
