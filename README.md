# SAS
Some of my SAS macros

## variable_macros

Macros for dealing with variables in SAS datasets.

```
%print_vars(some_dataset)
```
Provides just the variables part of a proc contents. 

```
%list_vars(some_dataset)
```
Returns a list of variables in a dataset that you might use as part of your code. You might choose to manipulate the string that gets returned, for example by adding or removing certain words. Can be useful in certain proc sql commands where the full list of variables is required. 

```
%var_exist(some_dataset , some_variable)
```
Returns 1 or 0. 

```
%var_type(some_dataset , some_variable)
```
Returns N or C. 

```
%var_length(some_dataset , some_variable)
```
Returns (memory) length of variable. 

```
%max_length(some_dataset , some_variable)
```
Returns maximum actual length of a variable (not the length of the memory assigned for the variable). Useful for helping you to decide which variables can be safely cropped to save memory.


```
%rename_var(some_dataset , old_var= , new_var=)
```
A handy macro for changing variable names. Uses ```proc datasets``` and so is efficient.
