# tidytable 0.4.0

* Function rewrite:
  + All functions are now written as `verb.()`
  + `dt_verb()` functions still work, but may be slowly deprecated in future releases
* Functionality improvements:
  + Enhanced selection now works in "by"" calls
  + `filter.()` now works with "by"
  + `pivot_wider.()` can now be used with an aggregation function
  + `pull.()` defaults to last column in a data.table
* New functions:
  + `ifelse.()`
  + `n.()`
  + `nest_by.()` replaces `dt_group_nest()`
  + `row_number.()`
  + `transmute.()`
  + `unnest.()` replaces `dt_unnest_legacy()`
  + Select helper `any_of.()`
* Functions with notable speed improvements:
  + `fill.()`
  + `pivot_longer.()`
  + `unnest.()`

# tidytable 0.3.2

* New class:
  + New `tidytable` class is a data.table subclass with cleaner printing.
  + Operates like a data.table in all other cases.
  + All tidytable functions automatically convert to `tidytable` class in the background.
* New functions:
  + `dt_get_dummies()`: Get dummy variables
  + `dt_separate()`
  + `as_tidytable()`: Convert to data.table/tidytable
  + `is_tidytable()`: Test if an object is a data.table/tidytable
  + `tidytable()`: Construct a data.table/tidytable
* `dt_pivot_longer()` "cols" arg now defaults to `dt_everything()` instead of `NULL`. The result is the same, but is more consistent with other enhanced selection functions
* `.N` now works when using `dt_slice()` & variants
* Function length limit bug no longer occurs when using `~` in `dt_map()`

# tidytable 0.3.1

* Updated `dt()` docs per CRAN instructions
* `dt()` now automatically converts data.frame input to a data.table
* `dt_left_join()` fix so columns are ordered correctly
* Add group by ability to `dt_mutate_across()`
* `dt_mutate()` & `dt_rename()` no longer modify by reference

### Breaking changes
* `dt_unnest_legacy()` drops "keep" argument, but automatically keeps all non-nested columns.
  + New translation is ~5-10x faster than before

# tidytable 0.3.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release
