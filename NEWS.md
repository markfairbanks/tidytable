# tidytable 0.5.0

* `tidytable` now works with quosures
* `tidyselect v0.1.1` compatibility: Updated functions to use `where()`
* Deprecations:
  + Old select helpers like `everything.()` are now replaced by normal helpers like `everything()`
  + `rename_across/_if/_at/_all` have been superseded by `rename_with.()`
* Breaking changes:
  + `group_split.()`: `keep` arg renamed to `.keep`
* New functions:
  + `lags.()` & `leads.()`
* Functionality improvements:
  + `nest_by.()`: Added `.keep` arg
* Functions with notable speed improvements:
  + `case.()`

# tidytable 0.4.1

* `tidytable` now utilizes `tidyselect` & `vctrs`
* Breaking changes:
  + `get_dummies.()`: `cols` arg defaults to `c(is.character, is.factor)`.
  The result is the same, but is more consistent with other enhanced selection functions
  + `mutate_across.()`: `.funs` argument renamed to `.fns` to match dplyr 1.0
* New functions:
  + `desc.()`
  + `unite.()`
* Functionality improvements:
  + Paged printing now works in Rmarkdown
  + `bind_rows.()`: Does name checking and fills missing
  + `distinct.()`: Added `.keep_all` argument
  + `fill.()`: Preserves column order when using "by"
  + `group_split.()`: Added `keep` arg
  + `nest.()`
    - Can now unnest multiple columns in one call
    - data.tables in a list column can now have different ncols
    and different column order
  + `select.()`: Now allows column renaming
  + `transmute.()`: Summary functions (`mean`/`max`/etc.) can now be used
* Functions with notable speed improvements:
  + `distinct.()`
  + `drop_na.()`
  + `relocate.()`
* Bug fixes:
  + `group_split.()`: Now returns a list of tidytables instead of data.tables
  + `left_join.()`: Now works with "by" columns of different names

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
