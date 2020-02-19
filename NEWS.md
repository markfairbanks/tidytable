# tidytable 0.3.1 (in development)

* Updated `dt()` docs per CRAN instructions
* `dt()` now automatically converts data.frame input to a data.table
* `dt_left_join()` fix so columns are ordered correctly
* Add group by ability to `dt_mutate_across()`
* `dt_mutate()` & `dt_rename()` no longer modify by reference

### Breaking changes
* `dt_unnest_legacy()` drops "keep" argument, but automatically keeps all non-nested columns.
  + New translation is ~10x faster than before

# tidytable 0.3.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release
