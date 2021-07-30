# tidytable 0.6.4

#### New functions
* `unnest_longer.()`
* `unnest_wider.()`

#### Functionality improvements
* `bind_rows.()`/`bind_cols.()`
  + Can splice lists of data frames using `!!!`
* `mutate.()`: Can use `glue::glue()` without specifying `.envir`
* `pull.()`: Added a `name` argument
* `separate.()`: Can omit output columns by using `NA` in `into`
* `tidytable()`: Can splice quosures
* `unnest.()`: Added `keep_empty` arg

#### Deprecations
* The following functions have all been deprecated due to the addition of
  `across.()` in v0.6.1
  + `arrange_across.()`
  + `mutate_across.()`
  + `summarize_across.()`

# tidytable 0.6.3

#### New functions
* `enframe.()`
* `first.()`
* `last.()`

#### Functionality improvements
* Subclasses and attributes are now preserved when using `bind_cols`,
`bind_rows`, `group_split`, or joins.
* `complete.()`: Added a `.by` argument
* `expand.()`: Added a `.by` argument
* `mutate.()`: Auto-names unnamed inputs
* `transmute.()`: Auto-names unnamed inputs

#### Bug fixes
* `expand.()` with `nesting.()`
  + Additional double vectors can have negative values (#282)

# tidytable 0.6.2

#### New functions
* `cur_group_id.()`/`cur_group_rows.()`
* `n_distinct.()`
* `nesting.()`

#### Functionality improvements
* `mutate.()`:
  + Added `.keep` argument
  + Added `.before`/`.after` arguments
* `tidytable()`: Added `.name_repair` argument

#### Functions with notable speed improvements
* `filter.()` when `.by` is used

# tidytable 0.6.1

#### New functions
* `across.()`
* `coalesce.()`

#### Functionality improvements
* `group_split.()`: Added a `.named` argument. If `.named = TRUE`, the resulting list will
have names that indicate the group they are a part of.
* `pivot_longer.()`: Can now use `names_to = c(".value", NA)` to drop the id column.

#### Bug fixes
* `if_all.()`/`if_any.()`
  + No longer try to use `.by` columns (#225)
  + Can use `n()` in filtering (#226)
* `pivot_longer.()`
  + No longer converts factor values to character (#202, #234)
  + Preserves column order when using `names_to = c(".value", "id")` (#235)

# tidytable 0.6.0

#### New functions
* `if_all.()`/`if_any.()`

#### Functionality improvements
* `replace_na.()`: Can now replace `NULL` values in list-columns
* Splicing lists using `!!!` now works in `crossing.()` and `expand_grid.()`
* The following tidyverse functions are now automatically converted to their tidytable `verb.()` equivalents when
used inside of tidytable's mutate/arrange/filter/summarize/slice.
  + Functions that previously failed but now work:
    - `n()`
    - `row_number()`
  + Functions that previously worked with tidytable but are now converted to their faster tidytable equivalents:
    - `if_else()`/`ifelse()`
    - `case_when()`
    - `desc()`
    - `replace_na()`
    
#### Bug fixes
* `pivot_longer.()`: Correctly pivots unbalanced datasets when `".value"` is used
in `names_to` (@moutikabdessabour, #212)

#### Functions with notable speed improvements
* `mutate_across.()`
* `mutate_rowwise.()` when `c_across.()` is used
* `slice.()`/`slice_head.()`/`slice_tail.()`/`slice_min.()`/`slice_max.()`

#### Other news
* `tidytable` has dropped the `tibble` package as a dependency and now uses `pillar` instead. 
`tibble` was originally used for its `trunc_mat()` function that enabled tidytables to print like
tibbles do in console. `pillar` is the same package that `tibble` uses in the background.

# tidytable 0.5.9

#### New functions
* `between.()`

#### Functionality improvements
* `as_tidytable()`: Gains `.name_repair` and `.keep_rownames` args
* `count.()`: Added `wt`, `sort` and `name` args. (@moutikabdessabour, #196)
* `mutate.()`: Variables are now updated in order when using ".by" (#166)
* `pivot_longer.()`
  + Added `names_prefix` arg
  + Can now use `".value"` in `names_to`
* `pivot_wider.()`: Can now use `".value"` in `names_glue`
* `separate.()`: Added `convert` arg
* `summarize.()`: Variables are now created in order (#188)

# tidytable 0.5.8

#### Breaking changes
* `pivot_longer.()`: `values_drop_na` is no longer the 5th argument in the function
* `pivot_wider.()`: `values_fn` & `values_fill` args are no longer the 6th & 7th args

#### New functions
* `c_across.()`
* `extract.()`
* `mutate_rowwise.()`
* `nest.()`
* `slice_sample.()`

#### Functionality improvements
* `pivot_longer.()`
  + New args: `names_sep`, `names_pattern`, `names_ptypes`, `names_transform`, `names_repair`,
  `values_ptypes`, `values_transform`, `fast_pivot`
  + `fast_pivot` arg: Faster pivoting. The "names" column will be a factor instead of character.
  Default is set to `FALSE` to match tidyverse semantics. Note: This option sets `variable.factor = TRUE`
  in `data.table::melt()`, which is what leads to it being faster.
* `pivot_wider.()`
  + New args: `names_prefix`, `names_glue`, `names_sort`, `names_repair`

# tidytable 0.5.7

#### Deprecations
* The `dt_verb()` versions of functions have been removed from the package.
They have been deprecated with warnings since v0.5.2

#### Functionality improvements
* `crossing.()`: Now works with data frame inputs (@moutikabdessabour, #143)
* `distinct.()`: Can rename columns (#153)
* `get_dummies.()`: Dummy columns are now sorted in alphabetical order
* `pivot_wider.()`: Added `values_fill` argument

#### Functions with notable speed improvements
* `get_dummies.()`
* `fill.()`
* `slice.()`

#### Bug fixes
* `mutate.()`: Can delete a column using `NULL` when `.by` is provided (#151)
* `mutate_across.()`: Can reference other columns in .fns call (#145)
* `slice.()`: Works when `.by` contains all columns (#158)
* `unnest.()`: Works when the only column is the list column (#144)

# tidytable 0.5.6

#### Breaking changes
* `bind_cols.()`: Name repair uses `vec_as_names()` instead of `vec_as_names_legacy()`

#### Functionality improvements
* `bind_cols.()`: Added `.name_repair` arg
* `unnest.()`: Added `names_sep` and `names_repair` args
* `print()`
  + Added `n`, `width`, and `n_extra` args
  + Now prints like tibbles in all cases (special thanks to @moutikabdessabour)
* `slice.()`: Can now drop specified rows with negative numbers

#### New functions
* `arrange_across.()`
* `case_when.()`
  + The old "case when" translation `case.()` will remain in the package,
  as it is called like `data.table::fcase()` but allows for the `default`
  to be a vector.
* `desc.()`

#### Functions with notable speed improvements
* `slice.()`
  
#### Deprecations
* `unnest.()`: `.keep_all` arg changed to `.drop`

# tidytable 0.5.5

#### Breaking changes
* `bind_rows.()`: Removed `.use_names` and `.fill` args
  + These are now assumed to be `TRUE`, which matches `dplyr` semantics.
* `complete.()`: `.fill` arg renamed to `fill`
* `pull.()`: defaults to `var = -1` instead of `var = NULL`
  + The result is the same when using the default value,
  but will break cases where `var = NULL` was explicitly called.
* `slice.()`: `.by` must be named when slicing by group

#### Functionality improvements
* `tidytable` now integrates better with tidyverse functions through `vctrs`. This means code such as
`vec_cbind(tibble::tibble(x = 1:3), tidytable(y = "foo"))`
will bind the results into a tidytable.
* `complete.()`: Can now append extra values
* `pull.()`: Numerical selection now works
* `slice.()`: Now uses `...` to select which rows to slice

#### New functions
* `semi_join.()`

#### Bug fixes
* `rename.()`: Works when there are spaces in the column names (#109)

#### Functions with notable speed improvements
* `replace_na.()`

# tidytable 0.5.4

#### Breaking changes
* Column names auto-generated by `summarize_across.()` now use a suffix
instead of a prefix

#### Functionality improvements
* `mutate_across.()`: Added `.names` arg to help with naming newly created columns
* `summarize_across.()`: Added `.names` arg to help with naming newly created columns
* `summarize.()`: Added `.sort` arg to optionally sort the resulting data.table

#### Bug fixes
* `transmute.()`: Doesn't drop "by" columns (#98)
* `slice.()`
  + Allows gaps in `rows` arg (#99)
  + Doesn't reorder columns when using `.by` (#101)
* `full_join.()`: No longer returns a keyed data.table (#102)

#### Internal
* Import `tibble`
  + Enables cleaner console printing of tidytables
  + `glimpse()` is now reexported
  + Will be replaced by `pillar` in a later release once `tibble::trunc_mat()` &
  `tibble::glimpse()` are moved there

# tidytable 0.5.3

#### Breaking changes
* `bind_rows.()`
  + `use.names` arg renamed to `.use_names`
  + `fill` arg renamed to `.fill`

#### New functions
* `complete.()`
* `crossing.()`
* `expand.()`
* `expand_grid.()`
* `uncount.()`

#### Functionality improvements
* `replace_na.()`: Now works on `data.frame`/`data.table` inputs as well as vectors

#### Deprecations
* `desc.()` has been deprecated due to incompatibility with character columns

# tidytable 0.5.2

* This version contains general performance improvements

#### Deprecations
* Group by arg `by` has been deprecated for `.by`.
This will allow "by" to be used as a column name in `mutate.()` & `summarize.()` in future releases.
* All `dt_verb()` functions are soft deprecated


#### New functions
* `separate_rows.()`

#### Functionality improvements
* `unnest.()`: Added `.keep_all` arg to keep all list columns that were not unnested

# tidytable 0.5.1

#### New functions
* `summarize_across.()` & `summarise_across.()`

#### Bug fixes:
* Fixed `distinct.()` bug where it wasn't returning unique rows

# tidytable 0.5.0

* `tidytable` now works with quosures
* `tidyselect v0.1.1` compatibility: Updated functions to use `where()`

#### Functionality improvements
* `nest_by.()`: Added `.keep` arg

#### Deprecations
* Old select helpers like `everything.()` are now replaced by normal helpers like `everything()`
* `rename_across/_if/_at/_all` have been superseded by `rename_with.()`

#### Breaking changes
* `group_split.()`: `keep` arg renamed to `.keep`
 
#### New functions
* `lags.()` & `leads.()`

#### Functions with notable speed improvements
* `case.()`

# tidytable 0.4.1

* `tidytable` now utilizes `tidyselect` & `vctrs`

#### Breaking changes
* `get_dummies.()`: `cols` arg defaults to `c(is.character, is.factor)`.
The result is the same, but is more consistent with other enhanced selection functions
* `mutate_across.()`: `.funs` argument renamed to `.fns` to match dplyr 1.0

#### New functions
* `desc.()`
* `unite.()`

#### Functionality improvements
* Paged printing now works in Rmarkdown
* `bind_rows.()`: Does name checking and fills missing
* `distinct.()`: Added `.keep_all` argument
* `fill.()`: Preserves column order when using "by"
* `group_split.()`: Added `keep` arg
* `nest_by.()`
  + Can now unnest multiple columns in one call
  + data.tables in a list column can now have different ncols
  and different column order
* `select.()`: Now allows column renaming
* `transmute.()`: Summary functions (`mean`/`max`/etc.) can now be used

#### Functions with notable speed improvements
* `distinct.()`
* `drop_na.()`
* `relocate.()`

#### Bug fixes
* `group_split.()`: Now returns a list of tidytables instead of data.tables
* `left_join.()`: Now works with "by" columns of different names

# tidytable 0.4.0

#### New syntax
* All functions are now written as `verb.()`
* `dt_verb()` functions still work, but may be slowly deprecated in future releases

#### Functionality improvements
* Enhanced selection now works in "by" calls
* `filter.()` now works with "by"
* `pivot_wider.()` can now be used with an aggregation function
* `pull.()` defaults to last column in a data.table

#### New functions
* `ifelse.()`
* `n.()`
* `nest_by.()` replaces `dt_group_nest()`
* `row_number.()`
* `transmute.()`
* `unnest.()` replaces `dt_unnest_legacy()`
* Select helper `any_of.()`

#### Functions with notable speed improvements
* `fill.()`
* `pivot_longer.()`
* `unnest.()`

# tidytable 0.3.2

#### tidytable class
* New `tidytable` class is a data.table subclass with cleaner printing.
* Operates like a data.table in all other cases.
* All tidytable functions automatically convert to `tidytable` class in the background.

#### New functions
* tidytable constructors:
  + `tidytable()`: Create a data.table/tidytable
  + `as_tidytable()`: Convert to data.table/tidytable
  + `is_tidytable()`: Test if an object is a data.table/tidytable
* `dt_get_dummies()`: Get dummy variables
* `dt_separate()`
* `dt_pivot_longer()` "cols" arg now defaults to `dt_everything()` instead of `NULL`.
The result is the same, but is more consistent with other enhanced selection functions
* `.N` now works when using `dt_slice()` & variants
* Function length limit bug no longer occurs when using `~` in `dt_map()`

# tidytable 0.3.1

* Updated `dt()` docs per CRAN instructions

#### Breaking changes
* `dt_mutate()` & `dt_rename()` no longer modify-by-reference
* `dt_unnest_legacy()` drops "keep" argument, but automatically keeps all non-nested columns.
  + New translation is ~5-10x faster than before

#### Functionality improvements
* `dt()` now automatically converts data.frame input to a data.table
* `dt_mutate_across()`: Added "by" arg

#### Bug fixes
* `dt_left_join()` fix so columns are ordered correctly

# tidytable 0.3.0

* Initial release
