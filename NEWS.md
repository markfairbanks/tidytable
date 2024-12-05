# tidytable 0.11.2

#### Bug fixes
* Empty `dt()` works with no warning, #824

# tidytable 0.11.1

#### Functionality improvements
* `pmap()` now preserves names (#809)
* Package developers no longer need to define `.datatable.aware = TRUE` when
  using `tidytable` in their package (#269)

#### Bug fixes
* Attempting to rename columns using `group_by()` now leads to an error (#799)
* `pmap()` family works with data frame inputs (#803)
* `filter()` properly handles when comparing to `NA` when `.by` is used (#812)
* `paged.print` has been removed since it was breaking console printing (#810)

# tidytable 0.11.0

#### Functionality improvements
* `distinct()` now works on data frames with list columns (#773)
* `pivot_wider()`: Gains `unused_fn` argument (#698)

#### Deprecations
* `verb.()` functions have been removed

#### Bug fixes
* `count()` has a more helpful error message when `pick()` is used (#778)
* `unnest(keep_empty = TRUE)` preserves vectors of length 0 (#783)

#### Functions with notable speed improvements
* `distinct()`

# tidytable 0.10.2

* Patch release to pass CRAN checks for future R release

# tidytable 0.10.1

#### New functions
* `reframe()`

#### Bug fixes
* `case_when()`: `.default` is used when calculating a common ptype (#751)
* `pivot_wider()`: Works correctly with dates (#759)

#### Functions with notable speed improvements
* `case_match()`: When comparing input vector to a length 1 literal

# tidytable 0.10.0

#### Deprecations
* `verb.()` functions are now deprecated (e.g. `mutate.()`). Users should now use
  the `verb()` versions of functions.

#### New functions
* `tribble()`

#### Functionality improvements
* `nest()`: Gains `.by` and `.key` args
* `tidytable()`: Auto-names unnamed inputs
* `slice_*()` family: `by` arg added to match `dplyr` semantics.
  `.by` can still be called by the user instead of `by`.
* `dt()`: Can use `let()` to add columns even on older versions of `data.table`

#### Bug fixes
* `expand_grid()`: Can use "unique" or "sorted" as column names (#718)

#### Breaking changes
* `slice_head()`/`_tail()`: `.by` arg must be explicitly named when slicing by group

#### Functions with notable speed improvements
* `mutate()`: When overwriting existing columns on data frames with a high
  number of columns (>1000)

# tidytable 0.9.2

#### New functions
* `cross_join()`
* `group_cols()`
* `map_vec()`
* `pick()`
* `separate_longer_delim()`
* `separate_wider_delim()`
* `separate_wider_regex()`

#### Functionality improvements
* `separate()`: Can now handle when too many or too few new names are
  specified in `into` arg (#666)
* `unnest_longer()`: Gains `keep_empty` arg
  
#### Bug fixes
* `separate()`: Can overwrite separated column without removal (#680)
* `na_if()`: Properly replaces `NA`s when `y` is a vector (#689)
* `across()`: Anonymous functions are properly translated (#699)
* `pivot_wider()`: `names_sort = FALSE` works (#705)

#### Deprecations
* Using `by = character()` in joins is now deprecated. Users should instead use `cross_join()`.

# tidytable 0.9.1

#### New functions
* `nest_join()`
* Ranking functions:
  * `min_rank()`
  * `dense_rank()`
  * `percent_rank()`
  * `cume_dist()`

#### Functionality improvements
* `%in%` falls back to `base::'%in%'` when input types aren't compatible
  with `vec_in()` (@krterberg, #632)
* `relocate()`: Can rename columns that are moved
* Joins: Can now do cross joins by specifying `by = character()`
* `group_by()`: Gains `.add` argument
* `ungroup()`: Gains `...` arguments
* Printing of grouped tidytables now shows grouping variables

#### Bug fixes
* `row_number()`: Works correctly on 0-row data frame when overwriting existing column (#639)
* `slice_head()`/`slice_tail()`: Properly slice on 0-row data frame (#642)

#### Functions with notable speed improvements
* `fill()`

# tidytable 0.9.0

#### Dotless functions!
* `tidytable` now exports dotless versions of all functions (e.g. `arrange()`/`mutate()`/etc.).
* `verb.()` syntax is still available to users for backwards compatibility.
  * Users can use both `tidytable` and `dplyr` by simply loading
    `dplyr` _after_ `tidytable`, as the `verb.()` functions won't be overwritten by `dplyr`.

#### New functions
* `dplyr`-style interface to grouping
  * `group_by()`/`ungroup()`
  * `group_vars()`
  * `is_grouped_df()`
  * `rowwise()`
* `add_tally()`/`tally()`
* `case_match()`

#### Functionality improvements
* `summarize()`: Gains `.unpack` argument
* `pivot_longer()`: Can pass a single ptype or function to
  `values_ptypes`/`values_transform`/`names_ptypes`/`names_transform` args.
* `unnest_longer()`/`unnest_wider()`: Can pass a single ptype or function to
  `ptype`/`transform` args.

#### Bug fixes
* `tidytable::'%in%'` dispatches to `base::'%in%'` when comparing with a list (#563)
* `pivot_wider()`: Works with column names with spaces (#569)
* `pivot_wider()`: `names_glue="{.value}_{somecolumn}"` assigns column names in
  correct order (@Darxor, #579)
* `left_join()`: Works when y matching columns in `by` is a non matching column of x (#625)

# tidytable 0.8.1

#### New Functions
* `consecutive_id.()`
* `if_else.()`
  * Note: Alternate syntax `ifelse.()` (introduced in v0.4.0) will remain in the package
* `%in%`

#### Functionality improvements
* `arrange.()`: Can use `.env` inside arrange expressions
* `case_when.()`: Gains `.default`/`.ptype`/`.size` args
* `coalesce.()`: Gains `.ptype`/`.size` args
* `relocate.()`: Now properly handles multiple columns selected in `.before` or `.after`
* `slice_min.()`/`slice_max.()`: Gain `with_ties` argument
  
#### Bug Fixes
* Nested calls to `across.()` are handled properly (#505)
* `across.()`: Can namespace functions in `.fns` arg (#511)
* `as_tidytable()`: Can keep row names when converting a matrix (#527)
* `unnest.()`: Handles empty data frames (@roboton, #530)
* `nth.()`: Extracts list elements (#534)
* `arrange.()`: Properly sorts `NA`s (#541)

#### Deprecations
* `arrange_across.`/`mutate_across.`/`summarize_across.` are now defunct. They have been
  deprecated with warnings since v0.6.4 (Jul 2021). Users must now use `across.()` inside
  `arrange.()`/`mutate.()`/`summarize.()`.

# tidytable 0.8.0

#### New functions
* `na_if.()`

#### Functionality improvements
* `expand_grid.()`: Works with data frame inputs
* `first.()`/`last.()`/`nth.()`: Gain `na_rm` arg
* `mutate_rowwise.()`: Gains `.keep`, `.before`, and `.after` args
* `tidytable()`: Auto-unpacks unnamed data frame inputs

#### Breaking changes
* `count.()`: Default name is now `n` instead of `N` to match dplyr semantics

#### Bug fixes
* `bind_cols.()`: Correctly handles lists (#446)

#### Functions with notable speed improvements
* `arrange.()`
* `case_when.()`: Faster when conditions evaluate to `NA`
* `group_split.()`
* `left_join.()`: Faster when `keep = FALSE` (the default)
* `select.()`
* `uncount.()`

#### Other notes
* `tidytable` no longer directly depends on `lifecycle`

# tidytable 0.7.2

#### New functions
* `pmap.()`

#### Functionality improvements
* `summarize.()`: Now sorts by the grouping variables when `.by` is used.
* `dt()`: Experimental support for tidy evaluation
* `as_tidytable()`: Now defaults to `.name_repair = "unique"` to relax restrictions on creating new tidytables

#### Functions with notable speed improvements
* `dt()`: Faster when adding new columns or updating existing columns
* `as_tidytable()`: Faster when converting `data.frame` or `list` objects to a tidytable
* `get_dummies.()`

#### Bug fixes
* `across.()`: `.cols` arg can find environment variables in custom functions (#389)
* Joins: Duplicate columns are treated properly when `keep = FALSE` (#397)
* Can use anonymous functions inside `map` functions inside `mutate.()` (#402)

# tidytable 0.7.1

#### New functions
* `add_count.()`
* `cur_column.()`
* `cur_data.()`

#### Bug fixes
* `unite.()`: New column is always placed before the first united column in the data frame,
regardless of order provided.
* Can use `.data` pronoun inside `desc()` in `arrange.()` (#371)

# tidytable 0.7.0

#### Functionality improvements
* `left`/`right`/`inner`/`full` joins gain `suffix` and `keep` args (#354)
* `rename.()`: Can now rename columns by position (#361)
* `unite.()`: The new column is placed before united columns to match tidyr behavior

#### New functions
* `new_tidytable()`

# tidytable 0.6.7

#### Functionality improvements
* `.data` and `.env` pronouns now work in `tidytable` functions
* `across.()`: Works inside of a named `mutate.()`. Useful with `rowSums`/`rowMeans` (#346)
* `first.()`/`last.()`: Gain a `default` arg
* `mutate.()`: Can use `stringr::str_glue()` without specifying `.envir`
* `replace_na.()`: Checks that `replace` arg only uses columns that exist in the data frame

#### New functions
* `nth.()`

#### Bug fixes
* `mutate.()`: Can assign to the same column when `.by = character(0)` (#332)

# tidytable 0.6.6

#### Functionality improvements
* `between()` is now auto-translated to `between.()` when used inside `tidytable` functions
* `%notin%` now uses `%chin%` on character vectors

#### Bug fixes
* `across.()`: Can pass extra arguments to a list of functions (#319)

# tidytable 0.6.5

#### New functions
* `fread.()`
* `fwrite.()`

#### Bug fixes
* Traceback error messages no longer unnecessarily print the full data frame (#305)

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
