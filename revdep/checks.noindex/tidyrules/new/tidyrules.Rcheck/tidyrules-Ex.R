pkgname <- "tidyrules"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tidyrules')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addBackquotes")
### * addBackquotes

flush(stderr()); flush(stdout())

### Name: addBackquotes
### Title: Add backquotes
### Aliases: addBackquotes
### Keywords: internal

### ** Examples





cleanEx()
nameEx("as_rulelist.data.frame")
### * as_rulelist.data.frame

flush(stderr()); flush(stdout())

### Name: as_rulelist.data.frame
### Title: as_rulelist method for a data.frame
### Aliases: as_rulelist.data.frame

### ** Examples

rules_df = tidytable::tidytable(rule_nbr = 1:2,
                                LHS      = c("var_1 > 50", "var_2 < 30"),
                                RHS      = c(2, 1)
                                )
as_rulelist(rules_df, estimation_type = "regression")



cleanEx()
nameEx("as_ruleset")
### * as_ruleset

flush(stderr()); flush(stdout())

### Name: as_ruleset
### Title: Get a ruleset from a rulelist
### Aliases: as_ruleset

### ** Examples

model_class_party = partykit::ctree(species ~ .,
                                    data = palmerpenguins::penguins
                                    )
as_ruleset(tidy(model_class_party))




cleanEx()
nameEx("augment.rulelist")
### * augment.rulelist

flush(stderr()); flush(stdout())

### Name: augment.rulelist
### Title: Augment a rulelist
### Aliases: augment.rulelist

### ** Examples

# Examples for augment ------------------------------------------------------
library("magrittr")

# C5 ----
att = modeldata::attrition
set.seed(100)
train_index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)

model_c5 = C50::C5.0(Attrition ~., data = att[train_index, ], rules = TRUE)
tidy_c5  =
  model_c5 %>%
  tidy() %>%
  set_validation_data(att[!train_index, ], "Attrition")

tidy_c5

augment(tidy_c5) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()

# augment with custom aggregator
augment(tidy_c5,output_counts = list(table(Attrition))) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()

# rpart ----
set.seed(100)
train_index = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)

model_class_rpart = rpart::rpart(Species ~ ., data = iris[train_index, ])
tidy_class_rpart  = tidy(model_class_rpart) %>%
  set_validation_data(iris[!train_index, ], "Species")
tidy_class_rpart

model_regr_rpart = rpart::rpart(Sepal.Length ~ ., data = iris[train_index, ])
tidy_regr_rpart  = tidy(model_regr_rpart) %>%
  set_validation_data(iris[!train_index, ], "Sepal.Length")
tidy_regr_rpart

# augment (classification case)
augment(tidy_class_rpart) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()

# augment (regression case)
augment(tidy_regr_rpart) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()

# party ----
pen = palmerpenguins::penguins %>%
  tidytable::drop_na(bill_length_mm)
set.seed(100)
train_index = sample(c(TRUE, FALSE), nrow(pen), replace = TRUE)

model_class_party = partykit::ctree(species ~ ., data = pen[train_index, ])
tidy_class_party  = tidy(model_class_party) %>%
  set_validation_data(pen[!train_index, ], "species")
tidy_class_party

model_regr_party =
  partykit::ctree(bill_length_mm ~ ., data = pen[train_index, ])
tidy_regr_party  = tidy(model_regr_party) %>%
  set_validation_data(pen[!train_index, ], "bill_length_mm")
tidy_regr_party

# augment (classification case)
augment(tidy_class_party) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()

# augment (regression case)
augment(tidy_regr_party) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()

# cubist ----
att         = modeldata::attrition
set.seed(100)
train_index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
cols_att    = setdiff(colnames(att), c("MonthlyIncome", "Attrition"))

model_cubist = Cubist::cubist(x = att[train_index, cols_att],
                              y = att[train_index, "MonthlyIncome"]
                              )

tidy_cubist = tidy(model_cubist) %>%
  set_validation_data(att[!train_index, ], "MonthlyIncome")
tidy_cubist

augment(tidy_cubist) %>%
  tidytable::unnest(augmented_stats, names_sep = "__") %>%
  tidytable::glimpse()




cleanEx()
nameEx("calculate.rulelist")
### * calculate.rulelist

flush(stderr()); flush(stdout())

### Name: calculate.rulelist
### Title: 'calculate' metrics for a rulelist
### Aliases: calculate.rulelist

### ** Examples

library("magrittr")
model_c5  = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
tidy_c5   = tidy(model_c5) %>%
            set_validation_data(modeldata::attrition, "Attrition") %>%
            set_keys(NULL)

# calculate default metrics (classification)
calculate(tidy_c5)

model_rpart = rpart::rpart(MonthlyIncome ~., data = modeldata::attrition)
tidy_rpart  =
  tidy(model_rpart) %>%
  set_validation_data(modeldata::attrition, "MonthlyIncome") %>%
  set_keys(NULL)

# calculate default metrics (regression)
calculate(tidy_rpart)

# calculate default metrics with a custom metric
#' custom function to get cumulative MAE
library("tidytable")
get_cumulative_MAE = function(rulelist, new_data, y_name, weight){

  priority_df =
    rulelist %>%
    select(rule_nbr) %>%
    mutate(priority = 1:nrow(rulelist)) %>%
    select(rule_nbr, priority)

  pred_df =
    predict(rulelist, new_data) %>%
    left_join(priority_df, by = "rule_nbr") %>%
    mutate(weight = local(weight)) %>%
    select(rule_nbr, row_nbr, weight, priority)

  new_data2 =
    new_data %>%
    mutate(row_nbr = 1:n()) %>%
    select(all_of(c("row_nbr", y_name)))

  rmse_till_rule = function(rn){

    if (is.character(rulelist$RHS)) {
      inter_df =
        pred_df %>%
        tidytable::filter(priority <= rn) %>%
        left_join(mutate(new_data, row_nbr = 1:n()), by = "row_nbr") %>%
        left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr") %>%
        nest(.by = c("RHS", "rule_nbr", "row_nbr", "priority", "weight")) %>%
        mutate(RHS = purrr::map2_dbl(RHS,
                                     data,
                                     ~ eval(parse(text = .x), envir = .y)
                                     )
               ) %>%
        unnest(data)
    } else {

      inter_df =
        pred_df %>%
        tidytable::filter(priority <= rn) %>%
        left_join(new_data2, by = "row_nbr") %>%
        left_join(select(rulelist, rule_nbr, RHS), by = "rule_nbr")
    }

    inter_df %>%
      summarise(rmse = MetricsWeighted::mae(RHS,
                                             .data[[y_name]],
                                             weight,
                                             na.rm = TRUE
                                             )
                ) %>%
      `[[`("rmse")
  }

  res = purrr::map_dbl(1:nrow(rulelist), rmse_till_rule)
  return(res)
}

calculate(tidy_rpart,
          metrics_to_exclude = NULL,
          list("cumulative_mae" = get_cumulative_MAE)
          )




cleanEx()
nameEx("plot.rulelist")
### * plot.rulelist

flush(stderr()); flush(stdout())

### Name: plot.rulelist
### Title: Plot method for rulelist
### Aliases: plot.rulelist

### ** Examples

library("magrittr")
att = modeldata::attrition
tidy_c5 =
  C50::C5.0(Attrition ~., data = att, rules = TRUE) %>%
  tidy() %>%
  set_validation_data(att, "Attrition") %>%
  set_keys(NULL)

plot(tidy_c5)




cleanEx()
nameEx("positionSpaceOutsideSinglequotes")
### * positionSpaceOutsideSinglequotes

flush(stderr()); flush(stdout())

### Name: positionSpaceOutsideSinglequotes
### Title: Position of space outside single quotes
### Aliases: positionSpaceOutsideSinglequotes
### Keywords: internal

### ** Examples





cleanEx()
nameEx("predict.rulelist")
### * predict.rulelist

flush(stderr()); flush(stdout())

### Name: predict.rulelist
### Title: 'predict' method for a rulelist
### Aliases: predict.rulelist

### ** Examples

model_c5 = C50::C5.0(species ~.,
                     data = palmerpenguins::penguins,
                     trials = 5,
                     rules = TRUE
                     )
tidy_c5 = tidy(model_c5)
tidy_c5

output_1 = predict(tidy_c5, palmerpenguins::penguins)
output_1 # different rules per 'keys' (`trial_nbr` here)

output_2 = predict(tidy_c5, palmerpenguins::penguins, multiple = TRUE)
output_2 # `rule_nbr` is a list-column of integer vectors




cleanEx()
nameEx("predict.ruleset")
### * predict.ruleset

flush(stderr()); flush(stdout())

### Name: predict.ruleset
### Title: 'predict' method for a ruleset
### Aliases: predict.ruleset

### ** Examples

model_c5 = C50::C5.0(species ~.,
                     data = palmerpenguins::penguins,
                     trials = 5,
                     rules = TRUE
                     )
tidy_c5_ruleset = as_ruleset(tidy(model_c5))
tidy_c5_ruleset

predict(tidy_c5_ruleset, palmerpenguins::penguins)




cleanEx()
nameEx("print.ruleset")
### * print.ruleset

flush(stderr()); flush(stdout())

### Name: print.ruleset
### Title: Print method for ruleset class
### Aliases: print.ruleset

### ** Examples

model_class_party = partykit::ctree(species ~ .,
                                    data = palmerpenguins::penguins
                                    )
as_ruleset(tidy(model_class_party))




cleanEx()
nameEx("prune.rulelist")
### * prune.rulelist

flush(stderr()); flush(stdout())

### Name: prune.rulelist
### Title: 'prune' rules of a rulelist
### Aliases: prune.rulelist

### ** Examples

library("magrittr")
model_c5  = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
tidy_c5   = tidy(model_c5) %>%
            set_validation_data(modeldata::attrition, "Attrition") %>%
            set_keys(NULL)

#' prune with defaults
prune_obj = prune(tidy_c5)
#' note that all other metrics are visible in the print output
prune_obj
plot(prune_obj)
prune_obj$pruned

#' prune with a different stop_expr_string threshold
prune_obj = prune(tidy_c5,
                  stop_expr_string = "relative__cumulative_coverage >= 0.2"
                  )
prune_obj #' as expected, has smaller then 10 rules as compared to default args
plot(prune_obj)
prune_obj$pruned

#' prune with a different stop_expr_string metric
st = "relative__cumulative_overlap <= 0.7 & relative__cumulative_overlap > 0"
prune_obj = prune(tidy_c5, stop_expr_string = st)
prune_obj #' as expected, has smaller then 10 rules as compared to default args
plot(prune_obj)
prune_obj$pruned




cleanEx()
nameEx("removeEmptyLines")
### * removeEmptyLines

flush(stderr()); flush(stdout())

### Name: removeEmptyLines
### Title: Remove empty lines
### Aliases: removeEmptyLines
### Keywords: internal

### ** Examples





cleanEx()
nameEx("reorder.rulelist")
### * reorder.rulelist

flush(stderr()); flush(stdout())

### Name: reorder.rulelist
### Title: Reorder the rules/rows of a rulelist
### Aliases: reorder.rulelist

### ** Examples

library("magrittr")
att = modeldata::attrition
tidy_c5 =
  C50::C5.0(Attrition ~., data = att, rules = TRUE) %>%
  tidy() %>%
  set_validation_data(att, "Attrition") %>%
  set_keys(NULL) %>%
  head(5)

# with defaults
reorder(tidy_c5)

# use 'cumulative_overlap' to break ties (if any)
reorder(tidy_c5, metric = c("cumulative_coverage", "cumulative_overlap"))

# reorder after 2 rules
reorder(tidy_c5, init = 2)




cleanEx()
nameEx("set_keys")
### * set_keys

flush(stderr()); flush(stdout())

### Name: set_keys
### Title: Set keys for a rulelist
### Aliases: set_keys

### ** Examples

model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
tidy_c5 = tidy(model_c5)
tidy_c5 # keys are: "trial_nbr"

tidy_c5[["rule_nbr"]] = 1:nrow(tidy_c5)
new_tidy_c5 = set_keys(tidy_c5, NULL) # remove all keys
new_tidy_c5

new_2_tidy_c5 = set_keys(new_tidy_c5, "trial_nbr") # set "trial_nbr" as key
new_2_tidy_c5

# Note that `tidy_c5` and `new_tidy_c5` are not altered.
tidy_c5
new_tidy_c5




cleanEx()
nameEx("set_validation_data")
### * set_validation_data

flush(stderr()); flush(stdout())

### Name: set_validation_data
### Title: Add 'validation_data' to a rulelist
### Aliases: set_validation_data

### ** Examples

att = modeldata::attrition
set.seed(100)
index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
model_c5 = C50::C5.0(Attrition ~., data = att[index, ], rules = TRUE)

tidy_c5 = tidy(model_c5)
tidy_c5

tidy_c5_2 = set_validation_data(tidy_c5,
                                validation_data = att[!index, ],
                                y_name = "Attrition",
                                weight = 1 # default
                                )
tidy_c5_2
tidy_c5 # not altered




cleanEx()
nameEx("strHead")
### * strHead

flush(stderr()); flush(stdout())

### Name: strHead
### Title: Vectorized semantic equivalent of 'head' for a string
### Aliases: strHead
### Keywords: internal

### ** Examples





cleanEx()
nameEx("strReplaceReduce")
### * strReplaceReduce

flush(stderr()); flush(stdout())

### Name: strReplaceReduce
### Title: Sequential string replace
### Aliases: strReplaceReduce
### Keywords: internal

### ** Examples





cleanEx()
nameEx("strSplitSingle")
### * strSplitSingle

flush(stderr()); flush(stdout())

### Name: strSplitSingle
### Title: String split a string
### Aliases: strSplitSingle
### Keywords: internal

### ** Examples





cleanEx()
nameEx("strTail")
### * strTail

flush(stderr()); flush(stdout())

### Name: strTail
### Title: Vectorized semantic equivalent of tail for a string
### Aliases: strTail
### Keywords: internal

### ** Examples





cleanEx()
nameEx("tidy.C5.0")
### * tidy.C5.0

flush(stderr()); flush(stdout())

### Name: tidy.C5.0
### Title: Get the rulelist from a C5 model
### Aliases: tidy.C5.0

### ** Examples

model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
tidy(model_c5)




cleanEx()
nameEx("tidy.constparty")
### * tidy.constparty

flush(stderr()); flush(stdout())

### Name: tidy.constparty
### Title: Get the rulelist from a party model
### Aliases: tidy.constparty

### ** Examples

pen = palmerpenguins::penguins
model_class_party = partykit::ctree(species ~ ., data = pen)
tidy(model_class_party)
model_regr_party = partykit::ctree(bill_length_mm ~ ., data = pen)
tidy(model_regr_party)




cleanEx()
nameEx("tidy.cubist")
### * tidy.cubist

flush(stderr()); flush(stdout())

### Name: tidy.cubist
### Title: Get the rulelist from a cubist model
### Aliases: tidy.cubist

### ** Examples

att = modeldata::attrition
cols_att    = setdiff(colnames(att), c("MonthlyIncome", "Attrition"))
model_cubist = Cubist::cubist(x = att[, cols_att],
                              y = att[["MonthlyIncome"]]
                              )
tidy(model_cubist)




cleanEx()
nameEx("tidy.rpart")
### * tidy.rpart

flush(stderr()); flush(stdout())

### Name: tidy.rpart
### Title: Get the rulelist from a rpart model
### Aliases: tidy.rpart

### ** Examples

model_class_rpart = rpart::rpart(Species ~ ., data = iris)
tidy(model_class_rpart)

model_regr_rpart = rpart::rpart(Sepal.Length ~ ., data = iris)
tidy(model_regr_rpart)




cleanEx()
nameEx("to_sql_case")
### * to_sql_case

flush(stderr()); flush(stdout())

### Name: to_sql_case
### Title: Extract SQL case statement from a rulelist
### Aliases: to_sql_case

### ** Examples

model_c5 = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
tidy(model_c5)
to_sql_case(tidy(model_c5))



cleanEx()
nameEx("varSpec")
### * varSpec

flush(stderr()); flush(stdout())

### Name: varSpec
### Title: Get variable specification for a Cubist/C5 object
### Aliases: varSpec
### Keywords: internal

### ** Examples

## Not run: 
##D data("attrition", package = "modeldata")
##D cols_att = setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))
##D 
##D cb_att = Cubist::cubist(x = attrition[, cols_att],
##D                         y = attrition[["MonthlyIncome"]]
##D                         )
##D varSpec(cb_att)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
