#*******************************************************************************
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
#*******************************************************************************

context("test-rulelist")

#### test predict (without setting validation data) ----

pen = palmerpenguins::penguins
model_c5 = C50::C5.0(species ~.,
                     data = pen,
                     trials = 5,
                     rules = TRUE
                     )
tidy_c5 = tidy(model_c5)
tidy_c5

output_1 = predict(tidy_c5, pen)
output_1 # different rules per 'keys' (`trial_nbr` here)

output_2 = predict(tidy_c5, pen, multiple = TRUE)
output_2 # `rule_nbr` is a list-column of integer vectors

test_that("creates a dataframe", {
  expect_is(output_1, "data.frame")
  expect_is(output_2, "data.frame")
})

test_that("check output column types", {
  expect_true(rlang::is_atomic(output_1$rule_nbr))
  expect_true(rlang::is_list(output_2$rule_nbr))
})

test_that("all row_nbr and keys combo exists", {
  all_combos =
    tidytable::expand_grid(row_nbr = 1:nrow(pen),
                           trial_nbr = unique(tidy_c5$trial_nbr)
                           )

  expected_to_be_empty_df =
    output_1 %>%
    distinct(row_nbr, trial_nbr) %>%
    data.table::fsetdiff(all_combos)

  expect_true(nrow(expected_to_be_empty_df) == 0)

  expected_to_be_empty_df =
    output_1 %>%
    distinct(row_nbr, trial_nbr) %>%
    data.table::fsetdiff(all_combos)

  expect_true(nrow(expected_to_be_empty_df) == 0)
})

#### test set_keys ----

test_that("tests for set_keys", {
  att = modeldata::attrition
  model_c5 = C50::C5.0(Attrition ~ ., data = att, rules = TRUE)
  tidy_c5 = tidy(model_c5)

  tidy_c5[["rule_nbr"]] = 1:nrow(tidy_c5)
  new_tidy_c5 = set_keys(tidy_c5, NULL) # remove all keys
  expect_true(is.null(attr(new_tidy_c5, "keys")))

  new_2_tidy_c5 = set_keys(new_tidy_c5, "trial_nbr") # set "trial_nbr" as key
  expect_true(attr(new_2_tidy_c5, "keys") == "trial_nbr")

  # check for no modification
  expect_true(is.null(attr(new_tidy_c5, "keys")))
})
#### test set_validation_data ----
test_that("test setting validation data", {
  att = modeldata::attrition
  set.seed(100)
  index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)
  model_c5 = C50::C5.0(Attrition ~., data = att[index, ], rules = TRUE)

  tidy_c5 = tidy(model_c5)

  tidy_c5_2 = set_validation_data(tidy_c5,
                                  validation_data = att[!index, ],
                                  y_name = "Attrition",
                                  weight = 1 # default
                                  )
  expect_false(is.null(attr(tidy_c5_2, "validation_data")))
  expect_true(is.null(attr(tidy_c5, "validation_data"))) # not altered

})

#### test as_rulelist ----

test_that("test as_rulelist", {
  rules_df = tidytable::tidytable(rule_nbr = 1:2,
                                  LHS      = c("var_1 > 50", "var_2 < 30"),
                                  RHS      = c(2, 1)
                                  )
  output = as_rulelist(rules_df, estimation_type = "regression")
  expect_true(inherits(output, "rulelist"))
})

#### test print ----
test_that("test setting validation data", {
  att = modeldata::attrition
  model_c5 = C50::C5.0(Attrition ~., data = att, rules = TRUE)
  tidy_c5 = tidy(model_c5)

  tidy_c5_2 = set_validation_data(tidy_c5,
                                  validation_data = att,
                                  y_name = "Attrition",
                                  weight = 1 # default
                                  )
  res = print(tidy_c5_2)
  expect_equal(res, tidy_c5_2)

  expect_equal(print(tidy_c5), tidy_c5)

})

#### test plot ----
test_that("test plot", {
  library("magrittr")
  att = modeldata::attrition

  # classification case
  tidy_c5 =
    C50::C5.0(Attrition ~., data = att, rules = TRUE) %>%
    tidy() %>%
    set_validation_data(att, "Attrition") %>%
    set_keys(NULL)

  res = plot(tidy_c5)
  expect_true(inherits(res, "pheatmap"))

  # regression case
  tidy_rpart =
    rpart::rpart(MonthlyIncome ~., data = att) %>%
    tidy() %>%
    set_validation_data(att, "MonthlyIncome") %>%
    set_keys(NULL)

  res = plot(tidy_rpart)
  expect_true(inherits(res, "pheatmap"))
})

#### test augment ----
test_that("test augment", {
  library("magrittr")

  # classification case ----
  att = modeldata::attrition
  set.seed(100)
  train_index = sample(c(TRUE, FALSE), nrow(att), replace = TRUE)

  model_c5 = C50::C5.0(Attrition ~., data = att[train_index, ], rules = TRUE)
  tidy_c5  =
    model_c5 %>%
    tidy() %>%
    set_validation_data(att[!train_index, ], "Attrition")

  output = augment(tidy_c5)
  output_unnested = tidytable::unnest(output,
                                      augmented_stats,
                                      names_sep = "__"
                                      )
  expect_true(inherits(output, "rulelist"))
  expect_true("augmented_stats" %in% colnames(output))
  expect_true(all(c("augmented_stats__support",
                    "augmented_stats__confidence",
                    "augmented_stats__lift"
                    ) %in% colnames(output_unnested)
                  )
              )

  # regression case
  set.seed(100)
  train_index = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)

  model_regr_rpart = rpart::rpart(Sepal.Length ~ ., data = iris[train_index, ])
  tidy_regr_rpart  = tidy(model_regr_rpart) %>%
    set_validation_data(iris[!train_index, ], "Sepal.Length")

  output = augment(tidy_regr_rpart)
  output_unnested = tidytable::unnest(output,
                                      augmented_stats,
                                      names_sep = "__"
                                      )
  expect_true(inherits(output, "rulelist"))
  expect_true("augmented_stats" %in% colnames(output))
  expect_true(all(c("augmented_stats__support",
                    "augmented_stats__RMSE",
                    "augmented_stats__IQR"
                    ) %in% colnames(output_unnested)
                  )
              )

  # augment with custom aggregator
  output = augment(tidy_c5, output_counts = list(table(Attrition)))
  output_unnested = tidytable::unnest(output,
                                      augmented_stats,
                                      names_sep = "__"
                                      )
  expect_true(inherits(output, "rulelist"))
  expect_true("augmented_stats" %in% colnames(output))
  expect_true(all(c("augmented_stats__output_counts"
                    ) %in% colnames(output_unnested)
                  )
              )
})

#### test calculate ----

test_that("test calculate", {
  library("magrittr")

  # classification
  model_c5  = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
  tidy_c5   = tidy(model_c5) %>%
              set_validation_data(modeldata::attrition, "Attrition") %>%
              set_keys(NULL)

  res = calculate(tidy_c5)
  expect_true(inherits(res, "data.frame"))
  class_metrics = c("cumulative_coverage",
                    "cumulative_overlap",
                    "cumulative_accuracy"
                    )
  expect_true(all(class_metrics %in% colnames(res)))

  # regression
  model_rpart = rpart::rpart(MonthlyIncome ~., data = modeldata::attrition)
  tidy_rpart  =
    tidy(model_rpart) %>%
    set_validation_data(modeldata::attrition, "MonthlyIncome") %>%
    set_keys(NULL)

  res = calculate(tidy_rpart)
  expect_true(inherits(res, "data.frame"))
  regr_metrics = c("cumulative_coverage",
                    "cumulative_overlap",
                    "cumulative_RMSE"
                    )
  expect_true(all(regr_metrics %in% colnames(res)))


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

  res = calculate(tidy_rpart,
                  metrics_to_exclude = NULL,
                  list("cumulative_mae" = get_cumulative_MAE)
                  )
  expect_true(inherits(res, "data.frame"))
  custom_regr_metrics = c("cumulative_coverage",
                          "cumulative_overlap",
                          "cumulative_RMSE",
                          "cumulative_mae"
                          )
  expect_true(all(custom_regr_metrics %in% colnames(res)))
})

#### test prune ----

test_that("test prune", {
  library("magrittr")
  model_c5  = C50::C5.0(Attrition ~., data = modeldata::attrition, rules = TRUE)
  tidy_c5   = tidy(model_c5) %>%
              set_validation_data(modeldata::attrition, "Attrition") %>%
              set_keys(NULL)

  #' prune with defaults
  prune_obj = prune(tidy_c5)
  #' note that all other metrics are visible in the print output
  expect_true(inherits(prune_obj, "prune_rulelist"))
  plot(prune_obj)

  #' prune with a different stop_expr_string threshold
  prune_obj = prune(tidy_c5,
                    stop_expr_string = "relative__cumulative_coverage >= 0.2"
                    )
  prune_obj #' as expected, has smaller then 10 rules as compared to default args
  plot(prune_obj)
  prune_obj$pruned

  #' prune with a different stop_expr_string metric
  prune_obj = prune(tidy_c5,
                    stop_expr_string = "relative__cumulative_overlap <= 0.7 & relative__cumulative_overlap > 0"
                    )
  prune_obj #' as expected, has smaller then 10 rules as compared to default args
  plot(prune_obj)
  prune_obj$pruned
})

#### test reorder ----

test_that("test reorder", {
  library("magrittr")
  att = modeldata::attrition
  tidy_c5 =
    C50::C5.0(Attrition ~., data = att, rules = TRUE) %>%
    tidy() %>%
    set_validation_data(att, "Attrition") %>%
    set_keys(NULL) %>%
    head(5)

  # with defaults
  res = reorder(tidy_c5)
  expect_true(inherits(res, "rulelist"))

  # use 'cumulative_overlap' to break ties (if any)
  res = reorder(tidy_c5, metric = c("cumulative_coverage", "cumulative_overlap"))
  expect_true(inherits(res, "rulelist"))

  # reorder after 2 rules
  res = reorder(tidy_c5, init = 2)
  expect_true(inherits(res, "rulelist"))
})
