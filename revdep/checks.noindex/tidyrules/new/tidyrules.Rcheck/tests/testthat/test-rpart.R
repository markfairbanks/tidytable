################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

context("test-rpart")

# setup some models ----
data("attrition", package = "modeldata")

# classification test
attrition_class =
  attrition %>%
  tidytable::mutate(
    tidytable::across(tidytable::where(is.ordered), ~ factor(.x, ordered = F))
    ) %>%
  tidytable::mutate(Attrition = factor(Attrition, levels = c("No", "Yes")))

rpart_att    = rpart::rpart(Attrition ~ ., data = attrition_class)
tr_att_class = tidy(rpart_att)

# regression test
attrition_reg =
  attrition %>%
  tidytable::mutate(
    tidytable::across(tidytable::where(is.ordered), ~ factor(.x, ordered = F))
    ) %>%
  tidytable::select(-Attrition)

rpart_att_reg = rpart::rpart(MonthlyIncome ~ ., data = attrition_reg)
tr_att_reg    = tidy(rpart_att_reg)

# BreastCancer
data(BreastCancer, package = "mlbench")
bc = BreastCancer %>%
  dplyr::select(-Id) %>%
  dplyr::mutate_if(is.ordered, function(x) x = factor(x,ordered = F))

bc_1m = rpart::rpart(Class ~ ., data = bc)

tr_bc_1 = tidy(bc_1m)

# variables with spaces
bc2 = bc

colnames(bc2)[which(colnames(bc2) == "Cell.size")] = "Cell size"
colnames(bc2)[which(colnames(bc2) == "Cell.shape")] = "Cell shape"

bc_2m = rpart::rpart(Class ~ ., data = bc2)

tr_bc_2 = tidy(bc_2m)

# function to check whether a rule is filterable
ruleFilterable = function(rule, data){
  dplyr::filter(data, eval(parse(text = rule)))
}

# function to check whether all rules are filterable
allRulesFilterable = function(tr, data){
  parse_status = sapply(
    tr[["LHS"]],
    function(arule){
      trydf = try(ruleFilterable(arule, data), silent = TRUE)
      if (nrow(trydf) == 0) print(arule)
      inherits(trydf, "data.frame")
    }
  )
  return(parse_status)
}

# test for error while ordered features are present ----
test_that("check error",{
  expect_error(tidy(rpart_att_1))})

# test output type ----

test_that("creates rulelist", {
  expect_is(tr_att_class, "rulelist")
  expect_is(tr_bc_1, "rulelist")
  expect_is(tr_bc_2, "rulelist")
  expect_is(tr_att_reg, "rulelist")
})

# test NA ----
test_that("Are NA present", {
  expect_false(anyNA(tr_att_class))
  expect_false(anyNA(tr_bc_1))
  expect_false(anyNA(tr_bc_2))
  expect_false(anyNA(tr_att_reg))
})

# test parsable ----
test_that("rules are parsable", {
  expect_true(all(allRulesFilterable(tr_att_class, attrition)))
  expect_true(all(allRulesFilterable(tr_bc_1, bc)))
  expect_true(all(allRulesFilterable(tr_bc_2, bc2)))
  expect_true(all(allRulesFilterable(tr_att_reg,attrition)))
})

