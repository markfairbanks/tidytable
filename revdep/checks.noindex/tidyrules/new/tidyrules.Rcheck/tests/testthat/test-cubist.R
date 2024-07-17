################################################################################
# This is the part of the 'tidy' R package hosted at
# https://github.com/talegari/tidy with GPL-3 license.
################################################################################

context("test-cubist")

# setup some models ----
# attrition
data("attrition", package = "modeldata")
cols_att = setdiff(colnames(attrition), c("MonthlyIncome", "Attrition"))

cb_att =
  Cubist::cubist(x = attrition[, cols_att],
                 y = attrition[["MonthlyIncome"]]
                 )
tr_att = tidy(cb_att)

# attrition with commitees
cb_att_2 =
  Cubist::cubist(x = attrition[, cols_att],
                 y = attrition[["MonthlyIncome"]],
                 committees = 7
                 )
tr_att_2 = tidy(cb_att_2)

# ames housing
ames   = AmesHousing::make_ames()
cb_ames = Cubist::cubist(x = ames[, setdiff(colnames(ames), c("Sale_Price"))],
                          y = log10(ames[["Sale_Price"]]),
                          committees = 3
                          )
tr_ames = tidy(cb_ames)


# column name has a space in it
data("Boston", package = "MASS")
boston_2 = Boston
names(boston_2)[6] = "r m"
names(boston_2)[13] = "l stat"
cb_boston = Cubist::cubist(x = boston_2[, -14], y = boston_2[[14]])
tr_boston = tidy(cb_boston)

# function to check whether a rule is filterable
ruleFilterable = function(rule, data){
  dplyr::filter(data, eval(parse(text = rule)))
}

# function to check whether all rules are filterable
allRulesFilterable = function(tr, data){
  parse_status = sapply(
    tr[["LHS"]]
    , function(arule){
        trydf = try(ruleFilterable(arule, data)
                   , silent = TRUE
                   )
        if(nrow(trydf) == 0){
          #print(arule)
        }
        inherits(trydf, "data.frame") && (nrow(trydf) > 0)
      }
    )
  return(parse_status)
}

# evaluate RHS
evalRHS = function(tr, data){

  message(deparse(substitute(data)))

  with_RHS = sapply(tr[["RHS"]],
    function(x){
      try(data %>%
            dplyr::mutate(RHS_ = eval(parse(text = x))) %>%
            dplyr::pull(RHS_) %>%
            is.numeric()
        , silent = TRUE
      )}
    , USE.NAMES = FALSE
    )

  # print(which(!with_RHS))
  return(all(with_RHS))
}

# test output type ----

test_that("creates tibble", {
  expect_is(tr_att, "rulelist")
  expect_is(tr_att_2, "rulelist")
  expect_is(tr_ames, "rulelist")
  expect_is(tr_boston, "rulelist")
})

# test NA ----
test_that("Are NA present", {
  expect_false(anyNA(tr_att))
  expect_false(anyNA(tr_att_2))
  expect_false(anyNA(tr_ames))
  expect_false(anyNA(tr_boston))
})

# test parsable ----
test_that("rules are parsable", {
  expect_true(all(allRulesFilterable(tr_att, attrition)))
  expect_true(all(allRulesFilterable(tr_att_2, attrition)))
  expect_true(all(allRulesFilterable(tr_ames, ames)))
  expect_true(all(allRulesFilterable(tr_boston, boston_2)))
})

# rhs is computable ----
test_that("rhs is computable", {
  expect_true(evalRHS(tr_att, attrition))
  expect_true(evalRHS(tr_att_2, attrition))
  expect_true(evalRHS(tr_ames, ames))
  expect_true(evalRHS(tr_boston, boston_2))
})

