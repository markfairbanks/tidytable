################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

context("test-party")

# setup some models ----
data("penguins", package = "palmerpenguins")

model_party_cl = partykit::ctree(species ~ .,data = penguins)
model_party_cl
tidy(model_party_cl)

model_party_re = partykit::ctree(bill_length_mm ~ .,
                                 data = penguins
                                 )
model_party_re
tidy(model_party_re)

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

# test output type ----

test_that("creates rulelist", {
  expect_is(tidy(model_party_cl), "rulelist")
  expect_is(tidy(model_party_re), "rulelist")
})

# test parsable ----
test_that("rules are parsable", {
  expect_true(all(allRulesFilterable(tidy(model_party_cl), penguins)))
  expect_true(all(allRulesFilterable(tidy(model_party_re), penguins)))
})

