################################################################################
# This is the part of the 'tidyrules' R package hosted at
# https://github.com/talegari/tidyrules with GPL-3 license.
################################################################################

context("test-c5")

# setup some models ----
# attrition
data("attrition", package = "modeldata")

c5_att = C50::C5.0(Attrition ~ ., data = attrition, rules = TRUE)
tr_att = tidy(c5_att)

# attrition with trials
c5_att_2 = C50::C5.0(Attrition ~ ., data = attrition
                      , trials = 7, rules = TRUE)
tr_att_2 = tidy(c5_att_2)

# ames housing
# ames has some space in Sale_Type levels
ames   = AmesHousing::make_ames()
ames
cb_ames = C50::C5.0(MS_SubClass ~ ., data = ames
                     , trials = 3, rules = TRUE)
tr_ames = tidy(cb_ames)

# column name has a space in it
ames   = AmesHousing::make_ames()
ames_2 = ames
colnames(ames_2)[which(colnames(ames_2) == "Bldg_Type")] = "Bldg Type"
colnames(ames_2)[which(colnames(ames_2) == "House_Style")] = "House Style"
c5_ames_2 = C50::C5.0(MS_SubClass ~ ., data = ames_2, rules = TRUE)
tr_ames_2 = tidy(c5_ames_2)

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
          # print(arule)
        }
        inherits(trydf, "data.frame")
      }
    )
  return(parse_status)
}

# test output type ----

test_that("creates tibble", {
  expect_is(tr_att, "rulelist")
  expect_is(tr_att_2, "rulelist")
  expect_is(tr_ames, "rulelist")
  expect_is(tr_ames_2, "rulelist")
})

# test NA ----
test_that("Are NA present", {
  expect_false(anyNA(tr_att))
  expect_false(anyNA(tr_att_2))
  expect_false(anyNA(tr_ames))
  expect_false(anyNA(tr_ames_2))
})

# test parsable ----
test_that("rules are parsable", {
  expect_true(all(allRulesFilterable(tr_att, attrition)))
  expect_true(all(allRulesFilterable(tr_att_2, attrition)))
  expect_true(all(allRulesFilterable(tr_ames, ames)))
  expect_true(all(allRulesFilterable(tr_ames_2, ames_2)))
})
