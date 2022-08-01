data("us_second_cancer")

test_that("error thrown for missing variable names if dattype is NULL", {
  expect_error(
    wide_zdata_lung %>%
      sir_byfutime(
        dattype = NULL,
        xbreak_var = "none",
        ybreak_vars = c("t_sublung.1", "t_grading.1"),
        futime_breaks = c(0, 1/12, 2/12, 1, 5, 10, Inf),
        count_var = "count_spc",
        refrates_df = rates,
        calc_total_row = TRUE,
        calc_total_fu = TRUE,
        region_var = NULL,
        age_var = NULL,
        sex_var = NULL,
        year_var = NULL,
        site_var = "t_icdcat.2", #using grouping by second cancer incidence
        futime_var = "p_futimeyrs",
        alpha = 0.05),
    "If dattype is NULL, all variable names for `region_var`, `age_var`, `sex_var`, `year_var`, `site_var` and `futime_var` need to be provided.")
  
})

