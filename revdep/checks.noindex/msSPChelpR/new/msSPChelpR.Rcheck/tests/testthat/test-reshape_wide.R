test_that("option timevar_max reduces number of columns", {
  data("us_second_cancer")
  
  t1_ti5 <- us_second_cancer %>% 
    reshape_wide(case_id_var = "fake_id", time_id_var = "SEQ_NUM", timevar_max = 5, datsize = 100)
  t1_ti2 <- us_second_cancer %>% 
    reshape_wide(case_id_var = "fake_id", time_id_var = "SEQ_NUM", timevar_max = 2, datsize = 100)
  
  expect_message(reshape_wide(us_second_cancer, case_id_var = "fake_id", time_id_var = "SEQ_NUM", timevar_max = 2, datsize = 100), 
                 "Wide dataset is limited to  2  cases per id")
  expect_true((ncol(t1_ti5) > ncol(t1_ti2)))
})


test_that("all columns are transposed and correctly named", {
  data("us_second_cancer")
  
  long_names <- paste0(colnames(us_second_cancer), ".1")
  
  wide_names <- us_second_cancer %>% 
    reshape_wide(case_id_var = "fake_id", time_id_var = "SEQ_NUM", timevar_max = 5, datsize = 100) %>%
    colnames()
  
  expect_true(sum(long_names %in% wide_names) == ncol(us_second_cancer) - 2)
})