test_that("custom variable name for status_var works", {
  data("us_second_cancer")
  
  #prep step - make wide data as this is the required format
  t1_ti <- us_second_cancer %>%
                     msSPChelpR::reshape_wide_tidyr(., case_id_var = "fake_id", 
                     time_id_var = "SEQ_NUM", timevar_max = 10) %>%
                  dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                        !is.na(t_site_icd.2)   ~ "SPC developed",
                                                        TRUE ~ NA_character_)) %>%
                  dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                               TRUE ~ 0)) %>%
    msSPChelpR::pat_status(., 
                        fu_end = "2017-12-31", 
                        dattype = "seer", 
                        status_var = "other_var_name", 
                        life_var = "p_alive.1", 
                        spc_var = NULL, 
                        birthdat_var = "datebirth.1", 
                        lifedat_var = "datedeath.1",
                        use_lifedatmin = FALSE, 
                        check = TRUE, 
                        as_labelled_factor = FALSE)
  
  expect_true(("other_var_name" %in% colnames(t1_ti)))
 
})