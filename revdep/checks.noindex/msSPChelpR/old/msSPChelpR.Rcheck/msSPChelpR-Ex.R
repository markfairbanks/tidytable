pkgname <- "msSPChelpR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('msSPChelpR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("asir")
### * asir

flush(stderr()); flush(stdout())

### Name: asir
### Title: Calculate age-standardized incidence rates
### Aliases: asir

### ** Examples

#load sample data
data("us_second_cancer")
data("standard_population")
data("population_us")

#make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    #only use sample
                    dplyr::filter(as.numeric(fake_id) < 200000) %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 2)
                    
#create count variable
usdata_wide <- usdata_wide %>%
                    dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                    TRUE ~ 0))
 
#remove cases for which no reference population exists
usdata_wide <- usdata_wide %>%
                    dplyr::filter(t_yeardiag.2 %in% c("1990 - 1994", "1995 - 1999", "2000 - 2004",
                                                       "2005 - 2009", "2010 - 2014"))
                    

#now we can run the function
msSPChelpR::asir(usdata_wide,
      dattype = "seer",
      std_pop = "ESP2013",
      truncate_std_pop = FALSE,
      futime_src = "refpop",
      summarize_groups = "none",
      count_var = "count_spc",
      refpop_df = population_us,
      region_var = "registry.1", 
      age_var = "fc_agegroup.1",
      sex_var = "sex.1",
      year_var = "t_yeardiag.2", 
      site_var = "t_site_icd.2",
      pyar_var = "population_pyar")





cleanEx()
nameEx("calc_futime")
### * calc_futime

flush(stderr()); flush(stdout())

### Name: calc_futime
### Title: Calculate follow-up time per case until end of follow-up
###   depending on pat_status - tidyverse version
### Aliases: calc_futime

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#prep step - create patient status variable
usdata_wide <- usdata_wide %>%
                  msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
                                         status_var = "p_status", life_var = "p_alive.1",
                                         birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
 
#now we can run the function
msSPChelpR::calc_futime(usdata_wide, 
                        futime_var_new = "p_futimeyrs", 
                        fu_end = "2017-12-31",
                        dattype = "seer", 
                        time_unit = "years",
                        status_var = "p_status",
                        lifedat_var = "datedeath.1", 
                        fcdat_var = "t_datediag.1", 
                        spcdat_var = "t_datediag.2")




cleanEx()
nameEx("calc_futime_tt")
### * calc_futime_tt

flush(stderr()); flush(stdout())

### Name: calc_futime_tt
### Title: Calculate follow-up time per case until end of follow-up
###   depending on pat_status - tidytable version
### Aliases: calc_futime_tt

### ** Examples

#load sample data
data("us_second_cancer")

#make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#prep step - create patient status variable
usdata_wide <- usdata_wide %>%
                  msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
                                         status_var = "p_status", life_var = "p_alive.1",
                                         birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
 
#now we can run the function
msSPChelpR::calc_futime_tt(usdata_wide, 
                        futime_var_new = "p_futimeyrs", 
                        fu_end = "2017-12-31",
                        dattype = "seer", 
                        time_unit = "years",
                        status_var = "p_status",
                        lifedat_var = "datedeath.1", 
                        fcdat_var = "t_datediag.1", 
                        spcdat_var = "t_datediag.2")




cleanEx()
nameEx("calc_refrates")
### * calc_refrates

flush(stderr()); flush(stdout())

### Name: calc_refrates
### Title: Calculate age-, sex-, cohort-, region-specific incidence rates
###   from a cohort
### Aliases: calc_refrates

### ** Examples

#load sample data
data("us_second_cancer")
data("population_us")

us_second_cancer %>%
  #create variable to indicate to be counted as case
  dplyr::mutate(is_case = 1) %>%
  #calculate refrates - warning: these are not realistic numbers, just showing functionality
  calc_refrates(dattype = "seer", , count_var = "is_case", refpop_df = population_us,
               region_var = "registry", age_var = "fc_agegroup", sex_var = "sex", 
               site_var = "t_site_icd")



cleanEx()
nameEx("ir_crosstab")
### * ir_crosstab

flush(stderr()); flush(stdout())

### Name: ir_crosstab
### Title: Calculate crude incidence rates and crosstabulate results by
###   break variables
### Aliases: ir_crosstab

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#prep step - create patient status variable
usdata_wide <- usdata_wide %>%
                  msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
                                         status_var = "p_status", life_var = "p_alive.1",
                                         birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
 
#now we can run the function
usdata_wide <- usdata_wide %>%
                 msSPChelpR::calc_futime(., 
                        futime_var_new = "p_futimeyrs", 
                        fu_end = "2017-12-31",
                        dattype = "seer", 
                        time_unit = "years",
                        status_var = "p_status",
                        lifedat_var = "datedeath.1", 
                        fcdat_var = "t_datediag.1", 
                        spcdat_var = "t_datediag.2")
                    
#for example, you can calculate incidence and summarize by sex and registry
msSPChelpR::ir_crosstab(usdata_wide,
      dattype = "seer",
      count_var = "count_spc",
      xbreak_var = "none",
      ybreak_vars = c("sex.1", "registry.1"),
      collapse_ci = FALSE,
      add_total = "no",
      add_n_percentages = FALSE,
      futime_var = "p_futimeyrs",
      alpha = 0.05)





cleanEx()
nameEx("ir_crosstab_byfutime")
### * ir_crosstab_byfutime

flush(stderr()); flush(stdout())

### Name: ir_crosstab_byfutime
### Title: Calculate crude incidence rates and cross-tabulate results by
###   break variables; cumulative FU-times as are used as xbreak_var
### Aliases: ir_crosstab_byfutime

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    #only use sample
                    dplyr::filter(as.numeric(fake_id) < 200000) %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 2)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#prep step - create patient status variable
usdata_wide <- usdata_wide %>%
                  msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
                                         status_var = "p_status", life_var = "p_alive.1",
                                         birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
 
#now we can run the function
usdata_wide <- usdata_wide %>%
                 msSPChelpR::calc_futime(., 
                        futime_var_new = "p_futimeyrs", 
                        fu_end = "2017-12-31",
                        dattype = "seer", 
                        time_unit = "years",
                        status_var = "p_status",
                        lifedat_var = "datedeath.1", 
                        fcdat_var = "t_datediag.1", 
                        spcdat_var = "t_datediag.2")
                    
#for example, you can calculate incidence and summarize by sex and registry
msSPChelpR::ir_crosstab_byfutime(usdata_wide,
      dattype = "seer",
      count_var = "count_spc",
      futime_breaks = c(0, .5, 1, 5, 10, Inf),
      ybreak_vars = c("sex.1", "registry.1"),
      collapse_ci = FALSE,
      add_total = "no",
      futime_var = "p_futimeyrs",
      alpha = 0.05)





cleanEx()
nameEx("pat_status")
### * pat_status

flush(stderr()); flush(stdout())

### Name: pat_status
### Title: Calculate patient status at specific end of follow-up -
###   tidyverse version
### Aliases: pat_status

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#now we can run the function
msSPChelpR::pat_status(usdata_wide, 
                       fu_end = "2017-12-31", 
                       dattype = "seer", 
                       status_var = "p_status", 
                       life_var = "p_alive.1", 
                       spc_var = NULL, 
                       birthdat_var = "datebirth.1", 
                       lifedat_var = "datedeath.1",
                       use_lifedatmin = FALSE, 
                       check = TRUE, 
                       as_labelled_factor = FALSE)
                       



cleanEx()
nameEx("pat_status_tt")
### * pat_status_tt

flush(stderr()); flush(stdout())

### Name: pat_status_tt
### Title: Calculate patient status at specific end of follow-up -
###   tidytable version
### Aliases: pat_status_tt

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#now we can run the function
msSPChelpR::pat_status_tt(usdata_wide, 
                       fu_end = "2017-12-31", 
                       dattype = "seer", 
                       status_var = "p_status", 
                       life_var = "p_alive.1", 
                       spc_var = NULL, 
                       birthdat_var = "datebirth.1", 
                       lifedat_var = "datedeath.1",
                       use_lifedatmin = FALSE, 
                       check = TRUE, 
                       as_labelled_factor = FALSE)
                       



cleanEx()
nameEx("renumber_time_id")
### * renumber_time_id

flush(stderr()); flush(stdout())

### Name: renumber_time_id
### Title: Renumber the time ID per case (i.e. Tumor sequence)
### Aliases: renumber_time_id

### ** Examples


data(us_second_cancer)
us_second_cancer %>%
 #only select first 10000 rows so example runs faster
 dplyr::slice(1:10000) %>%
 msSPChelpR::renumber_time_id(new_time_id_var = "t_tumid",
                             dattype = "seer",
                             case_id_var = "fake_id")
                             



cleanEx()
nameEx("renumber_time_id_tt")
### * renumber_time_id_tt

flush(stderr()); flush(stdout())

### Name: renumber_time_id_tt
### Title: Renumber the time ID per case (i.e. Tumor sequence) - tidytable
###   version
### Aliases: renumber_time_id_tt

### ** Examples


data(us_second_cancer)
us_second_cancer %>%
 #only select first 10000 rows so example runs faster
 dplyr::slice(1:10000) %>%
 msSPChelpR::renumber_time_id_tt(new_time_id_var = "t_tumid",
                             dattype = "seer",
                             case_id_var = "fake_id")




cleanEx()
nameEx("reshape_long")
### * reshape_long

flush(stderr()); flush(stdout())

### Name: reshape_long
### Title: Reshape dataset to long format - stats::reshape version
### Aliases: reshape_long

### ** Examples


data(us_second_cancer)

#prep step - reshape wide a sample of 10000 rows from us_second_cancer
usdata_wide_sample <- msSPChelpR::reshape_wide(us_second_cancer,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM", 
                         timevar_max = 2,
                         datsize = 10000)

#now we can reshape long again
msSPChelpR::reshape_long(usdata_wide_sample,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM")





cleanEx()
nameEx("reshape_long_tidyr")
### * reshape_long_tidyr

flush(stderr()); flush(stdout())

### Name: reshape_long_tidyr
### Title: Reshape dataset to wide format - tidyr version
### Aliases: reshape_long_tidyr

### ** Examples


data(us_second_cancer)

#prep step - reshape wide a sample of 10000 rows from us_second_cancer
usdata_wide_sample <- msSPChelpR::reshape_wide(us_second_cancer,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM", 
                         timevar_max = 2,
                         datsize = 10000)

#now we can reshape long again
msSPChelpR::reshape_long_tidyr(usdata_wide_sample,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM")





cleanEx()
nameEx("reshape_long_tt")
### * reshape_long_tt

flush(stderr()); flush(stdout())

### Name: reshape_long_tt
### Title: Reshape dataset to wide format - tidytable version
### Aliases: reshape_long_tt

### ** Examples


data(us_second_cancer)

#prep step - reshape wide a sample of 10000 rows from us_second_cancer
usdata_wide_sample <- msSPChelpR::reshape_wide(us_second_cancer,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM", 
                         timevar_max = 2,
                         datsize = 10000)

#now we can reshape long again
msSPChelpR::reshape_long_tt(usdata_wide_sample,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM")





cleanEx()
nameEx("reshape_wide")
### * reshape_wide

flush(stderr()); flush(stdout())

### Name: reshape_wide
### Title: Reshape dataset to wide format
### Aliases: reshape_wide

### ** Examples


data(us_second_cancer)

msSPChelpR::reshape_wide(us_second_cancer,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM", 
                         timevar_max = 2,
                         datsize = 10000)




cleanEx()
nameEx("reshape_wide_tidyr")
### * reshape_wide_tidyr

flush(stderr()); flush(stdout())

### Name: reshape_wide_tidyr
### Title: Reshape dataset to wide format - tidyr version
### Aliases: reshape_wide_tidyr

### ** Examples


data(us_second_cancer)

msSPChelpR::reshape_wide_tidyr(us_second_cancer,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM", 
                         timevar_max = 2,
                         datsize = 10000)




cleanEx()
nameEx("reshape_wide_tt")
### * reshape_wide_tt

flush(stderr()); flush(stdout())

### Name: reshape_wide_tt
### Title: Reshape dataset to wide format - tidytable version
### Aliases: reshape_wide_tt

### ** Examples


data(us_second_cancer)

msSPChelpR::reshape_wide_tt(us_second_cancer,
                         case_id_var = "fake_id", 
                         time_id_var = "SEQ_NUM", 
                         timevar_max = 2,
                         datsize = 10000)




cleanEx()
nameEx("sir_byfutime")
### * sir_byfutime

flush(stderr()); flush(stdout())

### Name: sir_byfutime
### Title: Calculate standardized incidence ratios with custom grouping
###   variables stratified by follow-up time
### Aliases: sir_byfutime

### ** Examples

#There are various preparation steps required, before you can run this function.
#Please refer to the Introduction vignette to see how to prepare your data
## Not run: 
##D usdata_wide %>%
##D   sir_byfutime(
##D         dattype = "seer",
##D         ybreak_vars = c("race.1", "t_dco.1"),
##D         xbreak_var = "none",
##D         futime_breaks = c(0, 1/12, 2/12, 1, 5, 10, Inf),
##D         count_var = "count_spc",
##D         refrates_df = us_refrates_icd2,
##D         calc_total_row = TRUE,
##D         calc_total_fu = TRUE,
##D         region_var = "registry.1",
##D         age_var = "fc_agegroup.1",
##D         sex_var = "sex.1",
##D         year_var = "t_yeardiag.1",
##D         site_var = "t_site_icd.1", #using grouping by second cancer incidence
##D         futime_var = "p_futimeyrs",
##D         alpha = 0.05)
##D         
## End(Not run)




cleanEx()
nameEx("sir_ratio")
### * sir_ratio

flush(stderr()); flush(stdout())

### Name: sir_ratio
### Title: Calculate Ratio of two SIRs or SMRs
### Aliases: sir_ratio sir_ratio_lci sir_ratio_uci

### ** Examples

#provide the two expected and observed count to get the ratio of SIRs/SMRs
msSPChelpR::sir_ratio(o1 = 2140, o2 = 3158, e1 = 1993, e2 = 2123)

#calculate lower confidence limit
msSPChelpR::sir_ratio_lci(o1 = 2140, o2 = 3158, e1 = 1993, e2 = 2123, alpha = 0.05)

#calculate upper confidence limit
msSPChelpR::sir_ratio_uci(o1 = 2140, o2 = 3158, e1 = 1993, e2 = 2123, alpha = 0.05)

#functions can be easily used inside dplyr::mutate function
library(dplyr)
test_df <- data.frame(sir_oth = c(1.07, 1.36, 0.96), 
                  sir_smo = c(1.49, 1.81, 1.41),
                  observed_oth = c(2140, 748, 1392),
                  expected_oth = c(1993, 550, 1443),
                  observed_smo = c(3158, 744, 2414),
                  expected_smo = c(2123, 412, 1711))

test_df %>%
  mutate(smo_ratio = sir_ratio(observed_oth, observed_smo, expected_oth, expected_smo),
         smo_ratio_lci = sir_ratio_lci(observed_oth, observed_smo, expected_oth, expected_smo),
         smo_ratio_uci = sir_ratio_uci(observed_oth, observed_smo, expected_oth, expected_smo))



cleanEx()
nameEx("summarize_sir_results")
### * summarize_sir_results

flush(stderr()); flush(stdout())

### Name: summarize_sir_results
### Title: Summarize detailed SIR results
### Aliases: summarize_sir_results

### ** Examples

#There are various preparation steps required, before you can run this function.
#Please refer to the Introduction vignette to see how to prepare your data
## Not run: 
##D summarize_sir_results(.,
##D     summarize_groups = c("region", "age", "year", "race"),
##D     summarize_site = TRUE,
##D     output = "long",  output_information = "minimal",
##D     add_total_row = "only",  add_total_fu = "no",
##D     collapse_ci = FALSE,  shorten_total_cols = TRUE,
##D     fubreak_var_name = "fu_time", ybreak_var_name = "yvar_name",
##D     xbreak_var_name = "none", site_var_name = "t_site",
##D     alpha = 0.05
##D     )
##D     
## End(Not run)
    



cleanEx()
nameEx("vital_status")
### * vital_status

flush(stderr()); flush(stdout())

### Name: vital_status
### Title: Calculate vital status at end of follow-up depending on
###   pat_status - tidyverse version
### Aliases: vital_status

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#prep step - create patient status variable
usdata_wide <- usdata_wide %>%
                  msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
                                         status_var = "p_status", life_var = "p_alive.1",
                                         birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
 
#now we can run the function
msSPChelpR::vital_status(usdata_wide, 
                        status_var = "p_status",
                        life_var_new = "p_alive_new", 
                        check = TRUE, 
                        as_labelled_factor = FALSE)




cleanEx()
nameEx("vital_status_tt")
### * vital_status_tt

flush(stderr()); flush(stdout())

### Name: vital_status_tt
### Title: Calculate vital status at end of follow-up depending on
###   pat_status - tidytable version
### Aliases: vital_status_tt

### ** Examples

#load sample data
data("us_second_cancer")

#prep step - make wide data as this is the required format
usdata_wide <- us_second_cancer %>%
                    msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
                    time_id_var = "SEQ_NUM", timevar_max = 10)
                    
#prep step - calculate p_spc variable
usdata_wide <- usdata_wide %>%
                 dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                                                       !is.na(t_site_icd.2)   ~ "SPC developed",
                                                       TRUE ~ NA_character_)) %>%
                 dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                                                              TRUE ~ 0))
                                                              
#prep step - create patient status variable
usdata_wide <- usdata_wide %>%
                  msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
                                         status_var = "p_status", life_var = "p_alive.1",
                                         birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
 
#now we can run the function
msSPChelpR::vital_status_tt(usdata_wide, 
                        status_var = "p_status",
                        life_var_new = "p_alive_new", 
                        check = TRUE, 
                        as_labelled_factor = FALSE)




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
