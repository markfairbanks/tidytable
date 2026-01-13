pkgname <- "CooRTweet"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('CooRTweet')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("prep_data")
### * prep_data

flush(stderr()); flush(stdout())

### Name: prep_data
### Title: prep_data
### Aliases: prep_data

### ** Examples

dt <- data.table::data.table(old_object_id = 1:3, old_account_id_y = 4:6)
dt <- prep_data(dt, object_id = "old_object_id", account_id = "old_account_id_y")




cleanEx()
nameEx("simulate_data")
### * simulate_data

flush(stderr()); flush(stdout())

### Name: simulate_data
### Title: simulate_data
### Aliases: simulate_data

### ** Examples

# Example usage of simulate_data
## Not run: 
##D set.seed(123) # For reproducibility
##D simulated_data <- simulate_data(
##D   n_accounts_coord = 100,
##D   n_accounts_noncoord = 50,
##D   n_objects = 20,
##D   min_participation = 2,
##D   time_window = 10
##D )
##D 
##D # Extract input
##D input_data <- simulated_data[[1]]
##D 
##D # Extract output and keep coordinated actors.
##D # This is expected correspond to CooRTweet results from `detect_group`
##D simulated_results <- simulated_data[[2]]
##D simulated_results <- simulated_results[simulated_results$coordinated == TRUE, ]
##D simulated_results$coordinated <- NULL
##D 
##D # Run CooRTweet using the input_data and the parameters used for simulation
##D results <- detect_groups(
##D   x = input_data,
##D   time_window = 10,
##D   min_participation = 2
##D )
##D 
##D # Sort data tables and check whether they are identical
##D data.table::setkeyv(simulated_results, names(simulated_results))
##D data.table::setkeyv(results, names(simulated_results))
##D 
##D identical(results, simulated_results)
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
