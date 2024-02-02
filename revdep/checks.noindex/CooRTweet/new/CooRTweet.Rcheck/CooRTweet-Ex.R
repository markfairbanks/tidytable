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
