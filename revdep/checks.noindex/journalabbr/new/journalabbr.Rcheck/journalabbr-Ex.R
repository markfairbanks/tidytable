pkgname <- "journalabbr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('journalabbr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("abbr_bib")
### * abbr_bib

flush(stderr()); flush(stdout())

### Name: abbr_bib
### Title: Journal field abbreviation of BibTeX file.
### Aliases: abbr_bib abbr_bib_only_journal

### ** Examples

require(journalabbr)
path <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
temptab <- abbr_bib(file = path, out.file = tempfile(fileext = ".bib"))

# add user csv
csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
temptab1 <- abbr_bib(file = path, out.file = tempfile(fileext = ".bib"), user.csv = csvpath)

# no return value
abbr_bib_only_journal(file = path, out.file = tempfile(fileext = ".bib"), user.csv = csvpath)




cleanEx()
nameEx("add_abbrtable")
### * add_abbrtable

flush(stderr()); flush(stdout())

### Name: add_abbrtable
### Title: Add user's journal abbreviation table
### Aliases: add_abbrtable

### ** Examples

csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
abbrtable_user <- add_abbrtable(file = csvpath, header = FALSE, sep = ",")
colnames(abbrtable_user)




cleanEx()
nameEx("read_bib2dt")
### * read_bib2dt

flush(stderr()); flush(stdout())

### Name: read_bib2dt
### Title: Parse a BibTeX file to a 'data.table'.
### Aliases: read_bib2dt

### ** Examples

# Read from .bib file:
file1 <- system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
dt1 <- read_bib2dt(file1)
colnames(dt1)

file2 <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
dt2 <- read_bib2dt(file2)
colnames(dt2)




cleanEx()
nameEx("replace_field")
### * replace_field

flush(stderr()); flush(stdout())

### Name: replace_field_journal
### Title: Replace field 'journal' with built-in data sets and user
###   provided data sets.
### Aliases: replace_field_journal replace_field_author

### ** Examples

csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
abbrtable_user <- add_abbrtable(file = csvpath, header = FALSE, sep = ",")
colnames(abbrtable_user)

file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
dt <- read_bib2dt(file)

newdt <- replace_field_journal(dt, abbrtable_user)
newdt1 <- replace_field_author(dt, author.connect = "and")
newdt2 <- replace_field_author(dt, author.connect = "&")




cleanEx()
nameEx("write_dt2bib")
### * write_dt2bib

flush(stderr()); flush(stdout())

### Name: write_dt2bib
### Title: Export a BibTeX 'data.table' to a .bib file.
### Aliases: write_dt2bib

### ** Examples

# Read from .bib file:
require(journalabbr)
file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
bib <- read_bib2dt(file)

# Write to .bib file:
write_dt2bib(bib, file = tempfile(fileext = ".bib"))




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
