pkgname <- "pangoling"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pangoling')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("causal_config")
### * causal_config

flush(stderr()); flush(stdout())

### Name: causal_config
### Title: Returns the configuration of a causal model
### Aliases: causal_config

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
causal_config(model = "gpt2")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("causal_next_tokens_pred_tbl")
### * causal_next_tokens_pred_tbl

flush(stderr()); flush(stdout())

### Name: causal_next_tokens_pred_tbl
### Title: Generate next tokens after a context and their predictability
###   using a causal transformer model
### Aliases: causal_next_tokens_pred_tbl

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
causal_next_tokens_pred_tbl(
  context = "The apple doesn't fall far from the",
  model = "gpt2"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("causal_pred_mats")
### * causal_pred_mats

flush(stderr()); flush(stdout())

### Name: causal_pred_mats
### Title: Generate a list of predictability matrices using a causal
###   transformer model
### Aliases: causal_pred_mats

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data("df_sent")
df_sent
list_of_mats <- causal_pred_mats(
                       x = df_sent$word,
                       by = df_sent$sent_n,  
                       model = "gpt2"
                )

# View the structure of the resulting list
list_of_mats |> str()

# Inspect the last rows of the first matrix
list_of_mats[[1]] |> tail()

# Inspect the last rows of the second matrix
list_of_mats[[2]] |> tail()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("causal_predictability")
### * causal_predictability

flush(stderr()); flush(stdout())

### Name: causal_words_pred
### Title: Compute predictability using a causal transformer model
### Aliases: causal_words_pred causal_tokens_pred_lst causal_targets_pred

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Using causal_targets_pred
causal_targets_pred(
  contexts = c("The apple doesn't fall far from the",
               "Don't judge a book by its"),
  targets = c("tree.", "cover."),
  model = "gpt2"
)

# Using causal_words_pred
causal_words_pred(
  x = df_sent$word,
  by = df_sent$sent_n,
  model = "gpt2"
)

# Using causal_tokens_pred_lst
preds <- causal_tokens_pred_lst(
  texts = c("The apple doesn't fall far from the tree.",
            "Don't judge a book by its cover."),
  model = "gpt2"
)
preds

# Convert the output to a tidy table
suppressPackageStartupMessages(library(tidytable))
map2_dfr(preds, seq_along(preds), 
~ data.frame(tokens = names(.x), pred = .x, id = .y))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("causal_preload")
### * causal_preload

flush(stderr()); flush(stdout())

### Name: causal_preload
### Title: Preloads a causal language model
### Aliases: causal_preload

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
causal_preload(model = "gpt2")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("df_jaeger14")
### * df_jaeger14

flush(stderr()); flush(stdout())

### Name: df_jaeger14
### Title: Self-Paced Reading Dataset on Chinese Relative Clauses
### Aliases: df_jaeger14
### Keywords: datasets

### ** Examples

# Basic exploration
head(df_jaeger14)

# Summarize reaction times by region
 library(tidytable)
df_jaeger14 |>
  group_by(region) |>
  summarize(mean_rt = mean(rt, na.rm = TRUE))



cleanEx()
nameEx("df_sent")
### * df_sent

flush(stderr()); flush(stdout())

### Name: df_sent
### Title: Example dataset: Two word-by-word sentences
### Aliases: df_sent
### Keywords: datasets

### ** Examples

# Load the dataset
data("df_sent")
df_sent



cleanEx()
nameEx("install_py_pangoling")
### * install_py_pangoling

flush(stderr()); flush(stdout())

### Name: install_py_pangoling
### Title: Install the Python packages needed for 'pangoling'
### Aliases: install_py_pangoling

### ** Examples


# Install with default settings:
## Not run: 
##D  install_py_pangoling()
## End(Not run)




cleanEx()
nameEx("installed_py_pangoling")
### * installed_py_pangoling

flush(stderr()); flush(stdout())

### Name: installed_py_pangoling
### Title: Check if the required Python dependencies for 'pangoling' are
###   installed
### Aliases: installed_py_pangoling

### ** Examples

## Not run: 
##D if (installed_py_pangoling()) {
##D  message("Python dependencies are installed.")
##D } else {
##D  warning("Python dependencies are missing. Please install `torch` and `transformers`.")
##D }
## End(Not run)



cleanEx()
nameEx("masked_config")
### * masked_config

flush(stderr()); flush(stdout())

### Name: masked_config
### Title: Returns the configuration of a masked model
### Aliases: masked_config

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
masked_config(model = "bert-base-uncased")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("masked_preload")
### * masked_preload

flush(stderr()); flush(stdout())

### Name: masked_preload
### Title: Preloads a masked language model
### Aliases: masked_preload

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
causal_preload(model = "bert-base-uncased")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("masked_targets_pred")
### * masked_targets_pred

flush(stderr()); flush(stdout())

### Name: masked_targets_pred
### Title: Get the predictability of a target word (or phrase) given a left
###   and right context
### Aliases: masked_targets_pred

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
masked_targets_pred(
  prev_contexts = c("The", "The"),
  targets = c("apple", "pear"),
  after_contexts = c(
    "doesn't fall far from the tree.",
    "doesn't fall far from the tree."
  ),
  model = "bert-base-uncased"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("masked_tokens_pred_tbl")
### * masked_tokens_pred_tbl

flush(stderr()); flush(stdout())

### Name: masked_tokens_pred_tbl
### Title: Get the possible tokens and their log probabilities for each
###   mask in a sentence
### Aliases: masked_tokens_pred_tbl

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
masked_tokens_pred_tbl("The [MASK] doesn't fall far from the tree.",
  model = "bert-base-uncased"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ntokens")
### * ntokens

flush(stderr()); flush(stdout())

### Name: ntokens
### Title: The number of tokens in a string or vector of strings
### Aliases: ntokens

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
ntokens(x = c("The apple doesn't fall far from the tree."), model = "gpt2")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("pangoling-package")
### * pangoling-package

flush(stderr()); flush(stdout())

### Name: pangoling-package
### Title: pangoling: Access to Large Language Model Predictions
### Aliases: pangoling pangoling-package
### Keywords: internal

### ** Examples

options(pangoling.verbose = FALSE) # Removes messages
options(pangoling.verbose = TRUE) # Show messages 



cleanEx()
nameEx("perplexity_calc")
### * perplexity_calc

flush(stderr()); flush(stdout())

### Name: perplexity_calc
### Title: Calculates perplexity
### Aliases: perplexity_calc

### ** Examples

probs <- c(.3, .5, .6)
perplexity_calc(probs, log.p = FALSE)
lprobs <- log(probs)
perplexity_calc(lprobs, log.p = TRUE)



cleanEx()
nameEx("set_cache_folder")
### * set_cache_folder

flush(stderr()); flush(stdout())

### Name: set_cache_folder
### Title: Set cache folder for HuggingFace transformers
### Aliases: set_cache_folder

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Not run: 
##D set_cache_folder("~/new_cache_dir")
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tokenize_lst")
### * tokenize_lst

flush(stderr()); flush(stdout())

### Name: tokenize_lst
### Title: Tokenize an input
### Aliases: tokenize_lst

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
tokenize_lst(x = c("The apple doesn't fall far from the tree."), 
             model = "gpt2")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("transformer_vocab")
### * transformer_vocab

flush(stderr()); flush(stdout())

### Name: transformer_vocab
### Title: Returns the vocabulary of a model
### Aliases: transformer_vocab

### ** Examples

## Don't show: 
if (installed_py_pangoling()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
transformer_vocab(model = "gpt2") |>
 head()
## Don't show: 
}) # examplesIf
## End(Don't show)



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
