## Pull Request Template

*Please remove this template before you submit*

If you are unsure of any of these steps feel free to reach out and I can help walk you through the process.

### Before you make a pull request

#### Code style and git
* If you are adding a new feature - please make sure we have discussed the feature in a GitHub issue.
* Please make your changes in a new branch. Branches should have descriptive names.
  + Ex: fix-drop_na-col-order
* Use descriptive commit messages
* Keep commit count to a relative minimum. For example - you typically shouldn't need 10 separate commits to add one
argument to a function. Please work out the kinks on your own locally before submitting the PR!
* Your code generally follows the [tidyverse style guide](https://style.tidyverse.org)

#### Package Update Checklist
* Add or update documentation (if necessary)
* Run `devtools::document()` - updates documentation files
* Update NEWS.md
* Add simple unit tests (if necessary)
* Run `devtools::test()` - runs unit tests
* Run `devtools::check()` - makes sure everything follows CRAN rules

### When making a pull request
* Give a descriptive title
* Provide a description of your changes
* If fixing an issue - put "Closes #xx" in your description so the issue is auto-closed.
