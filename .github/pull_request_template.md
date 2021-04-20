### Pull Request Template

*Please remove this template before you submit*

If you are unsure of any of these steps feel free to reach out and I can help walk you through the process.

#### Before you make a pull request
* If you are adding a new feature - please make sure you have opened an issue beforehand and we have discussed
the feature
* You have made your changes in a new/separate branch. Branches should have descriptive names.
  + Example 1: Fixing an issue
    - fix-drop_na-col-order
  + Example 2: Adding a new argument to an existing function
    - add-mutate-keep
* Use a descriptive commit message
* Keep commit count to a relative minimum. For example - you shouldn't need 5 separate commits to add one
argument to a function. Work out the kinks on your own locally before submitting the PR!
* Add or update documentation (if necessary)
* Run `devtools::document()` - updates documentation files
* Update NEWS.md
* Add simple unit tests (if necessary)
* Run `devtools::test()` - runs unit tests
* Run `devtools::check()` - makes sure everything follows CRAN rules

#### When making a pull request
* Give a descriptive title
* Provide a description of your changes
* If fixing an issue - put "Closes #xx" in your description so the issue is auto-closed.
