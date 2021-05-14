# test_that("cummean is consistent with cumsum() and seq_along()", {
#   x <- 1:5
#   expect_equal(cummean.(x), c(1, 1.5, 2, 2.5, 3))
#   expect_equal(cummean.(x), cumsum(x)/seq_along(x))
#
#   expect_equal(cummean.(numeric()), numeric())
# })
#
# test_that("cumany and cumall handle NAs consistently (#408, #3749, #4132)", {
#   batman <- c(NA, NA, NA, NA, NA)
#   expect_true(all(is.na(cumany.(batman))))
#   expect_true(all(is.na(cumall.(batman))))
#
#   # normal usecases
#   expect_identical(
#     cumall.(c(TRUE, NA, FALSE, NA)),
#     c(TRUE, NA, FALSE, FALSE)
#   )
#
#   expect_identical(
#     cumall.(c(FALSE, NA, TRUE)),
#     c(FALSE, FALSE, FALSE)
#   )
#
#   expect_identical(
#     cumall.(c(NA, TRUE)),
#     c(NA, NA)
#   )
#
#   expect_identical(
#     cumall.(c(NA, FALSE)),
#     c(NA, FALSE)
#   )
#
#   expect_identical(
#     cumany.(c(TRUE, NA, FALSE)),
#     c(TRUE, TRUE, TRUE)
#   )
#
#   expect_identical(
#     cumany.(c(FALSE, NA, TRUE)),
#     c(FALSE, NA, TRUE)
#   )
#
#   # scalars
#   expect_true(is.na(cumall.(NA)))
#   expect_true(is.na(cumany.(NA)))
#   expect_true(cumall.(TRUE))
#   expect_false(cumall.(FALSE))
#   expect_true(cumany.(TRUE))
#   expect_false(cumany.(FALSE))
#
#   # degenerate cases
#   expect_identical(
#     cumall.(logical()),
#     logical()
#   )
#
#   expect_identical(
#     cumany.(logical()),
#     logical()
#   )
#
#   # behaviour of degenerate logical vectors mimics that of base R functions
#   x <- as.raw(c(2L, 9L, 0L))
#   class(x) <- "logical"
#   expect_identical(cumall.(x), x == TRUE)
#   expect_identical(cumany.(x), c(TRUE, TRUE, TRUE))
# })
#
# test_that("cummean is not confused by FP error", {
#   a <- rep(99, 9)
#   expect_true(all(cummean.(a) == a))
# })
