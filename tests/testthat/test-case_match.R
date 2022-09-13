test_that("works with character", {
  vec <- c("a", "b", "c", "d")

  res <- case_match(vec,
                    c("a", "b") ~ "A",
                    "c" ~ "B")

  expect_equal(res, c("A", "A", "B", NA))
})

test_that("works with numeric & .default", {
  vec <- 1:4

  res <- case_match(vec,
                    c(1, 2) ~ 1,
                    3 ~ 2,
                    .default = 3)

  expect_equal(res, c(1, 1, 2, 3))
})
