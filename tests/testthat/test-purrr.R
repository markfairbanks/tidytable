test_that("anon function works dt_map()", {
  anon_list <- dt_map(c(1,2,3), function(.x) .x + 1)
  twiddle_list <- dt_map(c(1,2,3), ~.x + 1)

  expect_equal(anon_list, twiddle_list)
})

test_that("anon function works map.()", {
  anon_list <- map.(c(1,2,3), function(.x) .x + 1)
  twiddle_list <- map.(c(1,2,3), ~ .x + 1)

  expect_equal(anon_list, twiddle_list)
})

test_that("anon function works dt_map_dbl()", {
  anon_dbl <- dt_map_dbl(c(1,2,3), function(.x) .x + 1)
  twiddle_dbl <- dt_map_dbl(c(1,2,3), ~.x + 1)

  expect_equal(anon_dbl, twiddle_dbl)
})

test_that("anon function works map_dbl.()", {
  anon_dbl <- map_dbl.(c(1,2,3), function(.x) .x + 1)
  twiddle_dbl <- map_dbl.(c(1,2,3), ~ .x + 1)

  expect_equal(anon_dbl, twiddle_dbl)
})

test_that("anon function works dt_map_chr()", {
  anon_chr <- dt_map_chr(c(1,2,3), as.character)
  twiddle_chr <- dt_map_chr(c(1,2,3), ~as.character(.x))

  expect_equal(anon_chr, twiddle_chr)
})

test_that("anon function works map_chr.()", {
  anon_chr <- map_chr.(c(1,2,3), as.character)
  twiddle_chr <- map_chr.(c(1,2,3), ~as.character(.x))

  expect_equal(anon_chr, twiddle_chr)
})

test_that("anon function works dt_map2()", {
  anon_list <- dt_map2(c(1,2,3), c(1,2,3), function(.x,.y) .x + .y)
  twiddle_list <- dt_map2(c(1,2,3), c(1,2,3), ~.x + .y)

  expect_equal(anon_list, twiddle_list)
})

test_that("anon function works map2.()", {
  anon_list <- map2.(c(1,2,3), c(1,2,3), function(.x,.y) .x + .y)
  twiddle_list <- map2.(c(1,2,3), c(1,2,3), ~.x + .y)

  expect_equal(anon_list, twiddle_list)
})

test_that("anon function works dt_map2_dbl()", {
  anon_dbl <- dt_map2_dbl(c(1,2,3), c(1,2,3), function(.x,.y) .x + .y)
  twiddle_dbl <- dt_map2_dbl(c(1,2,3), c(1,2,3), ~.x + .y)

  expect_equal(anon_dbl, twiddle_dbl)
})

test_that("anon function works map2_dbl.()", {
  anon_dbl <- map2_dbl.(c(1,2,3), c(1,2,3), function(.x,.y) .x + .y)
  twiddle_dbl <- map2_dbl.(c(1,2,3), c(1,2,3), ~.x + .y)

  expect_equal(anon_dbl, twiddle_dbl)
})
