library(data.table)

sim_test <- function(n_users_coord = 5,
                     n_users_noncoord = 4,
                     n_objects = 5,
                     min_repetition = 3,
                     time_window = 10) {
  coordinated = NULL
  
  sim <- simulate_data(
    n_users_coord = n_users_coord,
    n_users_noncoord = n_users_noncoord,
    n_objects = n_objects,
    min_repetition = min_repetition,
    time_window = time_window
  )

    simulated_result <- data.table(sim[[2]])
    # test the minimum value of uncoordinated time_deltas
    delta_uncoord_min <- min(simulated_result[simulated_result$coordinated == FALSE, ]$time_delta)
    testthat::expect_gt(delta_uncoord_min, time_window)

    # test number of coordinated users
    users_coord <- unique(c(simulated_result[simulated_result$coordinated == TRUE, ]$id_user,
                            simulated_result[simulated_result$coordinated == TRUE, ]$id_user_y))

    testthat::expect_equal(length(users_coord), n_users_coord)

    # test the maximum value of coordinated time_deltas
    delta_coord_max <- max(simulated_result[simulated_result$coordinated == TRUE, ]$time_delta)
    testthat::expect_lte(delta_coord_max, time_window)

    # test number of non coordinated users
    users_noncoord <- unique(c(simulated_result[simulated_result$coordinated == FALSE, ]$id_user,
                            simulated_result[simulated_result$coordinated == FALSE, ]$id_user_y))

    testthat::expect_equal(length(users_noncoord), n_users_noncoord)


    simulated_result <- simulated_result[coordinated == TRUE]
    simulated_result <- simulated_result[, coordinated := NULL]

    result <- detect_coordinated_groups(sim[[1]],
      time_window = time_window,
      min_repetition = min_repetition
    )

    result_stats <- group_stats(result)
    simulated_stats <- group_stats(simulated_result)
    result_stats_users <- user_stats(result)
    simulated_stats_users <- user_stats(simulated_result)


  return(list(
    result_stats,
    simulated_stats,
    result_stats_users,
    simulated_stats_users
  ))
}


test_that("simulated data works with default parameters", {
  res <- sim_test()

  testthat::expect_equal(res[[1]], res[[2]])
  testthat::expect_equal(res[[3]], res[[4]])
})



test_that("simulated data works with high values", {
  testthat::skip_on_cran()
  res <- sim_test(n_users_coord =  50,
    n_users_noncoord = 100,
    n_objects = 100,
    min_repetition = 3,
    time_window = 100)

  testthat::expect_equal(res[[1]], res[[2]])
  testthat::expect_equal(res[[3]], res[[4]])
})


test_that("simulation is possible with random parameters", {
  testthat::skip_on_cran()
  # run 10 tests
  for (i in 1:10) {
    n_users_coord <- sample(3:10, size = 1)
    n_users_noncoord <- sample(3:10, size = 1)
    n_objects <- sample(3:10, size = 1)
    min_repetition <- sample(1:10, size = 1)
    time_window <- sample(1:60, size = 1)

    sim <- sim_test(
      n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window
    )


    testthat::expect_equal(sim[[1]], sim[[2]])
    testthat::expect_equal(sim[[3]], sim[[4]])


  }
})


test_that("balanced increase in number of users", {
  testthat::skip_on_cran()
  # increasing number of users
  # fails with only 2 users
  for (i in 3:6) {
    n_users_coord <- i
    n_users_noncoord <- i
    n_objects <- 5
    min_repetition <- 3
    time_window <- 10

    sim <- sim_test(
      n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window
    )

    testthat::expect_equal(sim[[1]], sim[[2]])
    testthat::expect_equal(sim[[3]], sim[[4]])
  }

})


test_that("unbalanced increase in number of users: A", {
  testthat::skip_on_cran()
  # increasing number of users
  # Fails sometimes with 2 coord users
  # seems to be an inherent problem of the simulation
  for (i in 3:6) {
    n_users_coord <- i
    n_users_noncoord <- i + i
    n_objects <- 5
    min_repetition <- 3
    time_window <- 10

    sim <- sim_test(
      n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window
    )


    testthat::expect_equal(sim[[1]], sim[[2]])
    testthat::expect_equal(sim[[3]], sim[[4]])

  }
})



test_that("unbalanced increase in number of users: B", {
  testthat::skip_on_cran()
  # increasing number of users
  for (i in 3:6) {
    n_users_coord <- i + i
    n_users_noncoord <- i
    n_objects <- 5
    min_repetition <- 3
    time_window <- 10

    sim <- sim_test(
      n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window
    )

    testthat::expect_equal(sim[[1]], sim[[2]])
    testthat::expect_equal(sim[[3]], sim[[4]])

  }
})


test_that("increase in number of objects", {
  testthat::skip_on_cran()
  # increasing number of objects
  for (i in 2:6) {
    n_users_coord <- 5
    n_users_noncoord <- 5
    n_objects <- i
    min_repetition <- 3
    time_window <- 10

    sim <- sim_test(
      n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window
    )

    testthat::expect_equal(sim[[1]], sim[[2]])
    testthat::expect_equal(sim[[3]], sim[[4]])

  }
})


test_that("larger time window", {
  testthat::skip_on_cran()
  # longer and longer time window
  for (i in seq(1, 120, 20)) {
    n_users_coord <- 5
    n_users_noncoord <- 5
    n_objects <- 5
    min_repetition <- 3
    time_window <- i

    sim <- sim_test(
      n_users_coord = n_users_coord,
      n_users_noncoord = n_users_noncoord,
      n_objects = n_objects,
      min_repetition = min_repetition,
      time_window = time_window
    )

    testthat::expect_equal(sim[[1]], sim[[2]])
    testthat::expect_equal(sim[[3]], sim[[4]])

  }
})
