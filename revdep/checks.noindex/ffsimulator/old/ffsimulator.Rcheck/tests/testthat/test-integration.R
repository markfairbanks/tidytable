test_that("MFL simulation works", {
  skip_on_cran()

  foureight <- mfl_connect(2021, 22627, user_agent = "asdf23409lkjsafd")
  foureight_sim <- ff_simulate(foureight, n_seasons = 2)
  week_sim <- ff_simulate_week(foureight,n = 10, verbose = FALSE, actual_schedule = FALSE)

  checkmate::expect_list(foureight_sim, len = 7)
  checkmate::expect_list(week_sim,len = 6)
  checkmate::expect_data_frame(week_sim$summary_week, nrows = 120, any.missing = FALSE)
  checkmate::expect_data_frame(foureight_sim$summary_simulation, nrows = 12, any.missing = FALSE)
  checkmate::expect_data_frame(foureight_sim$summary_season, nrows = 24, any.missing = FALSE)
  checkmate::expect_data_frame(foureight_sim$summary_week, nrows = 336, any.missing = FALSE)
})

test_that("Sleeper simulation works", {
  skip_on_cran()

  jml <- ff_connect(platform = "sleeper", league_id = "652718526494253056", season = 2021)
  jml_sim <- ff_simulate(jml, n_seasons = 2, verbose = FALSE, return = "all")
  jml_week_sim <- ff_simulate_week(jml,
                                   n = 10,
                                   verbose = FALSE,
                                   actual_schedule = FALSE,
                                   replacement_level = TRUE,
                                   return = "all")

  checkmate::expect_list(jml_sim, len = 14)
  checkmate::expect_list(jml_week_sim, len = 13)
  checkmate::expect_data_frame(jml_week_sim$summary_simulation, nrows = 12, any.missing = FALSE)
  checkmate::expect_data_frame(jml_sim$summary_simulation, nrows = 12, any.missing = FALSE)
  checkmate::expect_data_frame(jml_sim$summary_season, nrows = 24, any.missing = FALSE)
  checkmate::expect_data_frame(jml_sim$summary_week, nrows = 336, any.missing = FALSE)
})

test_that("Fleaflicker simulation works", {
  skip_on_cran()

  got <- fleaflicker_connect(2020, 206154)
  got_sim <- ff_simulate(got, n_seasons = 2, verbose = FALSE, replacement_level = TRUE)

  checkmate::expect_list(got_sim, len = 7)
  checkmate::expect_data_frame(got_sim$summary_simulation, nrows = 16, any.missing = FALSE)
  checkmate::expect_data_frame(got_sim$summary_season, nrows = 32, any.missing = FALSE)
  checkmate::expect_data_frame(got_sim$summary_week, nrows = 448, any.missing = FALSE)
})

test_that("ESPN simulation works", {
  skip_on_cran()

  tony <- espn_connect(season = 2020, league_id = 899513)
  tony_sim <- ff_simulate(tony, n_seasons = 2, verbose = FALSE, replacement_level = FALSE)

  checkmate::expect_list(tony_sim, len = 7)
  checkmate::expect_data_frame(tony_sim$summary_simulation, nrows = 10, any.missing = FALSE)
  checkmate::expect_data_frame(tony_sim$summary_season, nrows = 20, any.missing = FALSE)
  checkmate::expect_data_frame(tony_sim$summary_week, nrows = 280, any.missing = FALSE)
})

test_that("Actual Schedule - completed_season = no sim", {
  skip_on_cran()
  ssb <- mfl_connect(2020, 54040, user_agent = "asdfafd")
  testthat::expect_message(ssb_sim <- ff_simulate(ssb, n_seasons = 2, actual_schedule = TRUE),
                           regexp = "No unplayed weeks")

  checkmate::expect_list(ssb_sim, len = 3)
})

test_that("wins added works",{
  skip_on_cran()
  ssb <- mfl_connect(2021,54040)
  ssb_wa <- ff_wins_added(ssb, n_seasons = 2)

  checkmate::expect_list(ssb_wa, len = 15)
  checkmate::expect_data_frame(ssb_wa$war, min.rows = 200)
})
