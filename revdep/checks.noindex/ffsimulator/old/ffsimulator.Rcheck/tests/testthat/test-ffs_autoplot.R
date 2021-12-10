
test_that("autoplot works", {
  foureight_sim <- .ffs_cache("foureight_sim.rds")
  foureight_sim_week <- .ffs_cache("foureight_sim_week.rds")

  wins <- plot(foureight_sim, type = "wins")
  rank <- plot(foureight_sim, type = "rank")
  points <- plot(foureight_sim, type = "points")

  luck <- plot(foureight_sim_week, type = "luck")
  points_week <- plot(foureight_sim_week, type = "points")

  vdiffr::expect_doppelganger("wins plot", wins)
  vdiffr::expect_doppelganger("rank plot", rank)
  vdiffr::expect_doppelganger("points plot", points)
  vdiffr::expect_doppelganger("weekly luck plot", luck)
  vdiffr::expect_doppelganger("weekly points plot", points_week)
})
