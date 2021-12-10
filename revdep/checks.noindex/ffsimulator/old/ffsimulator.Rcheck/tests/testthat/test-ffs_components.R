
cache_names <- list.files(system.file("cache", package = "ffsimulator"))
cache <- lapply(cache_names, .ffs_cache)
cache_names <- gsub(pattern = "\\.rds$", replacement = "", x = cache_names)
names(cache) <- cache_names

test_that("ffs_adp_outcomes() works for both the simple and none injury models", {
  adp_outcomes <- ffs_adp_outcomes(
    scoring_history = cache$mfl_scoring_history,
    gp_model = "simple"
  )

  adp_outcomes_noinjury <- ffs_adp_outcomes(
    scoring_history = cache$espn_scoring_history,
    gp_model = "none"
  )

  adp_outcomes_week <- ffs_adp_outcomes_week(
    scoring_history = cache$mfl_scoring_history,
    pos_filter = c("QB","RB","WR","TE")
  )

  checkmate::expect_data_frame(adp_outcomes, min.rows = 500)
  checkmate::expect_data_frame(adp_outcomes_noinjury, min.rows = 500)
  checkmate::expect_data_frame(adp_outcomes_week, min.rows = 400)

  checkmate::expect_subset(
    names(adp_outcomes),
    c("pos", "rank", "prob_gp", "week_outcomes", "player_name", "fantasypros_id")
  )
  checkmate::expect_subset(
    names(adp_outcomes_noinjury),
    c("pos", "rank", "prob_gp", "week_outcomes", "player_name", "fantasypros_id")
  )
  checkmate::expect_subset(
    names(adp_outcomes_week),
    c("pos", "rank", "week_outcomes", "player_name", "fantasypros_id")
  )
})


test_that("ffs_generate_projections() returns a tibble and specific columns", {
  projected_scores <- ffs_generate_projections(
    adp_outcomes = cache$adp_outcomes,
    latest_rankings = cache$latest_rankings,
    n_seasons = 2,
    weeks = 1:5,
    rosters = cache$mfl_rosters
  )

  projected_scores_week <- ffs_generate_projections_week(
    adp_outcomes = cache$adp_outcomes_week,
    latest_rankings = cache$latest_rankings_week,
    n = 5,
    rosters = cache$mfl_rosters
    )

  checkmate::expect_data_frame(projected_scores, min.rows = 3500)
  checkmate::expect_data_frame(projected_scores_week, min.rows = 1200)

  checkmate::expect_subset(
    c("fantasypros_id", "pos", "projected_score", "season", "week"),
    names(projected_scores)
  )

  checkmate::expect_subset(
    c("fantasypros_id", "pos", "projected_score", "season", "week"),
    names(projected_scores_week)
  )
})

test_that("ffs_score_rosters() connects the scores to the rosters", {
  roster_scores <- ffs_score_rosters(
    projected_scores = cache$projected_scores,
    rosters = cache$mfl_rosters
  )

  checkmate::expect_data_frame(roster_scores, min.rows = 2000)

  checkmate::expect_subset(
    c(
      "fantasypros_id", "pos", "projected_score", "season", "week",
      "franchise_id", "pos_rank"
    ),
    names(roster_scores)
  )
})


test_that("ffs_optimize_lineups() returns a tibble and specific columns", {
  optimal_scores <- ffs_optimize_lineups(
    roster_scores = cache$roster_scores,
    lineup_constraints = cache$mfl_lineup_constraints,
    best_ball = FALSE
  )

  optimal_scores_bestball <- ffs_optimize_lineups(
    roster_scores = cache$roster_scores,
    lineup_constraints = cache$mfl_lineup_constraints,
    best_ball = TRUE
  )

  checkmate::expect_data_frame(optimal_scores, nrows = 72)
  checkmate::expect_data_frame(optimal_scores_bestball, nrows = 72)

  checkmate::expect_subset(
    c("franchise_id", "franchise_name", "season", "week", "optimal_score", "optimal_player_id", "optimal_player_score", "lineup_efficiency", "actual_score"),
    names(optimal_scores)
  )
  checkmate::expect_subset(
    c("franchise_id", "franchise_name", "season", "week", "optimal_score", "optimal_player_id", "optimal_player_score", "lineup_efficiency", "actual_score"),
    names(optimal_scores_bestball)
  )

  expect_equal(
    optimal_scores_bestball$optimal_score,
    optimal_scores_bestball$actual_score
  )
})

test_that("schedules returns a tibble and specific columns", {
  schedules <- ffs_build_schedules(
    n_seasons = 2,
    n_weeks = 5,
    franchises = cache$mfl_franchises
  )

  schedules_w_bye <- ffs_build_schedules(
    n_teams = 11,
    n_seasons = 2,
    n_weeks = 10
  )

  checkmate::expect_data_frame(schedules, nrows = 120)

  checkmate::expect_subset(
    c("season", "week", "franchise_id", "opponent_id"),
    names(schedules)
  )

  checkmate::expect_data_frame(schedules_w_bye, nrows = 220)
  checkmate::expect_subset(
    c("season", "week", "franchise_id", "opponent_id"),
    names(schedules_w_bye)
  )
})

test_that("summary functions return tibbles", {
  summary_week <- ffs_summarise_week(
    optimal_scores = cache$optimal_scores,
    schedules = cache$schedules
  )
  summary_season <- ffs_summarise_season(summary_week = summary_week)
  summary_simulation <- ffs_summarise_simulation(summary_season = summary_season)

  checkmate::expect_data_frame(summary_week, nrows = 72)
  checkmate::expect_data_frame(summary_season, nrows = 24)
  checkmate::expect_data_frame(summary_simulation, nrows = 12)

  checkmate::expect_subset(
    c(
      "season", "week", "franchise_name", "optimal_score",
      "lineup_efficiency", "team_score", "opponent_score", "result",
      "opponent_name", "allplay_wins", "allplay_games", "allplay_pct",
      "franchise_id", "optimal_player_id", "optimal_player_score","league_id"
    ),
    names(summary_week),
    label = "summary_week names check"
  )

  checkmate::expect_subset(
    c(
      "season","league_id", "franchise_id", "franchise_name", "h2h_wins", "h2h_winpct",
      "allplay_wins", "allplay_games", "allplay_winpct", "points_for",
      "points_against", "potential_points"
    ),
    names(summary_season),
    label = "summary_season names check"
  )

  checkmate::expect_subset(
    c(
      "franchise_id", "franchise_name", "seasons", "h2h_wins", "h2h_winpct",
      "allplay_wins", "allplay_winpct", "points_for", "points_against",
      "potential_points"
    ),
    names(summary_simulation),
    label = "summary_simulation names check"
  )
})
