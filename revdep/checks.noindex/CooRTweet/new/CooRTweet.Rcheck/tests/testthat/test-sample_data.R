library(data.table)

if (testthat:::on_cran()) {
    data.table::setDTthreads(threads = 2)
}

testthat::test_that("Sample dataset is consistent", {
    testthat::expect_equal(
        length(russian_coord_tweets$content_id),
        nrow(russian_coord_tweets)
    )
})

testthat::test_that("Self-coordinated posts are filtered out", {
    result <- detect_groups(russian_coord_tweets,
        min_participation = 5,
        time_window = 10,
        remove_loops = TRUE
    )

    coord_graph <- generate_coordinated_network(result)

    # Generate summary of account statistics
    summary_accounts <- account_stats(coord_graph, result, weight_threshold = "none")
    account_self_coord <- summary_accounts[account_id == "9fa51ef17278c01d13d313741eddfc0b"]
    testthat::expect_equal(nrow(account_self_coord), 0)
})

testthat::test_that("Self-coordinated posts are not filtered out", {
    result <- detect_groups(russian_coord_tweets,
                            min_participation = 5,
                            time_window = 10,
                            remove_loops = FALSE
    )

    coord_graph <- generate_coordinated_network(result)

    # Generate summary of account statistics
    summary_accounts <- account_stats(coord_graph, result, weight_threshold = "none")
    account_self_coord <- summary_accounts[account_id == "9fa51ef17278c01d13d313741eddfc0b"]
    testthat::expect_gt(nrow(account_self_coord), 0)
})
