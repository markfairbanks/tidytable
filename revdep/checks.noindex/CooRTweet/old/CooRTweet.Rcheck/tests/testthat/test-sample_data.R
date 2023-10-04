library(data.table)

if (testthat:::on_cran()) {
    data.table::setDTthreads(threads = 2)
}

test_that("Sample dataset is consistent", {
    testthat::expect_equal(
        length(russian_coord_tweets$content_id),
        nrow(russian_coord_tweets)
    )
})

test_that("Self-coordinated posts are filtered out", {
    result <- detect_coordinated_groups(russian_coord_tweets,
        min_repetition = 5,
        time_window = 10
    )

    # Generate summary of user statistics
    summary_users <- user_stats(result)
    user_self_coord <- summary_users[id_user == "505dd470041822f34f915d9ec0f2667c"]
    testthat::expect_equal(user_self_coord$total_posts, 1)
    testthat::expect_equal(user_self_coord$mean_time_delta, 10)
})
