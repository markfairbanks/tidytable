
data("panc8_small")

test_that(
  "Ridge regression", {
    panc8_small <- RidgeRegression(panc8_small, c("tech", "dataset"), "celltype", replace = TRUE)
    expect_equal(panc8_small[['RNA']]@scale.data[1, 1], -0.4406119, tolerance = 1e-7)
    expect_equal(panc8_small[['RNA']]@scale.data[10, 10], 0.991299, tolerance = 1e-7)
  }
)