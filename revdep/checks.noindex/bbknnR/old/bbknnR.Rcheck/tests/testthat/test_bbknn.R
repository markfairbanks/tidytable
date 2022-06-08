
test_that(
  "BBKNN with RcppAnnoy", {
    data("panc8_small")
    panc8_small <- RunBBKNN(panc8_small, "tech", approx = T, use_annoy = T)
    expect_s4_class(object = panc8_small[['bbknn']], class = "Graph")
  }
)

# test_that(
#   "BBKNN with pynndescent", {
#     data("panc8_small")
#     panc8_small <- RunBBKNN(panc8_small, "tech", approx = T, use_annoy = F)
#   }
# )
# 
# test_that(
#   "BBKNN with other methods", {
#     data("panc8_small")
#     panc8_small <- RunBBKNN(panc8_small, "tech", approx = F, use_faiss = F)
#   }
# )

