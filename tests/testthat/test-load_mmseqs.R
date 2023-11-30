test_that("loads the file", {
  load_mmseqs(file = "test_data/test_lca.tsv")
})

test_that("Sample name option works", {
  df <- load_mmseqs(file = "test_data/test_lca.tsv", include_name = TRUE)
  expect_equal(unique(df$sample),"test")
})

test_that("Filtering works", {
  df <- load_mmseqs(file = "test_data/test_lca.tsv", frag_thr = 2)
  expect_equal(min(df$total_fragments),2)
})
