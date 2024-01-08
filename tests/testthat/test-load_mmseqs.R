test_that("loads the file", {
  load_mmseqs(file = system.file("extdata", "test_lca.tsv",mustWork = TRUE))
})

test_that("Sample name option works", {
  df <- load_mmseqs(file = system.file("extdata", "test_lca.tsv",mustWork = TRUE), include_name = TRUE)
  expect_equal(unique(df$sample),"test")
})

test_that("Filtering works", {
  df <- load_mmseqs(file = system.file("extdata", "test_lca.tsv",mustWork = TRUE), frag_thr = 2)
  expect_equal(min(df$total_fragments),2)
})
