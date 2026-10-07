## The factorial tables are 1.5 GB. Each run folder must get a hard link to one shared copy, not
## its own copy, and the result must still be readable the way Biomass_speciesParameters reads it.

test_that("a second run folder gets hard links to the same files as the first", {
  scratch <- withr::local_tempdir()
  cachePath <- file.path(scratch, "cache")
  store <- file.path(cachePath, "factorialFiles")
  inputs1 <- file.path(scratch, "run1", "inputs")
  inputs2 <- file.path(scratch, "run2", "inputs")
  cohortData <- data.frame(pixelGroup = 1:20, speciesCode = rep(c("a", "b"), 10), B = 101:120)
  speciesTable <- data.frame(species = c("a", "b"), longevity = c(150L, 200L))

  res1 <- saveFactorialFiles(cohortData, speciesTable, inputPath = inputs1, cachePath = cachePath)
  mtime1 <- file.info(file.path(store, basename(res1)))$mtime
  Sys.sleep(1.1)
  res2 <- saveFactorialFiles(cohortData, speciesTable, inputPath = inputs2, cachePath = cachePath)

  expect_identical(dirname(res1), rep(normalizePath(inputs1), 2))
  expect_identical(dirname(res2), rep(normalizePath(inputs2), 2))
  expect_identical(basename(res1), basename(res2))
  expect_true(all(file.exists(res1), file.exists(res2)))

  inStore <- file.path(store, basename(res1))
  inodes <- function(x) fs::file_info(x)$inode
  expect_equal(inodes(res1), inodes(inStore))
  expect_equal(inodes(res2), inodes(inStore))
  expect_identical(file.info(inStore)$mtime, mtime1) ## the second call did not write again
  expect_gte(min(as.integer(fs::file_info(inStore)$hard_links)), 3L) ## store + two runs

  ## the reader in Biomass_speciesParameters
  cd <- arrow::open_dataset(res2[["cohortData"]], format = "feather") |> as.data.frame()
  expect_equal(nrow(cd), nrow(cohortData))
  expect_equal(sort(cd$B), cohortData$B)
})
