## The factorial tables are 1.5 GB. With reproducible.destinationPathShared set, each run's outputPath
## must get a hard link to one shared copy, written once; and Biomass_speciesParameters must read it.

cohortData <- data.frame(pixelGroup = 1:20, speciesCode = rep(c("a", "b"), 10), B = 101:120)
speciesTable <- data.frame(species = c("a", "b"), longevity = c(150L, 200L))

## counts how many times the module writes a feather file
countWrites <- function(env = parent.frame()) {
  n <- new.env()
  n$n <- 0L
  orig <- arrow::write_feather
  testthat::local_mocked_bindings(
    write_feather = function(...) { n$n <- n$n + 1L; orig(...) },
    .package = "arrow", .env = env
  )
  n
}

test_that("with a shared store, two outputPaths get hard links to one copy, written once", {
  scratch <- withr::local_tempdir()
  shared <- file.path(scratch, "shared")
  dir.create(shared)
  withr::local_options(reproducible.destinationPathShared = shared)
  nWrites <- countWrites()
  out1 <- file.path(scratch, "run1", "outputs")
  out2 <- file.path(scratch, "run2", "outputs")

  res1 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = out1)
  res2 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = out2)

  expect_identical(nWrites$n, 2L) ## one per table, not one per run
  expect_identical(basename(res1), basename(res2))
  expect_identical(unname(dirname(res2)), rep(normalizePath(out2), 2))
  inShared <- file.path(shared, basename(res1))
  inode <- function(x) fs::file_info(x)$inode
  expect_equal(inode(res1), inode(inShared))
  expect_equal(inode(res2), inode(inShared))
  expect_gte(min(as.integer(fs::file_info(inShared)$hard_links)), 3L) ## shared + two runs

  ## the reader in Biomass_speciesParameters
  cd <- arrow::open_dataset(res2[["cohortData"]], format = "feather") |> as.data.frame()
  expect_equal(sort(cd$B), cohortData$B)
  st <- arrow::open_dataset(res2[["speciesTable"]], format = "feather") |> as.data.frame()
  expect_equal(st$species, speciesTable$species)
})

test_that("without a shared store the file is written into outputPath", {
  scratch <- withr::local_tempdir()
  withr::local_options(reproducible.destinationPathShared = NULL, reproducible.inputPaths = NULL)
  nWrites <- countWrites()
  out <- file.path(scratch, "outputs")
  res <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = out)
  expect_true(all(file.exists(res)))
  expect_identical(nWrites$n, 2L)
  expect_equal(as.integer(fs::file_info(res)$hard_links), c(1L, 1L))
  expect_identical(list.files(scratch), "outputs")
})

test_that("a different digest gives different file names", {
  scratch <- withr::local_tempdir()
  withr::local_options(reproducible.destinationPathShared = NULL, reproducible.inputPaths = NULL)
  res1 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = file.path(scratch, "a"))
  res2 <- saveFactorialFiles(cohortData, speciesTable, dig = "def", destinationPath = file.path(scratch, "a"))
  expect_false(any(basename(res1) %in% basename(res2)))
})

test_that("the digest covers every parameter that defines the factorial", {
  base <- factorialDigest(list(longevity = 1:3), 10, 9, 5000L)
  expect_identical(base, factorialDigest(list(longevity = 1:3), 10, 9, 5000L))
  expect_false(base == factorialDigest(list(longevity = 1:4), 10, 9, 5000L))
  expect_false(base == factorialDigest(list(longevity = 1:3), 11, 9, 5000L))
  expect_false(base == factorialDigest(list(longevity = 1:3), 10, 8, 5000L))
  expect_false(base == factorialDigest(list(longevity = 1:3), 10, 9, 6000L))
})
