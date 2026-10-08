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

## ---- the ways the files can be shared ----------------------------------------------------------

inode <- function(x) fs::file_info(x)$inode
mtime <- function(x) file.info(x)$mtime

test_that("route 2: a shared outputPath, no shared store: the second save reuses the files", {
  scratch <- withr::local_tempdir()
  withr::local_options(reproducible.destinationPathShared = NULL, reproducible.inputPaths = NULL)
  nWrites <- countWrites()
  out <- file.path(scratch, "sharedOutputs")

  res1 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = out)
  before <- list(inode = inode(res1), mtime = mtime(res1))
  Sys.sleep(1.1)
  res2 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = out)

  expect_identical(res1, res2)
  expect_identical(nWrites$n, 2L) ## one per table, from the first run only
  expect_equal(inode(res2), before$inode)
  expect_identical(mtime(res2), before$mtime)
})

test_that("route 3 (no shared store, per-run outputPaths): each run has its own file", {
  scratch <- withr::local_tempdir()
  withr::local_options(reproducible.destinationPathShared = NULL, reproducible.inputPaths = NULL)
  nWrites <- countWrites()
  res1 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = file.path(scratch, "run1"))
  res2 <- saveFactorialFiles(cohortData, speciesTable, dig = "abc", destinationPath = file.path(scratch, "run2"))
  expect_identical(nWrites$n, 4L)
  expect_false(any(inode(res1) %in% inode(res2)))
  expect_equal(as.integer(fs::file_info(c(res1, res2))$hard_links), rep(1L, 4))
})

test_that("two processes saving into an empty shared store at once leave one intact file", {
  skip_on_os("windows")
  skip_if_not_installed("parallel")
  scratch <- withr::local_tempdir()
  shared <- file.path(scratch, "shared")
  dir.create(shared)
  withr::local_options(reproducible.destinationPathShared = shared)
  big <- data.frame(pixelGroup = 1:2e6, B = runif(2e6)) ## ~30 MB, so the writes overlap
  go <- Sys.time() + 3
  jobs <- lapply(1:2, function(i) {
    parallel::mcparallel({
      while (Sys.time() < go) Sys.sleep(0.01)
      try(saveFactorialFiles(big, speciesTable, dig = "abc",
                             destinationPath = file.path(scratch, paste0("run", i))))
    })
  })
  res <- parallel::mccollect(jobs, wait = TRUE, timeout = 120)
  expect_length(res, 2L)
  expect_false(any(vapply(res, inherits, logical(1), "try-error")))

  inShared <- list.files(shared, pattern = "\\.df$", full.names = TRUE)
  expect_length(inShared, 2L) ## one per table
  cd <- arrow::open_dataset(grep("cohortData", inShared, value = TRUE), format = "feather") |> as.data.frame()
  expect_equal(nrow(cd), nrow(big))
  for (i in 1:2) {
    f <- file.path(scratch, paste0("run", i), basename(inShared))
    expect_true(all(file.exists(f)))
    expect_equal(inode(f), inode(inShared))
  }
})

## ---- the event: runs with `.plotInitialTime = NA` ---------------------------------------------

## Runs `init` and then `save` of the module without the experiment (it needs Biomass_core), so the
## species table is the one `Init` builds from `argsForFactorial` and the cohortData table is empty.
saveEventRun <- function(outputPath, plotInitialTime = NA, shared = NULL) {
  withr::local_options(reproducible.destinationPathShared = shared, .local_envir = parent.frame())
  paths <- testPaths
  paths$outputPath <- outputPath
  paths$cachePath <- file.path(dirname(outputPath), "cache")
  sim <- SpaDES.core::simInit(
    times = list(start = 0, end = 0), modules = moduleName, paths = paths,
    params = list(Biomass_speciesFactorial = list(
      .plotInitialTime = plotInitialTime, runExperiment = FALSE, readExperimentFiles = FALSE)),
    objects = list(argsForFactorial = list(cohortsPerPixel = 1:2, growthcurve = c(0.65, 0.85),
                                           mortalityshape = c(20, 25), longevity = c(125, 225),
                                           mANPPproportion = c(3.5, 4.5)))
  )
  sim <- SpaDES.core::spades(sim, events = "init") ## schedules `save`; BSP's `init` would come next
  list(queued = SpaDES.core::events(sim), sim = SpaDES.core::spades(sim))
}

test_that("L137: the save event runs and sets the paths even with .plotInitialTime = NA", {
  scratch <- withr::local_tempdir()
  r <- saveEventRun(file.path(scratch, "outputs"), plotInitialTime = NA)
  ## `save` is queued at start(sim), before any other module's `init` (priority 1)
  q <- r$queued[r$queued$eventType == "save", ]
  expect_identical(nrow(q), 1L)
  expect_equal(as.numeric(q$eventTime), 0)
  expect_lt(q$eventPriority, 1)

  sim <- r$sim
  expect_false(is.null(sim$cohortDataFactorial_path))
  expect_false(is.null(sim$speciesTableFactorial_path))
  expect_identical(dirname(as.character(sim$cohortDataFactorial_path)), normalizePath(file.path(scratch, "outputs")))
  ## route 1: these are the paths a downstream module (Biomass_speciesParameters) is given and opens
  expect_s3_class(arrow::open_dataset(sim$cohortDataFactorial_path, format = "feather"), "Dataset")
  st <- arrow::open_dataset(sim$speciesTableFactorial_path, format = "feather") |> as.data.frame()
  expect_gt(nrow(st), 0L)
  expect_true("species" %in% names(st))
})

test_that("route 1: a second run with the same digest and outputPath does not rewrite the files", {
  scratch <- withr::local_tempdir()
  out <- file.path(scratch, "outputs")
  s1 <- saveEventRun(out)$sim
  f <- c(as.character(s1$cohortDataFactorial_path), as.character(s1$speciesTableFactorial_path))
  before <- list(inode = inode(f), mtime = mtime(f))
  Sys.sleep(1.1)
  s2 <- saveEventRun(out)$sim
  expect_identical(as.character(s2$cohortDataFactorial_path), f[[1]])
  expect_equal(inode(f), before$inode)
  expect_identical(mtime(f), before$mtime)
})
