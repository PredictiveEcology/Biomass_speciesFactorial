## The biomass the factorial's cohorts start at. A numeric `initialB` starts every cohort there;
## `NA` starts each at its species' maxANPP, as a new cohort on bare ground starts in LANDIS-II
## Biomass Succession and in Biomass_core (`initialB = NA`).

sp <- factorialSpeciesTable(
  growthcurve = 0.7, mortalityshape = 20L, longevity = 150L,
  mANPPproportion = c(3.5, 5), cohortsPerPixel = 1:2
)
spEco <- factorialSpeciesEcoregion(sp, maxBInFactorial = 5000L)

test_that("a numeric initialB starts every cohort at that biomass", {
  cd <- factorialCohortData(sp, spEco, initialB = 10)
  expect_identical(nrow(cd), nrow(sp))
  expect_identical(unique(cd$age), 1L)
  expect_equal(unique(cd$B), 10)
})

test_that("initialB = NA starts each cohort at its species' maxANPP", {
  cd <- factorialCohortData(sp, spEco, initialB = NA)
  expect_identical(as.character(cd$speciesCode), as.character(spEco$speciesCode))
  ## 5000 * 3.5 / 100 and 5000 * 5 / 100, not maxBInFactorial / 30 (167)
  expect_setequal(unique(cd$B), c(175L, 250L))
  expect_equal(cd$B, spEco$maxANPP)
})

test_that("the NA rule has its own digest, and a numeric initialB keeps its old one", {
  args <- list(longevity = 150L)
  old <- function(initialB) reproducible::CacheDigest(list(args, initialB, 9, 5000L))$outputHash
  expect_false(identical(factorialDigest(args, NA, 9, 5000L), old(NA)))
  expect_identical(factorialDigest(args, 10, 9, 5000L), old(10))
})
