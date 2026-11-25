test_that("speciesLongevities gives the unique integer longevities", {
  sp <- data.frame(species = c("a", "b", "c"), longevity = c(240, 140, 240))
  expect_identical(speciesLongevities(sp), c(140L, 240L))
  expect_error(speciesLongevities(data.frame(species = "a")), "longevity")
})
