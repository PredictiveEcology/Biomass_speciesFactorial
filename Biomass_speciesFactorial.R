defineModule(sim, list(
  name = "Biomass_speciesFactorial",
  description = paste("Build and simulate a fully factorial combination of selected",
                      "species traits to be used in LANDIS-II type models."),
  keywords = "",
  authors = c(
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(Biomass_speciesFactorial = "0.0.12"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Biomass_speciesFactorial.Rmd")), ## same file
  reqdPkgs = list("crayon", "ggplot2", "viridis", "terra",
                  "PredictiveEcology/LandR@development (>= 1.0.7.9025)",
                  "PredictiveEcology/SpaDES.project (>= 0.0.7.9013)"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "character", NA_character_, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("factorialSize", "character", "small", "medium", "large",
                    paste("If user does not supply an explicit argsForFactoria, then they can",
                          "specify either 'small', 'medium' or 'large' to take default ones that",
                          "have different numbers of factorial combinations.",
                          "Smaller is faster and uses less RAM; larger is slower and uses more RAM.")),
    defineParameter("maxBInFactorial", "integer", 5000L, NA, NA,
                    "The arbitrary maximum biomass for the factorial simulations. This ",
                    "is a per-species maximum within a pixel"),
    defineParameter("readExperimentFiles", "logical", TRUE, NA, NA,
                    paste("Reads all the cohortData files that were saved to disk during the",
                          "experiment. Note that this can be run even if `runExperiment = FALSE`.")),
    defineParameter("runExperiment", "logical", TRUE, NA, NA,
                    paste("A logical indicating whether to run the experiment (may take time).",
                          "See `readExperimentFiles`, which may be useful if the `cohortData` files",
                          "have already been saved and all that is needed is reading them in."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "argsForFactorial", objectClass = "list",
                 desc = paste(
                   "A named list of parameters in the species Table, with the range of values",
                   "they each should take. Internally, this module will run `expand.grid` on these,",
                   "then will take the 'upper triangle' of the array, including the diagonal."
                 ),
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortDataFactorial", objectClass = "data.table",
                  desc = paste(
                    "A large `cohortData` table (*sensu* `Biomass_core`) columns necessary for",
                    "running `Biomass_core`, e.g., `longevity`, `growthcurve`, `mortalityshape`, etc..",
                    "It will have unique species for unique combination of the `argsForFactorial`,",
                    "and a fixed  value for all other species traits.",
                    "Currently, these are set to defaults internally."
                  )
    ),
    createsOutput(objectName = "factorialOutputs", objectClass = "data.table",
                  desc = paste(
                    "A data.table of the `outputs(sim)` that is used during the factorial.",
                    "This will give the file names of all the `cohortData` files that were produced."
                  )
    ),
    createsOutput(objectName = "speciesTableFactorial", objectClass = "data.table",
                  desc = paste(
                    "A large species table (*sensu* `Biomass_core`) with all columns necessary for",
                    "running `Biomass_core`, e.g., `longevity`, `growthcurve`, `mortalityshape`, etc..",
                    "It will  have unique species for unique combination of the `argsForFactorial`,",
                    "and a fixed value for all other species traits.",
                    "Currently, these are set to defaults internally."
                  )
    )
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Biomass_speciesFactorial = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      if (isTRUE(P(sim)$runExperiment))
        sim <- scheduleEvent(sim, start(sim), "Biomass_speciesFactorial", "runExperiment", eventPriority = -1) # make it happen right away
      if (isTRUE(P(sim)$readExperimentFiles))
        sim <- scheduleEvent(sim, start(sim), "Biomass_speciesFactorial", "readExperimentFiles", eventPriority = -1) # make it happen right away
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Biomass_speciesFactorial", "plot", eventPriority = -1) # make it happen right away
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Biomass_speciesFactorial", "save")
    },
    runExperiment = {
      Cache(RunExperiment, speciesTableFactorial = sim$speciesTableFactorial, paths = mod$paths,
            times = mod$times, modules = modules(sim),
            maxBInFactorial = P(sim)$maxBInFactorial, factorialOutputs = sim$factorialOutputs,
            knownDigest = mod$dig, omitArgs = c("speciesTableFactorial", "factorialOutputs", "maxBInFactorial"))
    },
    readExperimentFiles = {
      sim$cohortDataFactorial <- Cache(ReadExperimentFiles, sim$factorialOutputs,
                                       .cacheExtra = mod$dig, omitArgs = c("factorialOutputs"))
    },
    plot = {
      plotFun(sim) # example of a plotting function
    },
    save = {
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

Init <- function(sim) {

  # The goal of this Init is to get the list of files so that we can "skip" the main runExperiment event
  #   if desired. We will still have the list of files that would be created.
  endTime <- max(sim$argsForFactorial$longevity)
  mod$times <- list(start = 0, end = endTime)

  message("Setting up factorial combinations of species traits, and associated initial cohortData table")
  mod$dig <- CacheDigest(sim$argsForFactorial)$outputHash
  mod$pathsOrig <- getPaths()
  on.exit({
    suppressMessages(do.call(setPaths, mod$pathsOrig))
  })
  mod$paths <- mod$pathsOrig
  mod$paths$outputPath <- file.path(dataPath(sim), paste0("factorial_", mod$dig))
  mod$paths$modulePath <- file.path(dataPath(sim), "module")
  # mod$paths$outputPath <- dataPath(sim)

  sim$factorialOutputs <- Cache(factorialOutputs, times = mod$times,
                                paths = mod$paths, .cacheExtra = mod$dig)
  # Next sequence is all dependent on argsForFactorial, so do digest once
  species1And2 <- Cache(do.call, factorialSpeciesTable, sim$argsForFactorial,
                        .cacheExtra = mod$dig, omitArgs = c("args"))
  speciesTable <- Cache(factorialSpeciesTableFillOut, species1And2,
                        .cacheExtra = mod$dig, omitArgs = "speciesTable")
  sim$speciesTableFactorial <- speciesTable

  return(invisible(sim))
}

factorialOutputs <- function(times, paths) {
  outputs <- data.frame(expand.grid(objectName = "cohortData",
                                    saveTime = unique(seq(times$start, times$end, by = 10)),
                                    eventPriority = 1, fun = "qs::qsave",
                                    stringsAsFactors = FALSE))
  suppressMessages({
    ss <- simInit(paths = paths, outputs = outputs, times = mod$times)
  })
  outputs(ss)
}

RunExperiment <- function(speciesTableFactorial, maxBInFactorial, knownDigest, factorialOutputs,
                          paths, times, modules) {
  speciesEcoregion <- Cache(factorialSpeciesEcoregion,
                            speciesTableFactorial,
                            maxBInFactorial = maxBInFactorial,
                            .cacheExtra = knownDigest,
                            omitArgs = c("speciesTable"))
  cohortData <- Cache(factorialCohortData,
                      speciesTableFactorial,
                      speciesEcoregion,
                      .cacheExtra = knownDigest,
                      omitArgs = c("speciesTable", "speciesEcoregion"))

  ## Maps
  pixelGroupMap <- factorialPixelGroupMap(cohortData)
  studyArea <- as.polygons(terra::ext(pixelGroupMap), crs="")
  # studyArea <- as(extent(pixelGroupMap), 'SpatialPolygons')
  crs(studyArea) <- crs(pixelGroupMap)
  rasterToMatch <- pixelGroupMap
  ecoregionMap <- pixelGroupMap
  levels(ecoregionMap) <- data.frame(ID = 1:max(cohortData$pixelGroup, na.rm = TRUE),
                                     ecoregion = 1, ecoregionGroup = 1, stringsAsFactors = TRUE)

  ## Simple Tables
  minRelativeB <- data.table("ecoregionGroup" = factor(1), minRelativeBDefaults())
  ecoregion <- data.table("ecoregionGroup" = as.factor(1), 'active' = 'yes')

  ## Make sppColors
  sppColors <- viridis::viridis(n = NROW(speciesTableFactorial))
  names(sppColors) <-  speciesTableFactorial$species


  if (!"Biomass_core" %in% unlist(modules)) {
    moduleNameAndBranch <- c("PredictiveEcology/Biomass_core@development (>= 1.3.9)")
    modules <- Require::extractPkgName(moduleNameAndBranch)
    getModule(moduleNameAndBranch, modulePath = paths$modulePath, overwrite = TRUE) # will only overwrite if wrong version
  } else {
    paths$modulePath <- mod$pathsOrig$modulePath
    modules <- "Biomass_core"
  }

  parameters <- list(
    Biomass_core = list(
      .maxMemory = 1e9,
      .plots = NULL,
      .saveInitialTime = NA,
      .saveInterval = NA,
      .useParallel = 1,
      .useCache = NULL,
      calcSummaryBGM = NULL,
      initialBiomassSource = "cohortData",
      seedingAlgorithm = "noSeeding",
      sppEquivCol = "B_factorial",
      successionTimestep = 10,
      vegLeadingProportion = 0
    )
  )

  # Get modules
  #Tree species that are important to us
  speciesLayers <- "species"

  ## 2022-06-30 AMC: cannot pass an empty data.table or Biomass_core tries to guess with
  ## actual species names, and will fail when calling sppHarmonize().
  sppEquivFactorial <- data.table(B_factorial = speciesTableFactorial$species)

  ## speciesLayers needed or module stops, but object unused
  objects <- list(
    cohortData = cohortData,
    ecoregion = ecoregion,
    ecoregionMap = ecoregionMap,
    minRelativeB = minRelativeB,
    pixelGroupMap = pixelGroupMap,
    species = speciesTableFactorial,
    speciesEcoregion = speciesEcoregion,
    speciesLayers = speciesLayers,
    sppColorVect = sppColors,
    sppEquiv = sppEquivFactorial,
    sppNameVector = speciesTableFactorial$species,
    studyArea = studyArea,
    rasterToMatch = rasterToMatch
  )

  opts <- options(
    "LandR.assertions" = FALSE,
    "LandR.verbose" = 0,
    "spades.recoveryMode" = FALSE,
    "spades.moduleCodeChecks" = FALSE # Turn off all module's code checking
  )

  message("Running simulation with all combinations; cohortData objects are saved in ", paths$outputPath)
  on.exit({
    suppressMessages(do.call(setPaths, mod$pathsOrig))
    options(opts)
  }, add = TRUE)

  mySimOut <- Cache(simInitAndSpades,
                    times = times,
                    params = parameters,
                    modules = modules,
                    paths = paths,
                    objects = objects,
                    outputs = factorialOutputs,
                    # quick = "paths",
                    debug = 1,
                    outputObjects = "pixelGroupMap",
                    .cacheExtra = list(knownDigest, paths$outputPath),
                    omitArgs = c("objects", "params", "debug", "paths"))

  return(invisible())
}

ReadExperimentFiles <- function(factorialOutputs) {
  factorialOutputs <- as.data.table(factorialOutputs)[objectName == "cohortData"]
  fEs <- .fileExtensions()
  cdsList <- by(factorialOutputs, factorialOutputs[, "saveTime"], function(x) {
    fE <- reproducible:::fileExt(x$file)
    wh <- fEs[fEs$exts %in% fE,]
    message(crayon::green("reading: "))
    cat(crayon::green(x$file, "..."))
    cd <- getFromNamespace(wh$fun, ns = asNamespace(wh$package))(x$file)[, .(speciesCode, age, B, pixelGroup)]
    cat(crayon::green(" Done!\n"))
    return(cd)
  })
  message("rbindlisting the cohortData objects")
  cds <- rbindlist(cdsList, use.names = TRUE, fill = TRUE)

  return(invisible(cds))
}

### template for plot events
plotFun <- function(sim) {
  cohortDataForPlot <- Cache(subsampleForPlot, sim$cohortDataFactorial,
                             sim$speciesTableFactorial,
                             .cacheExtra = mod$dig,
                             omitArgs = c("cds", "speciesTableFactorial"))
  # Filename on Windows can't have colon ":"
  Plots(cohortDataForPlot, usePlot = FALSE,
        fn = ggplotFactorial, filename = paste0("cohortFactorial_", gsub(":", "_", Sys.time())),
        ggsaveArgs = list( width = 12, height = 7))
  return(invisible(sim))
}

subsampleForPlot <- function(cds, speciesTableFactorial) {
  uniq <- unique(cds$pixelGroup)
  sam <- Cache(sample, uniq, 64)
  ff <- cds[pixelGroup %in% sam];
  ff[, Sp := gsub(".+_", "", speciesCode)];
  ff[grep("^Sp", Sp, invert = TRUE), Sp := "Single"]
  ff[, maxB := max(B), by = "pixelGroup"]
  setkeyv(ff, c("pixelGroup", "Sp", "age"))
  ff[, diffB := B[2] - B[1], by = c("age", "pixelGroup")]
  ff[is.na(diffB), diffB := 0]
  ff[, maxDiffB := max(diffB, na.rm = TRUE), by = c("pixelGroup")]
  setorderv(ff, "maxDiffB")

  ff <- speciesTableFactorial[ff, on = c("species" = "speciesCode")]

  ff[, Title := paste0(maxDiffB, "_", pixelGroup)]
  ff[,
     params := paste0(unique(Sp),"(l=",unique(longevity),";g=",unique(growthcurve), ";m=",unique(mortalityshape),";p=", unique(mANPPproportion ),")"),
     by = c("Sp", "pixelGroup")]
  ff[, Title := paste0(unique(params), collapse = "\n"), by = "pixelGroup"]
  ff
}

ggplotFactorial <- function(ff) {
  sam <- unique(ff$pixelGroup)
  title <- paste0("Factorial Experiment: ", length(sam), " random plot")
  gg1 <- ggplot(ff, aes(x = age, y = B, colour = Sp)) +
    geom_line() +
    facet_wrap(~ Title, nrow = ceiling(sqrt(length(sam))), scales = "fixed") +
    ggtitle(title) +
    theme(strip.text.x = element_text(size = 5)) #+

  gg1
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("argsForFactorial")) {
    sim$argsForFactorial <-
      switch(P(sim)$factorialSize,
             large = list(cohortsPerPixel = 1:2,
                          growthcurve = seq(0.65, 0.85, 0.02),
                          mortalityshape = seq(20, 25, 2),
                          longevity = seq(125, 600, 25), # not 600 -- too big
                          mANPPproportion = seq(3.5, 6, 0.25)),
             medium = list(cohortsPerPixel = 1:2,
                           growthcurve = seq(0.65, 0.85, 0.02),
                           mortalityshape = seq(20, 25, 2),
                           longevity = seq(125, 600, 50),
                           mANPPproportion = seq(3.5, 6, 0.3)),
             small = list(cohortsPerPixel = 1:2,
                          growthcurve = seq(0.65, 0.85, 0.2),
                          mortalityshape = seq(20, 25, 5),
                          longevity = seq(125, 600, 100),
                          mANPPproportion = seq(3.5, 6, 1))
      )
  }
  return(invisible(sim))
}
