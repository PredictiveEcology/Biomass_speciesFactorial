## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "Biomass_speciesFactorial",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Eliot", "J.B."), family = "McIntire", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.9.9004", Biomass_speciesFactorial = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Biomass_speciesFactorial.Rmd")), ## same file
  reqdPkgs = list("ggplot2", "raster", "PredictiveEcology/LandR@minRelativeB (>= 1.0.6.9007)",
                  "crayon"),
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
    defineParameter("runExperiment", "logical", TRUE, NA, NA,
                    "A logical indicating whether to run the experiment (may take time). See ",
                    "readExperimentFiles, which may be useful if the cohortData files have already ",
                    "been saved and all that is needed is reading them in."),
    defineParameter("readExperimentFiles", "logical", TRUE, NA, NA,
                    "Reads all the cohortData files that were saved to disk during the ",
                    "experiment. Note that this can be run even if 'runExperiment = FALSE'."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "argsForFactorial", objectClass = "list",
                 desc = "A named list of parameters in the species Table, with the range of values ",
                 "they each should take. Internally, this module will run expand.grid on these, then ",
                 "will take the 'upper triangle' of the array, including the diagonal", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "factorialOutputs", objectClass = "data.table",
                  desc = "A data.table of the outputs(sim) that is used during the factorial. ",
                  "This will give the file names of all the cohortData files that were produced."),
    createsOutput(objectName = "speciesTableFactorial", objectClass = "data.table",
                  desc = "A large species table (sensu Biomass_core) with all columns necessary for ",
                  "running Biomass_core, e.g., longevity, growthcurve, mortalityshape, etc. It will ",
                  "have unique species for unique combination of the argsForFactorial, and a fixed ",
                  "value for all other species traits. Currently, these are set to defaults internally."),
    createsOutput(objectName = "cohortDataFactorial", objectClass = "data.table",
                  desc = "A large cohortData table (sensu Biomass_core) columns necessary for ",
                  "running Biomass_core, e.g., longevity, growthcurve, mortalityshape, etc. It will ",
                  "have unique species for unique combination of the argsForFactorial, and a fixed ",
                  "value for all other species traits. Currently, these are set to defaults internally.")

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
      sim <- RunExperiment(sim)
    },
    readExperimentFiles = {
      sim <- ReadExperimentFiles(sim)
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

### template initialization
Init <- function(sim) {

  # The goal of this Init is to get the list of files so that we can "skip" the main runExperiment event
  #   if desired. We will still have the list of files that would be created.
  mod$times <- list(start = 0, end = max(sim$argsForFactorial$longevity))
  outputs <- data.frame(expand.grid(objectName = "cohortData",
                                    saveTime = unique(seq(mod$times$start, mod$times$end, by = 10)),
                                    eventPriority = 1, fun = "qs::qsave",
                                    stringsAsFactors = FALSE))
  mod$pathsOrig <- getPaths()
  on.exit({
    suppressMessages(do.call(setPaths, mod$pathsOrig))
  })
  mod$paths <- mod$pathsOrig
  mod$paths$outputPath <- dataPath(sim)
  suppressMessages(ss <- simInit(paths = mod$paths, outputs = outputs, times = mod$times))
  sim$factorialOutputs <- outputs(ss)
  message("Setting up factorial combinations of species traits, and associated initial cohortData table")

  # Next sequence is all dependent on argsForFactorial, so do digest once
  dig <- fastdigest::fastdigest(sim$argsForFactorial)
  species1And2 <- Cache(do.call, factorialSpeciesTable, sim$argsForFactorial,
                        .cacheExtra = dig, omitArgs = c("args"))
  mod$speciesEcoregion <- Cache(factorialSpeciesEcoregion, species1And2, .cacheExtra = dig, omitArgs = c("speciesTable"))
  mod$cohortData <- Cache(factorialCohortData, species1And2, mod$speciesEcoregion, .cacheExtra = dig,
                          omitArgs = c("speciesTable", "speciesEcoregion"))
  speciesTable <- Cache(factorialSpeciesTableFillOut, species1And2, .cacheExtra = dig,
                        omitArgs = "speciesTable")
  sim$speciesTableFactorial <- speciesTable[,.(species, longevity, growthcurve, mortalityshape, mANPPproportion)]

  return(invisible(sim))
}

RunExperiment <- function(sim) {

  # Maps
  pixelGroupMap <- factorialPixelGroupMap(mod$cohortData)
  studyArea <- as(extent(pixelGroupMap), 'SpatialPolygons')
  crs(studyArea) <- crs(pixelGroupMap)
  rasterToMatch <- pixelGroupMap
  ecoregionMap <- pixelGroupMap
  levels(ecoregionMap) <- data.frame(ID = 1:max(mod$cohortData$pixelGroup, na.rm = TRUE),
                                     ecoregion = 1, ecoregionGroup = 1, stringsAsFactors = TRUE)

  # Simple Tables
  minRelativeB <- data.table("ecoregionGroup" = factor(1), minRelativeBDefaults())
  ecoregion <- data.table("ecoregionGroup" = as.factor(1), 'active' = 'yes')

  #Make sppColors
  sppColors <- viridis::viridis(n = NROW(sim$speciesTableFactorial))
  names(sppColors) <-  sim$speciesTableFactorial$species

  moduleNameAndBranch <- c("Biomass_core@development") #, "Biomass_speciesParameters@EliotTweaks")
  modules <- gsub("@.+", "", moduleNameAndBranch)
  if (!dir.exists(file.path(modulePath(sim), modules)))
    try(Cache(getModule, file.path("PredictiveEcology", moduleNameAndBranch), #modulePath = getPaths()$modulePath,
              overwrite = FALSE))

  parameters <- list(
    Biomass_core = list(.saveInitialTime = NA,
                        .saveInterval = NA,
                        .useParallel = 1,
                        seedingAlgorithm = "noSeeding",
                        calcSummaryBGM = NULL,
                        .plots = NULL,
                        .maxMemory = 1e9,
                        .useCache = ".inputObjects",
                        successionTimestep = 10,
                        initialBiomassSource = "cohortData",
                        vegLeadingProportion = 0
    ))

  # Get modules
  #Tree species that are important to us
  speciesLayers <- "species"

  #sppEquiv needed or module stops, but object unused, likewise with speciesLayers
  objects <- list(
    "studyArea" = studyArea,
    "rasterToMatch" = rasterToMatch,
    cohortData = mod$cohortData,
    species = sim$speciesTableFactorial,
    speciesEcoregion = mod$speciesEcoregion,
    pixelGroupMap = pixelGroupMap,
    speciesLayers = speciesLayers,
    minRelativeB = minRelativeB,
    ecoregion = ecoregion,
    ecoregionMap = ecoregionMap,
    sppEquiv = data.table(),
    sppColorVect = sppColors
  )


  # set.seed(161616)

  opts <- options(
    "LandR.assertions" = FALSE,
    "LandR.verbose" = 0,
    "spades.recoveryMode" = FALSE,
    "spades.moduleCodeChecks" = FALSE # Turn off all module's code checking
  )


  message("Running simulation with all combinations; cohortData objects are saved in ", mod$paths$outputPath)
  on.exit({
    suppressMessages(do.call(setPaths, pathsOrig))
    options(opts)
  })
  # Put this in now in case it crashes
  mySimIn <- simInit(
    times = mod$times, params = parameters, modules = modules,
    paths = mod$paths,
    objects = objects, outputs = sim$factorialOutputs#,
    #    debug = 1,
    #.cacheExtra = dig, omitArgs = c("params", "paths", "objects", "outputs")
  )
  # don't need the simList --> we are doing this for the sideeffects of cohortData files
  mySimOut <- spades(mySimIn, debug = 1)
  return(invisible(sim))
}

ReadExperimentFiles <- function(sim) {


  fEs <- .fileExtensions()
  cdsList <- by(sim$factorialOutputs, sim$factorialOutputs[, "saveTime"], function(x) {
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

  sim$cohortDataFactorial <- cds
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  Plots(sim$cohortDataFactorial, usePlot = FALSE,
        fn = ggplotFactorial, filename = paste0("cohortFactorial_", Sys.time()),
        speciesTableFactorial = sim$speciesTableFactorial,
        ggsaveArgs = list( width = 12, height = 7))
  return(invisible(sim))
}


ggplotFactorial <- function(cds, speciesTableFactorial) {
  uniq <- unique(cds$pixelGroup)
  cds[, Sp := gsub(".+_", "", speciesCode)];
  cds[grep("^Sp", Sp, invert = TRUE), Sp := "Single"]
  cds[, maxB := max(B), by = "pixelGroup"]
  setkeyv(cds, c("pixelGroup", "Sp", "age"))
  cds[, diffB := B[2] - B[1], by = c("age", "pixelGroup")]
  cds[is.na(diffB), diffB := 0]
  cds[, maxDiffB := max(diffB, na.rm = TRUE), by = c("pixelGroup")]
  setorderv(cds, "maxDiffB")

  # maxDiffBs <- head(unique(cds$maxDiffB), 40)

  sam <- Cache(sample, uniq, 64)
  ff <- cds[pixelGroup %in% sam];
  # ff <- cds[maxDiffB %in% maxDiffBs];
  ff <- speciesTableFactorial[ff, on = c("species" = "speciesCode")]

  # ff <- ff[pixelGroup %in% sample(unique(ff$pixelGroup), 49)];
  ff[, Title := paste0(maxDiffB, "_", pixelGroup)]
  ff[,
     params := paste0(unique(Sp),"(l=",unique(longevity),";g=",unique(growthcurve), ";m=",unique(mortalityshape),";p=", unique(mANPPproportion ),")"),
     by = c("Sp", "pixelGroup")]
  ff[, Title := paste0(unique(params), collapse = "\n"), by = "pixelGroup"]
  title <- paste0("Factorial Experiment: ", length(sam), "random")
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
    sim$argsForFactorial <- list(cohortsPerPixel = 1:2,
                                 growthcurve = seq(0.65, 0.85, 0.02),
                                 mortalityshape = seq(20, 25, 1),
                                 longevity = seq(125, 300, 25),
                                 mANPPproportion = seq(3.5, 6, 0.25))
  }
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
