---
title: "Biomass_speciesFactorial Manual"
subtitle: "v.0.0.13"
date: "Last updated: 2025-08-25"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_Biomass_speciesFactorial.bib
link-citations: true
always_allow_html: true
---

# Biomass_speciesFactorial Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:Biomass-speciesFactorial) *Biomass_speciesFactorial*




[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Create and run a factorial simulation experiment for LANDIS-II-like species traits.

### Module inputs and parameters

Table \@ref(tab:moduleInputs-Biomass-speciesFactorial) shows the full list of module inputs.

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-Biomass-speciesFactorial)(\#tab:moduleInputs-Biomass-speciesFactorial)List of (ref:Biomass-speciesFactorial) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> argsForFactorial </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> A named list of parameters in the species Table, with the range of values they each should take. Internally, this module will run `expand.grid` on these, then will take the 'upper triangle' of the array, including the diagonal. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-Biomass-speciesFactorial))


<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-Biomass-speciesFactorial)(\#tab:moduleParams-Biomass-speciesFactorial)List of (ref:Biomass-speciesFactorial) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> screen </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used by Plots function, which can be optionally used here </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .seed </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of seeds to use for each event (names). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should caching of events or module be used? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> factorialSize </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> medium </td>
   <td style="text-align:left;"> small </td>
   <td style="text-align:left;"> large </td>
   <td style="text-align:left;"> If user does not supply an explicit `argsForFactorial`, then they can specify either 'small', 'medium' or 'large' to take default ones that have different numbers of factorial combinations. Smaller is faster and uses less RAM; larger is slower and uses more RAM. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> initialB </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> initial cohort biomass at age = 1. If NA, will use `maxBInFactorial`/30 akin to the LANDIS-II Biomass Succession default. Must be greater than `P(sim)$minCohortBiomass </td>
  </tr>
  <tr>
   <td style="text-align:left;"> maxBInFactorial </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 5000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The arbitrary maximum biomass for the factorial simulations. This is a per-species maximum within a pixel. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minCohortBiomass </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The smallest amount of biomass before a cohort is removed from a simulation. Barring removal via this parameter, cohorts can persist with B = 1 until age = longevity. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> readExperimentFiles </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Reads all the `cohortData` files that were saved to disk during the experiment. Note that this can be run even if `runExperiment = FALSE`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> runExperiment </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> A logical indicating whether to run the experiment (may take time). See `readExperimentFiles`, which may be useful if the `cohortData` files have already been saved and all that is needed is reading them in. </td>
  </tr>
</tbody>
</table>

### Events

<!-- TODO -->
Describe what happens for each event type.

### Plotting

<!-- TODO -->
Write what is plotted.

### Saving

<!-- TODO -->
Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-Biomass-speciesFactorial)).

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-Biomass-speciesFactorial)(\#tab:moduleOutputs-Biomass-speciesFactorial)List of (ref:Biomass-speciesFactorial) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> cohortDataFactorial </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A large `cohortData` table ( sensu `Biomass_core`) columns necessary for running `Biomass_core`, e.g., `longevity`, `growthcurve`, `mortalityshape`, etc.. It will have unique species for unique combination of the `argsForFactorial`, and a fixed value for all other species traits. Currently, these are set to defaults internally. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> factorialOutputs </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A data.table of the `outputs(sim)` that is used during the factorial. This will give the file names of all the `cohortData` files that were produced. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesTableFactorial </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A large species table ( sensu `Biomass_core`) with all columns necessary for running `Biomass_core`, e.g., `longevity`, `growthcurve`, `mortalityshape`, etc.. It will have unique species for unique combination of the `argsForFactorial`, and a fixed value for all other species traits. Currently, these are set to defaults internally. </td>
  </tr>
</tbody>
</table>

## Usage

<!-- TODO: updates and testing needed -->

:::{.rmdwarning}
⚠️ this section is very out of date and is unlikely to work ⚠️
:::


``` r
## This script is for running the species traits factorial design.
## It requires a version of Biomass_core that is .gitignored:
##  - the version had many changes to accommodate the 'no regeneration' scenario;
## - many of the other changes have been subsequently incorporated into Biomass_core, so it may work with a newer version
options(
  reproducible.cacheSaveFormat = "qs",
  reproducible.showSimilar = TRUE,
  reproducible.showSimilarDepth = 5,
  reproducible.useMemoise = FALSE,
  spades.moduleCodeChecks = FALSE,
  spades.recoveryMode = FALSE
)

## Packages
if (!require("Require")) {
  install.packages("Require"); require("Require")
}
Require("PredictiveEcology/SpaDES.install", upgrade = FALSE)
installSpaDES()
Require(c("PredictiveEcology/SpaDES.core@development (== 1.0.9.9004)",
          "reproducible"), upgrade = FALSE)

## Modules
setPaths(rasterPath = tempdir(),
         cachePath =  file.path(tempdir(), "Cache"),
         modulePath = file.path("modules"),
         inputPath = file.path(getwd(), "inputs"),
         outputPath = file.path(getwd(),"outputs"))

moduleNameAndBranch <- c("Biomass_speciesParameters@EliotTweaks",
                         "Biomass_speciesFactorial@main")
lapply(moduleNameAndBranch, function(modName) {
  Cache(getModule, file.path("PredictiveEcology", modName), overwrite = TRUE)
})
modules <- gsub("@.+", "", moduleNameAndBranch)
modules <- c("Biomass_speciesFactorial", modules)

outputs <- data.frame(expand.grid(
  objectName = c("species", "speciesEcoregion"),
  saveTime = 0,
  eventPriority = 10, fun = "qs::qsave",
  stringsAsFactors = FALSE
))

## Slow and large
objects <- list(argsForFactorial = list(cohortsPerPixel = 1:2,
                                        growthcurve = seq(0.65, 0.85, 0.02),
                                        mortalityshape = seq(20, 25, 1),
                                        longevity = seq(125, 400, 25), # not 600 -- too big
                                        mANPPproportion = seq(3.5, 6, 0.25))
)

## Fast
objects <- list(argsForFactorial = list(cohortsPerPixel = 1:2,
                                        growthcurve = seq(0.65, 0.85, 0.1),
                                        mortalityshape = seq(20, 25, 5),
                                        longevity = seq(125, 600, 100),
                                        mANPPproportion = seq(3.5, 6, 1))
)
## Medium
objects <- list(argsForFactorial = list(cohortsPerPixel = 1:2,
                                        growthcurve = seq(0.65, 0.85, 0.02),
                                        mortalityshape = seq(20, 25, 2),
                                        longevity = seq(125, 600, 50),
                                        mANPPproportion = seq(3.5, 6, 0.3))
)

simOut <- simInitAndSpades(
  times = list(start = 0, end = 0), # params = parameters, 
  modules = modules, 
  params = list(Biomass_speciesFactorial = list(.plots = NA,#"pdf",
                                                runExperiment = T),
                Biomass_speciesParameters = list(.plots = "pdf")),
  outputs = outputs,
  objects = objects,
  debug = 1
)
```


### Links to other modules

- <https://github.com/PredictiveEcology/Biomass_speciesData>;
- <https://github.com/PredictiveEcology/Biomass_speciesParameterization>;
- <https://github.com/PredictiveEcology/Biomass_borealDataPrep>;
- <https://github.com/PredictiveEcology/Biomass_core>;

### Getting help

- <https://github.com/PredictiveEcology/Biomass_speciesFactorial/issues>

## References

<!-- autogenerated from bibligraphy -->
