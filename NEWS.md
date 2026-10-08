Known issues: <https://github.com/PredictiveEcology/Biomass_speciesFactorial/issues>

# Biomass_speciesFactorial (development version)

* Save `cohortDataFactorial` and `speciesTableFactorial` as one feather file each, named by the module digest, via `prepInputs()` with a `dlFun` that runs only if the file is missing. With `reproducible.destinationPathShared` set, one shared copy is written and each run's `outputPath(sim)` gets a hard link, instead of a 1.5 GB `arrow::write_dataset()` copy per run. With `readExperimentFiles = FALSE` there is no cohortData table, so only the species table is saved and `cohortDataFactorial_path` is not set. Add `arrow` to `reqdPkgs`.
* Schedule the `save` event at `start(sim)` instead of `.plotInitialTime`: with `.plotInitialTime = NA` it never ran and `cohortDataFactorial_path`/`speciesTableFactorial_path` stayed `NULL` (found by Alex Chubaty).
* Fix the digest of the factorial (`mod$dig`): `minCohortBiomass` and `maxBInFactorial` were not in it (misplaced bracket, found by Alex Chubaty), and `P(sim)$minCohortB` partial-matched `minCohortBiomass`. File and cache names change, so the experiment reruns once.

# Biomass_speciesFactorial 1.0.1

* Wire the factorial outputs through `registerOutputs()`: register the `cohortDataFactorial` and `speciesTableFactorial` dataset paths as module outputs, coercing the paths to character because `registerOutputs()` chokes on the `fs_path` class.
* Migrate persisted-object storage from `qs` to `qs2` (drop `qs` from `reqdPkgs`; update the Rmd/md accordingly).
* Pass `initialB` through to `Biomass_core` alongside `minCohortBiomass`, and derive factorial save times from `times$start`/`times$end` instead of a hardcoded `seq(0, 100, by = 10)`.
* Fix Cache digest handling.

# Biomass_speciesFactorial 1.0.0

* Store the factorial results as on-disk `arrow` datasets: write `cohortDataFactorial` and `speciesTableFactorial` via `arrow::write_dataset()` (feather format, faster than parquet at this scale), add module parameters pointing to the dataset paths, and drop the in-memory arrow pointers before `Cache()`-ing the `simList` (PR #11).
* Rebuild the module manual/vignette from the current template: LaTeX/formatting fixes, add `citations/` (bibliography plus Ecology Letters CSL), and fix the GitHub Actions Rmd-render workflow.
* Merge contributed fixes (PR #8): update the SpaDES.project pointer.

# Biomass_speciesFactorial 0.0.13

* Add cohort-biomass parameters `initialB` and `minCohortBiomass`: default `initialB` to `round(maxBInFactorial / 30)` (LANDIS-II BSM default) when `NA`, guard against `NA` values, and validate that `initialB > minCohortBiomass`.
* Make `Biomass_core` execution optional and decoupled: move `runExperiment`/`readExperimentFiles` into the `init` event as `Cache()`d calls, reuse an existing `Biomass_core` in the project when present (otherwise fetch it via `getModule()`), and stop forcing all modules to be run together.
* Modernize the toolchain: switch from `SpaDES.install` to `SpaDES.project`, require `SpaDES.core (>= 2.0.2.9010)`, replace `raster` with `terra`, add `data.table` to `reqdPkgs`, qualify `getModule()` calls with the `PredictiveEcology/` prefix, and use `inputPath()` instead of `dataPath()`.
* Metadata and housekeeping: use the `person` class for authors, pass `sppEquiv`/`sppNameVector`/`sppEquivCol` to `Biomass_core`, keep nested module code under a `submodules/` subdirectory, and update the GitHub Actions Rmd workflow.
