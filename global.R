getOrUpdatePkg <- function(p, minVer, repo) {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("Require", "0.3.1.14")
getOrUpdatePkg("SpaDES.project", "0.0.8.9027")
getOrUpdatePkg("reproducible", "2.0.10.9005")
getOrUpdatePkg("SpaDES.core", "2.0.3.9001")

################### RUNAME

if (SpaDES.project::user("tmichele")) setwd("~/projects/Edehzhie/")
if (SpaDES.project::user("emcintir")) {
  # SpaDES.project::pkgload2("~/GitHub/SpaDES.project")
  setwd("~/GitHub")
  .fast <- T
}
################ SPADES CALL
library(SpaDES.project)
out <- SpaDES.project::setupProject(
  runName = "IansTest",
  updateRprofile = TRUE,
  Restart = TRUE,
  paths = list(projectPath = runName,
               # inputPath = "inputs",
               scratchPath = "~/scratch"),
  modules =
    file.path("PredictiveEcology",
              c("canClimateData@usePrepInputs",
                paste0(# development
                  c("Biomass_borealDataPrep",
                    "Biomass_core",
                    "Biomass_speciesData",
                    "Biomass_speciesFactorial",
                    "Biomass_speciesParameters",
                    "fireSense_IgnitionFit",
                    "fireSense_EscapeFit",
                    "fireSense_SpreadFit",
                    "fireSense_dataPrepFit",
                    "fireSense_dataPrepPredict",
                    "fireSense_IgnitionPredict",
                    "fireSense_EscapePredict",
                    "fireSense_SpreadPredict"),
                  "@development")
              )),
  functions = "tati-micheletti/Edehzhie@master/inputs/outterFuns.R",
  options = list(spades.allowInitDuringSimInit = TRUE,
                 spades.allowSequentialCaching = F,
                 reproducible.showSimilar = TRUE,
                 reproducible.memoisePersist = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 reproducible.inputPaths = "~/data",
                 # reproducible.inputPaths = "/mnt/e/linux/data",
                 LandR.assertions = FALSE,
                 reproducible.cacheSpeed = "fast",
                 reproducible.gdalwarp = TRUE,
                 reproducible.showSimilarDepth = 7,
                 gargle_oauth_cache = if (machine("W-VIC-A127585")) "~/.secret" else NULL,
                 gargle_oauth_email =
                   if (user("emcintir")) "eliotmcintire@gmail.com" else if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 SpaDES.project.fast = isTRUE(.fast),
                 spades.recoveryMode = FALSE
  ),
  times = list(start = 2011,
               end = 2025),
  params = list(.globals = list(.plots = NA,
                                .plotInitialTime = NA,
                                sppEquivCol = 'Boreal',
                                .useCache = c(".inputObjects", "init", "prepIgnitionFitData"))),
  # require = "PredictiveEcology/reproducible@reproducibleTempCacheDir (>= 2.0.8.9010)", # so can use Cache next
  # studyArea = Cache(studyAreaGenerator()),
  # rasterToMatch = Cache(rtmGenerator(sA = studyArea)),
  # studyAreaLarge = Cache(studyAreaGenerator(large = TRUE, destPath = paths[["inputPath"]])),
  # rasterToMatchLarge = Cache(rtmGenerator(sA = studyAreaLarge, destPath = paths[["inputPath"]])),
  # sppEquiv = sppEquiv_CA(runName),
  # params = list(.globals = list(sppEquivCol = runName,
  #                               dataYear = "2011",
  #                               .plotInitialTime = NA,
  #                               .useCache = c(".inputObjects", "init")),
  #                               #.studyAreaName = "NT"),
  #               fireSense_IgnitionFit = list(lb = list(coef = 0,
  #                                                      #I got this from running only dataPrepFit, then quantile(MDC, probs = 0.05) NOTE: Ian's notes
  #                                                      knots = list(MDC = 100)),
  #                                            ub = list(coef = 20,
  #                                                      #and the upper quantile was 0.9 NOTE: Ian's notes
  #                                                      knots = list(MDC = 156))),
  #               canClimateData = list(.runName = runName,
  #                                     .useCache = ".inputObjects",
  #                                     climateGCM = "CanESM5",
  #                                     climateSSP = "370",
  #                                     historicalFireYears = 1991:2020)#,
  #                                     #studyAreaName = "NT")
  # ),
  studyArea = (function(x) {Canada <- prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000b21a_e.zip",
                                                 destinationPath = paths$inputPath)
  Ecozones <- prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                         desitnationPath = paths$inputPath)
  NWT <- Canada[Canada$PRENAME == "Northwest Territories",]

  Taiga <- Ecozones[Ecozones$ZONE_NAME == "Taiga Shield",]

  TaigaNWT <- postProcess(Taiga, cropTo = NWT, maskTo = NWT, projectTo = NWT)
  TaigaNWT})() |> Cache()
  ,
  studyAreaLarge = studyArea,
  require = c("reproducible", "SpaDES.core", "PredictiveEcology/LandR@development (>= 1.1.0.9073"),
  packages = c("googledrive", 'RCurl', 'XML',
               "PredictiveEcology/SpaDES.core@sequentialCaching (>= 2.0.3.9000)",
               "PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9004)"),
  useGit = "sub"
)

if (SpaDES.project::user("emcintir"))
  SpaDES.project::pkgload2(
    list(file.path("~/GitHub", c("reproducible", "SpaDES.core", "SpaDES.tools", "LandR", "climateData", "fireSenseUtils",
                                 "PSPclean")),
         "~/GitHub/SpaDES.project"))
unlink(dir(tempdir(), recursive = TRUE, full.names = TRUE))
# undebug(reproducible:::.callArchiveExtractFn)
snippsim <- do.call(SpaDES.core::simInitAndSpades, out)


