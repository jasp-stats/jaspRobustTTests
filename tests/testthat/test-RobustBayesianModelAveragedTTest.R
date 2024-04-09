context("Robust Bayesian Model-Averaged T-Test")

# all output test
options <- analysisOptions("RobustBayesianModelAveragedTTest")
options$advancedMcmcAdaptation <- 5000
options$advancedMcmcChains <- 4
options$advancedMcmcSamples <- 5000
options$advancedMcmcThin <- 1
options$bayesFactorType <- "BF10"
options$dependent <- "contNormal"
options$group <- "facExperim"
options$inferenceCiWidth <- 0.95
options$inferenceConditionalParameterEstimates <- TRUE
options$inferenceIndividualModels <- TRUE
options$inferenceIndividualModelsSingleModel <- FALSE
options$inferenceIndividualModelsSingleModelNumber <- 1
options$inferenceModelsOverview <- TRUE
options$inferenceModelsOverviewBfComparison <- "inclusion"
options$inferenceModelsOverviewOrder <- "modelNumber"
options$inferencePrecisionAllocationAsStandardDeviationRatio <- TRUE
options$inferenceShortenPriorName <- FALSE
options$mcmcDiagnosticsOverviewTable <- TRUE
options$mcmcDiagnosticsPlotEffect <- TRUE
options$mcmcDiagnosticsPlotOutliers <- FALSE
options$mcmcDiagnosticsPlotSingleModel <- FALSE
options$mcmcDiagnosticsPlotSingleModelNumber <- 1
options$mcmcDiagnosticsPlotTypeAutocorrelation <- FALSE
options$mcmcDiagnosticsPlotTypePosteriorSamplesDensity <- FALSE
options$mcmcDiagnosticsPlotTypeTrace <- TRUE
options$mcmcDiagnosticsPlotUnequalVariances <- FALSE
options$modelsEffect <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                  lambda = "1", mu = "0", name = "#", nu = "2", priorWeight = "1",
                                  sigma = "1", theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                  type = "normal", x0 = "0"))
options$modelsEffectNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                      lambda = "1", mu = "0", name = "#", nu = "2", priorWeight = "1",
                                      sigma = "1", theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                      type = "spike", x0 = "0"))
options$modelsOutliers <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                    lambda = "1", mu = "0", name = "#", nu = "2", priorWeight = "1",
                                    sigma = "1", theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                    type = "exponential", x0 = "0"))
options$modelsOutliersNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                        lambda = "1", mu = "0", name = "#", nu = "2", priorWeight = "1",
                                        sigma = "1", theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                        type = "None", x0 = "0"))
options$modelsUnequalVariances <- list(list(a = "0", alpha = "1", b = "1", beta = "1", k = "1",
                                            lambda = "1", mu = "0", name = "#", nu = "2", priorWeight = "1",
                                            sigma = "1", theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                            type = "beta", x0 = "0"))
options$modelsUnequalVariancesNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                                lambda = "1", mu = "0", name = "#", nu = "2", priorWeight = "1",
                                                sigma = "1", theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                                type = "spike", x0 = "0.5"))
options$plotHeight <- 340
options$plotWidth <- 420
options$plotsPooledEstimatesEffect <- TRUE
options$plotsPooledEstimatesOutliers <- TRUE
options$plotsPooledEstimatesPriorDistribution <- TRUE
options$plotsPooledEstimatesType <- "averaged"
options$plotsPooledEstimatesUnequalVariances <- TRUE
options$priorDistributionPlot <- TRUE
options$seed <- 1
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("RobustBayesianModelAveragedTTest", "debug.csv", options)


test_that("Models Diagnostics Overview table results match", {
  table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("normal", 16360, 1, "Spike(0)", "Spike(0.5)", "", 1.00007499916945,
                                      "t", 2779, 2, "Spike(0)", "Spike(0.5)", "Exponential(1)", 1.00119620641672,
                                      "normal", 11693, 3, "Spike(0)", "Beta(1, 1)", "", 1.00031440536019,
                                      "t", 2242, 4, "Spike(0)", "Beta(1, 1)", "Exponential(1)", 1.00210020447055,
                                      "normal", 17621, 5, "Normal(0, 1)", "Spike(0.5)", "", 0.999893437607925,
                                      "t", 2603, 6, "Normal(0, 1)", "Spike(0.5)", "Exponential(1)",
                                      1.00102698597446, "normal", 13837, 7, "Normal(0, 1)", "Beta(1, 1)",
                                      "", 0.999995614305463, "t", 2972, 8, "Normal(0, 1)", "Beta(1, 1)",
                                      "Exponential(1)", 1.00151718392758))
})

test_that("titleless-plot-5 matches", {
  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model5"]][["collection"]][["diagnostics_model5_delta"]][["collection"]][["diagnostics_model5_delta_trace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-5")
})

test_that("titleless-plot-6 matches", {
  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model6"]][["collection"]][["diagnostics_model6_delta"]][["collection"]][["diagnostics_model6_delta_trace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-6")
})

test_that("titleless-plot-7 matches", {
  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model7"]][["collection"]][["diagnostics_model7_delta"]][["collection"]][["diagnostics_model7_delta_trace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-7")
})

test_that("titleless-plot-8 matches", {
  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model8"]][["collection"]][["diagnostics_model8_delta"]][["collection"]][["diagnostics_model8_delta_trace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-8")
})

test_that("Model Averaged Effect Size Estimate plot matches", {
  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_delta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-averaged-effect-size-estimate")
})

test_that("Model Averaged Outliers Estimate (Degrees of Freedom) plot matches", {
  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_nu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-averaged-outliers-estimate-degrees-of-freedom-")
})

test_that("Model Averaged Heterogeneity Estimate (Precision Allocation) plot matches", {
  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_rho"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-averaged-heterogeneity-estimate-precision-allocation-")
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model1"]][["collection"]][["individualModels_model1_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.202706796931575, "normal", -149.428000361031, 0.0281431415503297,
                                      0.125))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model1"]][["collection"]][["individualModels_model1_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Spike(0)", "None", "Spike(0.5)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(7794, 2.14862101729443, 3.61223328632155, 3.41355518156917, 1.00012787245522,
                                      "Degrees of freedom (<unicode>)", 6.18786113733111))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.90507601566439, "t", -146.884499888534, 0.358097092588349, 0.125
                                 ))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Spike(0)", "Exponential(1)", "Spike(0.5)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13837, 1.11279834652385, 1.46548824457317, 1.00000525703138, "Standard deviation ratio",
                                      1.93185520540739))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.73502973297545, "normal", -147.473865564586, 0.198628944149509,
                                      0.125))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Spike(0)", "None", "Beta(1, 1)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13004, 0.878632386612813, 1.27435521589437, 1.0001371546682, "Standard deviation ratio",
                                      1.82696924709567, 8966, 2.19695321260228, 3.88102192071681,
                                      3.65250661917882, 1.00027344997934, "Degrees of freedom (<unicode>)",
                                      6.85660857786382))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.69860385938474, "t", -147.490904649921, 0.195273159560219, 0.125
                                 ))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Spike(0)", "Exponential(1)", "Beta(1, 1)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(19303, -0.451459662514923, -0.065704096488295, -0.0659830905554011,
                                      0.999870174826536, "Effect size (<unicode>)", 0.320191147260061
                                 ))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0411614625597134, "normal", -150.999574739696, 0.0058458342105321,
                                      0.125))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Normal(0, 1)", "None", "Spike(0.5)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11846, -0.507003729871937, -0.187157357796684, -0.181777173355063,
                                      1.00011028379303, "Effect size (<unicode>)", 0.115234444510556,
                                      8714, 2.26024069090099, 3.62278104397417, 3.4341875456885, 0.999955741604095,
                                      "Degrees of freedom (<unicode>)", 6.05244325440928))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.914053058355319, "t", -148.016055473333, 0.115497464019434,
                                      0.125))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Normal(0, 1)", "Exponential(1)", "Spike(0.5)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(16981, -0.444926299900042, -0.0629479581029229, -0.0637454898550704,
                                      0.99999022175752, "Effect size (<unicode>)", 0.322725863693496,
                                      16588, 1.11825181871387, 1.46777515903796, 0.999874855266286,
                                      "Standard deviation ratio", 1.93104367109392))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.301200624552376, "normal", -149.045566280789, 0.0412535745887452,
                                      0.125))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Normal(0, 1)", "Exponential(1)", "Beta(1, 1)"))
})

test_that("Model Averaged Estimates table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.369930135366165, -0.0340366150322011, 0, "Effect size (<unicode>)",
                                      0.0677702428187115, 0.990141815094202, 1.19058117946987, 1,
                                      "Standard deviation ratio", 1.7984616472451, 2.22601746588366,
                                      "<unicode>", 4.05208957783863, "Degrees of freedom (<unicode>)",
                                      "<unicode>"))
})

test_that("Conditional Estimates table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.491418256350489, -0.156640024351313, -0.162064387253912, "Effect size (<unicode>)",
                                      0.204739229031045, 0.931597031234626, 1.38223387565695, 1.37732185544024,
                                      "Standard deviation ratio", 1.8806906724225, 2.17310523132912,
                                      3.70232463864694, 3.4967873556873, "Degrees of freedom (<unicode>)",
                                      6.35406419660253))
})

test_that("Models Overview table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.202706796931575, "normal", -149.428000361031, 1, 0.0281431415503297,
                                      "Spike(0)", "Spike(0.5)", "", 0.125, 3.90507601566439, "t",
                                      -146.884499888534, 2, 0.358097092588349, "Spike(0)", "Spike(0.5)",
                                      "Exponential(1)", 0.125, 1.73502973297545, "normal", -147.473865564586,
                                      3, 0.198628944149509, "Spike(0)", "Beta(1, 1)", "", 0.125, 1.69860385938474,
                                      "t", -147.490904649921, 4, 0.195273159560219, "Spike(0)", "Beta(1, 1)",
                                      "Exponential(1)", 0.125, 0.0411614625597134, "normal", -150.999574739696,
                                      5, 0.0058458342105321, "Normal(0, 1)", "Spike(0.5)", "", 0.125,
                                      0.914053058355319, "t", -148.016055473333, 6, 0.115497464019434,
                                      "Normal(0, 1)", "Spike(0.5)", "Exponential(1)", 0.125, 0.301200624552376,
                                      "normal", -149.045566280789, 7, 0.0412535745887452, "Normal(0, 1)",
                                      "Beta(1, 1)", "", 0.125, 0.425171161647673, "t", -148.717687961871,
                                      8, 0.057260789332878, "Normal(0, 1)", "Beta(1, 1)", "Exponential(1)",
                                      0.125))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.281817370350586, "4/8", 0.219857662151589, 0.5, "Effect", 0.970119076427619,
                                      "4/8", 0.492416467631352, 0.5, "Heterogeneity", 2.65134751182814,
                                      "4/8", 0.72612850550088, 0.5, "Outliers"))
})

test_that("Alternative plot matches", {
  plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-alternative")
})

test_that("Null plot matches", {
  plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_null"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-null")
})

test_that("Alternative plot matches", {
  plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_alternative"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity-alternative")
})

test_that("Null plot matches", {
  plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_null"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity-null")
})

test_that("Alternative plot matches", {
  plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_outliers"]][["collection"]][["priorPlots_outliers_alternative"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "outliers-alternative")
})
