context("Bayesian Model-Averaged T-Test")

# all output test
options <- analysisOptions("robttBayesianModelAveraged")
options$dependent <- "contNormal"
options$group <- "facExperim"
options$inferenceIndividualModels <- TRUE
options$inferenceModelsOverview <- TRUE
options$inferenceModelsOverviewBfComparison <- "inclusion"
options$inferenceModelsOverviewOrder <- "modelNumber"
options$mcmcDiagnosticsOverviewTable <- TRUE
options$mcmcDiagnosticsPlotEffect <- TRUE
options$mcmcDiagnosticsPlotTypeTrace <- TRUE
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
options$plotsPooledEstimatesUnequalVariances <- TRUE
options$priorDistributionPlot <- TRUE
set.seed(1)
results <- runAnalysis("robttBayesianModelAveraged", "debug.csv", options)


test_that("Models Diagnostics Overview table results match", {
  table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("normal", 15607, 1, "Spike(0)", "Spike(0.5)", "", 0.999986113497827,
                                      "t", 2078, 2, "Spike(0)", "Spike(0.5)", "Exponential(1)", 1.00049911409981,
                                      "normal", 12775, 3, "Spike(0)", "Beta(1, 1)", "", 1.00026649703854,
                                      "t", 1683, 4, "Spike(0)", "Beta(1, 1)", "Exponential(1)", 1.00473688892118,
                                      "normal", 19860, 5, "Normal(0, 1)", "Spike(0.5)", "", 1.00005175940705,
                                      "t", 1907, 6, "Normal(0, 1)", "Spike(0.5)", "Exponential(1)",
                                      1.00256299577318, "normal", 15148, 7, "Normal(0, 1)", "Beta(1, 1)",
                                      "", 1.00043029984581, "t", 3157, 8, "Normal(0, 1)", "Beta(1, 1)",
                                      "Exponential(1)", 1.0013008829332))
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
                                 list(0.20297258871127, "normal", -149.427734093773, 0.0281790033505578,
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
                                 list(7265, 2.14060079981735, 3.60901622922147, 3.39611359598575, 1.00027322766698,
                                      "Degrees of freedom (<unicode>)", 6.23375679655017))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.88985701005965, "t", -146.888015361228, 0.357200007903341, 0.125
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
                                 list(15639, 1.11324045689893, 1.46739308267683, 0.999942538222159,
                                      "Standard deviation ratio", 1.93459524852023))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.74018482672204, "normal", -147.472495960201, 0.199101604968531,
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
                                 list(11205, 0.879499386818397, 1.27306717326617, 1.00023007992036,
                                      "Standard deviation ratio", 1.83700414043601, 9632, 2.19454877566263,
                                      3.88499352613532, 3.65627725431496, 1.0005051496691, "Degrees of freedom (<unicode>)",
                                      6.87797825833588))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.69809209457742, "t", -147.492154332004, 0.195225812294648, 0.125
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
                                 list(22090, -0.450740659440704, -0.0668450651176389, -0.0656265631802683,
                                      0.999977846628919, "Effect size (<unicode>)", 0.310705985639928
                                 ))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0411820278361262, "normal", -151.000085346499, 0.00584873785016748,
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
                                 list(11740, -0.507188852597617, -0.18649817347009, -0.183254650951343,
                                      1.00009032283525, "Effect size (<unicode>)", 0.111403349562508,
                                      8491, 2.23486130144927, 3.61893810094358, 3.42435216292183,
                                      1.00057864993288, "Degrees of freedom (<unicode>)", 6.07666374048183
                                 ))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920795323064213, "t", -148.010565074534, 0.116250361928053,
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
                                 list(17772, -0.4464143203645, -0.065970247252407, -0.0662725985721301,
                                      1.00043029984581, "Effect size (<unicode>)", 0.320205451325712,
                                      15990, 1.1138965484976, 1.46498158737954, 0.999976574876581,
                                      "Standard deviation ratio", 1.93493421050054))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.301468476293766, "normal", -149.045721266956, 0.0412887458560679,
                                      0.125))
})

test_that("Priors table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Normal(0, 1)", "None", "Beta(1, 1)"))
})

test_that("Model Estimates table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13544, -0.492917919574181, -0.173276403869793, -0.172047195055316,
                                      1.00034267049279, "Effect size (<unicode>)", 0.146374175728566,
                                      15002, 0.8674421465586, 1.25002576033157, 1.00008758770646,
                                      "Standard deviation ratio", 1.78623909038703, 9978, 2.28490895231027,
                                      3.88569766116224, 3.66462457500689, 0.999962077822006, "Degrees of freedom (<unicode>)",
                                      6.79306568834832))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.422375675325665, "t", -148.724915266258, 0.0569057258486378,
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
                                 list(-0.361835315723003, -0.0350511605032773, 0, "Effect size (<unicode>)",
                                      0.052846623649321, 0.991602899880956, 1.18881557568731, 1, "Standard deviation ratio",
                                      1.79381471135139, 2.2329382182146, "<unicode>", 4.01720200952359,
                                      "Degrees of freedom (<unicode>)", "<unicode>"))
})

test_that("Conditional Estimates table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.504212387667142, -0.157278598178095, -0.157579401334335, "Effect size (<unicode>)",
                                      0.199360828218836, 0.923759378963753, 1.38377859813451, 1.37820500722722,
                                      "Standard deviation ratio", 1.89350138729852, 2.17962109915267,
                                      3.70329343686007, 3.47626140329828, "Degrees of freedom (<unicode>)",
                                      6.51897634834214))
})

test_that("Models Overview table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.20297258871127, "normal", -149.427734093773, 1, 0.0281790033505578,
                                      "Spike(0)", "Spike(0.5)", "", 0.125, 3.88985701005965, "t",
                                      -146.888015361228, 2, 0.357200007903341, "Spike(0)", "Spike(0.5)",
                                      "Exponential(1)", 0.125, 1.74018482672204, "normal", -147.472495960201,
                                      3, 0.199101604968531, "Spike(0)", "Beta(1, 1)", "", 0.125, 1.69809209457742,
                                      "t", -147.492154332004, 4, 0.195225812294648, "Spike(0)", "Beta(1, 1)",
                                      "Exponential(1)", 0.125, 0.0411820278361262, "normal", -151.000085346499,
                                      5, 0.00584873785016748, "Normal(0, 1)", "Spike(0.5)", "", 0.125,
                                      0.920795323064213, "t", -148.010565074534, 6, 0.116250361928053,
                                      "Normal(0, 1)", "Spike(0.5)", "Exponential(1)", 0.125, 0.301468476293766,
                                      "normal", -149.045721266956, 7, 0.0412887458560679, "Normal(0, 1)",
                                      "Beta(1, 1)", "", 0.125, 0.422375675325665, "t", -148.724915266258,
                                      8, 0.0569057258486378, "Normal(0, 1)", "Beta(1, 1)", "Exponential(1)",
                                      0.125))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.282533994110953, "4/8", 0.220293571482926, 0.5, "Effect", 0.970528340554771,
                                      "4/8", 0.492521888967884, 0.5, "Heterogeneity", 2.64407460389926,
                                      "4/8", 0.72558190797468, 0.5, "Outliers"))
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
