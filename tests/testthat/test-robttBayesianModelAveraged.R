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
options$setSeed <- TRUE
options$seed <- 1
set.seed(1)
results <- runAnalysis("robttBayesianModelAveraged", "debug.csv", options)


test_that("Models Diagnostics Overview table results match", {
  table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("normal", 16360, 1, "Spike(0)", "Spike(0.5)", "", 1.00007499916945,
                                      "t", 2724, 2, "Spike(0)", "Spike(0.5)", "Exponential(1)", 1.00117695024109,
                                      "normal", 13205, 3, "Spike(0)", "Beta(1, 1)", "", 1.00002962748119,
                                      "t", 3160, 4, "Spike(0)", "Beta(1, 1)", "Exponential(1)", 1.00109961339086,
                                      "normal", 18574, 5, "Normal(0, 1)", "Spike(0.5)", "", 1.00017551065821,
                                      "t", 2944, 6, "Normal(0, 1)", "Spike(0.5)", "Exponential(1)",
                                      1.00115131681609, "normal", 14409, 7, "Normal(0, 1)", "Beta(1, 1)",
                                      "", 1.00031690222577, "t", 2552, 8, "Normal(0, 1)", "Beta(1, 1)",
                                      "Exponential(1)", 1.0006297839035))
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
                                 list(0.202514551430172, "normal", -149.428000361031, 0.0281172012890911,
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
                                 list(7381, 2.15677312588384, 3.6351323137615, 3.44115954531562, 1.00037636859494,
                                      "Degrees of freedom (<unicode>)", 6.26607136306903))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.90167604779319, "t", -146.884136943229, 0.357896898668426, 0.125
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
                                 list(15439, 1.11314091657139, 1.46772386960544, 1.00000146389591, "Standard deviation ratio",
                                      1.93838427791191))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.73516201453881, "normal", -147.472882318604, 0.19864107977058,
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
                                 list(12454, 0.877977740378845, 1.27329268174649, 1.00011601484972,
                                      "Standard deviation ratio", 1.81802479211572, 10004, 2.20780144214331,
                                      3.88623371801323, 3.66833185434915, 1.00027311040046, "Degrees of freedom (<unicode>)",
                                      6.87650687666333))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.69616880299422, "t", -147.491137115337, 0.195047824095846, 0.125
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
                                 list(21217, -0.456196998157008, -0.0632253071362019, -0.0633274542971189,
                                      0.999930477885847, "Effect size (<unicode>)", 0.322425134485338
                                 ))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0411713579904948, "normal", -150.998413617823, 0.0058472313621191,
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
                                 list(11603, -0.508928412678759, -0.187925590916052, -0.183974511259051,
                                      0.999937013341529, "Effect size (<unicode>)", 0.116239237047911,
                                      8665, 2.23347834127843, 3.62459351549579, 3.4314488081881, 1.00054717512122,
                                      "Degrees of freedom (<unicode>)", 6.10892487049728))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.918473170037678, "t", -148.010867607686, 0.115991195564454,
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
                                 list(16467, -0.448491547049675, -0.0638019622826326, -0.0655714483154314,
                                      0.999994001449137, "Effect size (<unicode>)", 0.322446751011224,
                                      17112, 1.11638788181113, 1.46683698490475, 1.0001063494857,
                                      "Standard deviation ratio", 1.9321573920878))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.301416271346288, "normal", -149.043957964094, 0.0412818910940857,
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
                                 list(13547, -0.485018990276564, -0.172071887431753, -0.168483319944021,
                                      1.00003213622474, "Effect size (<unicode>)", 0.138111298832829,
                                      15918, 0.864566555929416, 1.2520613770588, 1.00002275044703,
                                      "Standard deviation ratio", 1.77849773982719, 9214, 2.27682109527648,
                                      3.85376112609263, 3.63534981616705, 1.00016910195186, "Degrees of freedom (<unicode>)",
                                      6.69543342171784))
})

test_that("Information table results match", {
  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.424508747094624, "t", -148.718235804918, 0.0571766781554101,
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
                                 list(-0.368667913528343, -0.0352514648936077, 0, "Effect size (<unicode>)",
                                      0.046187716858959, 0.998738002459905, 1.18737159857526, 1, "Standard deviation ratio",
                                      1.78959161630765, 2.24549111221717, "<unicode>", 4.03923606757242,
                                      "Degrees of freedom (<unicode>)", "<unicode>"))
})

test_that("Conditional Estimates table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.498752274598366, -0.157594182880648, -0.160098009745175, "Effect size (<unicode>)",
                                      0.207501375052993, 0.930552127793957, 1.38181290899738, 1.37470403970583,
                                      "Standard deviation ratio", 1.88117508984913, 2.1892993440075,
                                      3.72309767036248, 3.51709158897891, "Degrees of freedom (<unicode>)",
                                      6.52426036945621))
})

test_that("Models Overview table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.202514551430172, "normal", -149.428000361031, 1, 0.0281172012890911,
                                      "Spike(0)", "Spike(0.5)", "", 0.125, 3.90167604779319, "t",
                                      -146.884136943229, 2, 0.357896898668426, "Spike(0)", "Spike(0.5)",
                                      "Exponential(1)", 0.125, 1.73516201453881, "normal", -147.472882318604,
                                      3, 0.19864107977058, "Spike(0)", "Beta(1, 1)", "", 0.125, 1.69616880299422,
                                      "t", -147.491137115337, 4, 0.195047824095846, "Spike(0)", "Beta(1, 1)",
                                      "Exponential(1)", 0.125, 0.0411713579904948, "normal", -150.998413617823,
                                      5, 0.0058472313621191, "Normal(0, 1)", "Spike(0.5)", "", 0.125,
                                      0.918473170037678, "t", -148.010867607686, 6, 0.115991195564454,
                                      "Normal(0, 1)", "Spike(0.5)", "Exponential(1)", 0.125, 0.301416271346288,
                                      "normal", -149.043957964094, 7, 0.0412818910940857, "Normal(0, 1)",
                                      "Beta(1, 1)", "", 0.125, 0.424508747094624, "t", -148.718235804918,
                                      8, 0.0571766781554101, "Normal(0, 1)", "Beta(1, 1)", "Exponential(1)",
                                      0.125))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.282539627390995, "4/8", 0.220296996176069, 0.5, "Effect", 0.969075562418576,
                                      "4/8", 0.492147473115922, 0.5, "Heterogeneity", 2.65113541974959,
                                      "4/8", 0.726112596484136, 0.5, "Outliers"))
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
