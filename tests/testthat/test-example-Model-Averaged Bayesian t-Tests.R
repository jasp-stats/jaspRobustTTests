context("Example: Model-Averaged Bayesian t-Tests")
skip_on_os("mac") # MCMC sampling via Stan does not match across OS
# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("BayesianModelAveragedTTest (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Model-Averaged Bayesian t-Tests.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianModelAveragedTTest", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # manual tests for complete output, MCMC sampling via Stan does not match across OS, tests generated on windows
  expect_equal(results$status, "complete")
  skip_on_os(os = c("mac", "linux"))

  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model4"]][["collection"]][["diagnostics_model4_delta"]][["collection"]][["diagnostics_model4_delta_autocorrelations"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_")

  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model4"]][["collection"]][["diagnostics_model4_delta"]][["collection"]][["diagnostics_model4_delta_samples"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_")

  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model4"]][["collection"]][["diagnostics_model4_delta"]][["collection"]][["diagnostics_model4_delta_trace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_")

  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model4"]][["collection"]][["diagnostics_model4_rho"]][["collection"]][["diagnostics_model4_rho_autocorrelations"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_")

  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model4"]][["collection"]][["diagnostics_model4_rho"]][["collection"]][["diagnostics_model4_rho_samples"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_")

  plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model4"]][["collection"]][["diagnostics_model4_rho"]][["collection"]][["diagnostics_model4_rho_trace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_")

  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_delta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-7_model-averaged-effect-size-estimate")

  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_rho"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-8_model-averaged-heterogeneity-estimate-precision-allocation-")

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9675, -2.14974630486795, -1.29798317833598, -1.29368278336932,
     1.00039430686735, "Effect size (<unicode>)", -0.482552483246723,
     8727, 0.172020510994012, 0.304312431798703, 1.00069906930287,
     "Standard deviation ratio", 0.55490395189684))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(121.007998311444, "normal", -85.0651821173196, 0.975808012056894,
     0.25))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Normal(0, 1)", "Beta(1, 1)"))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-2.13874731708292, -1.26730522222401, -1.28722137720934, "Effect size (<unicode>)",
     -0.112780414010577, 0.169438434799871, 0.321178818437376, 0.30509079093343,
     "Standard deviation ratio", 0.5615668788058))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-2.1440840072826, -1.29668239269304, -1.29744202320479, "Effect size (<unicode>)",
     -0.474366896393669, 0.169438434799871, 0.319336958439774, 0.304784304398481,
     "Standard deviation ratio", 0.554366364884697))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.000148533167757466, "normal", -94.9540567676998, 1, 4.95086046958607e-05,
     "Spike(0)", "Spike(0.5)", 0.25, 0.0657448895606949, "normal",
     -88.8829566034131, 2, 0.0214449968699503, "Spike(0)", "Beta(1, 1)",
     0.25, 0.00811433568364977, "normal", -90.9561290564741, 3, 0.00269748246846645,
     "Normal(0, 1)", "Spike(0.5)", 0.25, 121.007998311444, "normal",
     -85.0651821173196, 4, 0.975808012056894, "Normal(0, 1)", "Beta(1, 1)",
     0.25))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(45.5235174253976, "2/4", 0.97850549452536, 0.5, "Effect", 363.034674072947,
     "2/4", 0.997253008926844, 0.5, "Heterogeneity"))

})

test_that("RobustBayesianModelAveragedTTest (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Model-Averaged Bayesian t-Tests.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RobustBayesianModelAveragedTTest", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # manual tests for complete output, MCMC sampling via Stan does not match across OS, tests generated on windows
  expect_equal(results$status, "complete")
  skip_on_os(os = c("mac", "linux"))

  table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("normal", 13241, 1, "Spike(0)", "Spike(0.5)", "", 1.00023650474088,
     "t", 2299, 2, "Spike(0)", "Spike(0.5)", "Exponential(1)", 1.00218922595945,
     "normal", 4860, 3, "Spike(0)", "Beta(1, 1)", "", 1.0009408364281,
     "t", 3558, 4, "Spike(0)", "Beta(1, 1)", "Exponential(1)", 1.00072565245979,
     "normal", 12698, 5, "Normal(0, 1)", "Spike(0.5)", "", 1.00007986271743,
     "t", 488, 6, "Normal(0, 1)", "Spike(0.5)", "Exponential(1)",
     1.00903594061871, "normal", 7912, 7, "Normal(0, 1)", "Beta(1, 1)",
     "", 1.00069346086686, "t", 3431, 8, "Normal(0, 1)", "Beta(1, 1)",
     "Exponential(1)", 1.00048537921989))

  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_delta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_model-averaged-effect-size-estimate")

  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_nu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_model-averaged-outliers-estimate-degrees-of-freedom-")

  plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_rho"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_model-averaged-heterogeneity-estimate-precision-allocation-")

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model1"]][["collection"]][["individualModels_model1_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.000178921002305339, "normal", -94.9540567676998, 2.55594898822573e-05,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model1"]][["collection"]][["individualModels_model1_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Spike(0)", "None", "Spike(0.5)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8909, 2.03466714412037, 2.90634788684166, 2.65518535614789, 1.00026677041575,
     "Degrees of freedom (<unicode>)", 5.19995520073739))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00160474774118989, "t", -92.7604829591974, 0.00022919713394385,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model2"]][["collection"]][["individualModels_model2_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Spike(0)", "Exponential(1)", "Spike(0.5)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4860, 0.126835380293349, 0.228311363762016, 1.00045623200789,
     "Standard deviation ratio", 0.458109787758793))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0789593407935127, "normal", -88.8755040250309, 0.0111540887568744,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model3"]][["collection"]][["individualModels_model3_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Spike(0)", "None", "Beta(1, 1)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8319, 0.131610273157161, 0.290746599590707, 1.00001552406521,
     "Standard deviation ratio", 0.676204553302394, 11080, 2.04663550325388,
     3.30476776542303, 3.00687524396454, 1.00021031354704, "Degrees of freedom (<unicode>)",
     6.255680318545))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0413410786512941, "t", -89.5172521604174, 0.00587119388047214,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model4"]][["collection"]][["individualModels_model4_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Spike(0)", "Exponential(1)", "Beta(1, 1)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(14096, -2.06160266710446, -1.27131672989137, -1.27470042932183,
     0.999972658900297, "Effect size (<unicode>)", -0.460752059312486
    ))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00974202120732285, "normal", -90.9581624469212, 0.00138978313008522,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model5"]][["collection"]][["individualModels_model5_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Normal(0, 1)", "None", "Spike(0.5)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4154, -1.76065655818561, -0.939939420333765, -0.913011442186267,
     1.00144371729857, "Effect size (<unicode>)", -0.270355697893507,
     6994, 2.0433637795801, 2.75545544822972, 2.54515594197349, 1.00044103308535,
     "Degrees of freedom (<unicode>)", 4.65595328950183))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.312738032534489, "t", -87.5315620405181, 0.0427662020905325,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model6"]][["collection"]][["individualModels_model6_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Normal(0, 1)", "Exponential(1)", "Spike(0.5)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9039, -2.14484427748854, -1.29551222207834, -1.28662347888183,
     1.00028445685528, "Effect size (<unicode>)", -0.464774555285236,
     8696, 0.169713381217607, 0.304597265370619, 1.00069346086686,
     "Standard deviation ratio", 0.551793319720594))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.06488923488353, "normal", -85.0680991100608, 0.502306780871134,
     0.125))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model7"]][["collection"]][["individualModels_model7_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Normal(0, 1)", "None", "Beta(1, 1)"))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempCoef"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6866, -1.7502341651005, -0.946890431998792, -0.923944659671187,
     1.00046962735889, "Effect size (<unicode>)", -0.29492689993204,
     9946, 0.185223527097386, 0.390042817008862, 1.00005270082939,
     "Standard deviation ratio", 0.822681913495865, 10142, 2.08330120080814,
     3.17366816293218, 2.89355630085667, 1.00015444241919, "Degrees of freedom (<unicode>)",
     5.83114004782539))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempInfo"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.41700990865444, "t", -85.2090781949613, 0.43625719464708, 0.125
    ))

  table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model8"]][["collection"]][["individualModels_model8_tempPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Normal(0, 1)", "Exponential(1)", "Beta(1, 1)"))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-2.03693871699629, -1.10752615808716, -1.09063573220677, "Effect size (<unicode>)",
     -0.224537923972754, 0.176640212914253, 0.393248422355206, 0.343637523062901,
     "Standard deviation ratio", 1, 2.13395209104204, "<unicode>",
     "<unicode>", "Degrees of freedom (<unicode>)", "<unicode>"
    ))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-2.04447324209888, -1.12584784146225, -1.09879977526559, "Effect size (<unicode>)",
     -0.338647780680368, 0.174144004589612, 0.365262980712177, 0.336158866026436,
     "Standard deviation ratio", 0.725262692004367, 2.07642979938861,
     3.13476939547402, 2.8558471125241, "Degrees of freedom (<unicode>)",
     5.74377972811865))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.000178921002305339, "normal", -94.9540567676998, 1, 2.55594898822573e-05,
     "Spike(0)", "Spike(0.5)", "", 0.125, 0.00160474774118989, "t",
     -92.7604829591974, 2, 0.00022919713394385, "Spike(0)", "Spike(0.5)",
     "Exponential(1)", 0.125, 0.0789593407935127, "normal", -88.8755040250309,
     3, 0.0111540887568744, "Spike(0)", "Beta(1, 1)", "", 0.125,
     0.0413410786512941, "t", -89.5172521604174, 4, 0.00587119388047214,
     "Spike(0)", "Beta(1, 1)", "Exponential(1)", 0.125, 0.00974202120732285,
     "normal", -90.9581624469212, 5, 0.00138978313008522, "Normal(0, 1)",
     "Spike(0.5)", "", 0.125, 0.312738032534489, "t", -87.5315620405181,
     6, 0.0427662020905325, "Normal(0, 1)", "Spike(0.5)", "Exponential(1)",
     0.125, 7.06488923488353, "normal", -85.0680991100608, 7, 0.502306780871134,
     "Normal(0, 1)", "Beta(1, 1)", "", 0.125, 5.41700990865444, "t",
     -85.2090781949613, 8, 0.43625719464708, "Normal(0, 1)", "Beta(1, 1)",
     "Exponential(1)", 0.125))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(56.8702388857981, "4/8", 0.982719960738832, 0.5, "Effect", 21.5170748892843,
     "4/8", 0.955589258155561, 0.5, "Heterogeneity", 0.94221441234182,
     "4/8", 0.485123787752028, 0.5, "Outliers"))

})

