context("Example: Truncated Bayesian Model-Averaged T-Test")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("TruncatedBayesianModelAveragedTTest (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Truncated Bayesian Model-Averaged T-Test.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TruncatedBayesianModelAveragedTTest", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.187243504013457, -0.057686521003303, -0.0306309252238006, "Effect size (<unicode>)",
     0, 0.977396975731345, 0.998856736222883, 1, "Standard deviation ratio",
     1))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.08483373931714, "2/4", 0.520345444751076, 0.5, "Effect", 0.0549563872172106,
     "2/4", 0.0520935157918594, 0.5, "Heterogeneity"))

})

test_that("TruncatedBayesianModelAveragedTTest (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Truncated Bayesian Model-Averaged T-Test.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("TruncatedBayesianModelAveragedTTest", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.191332011970381, -0.0771045364928388, -0.0859853013820026,
     "Effect size (<unicode>)", 0, 0.977293471223981, 0.998861938327014,
     1, "Standard deviation ratio", 1))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.279643107941, "truncated normal", -940.902147554057, 1, 0.299006967559218,
     "Spike(0)", "Spike(0.5)", 0.25, 0.0502937433335049, "truncated normal",
     -943.79997164296, 2, 0.0164881639492703, "Spike(0)", "Beta(1, 1)",
     0.25, 5.54197789188568, "truncated normal", -940.127500278079,
     3, 0.648793284416037, "Cauchy(0, 0.71)[-Inf, 0]", "Spike(0.5)",
     0.25, 0.111102394737189, "truncated normal", -943.02713930999,
     4, 0.0357115840755095, "Cauchy(0, 0.71)[-Inf, 0]", "Beta(1, 1)",
     0.25))

  table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.1696210182984, "2/4", 0.684504868491546, 0.5, "Effect", 0.0550746298241593,
     "2/4", 0.0521997480247799, 0.5, "Heterogeneity"))

})

