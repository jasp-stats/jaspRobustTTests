#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Bayesian Model-Averaged T-Test
#'
BayesianModelAveragedTTest <- function(
          data = NULL,
          version = "1",
          advancedMcmcAdaptation = 5000,
          advancedMcmcChains = 4,
          advancedMcmcSamples = 5000,
          advancedMcmcThin = 1,
          bayesFactorType = "BF10",
          dependent = list(types = list(), value = ""),
          group = list(types = list(), value = ""),
          inferenceCiWidth = 0.95,
          inferenceConditionalParameterEstimates = TRUE,
          inferenceIndividualModels = FALSE,
          inferenceIndividualModelsSingleModel = FALSE,
          inferenceIndividualModelsSingleModelNumber = 1,
          inferenceModelsOverview = FALSE,
          inferenceModelsOverviewBfComparison = "inclusion",
          inferenceModelsOverviewOrder = "modelNumber",
          inferencePrecisionAllocationAsStandardDeviationRatio = TRUE,
          inferenceShortenPriorName = FALSE,
          mcmcDiagnosticsOverviewTable = FALSE,
          mcmcDiagnosticsPlotEffect = FALSE,
          mcmcDiagnosticsPlotOutliers = FALSE,
          mcmcDiagnosticsPlotSingleModel = FALSE,
          mcmcDiagnosticsPlotSingleModelNumber = 1,
          mcmcDiagnosticsPlotTypeAutocorrelation = FALSE,
          mcmcDiagnosticsPlotTypePosteriorSamplesDensity = FALSE,
          mcmcDiagnosticsPlotTypeTrace = FALSE,
          mcmcDiagnosticsPlotUnequalVariances = FALSE,
          modelsEffect = list(list(name = "#", type = "normal")),
          modelsEffectNull = list(list(name = "#", type = "spike")),
          modelsUnequalVariances = list(list(alpha = "1", beta = "1", name = "#", type = "beta")),
          modelsUnequalVariancesNull = list(list(name = "#", type = "spike", x0 = "0.5")),
          plotHeight = 340,
          plotWidth = 420,
          plotsPooledEstimatesEffect = FALSE,
          plotsPooledEstimatesOutliers = FALSE,
          plotsPooledEstimatesPriorDistribution = TRUE,
          plotsPooledEstimatesType = "averaged",
          plotsPooledEstimatesUnequalVariances = FALSE,
          priorDistributionPlot = FALSE,
          priorsNull = FALSE,
          seed = 1,
          setSeed = FALSE,
          truncation = "truncationStandardDeviation",
          truncationBoundsBothSamplesLower = -3,
          truncationBoundsBothSamplesUpper = 3,
          truncationBoundsGrouping = "perSample",
          truncationBoundsSample1Lower = -3,
          truncationBoundsSample1Upper = 3,
          truncationBoundsSample2Lower = -3,
          truncationBoundsSample2Upper = 3,
          truncationStandardDeviationBothSamples = 3,
          truncationStandardDeviationGrouping = "perSample",
          truncationStandardDeviationSample1 = 3,
          truncationStandardDeviationSample2 = 3) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent", "group", "modelsEffect", "modelsEffectNull", "modelsUnequalVariances", "modelsUnequalVariancesNull")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspRobustTTests", "BayesianModelAveragedTTest", "BayesianModelAveragedTTest.qml", options, version, TRUE))
}