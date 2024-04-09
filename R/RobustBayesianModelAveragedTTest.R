#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

RobustBayesianModelAveragedTTest <- function(jaspResults, dataset, options) {

  # load & check data (re-using .ttestBayesian functions)
  if (.robttCheckReady(options)) {
    dataset <- .robttReadData(dataset, options)
    errors  <- .robttGetErrorsPerVariable(dataset, options)
  }

  # get the priors
  .robttGetPriors(jaspResults, options)

  # show the model preview
  if (is.null(jaspResults[["model"]]))
   .robttModelPreviewTable(jaspResults, options)

  # priors plot
  if (options[["priorDistributionPlot"]])
    .robttPriorsPlots(jaspResults, options)

  # fit model model
  if (.robttCheckReady(options))
    .robttFitModel(jaspResults, dataset, options)

  ### Inference
  # default summary
  if (.robttCheckReady(options))
    .robttSummaryTable(jaspResults, options)
  # models overview
  if (options[["inferenceModelsOverview"]])
    .robttModelsOvervievTable(jaspResults, options)
  # models summary
  if (options[["inferenceIndividualModels"]])
    .robttModelsSummaryTable(jaspResults, options)

  ### Plots
  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robttEstimatesPlot(jaspResults, options, "delta")
  if (options[["plotsPooledEstimatesUnequalVariances"]])
    .robttEstimatesPlot(jaspResults, options, "rho")
  if (options[["plotsPooledEstimatesOutliers"]])
    .robttEstimatesPlot(jaspResults, options, "nu")

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robttDiagnosticsOverviewTable(jaspResults, options)
  # plots
  if ((
    options[["mcmcDiagnosticsPlotEffect"]]    ||
    options[["mcmcDiagnosticsPlotUnequalVariances"]]   ||
    options[["mcmcDiagnosticsPlotOutliers"]]
  ) ||
  (
    options[["mcmcDiagnosticsPlotTypeTrace"]]           ||
    options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] ||
    options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]
  ))
  .robttDiagnosticsPlots(jaspResults, options)


  return()
}

.robttRobustDependencies <- c(
  "dependent", "group",
  "modelsEffect", "modelsEffectNull", "modelsUnequalVariances", "modelsUnequalVariancesNull", "modelsOutliers", "modelsOutliersNull",
  "advancedMcmcAdaptation", "advancedMcmcSamples", "advancedMcmcChains",
  "seed", "setSeed"
)
