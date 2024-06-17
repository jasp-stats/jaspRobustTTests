.robttCommon <- function(jaspResults, dataset, options, analysis) {

  # load & check data (re-using .ttestBayesian functions)
  if (.robttCheckReady(options)) {
    dataset <- .robttReadData(dataset, options)
    errors  <- .robttGetErrorsPerVariable(dataset, options)
  }

  if (analysis == "truncated") {
    options <- .robttEvalTruncation(options)
  }

  # get the priors
  .robttGetPriors(jaspResults, options, analysis)

  # show the model preview
  if (is.null(jaspResults[["model"]]))
    .robttModelPreviewTable(jaspResults, options, analysis)

  # priors plot
  if (options[["priorDistributionPlot"]])
    .robttPriorsPlots(jaspResults, options, analysis)

  # fit model model
  if (.robttCheckReady(options))
    .robttFitModel(jaspResults, dataset, options, analysis)

  ### Inference
  # default summary
  if (.robttCheckReady(options))
    .robttSummaryTable(jaspResults, options, analysis)
  # models overview
  if (options[["inferenceModelsOverview"]])
    .robttModelsOvervievTable(jaspResults, options, analysis)
  # models summary
  if (options[["inferenceIndividualModels"]])
    .robttModelsSummaryTable(jaspResults, options, analysis)

  ### Plots
  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robttEstimatesPlot(jaspResults, options, "delta", analysis)
  if (options[["plotsPooledEstimatesUnequalVariances"]])
    .robttEstimatesPlot(jaspResults, options, "rho", analysis)
  if (analysis == "robust" && options[["plotsPooledEstimatesOutliers"]])
    .robttEstimatesPlot(jaspResults, options, "nu", analysis)

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robttDiagnosticsOverviewTable(jaspResults, options, analysis)
  # plots
  if (.robttCheckDiagnostics(options, TRUE, analysis))
    .robttDiagnosticsPlots(jaspResults, options, analysis)


  return()
}

.robttGetDependencies <- function(analysis) {

  commonDependencies <- c(
      "dependent", "group",
      "modelsEffect", "modelsEffectNull", "modelsUnequalVariances", "modelsUnequalVariancesNull",
      "advancedMcmcAdaptation", "advancedMcmcSamples", "advancedMcmcChains",
      "seed", "setSeed"
    )

  if (analysis == "robust") {
    commonDependencies <- (c(commonDependencies, "modelsOutliers", "modelsOutliersNull"))
  } else if (analysis == "truncated") {
    commonDependencies <- (c(commonDependencies, "modelCommonMean", "modelCommonVariance",
                             "truncation",
                             "truncationStandardDeviationGrouping", "truncationStandardDeviationSample1", "truncationStandardDeviationSample2", "truncationStandardDeviationBothSamples",
                             "truncationBoundsGrouping", "truncationBoundsSample1Lower", "truncationBoundsSample1Upper", "truncationBoundsSample2Lower", "truncationBoundsSample2Upper",
                             "truncationBoundsBothSamplesLower", "truncationBoundsBothSamplesUpper"
                             ))
  }

  return(commonDependencies)
}

# priors related functions
.robttExtractPriorsFromOptions <- function(optionsPrior) {

  if (optionsPrior[["type"]] == "None")
    return(RoBTT::prior_none())

  optionsPrior <- .robttEvalOptionsToPriors(optionsPrior)
  optionsPrior <- .robttMapOptionsToPriors(optionsPrior)

  return(do.call(
    what = RoBTT::prior,
    args = optionsPrior
  ))
}
.robttEvalOptionsToPriors      <- function(x) {

  evalNames <-
    c(
      "a",
      "b",
      "alpha",
      "beta",
      "nu",
      "x0",
      "mu",
      "sigma",
      "theta",
      "lambda",
      "k",
      "priorWeight",
      "truncationLower",
      "truncationUpper"
    )
  for (n in evalNames) {
    if (!is.null(x[[n]]))
      x[[n]] <- eval(parse(text = x[[n]]))
  }

  return(x)
}
.robttMapOptionsToPriors       <- function(optionsPrior) {

  arguments <- list()

  arguments[["distribution"]] <- switch(
    optionsPrior[["type"]],
    "gammaAB" = "gamma",
    "gammaK0" = "gamma",
    optionsPrior[["type"]]
  )

  arguments[["parameters"]] <- switch(
    optionsPrior[["type"]],
    "normal"      = list("mean" = optionsPrior[["mu"]], "sd" = optionsPrior[["sigma"]]),
    "t"           = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"      = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"     = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"     = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"    = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal"   = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"        = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"     = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "exponential" = list("rate" = optionsPrior[["lambda"]]),
    "spike"       = list("location" = optionsPrior[["x0"]])
  )

  if(!arguments[["distribution"]] %in% c("spike", "uniform")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  arguments[["prior_weights"]] = optionsPrior[["priorWeight"]]

  return(arguments)
}

# helper functions
.robttCheckReady           <- function(options) {

  return(options[["dependent"]] != "" && options[["group"]] != "" )
}
.robttGetPriors            <- function(jaspResults, options, analysis) {

  if (!is.null(jaspResults[["priors"]])) {
    return()
  } else {
    priors <- createJaspState()
    priors$dependOn(.robttGetDependencies(analysis))
    jaspResults[["priors"]] <- priors
  }

  object <- list()
  for(type in c("", "Null")) {

    priorElements <- switch(
      analysis,
      "averaged"  = paste0(c("modelsEffect", "modelsUnequalVariances"), type),
      "robust"    = paste0(c("modelsEffect", "modelsUnequalVariances", "modelsOutliers"),  type),
      "truncated" = paste0(c("modelsEffect", "modelsUnequalVariances"), type)
    )

    for (i in seq_along(priorElements)) {

      if(length(options[[priorElements[i]]]) == 0){
        # remove component if prior is unspecified
        tmpPrior <- NULL
      }else if(options[[priorElements[i]]][[1]][["type"]] == "none"){
        # use default null hypothesis prior if prior is none
        tmpPrior <- RoBTT::prior_none()
      }else{
        tmpPrior <- try(.robttExtractPriorsFromOptions(options[[priorElements[i]]][[1]]))
        if (jaspBase::isTryError(tmpPrior))
          .quitAnalysis(tmpPrior)
      }

      object[[priorElements[i]]] <- tmpPrior
    }
  }

  if (analysis != "robust") {
    object[["modelsOutliersNull"]] <- RoBTT::prior_none()
  }

  if (analysis == "truncated") {
    for(priorElement in c("modelCommonMean", "modelCommonVariance")) {

      tmpPrior <- try(.robttExtractPriorsFromOptions(options[[priorElement]][[1]]))
      if (jaspBase::isTryError(tmpPrior))
        .quitAnalysis(tmpPrior)

      object[[priorElement]] <- tmpPrior
    }
  }

  priors[["object"]] <- object

  return()
}
.robttReadData             <- function(dataset = NULL, options) {

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(
      columns.as.numeric  = if (options[["dependent"]] != "") options[["dependent"]],
      columns.as.factor   = if (options[["group"]] != "")     options[["group"]]
    )
  }
  return(dataset)
}
.robttGetErrorsPerVariable <- function(dataset, options) {

  errors <- list()


  dependents <- unlist(options[["dependent"]])
  grouping   <- options[["group"]]

  # analysis breaking errors
  if (grouping != "") {
    .hasErrors(dataset, "run", type = 'factorLevels',
               factorLevels.target = grouping, factorLevels.amount = '!= 2',
               exitAnalysisIfErrors = TRUE)
  } else {
    grouping <- NULL
  }

  for (var in dependents) {

    errors[[var]] <- .hasErrors(dataset, message = 'short',
                                type = c('infinity','observations','variance'),
                                all.target = var, observations.amount = "< 2",
                                all.grouping = grouping)
  }

  return(errors)
}
.robttEvalTruncation       <- function(options) {

  evalNames <-
    c(
      "truncationStandardDeviationSample1",
      "truncationStandardDeviationSample2",
      "truncationStandardDeviationBothSamples",
      "truncationBoundsSample1Lower",
      "truncationBoundsSample1Upper",
      "truncationBoundsSample2Lower",
      "truncationBoundsSample2Upper",
      "truncationBoundsBothSamplesLower",
      "truncationBoundsBothSamplesUpper"
    )

  for (n in evalNames) {
    if (!is.null(options[[n]]))
      options[[n]] <- eval(parse(text = options[[n]]))
  }

  return(options)
}
.robttGetTruncation        <- function(options) {

  if (options[["truncation"]] == "truncationStandardDeviation") {
    truncation <- switch(
      options[["truncationStandardDeviationGrouping"]],
      # (JASP uses opposite ordering that the package)
      "perSample" = list(
        sigma1 = options[["truncationStandardDeviationSample2"]],
        sigma2 = options[["truncationStandardDeviationSample1"]]
      ),
      "bothSamples" = list(
        sigma = options[["truncationStandardDeviationBothSamples"]]
      )
    )
  } else {
    truncation <- switch(
      options[["truncationBoundsGrouping"]],
      "perSample" = list(
        # (JASP uses opposite ordering that the package)
        x1 = c(options[["truncationBoundsSample2Lower"]], options[["truncationBoundsSample2Upper"]]),
        x2 = c(options[["truncationBoundsSample1Lower"]], options[["truncationBoundsSample1Upper"]])
      ),
      "bothSamples" = list(
        x = c(options[["truncationBoundsBothSamplesLower"]], options[["truncationBoundsBothSamplesUpper"]])
      )
    )
  }

  return(truncation)
}

# table filling functions
.robttTableFillCoef           <- function(jaspTable, resultsTable, options, individual = FALSE, groupLabels = NULL, analysis) {

  overtitleCi <- gettextf("%s%% CI", 100 * options[["inferenceCiWidth"]])
  # add columns
  jaspTable$addColumnInfo(name = "terms",  title = "",                type = "string")
  jaspTable$addColumnInfo(name = "mean",   title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  jaspTable$addColumnInfo(name = "upperCI",title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (individual) {
    jaspTable$addColumnInfo(name = "ess",         title = gettext("ESS"),           type = "integer")
    jaspTable$addColumnInfo(name = "rHat",        title = gettext("R-hat"),         type = "number")
  }


  if (is.null(resultsTable))
    return(jaspTable)

  # deal with summary/individual tables
  resultsTable <- .robttAllignSummaryTableNames(resultsTable)


  # fill rows
  rowsToFill <- switch(
    analysis,
    "averaged"  = c("delta", "rho"),
    "robust"    = c("delta", "rho", "nu"),
    "truncated" = c("delta", "rho")
  )
  for (i in c(1:nrow(resultsTable))[rownames(resultsTable) %in% rowsToFill]) {

    if (rownames(resultsTable)[i] == "rho" && options[["inferencePrecisionAllocationAsStandardDeviationRatio"]]) {
      tempRow <- list(
        terms    = "Standard deviation ratio",
        mean     = if (!individual) attr(resultsTable, "mean_sdr"),
        median   = exp(RoBTT::rho2logsdr$fun(resultsTable[i, "Median"])),
        lowerCI  = exp(RoBTT::rho2logsdr$fun(resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)])),
        upperCI  = exp(RoBTT::rho2logsdr$fun(resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)]))
      )
    } else {
      tempRow <- list(
        terms    = .robttCoefNames(rownames(resultsTable)[i], options),
        mean     = resultsTable[i, "Mean"],
        median   = resultsTable[i, "Median"],
        lowerCI  = resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)],
        upperCI  = resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)]
      )
    }

    if (individual) {
      tempRow[["ess"]]   <- round(resultsTable[i, "ESS"])
      tempRow[["rHat"]]  <- resultsTable[i, "R_hat"]
    }

    jaspTable$addRows(tempRow)
  }

  # add footnotes
  footnotes <- attr(resultsTable, "footnotes")
  for (i in seq_along(footnotes))
    jaspTable$addFootnote(footnotes[i])

  # add information about estimate direction
  if (!is.null(groupLabels) && "delta" %in% rownames(resultsTable))
    jaspTable$addFootnote(gettextf("The effect size corresponds to the standardized mean difference of group '%1$s' - '%2$s'.", groupLabels[1], groupLabels[2]))
  if (!is.null(groupLabels) && "rho" %in% rownames(resultsTable)) {
    if (options[["inferencePrecisionAllocationAsStandardDeviationRatio"]])
      jaspTable$addFootnote(gettextf("The standard deviation ratio corresponds to the ratio of standard deviations of group '%1$s' / '%2$s'.", groupLabels[1], groupLabels[2]))
    else
      jaspTable$addFootnote(gettextf("The precision allocation corresponds to the proportion of total precision allocated to group '%1$s'.", groupLabels[1]))
  }


  return(jaspTable)
}
.robttCoefNames               <- function(coefficient, options) {
  if (coefficient == "delta")
    return(gettextf("Effect size (%s)", "\u03B4"))
  else if (coefficient == "rho")
    return(gettextf("Precision allocation (%s)","\u03C1"))
  else if (coefficient == "nu")
    return(gettextf("Degrees of freedom (%s)","\u03BD"))
}
.robttAllignSummaryTableNames <- function(resultsTable) {

  rownames(resultsTable) <- gsub("delta[1]", "delta", rownames(resultsTable), fixed = TRUE)
  rownames(resultsTable) <- gsub("rho[1]",   "rho",   rownames(resultsTable), fixed = TRUE)
  rownames(resultsTable) <- gsub("nu[1]",    "nu",    rownames(resultsTable), fixed = TRUE)

  return(resultsTable)
}

# main functions
.robttPriorsPlots              <- function(jaspResults, options, analysis) {

  # create / access the container
  if (!is.null(jaspResults[["priorPlots"]]))
    priorPlots <- jaspResults[["priorPlots"]]
  else {
    priorPlots <- createJaspContainer(title = gettext("Prior Plots"))
    priorPlots$dependOn(c("priorDistributionPlot", "inferencePrecisionAllocationAsStandardDeviationRatio"))
    priorPlots$position <- 2
    jaspResults[["priorPlots"]] <- priorPlots
  }

  # extract the priors
  priors <- jaspResults[["priors"]][["object"]]

  # create container for each of the parameters
  analysisParameters <- switch(
    analysis,
    "averaged"  = c("effect", "heterogeneity"),
    "robust"    = c("effect", "heterogeneity", "outliers"),
    "truncated" = c("effect", "heterogeneity")
  )
  for (parameter in analysisParameters) {

    if (!is.null(priorPlots[[parameter]]))
      parameterContainer <- priorPlots[[parameter]]
    else {
      parameterContainer <- createJaspContainer(title = switch(
        parameter,
        "effect"          = gettext("Effect"),
        "heterogeneity"   = gettext("Unequal variances"),
        "outliers"        = gettext("Outliers")
      ))
      parameterContainer$position <- switch(
        parameter,
        "effect"          = 1,
        "heterogeneity"   = 2,
        "outliers"        = 3
      )
      priorPlots[[parameter]] <- parameterContainer
    }

    # create container for null and alternative models
    for (type in c("null", "alternative")) {

      if (!is.null(parameterContainer[[type]]))
        next

      tempPriors <- switch(
        paste0(parameter, "-", type),
        "effect-alternative"        = priors[["modelsEffect"]],
        "effect-null"               = priors[["modelsEffectNull"]],
        "heterogeneity-alternative" = priors[["modelsUnequalVariances"]],
        "heterogeneity-null"        = priors[["modelsUnequalVariancesNull"]],
        "outliers-alternative"      = priors[["modelsOutliers"]],
        "outliers-null"             = priors[["modelsOutliersNull"]]
      )

      if (length(tempPriors) == 0 || BayesTools::is.prior.none(tempPriors))
        next

      typePrior <- createJaspPlot(width = 400,  height = 300, title = switch(
        type,
        "null"         = gettext("Null"),
        "alternative"  = gettext("Alternative")
      ))
      typePrior$position <- switch(
        type,
        "null"         = 1,
        "alternative"  = 2
      )
      typePrior$dependOn(switch(
        parameter,
        "effect"        = c("modelsEffect", "modelsEffectNull"),
        "heterogeneity" = c("modelsUnequalVariances", "modelsUnequalVariancesNull"),
        "outliers"      = c("modelsOutliers", "modelsOutliersNull")
      ))
      parameterContainer[[type]] <- typePrior

      p <- try(plot(
        tempPriors,
        plot_type = "ggplot",
        par_name  = switch(
          parameter,
          "effect"        = bquote(delta),
          "heterogeneity" = bquote(rho),
          "outliers"      = bquote(nu)),
        xlim            = if(parameter == "heterogeneity" && options[["inferencePrecisionAllocationAsStandardDeviationRatio"]]) log(2^c(-4,4)),
        transformation  = switch(
          parameter,
          "effect"        = NULL,
          "heterogeneity" = if(options[["inferencePrecisionAllocationAsStandardDeviationRatio"]]) RoBTT::rho2logsdr else NULL,
          "outliers"      = "lin")
        ,
        transformation_arguments = if(parameter == "outliers") list(a = 2, b = 1),
        transformation_settings  = parameter == "heterogeneity"
      ))

      if (jaspBase::isTryError(p)) {
        typePrior$setError(p)
        next
      }

      if (parameter == "heterogeneity" && options[["inferencePrecisionAllocationAsStandardDeviationRatio"]]) {
        p <- p + ggplot2::scale_x_continuous("Standard deviation ratio", limits = log(2^c(-4,4)), breaks = log(2^seq(-4,4,1)), labels = round(2^seq(-4,4,1), 3))
      }

      p <- p + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()

      typePrior[["plotObject"]] <- p
    }
  }

  return()
}
.robttModelPreviewTable        <- function(jaspResults, options, analysis) {

  # create / access the container
  if (!is.null(jaspResults[["modelPreview"]])) {
    return()
  } else {
    modelPreview <- createJaspContainer(title = gettext("Model Preview"))
    modelPreview$dependOn(.robttGetDependencies(analysis))
    modelPreview$position <- 1
    jaspResults[["modelPreview"]] <- modelPreview
  }


  # extract the priors
  priors  <- jaspResults[["priors"]][["object"]]


  # set error if no priors are specified
  if (
    (length(priors[["modelsEffect"]])           == 0 && length(priors[["modelsEffectNull"]])        == 0) ||
    (length(priors[["modelsUnequalVariances"]]) == 0 && length(priors[["modelsUnequalVariancesNull"]]) == 0) ||
    (analysis == "robust" && length(priors[["modelsOutliers"]]) == 0 && length(priors[["modelsOutliersNull"]]) == 0)
  ) {
    priorsError <- createJaspTable()
    priorsError$setError(gettext("Please specify a prior distribution for each parameter in the Models specification section (either null or alternative)."))
    modelPreview[["priorsError"]] <- priorsError
    return()
  }

  # create the setup table
  fitSummary   <- RoBTT::check_setup(
    prior_delta      = priors[["modelsEffect"]],
    prior_rho        = priors[["modelsUnequalVariances"]],
    prior_nu         = if (analysis == "robust") priors[["modelsOutliers"]],
    prior_delta_null = priors[["modelsEffectNull"]],
    prior_rho_null   = priors[["modelsUnequalVariancesNull"]],
    prior_nu_null    = priors[["modelsOutliersNull"]],
    models           = TRUE,
    silent           = TRUE
  )


  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  for (i in 1:if(analysis == "robust") 3 else 2) {
    tempRow <- list(
      terms     = if (i == 1) gettext("Effect") else if (i == 2) gettext("Unequal variances") else if (i == 3) gettext("Outliers"),
      models    = paste0(fitSummary[["components"]][[i, "models"]], "/", attr(fitSummary[["components"]], "n_models")),
      priorProb = fitSummary[["components"]][[i, "prior_prob"]]
    )

    overallSummary$addRows(tempRow)
  }
  overallSummary$addFootnote(gettext("This analysis uses MCMC and might require a prolonged time to complete."), symbol = "\u26A0")
  modelPreview[["overallSummary"]] <- overallSummary


  ### create models overview table
  modelsSummary <- createJaspTable(title = gettext("Model Specification Preview"))
  modelsSummary$position <- 2

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",              title = "#",                          type = "integer")
  modelsSummary$addColumnInfo(name = "distribution",        title = gettext("Distribution"),      type = "string")
  modelsSummary$addColumnInfo(name = "priorEffect",         title = gettext("Effect"),            type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorHeterogeneity",  title = gettext("Unequal variances"), type = "string", overtitle = overtitlePrior)
  if (analysis == "robust") {
    modelsSummary$addColumnInfo(name = "priorOutliers",       title = gettext("Outliers"),          type = "string", overtitle = overtitlePrior)
  }
  modelsSummary$addColumnInfo(name = "priorProb",           title = gettext("P(M)"),              type = "number")

  for (i in 1:nrow(fitSummary[["summary"]])) {
    tempRow <- list(
      number             = fitSummary[["summary"]][i, "Model"],
      distribution       = fitSummary[["summary"]][i, "Distribution"],
      priorEffect        = fitSummary[["summary"]][i, "delta"],
      priorHeterogeneity = fitSummary[["summary"]][i, "rho"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"]
    )

    if (analysis == "robust") {
      tempRow$priorOutliers <- fitSummary[["summary"]][i, "nu"]
    }

    modelsSummary$addRows(tempRow)
  }
  modelsSummary$addFootnote(gettext("The analysis will estimate multiple models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")

  modelPreview[["modelsSummary"]] <- modelsSummary


  return()
}
.robttFitModel                 <- function(jaspResults, dataset, options, analysis) {

  if (!is.null(jaspResults[["model"]]))
    return()

  model <- createJaspState()
  model$dependOn(.robttGetDependencies(analysis))
  jaspResults[["model"]] <- model


  priors <- jaspResults[["priors"]]$object
  fit    <- try(RoBTT::RoBTT(
    # data (JASP uses opposite ordering that the package)
    x1 = dataset[[options[["dependent"]]]][dataset[[options[["group"]]]] == levels(dataset[[options[["group"]]]])[2]],
    x2 = dataset[[options[["dependent"]]]][dataset[[options[["group"]]]] == levels(dataset[[options[["group"]]]])[1]],

    # priors
    prior_delta      = priors[["modelsEffect"]],
    prior_rho        = priors[["modelsUnequalVariances"]],
    prior_nu         = if (analysis == "robust") priors[["modelsOutliers"]],
    prior_delta_null = priors[["modelsEffectNull"]],
    prior_rho_null   = priors[["modelsUnequalVariancesNull"]],
    prior_nu_null    = priors[["modelsOutliersNull"]],
    prior_mu         = if (analysis == "truncated") priors[["modelCommonMean"]],
    prior_sigma2     = if (analysis == "truncated") priors[["modelCommonVariance"]],
    truncation       = if (analysis == "truncated") .robttGetTruncation(options),
    # sampling settings
    chains  = options[["advancedMcmcChains"]],
    warmup  = options[["advancedMcmcAdaptation"]],
    iter    = options[["advancedMcmcAdaptation"]] + options[["advancedMcmcSamples"]],
    thin    = options[["advancedMcmcThin"]],
    # additional settings
    convergence_checks = RoBTT::set_convergence_checks(
      max_Rhat            = 1.05,
      min_ESS             = 500
    ),
    save     = "all",
    seed     = .getSeedJASP(options),
    silent   = TRUE,
    is_JASP  = TRUE
  ))

  # forward group labels for later usage
  attr(fit, "groupLabels") <- levels(dataset[[options[["group"]]]])

  # error handling
  if (jaspBase::isTryError(fit))
    .quitAnalysis(fit)


  # update the fit and reset notifier
  model[["object"]] <- fit

  return()
}
.robttSummaryTable             <- function(jaspResults, options, analysis) {

  if (!is.null(jaspResults[["mainSummary"]])) {
    return()
  } else {
    # create container
    mainSummary <- createJaspContainer(title = gettext("Summary"))
    mainSummary$position <- 3
    mainSummary$dependOn( c(
      .robttGetDependencies(analysis), "bayesFactorType", "inferenceCiWidth",
      "inferenceConditionalParameterEstimates", "inferencePrecisionAllocationAsStandardDeviationRatio"))
    jaspResults[["mainSummary"]] <- mainSummary
  }

  if (is.null(jaspResults[["model"]])) {
    if (options[["inferenceConditionalParameterEstimates"]]) {
      conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"), dependencies = "inferenceConditionalParameterEstimates")
      conditionalSummary <- .robttTableFillCoef(conditionalSummary, NULL, options, analysis = analysis)
      jaspResults[["mainSummary"]][["conditionalSummary"]] <- conditionalSummary
    }
    return()
  }

  # remove the model preview
  jaspResults[["modelPreview"]] <- NULL

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    logBF        = options[["bayesFactorType"]] == "LogBF10",
    BF01         = options[["bayesFactorType"]] == "BF01",
    probs        = c(.5 + c(-1, 1) * options[["inferenceCiWidth"]] / 2),
    conditional  = options[["inferenceConditionalParameterEstimates"]],
  )

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                   type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"),    type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),      type = "number")
  overallSummary$addColumnInfo(name = "postProb",  title = gettext("P(M|data)"), type = "number")
  overallSummary$addColumnInfo(name = "BF",        title = titleBF,              type = "number")

  for (i in 1:nrow(fitSummary[["components"]])) {
    overallSummary$addRows(list(
      terms     = rownames(fitSummary[["components"]])[i],
      models    = paste0(fitSummary[["components"]][i, "models"], "/",  attr(fitSummary[["components"]], "n_models")[i]),
      priorProb = fitSummary[["components"]][i, "prior_prob"],
      postProb  = fitSummary[["components"]][i, "post_prob"],
      BF        = fitSummary[["components"]][i, 4]
    ))
  }

  errorsAndWarnings <- RoBTT::check_RoBTT(fit)
  for (i in seq_along(errorsAndWarnings)) {
    overallSummary$addFootnote(symbol = gettext("Warning:"), errorsAndWarnings[i])
  }

  mainSummary[["overallSummary"]] <- overallSummary


  ### create model averaged results tables
  # estimate table
  averagedSummary <- createJaspTable(title = gettext("Model Averaged Estimates"))
  averagedSummary$position <- 2
  attr(fitSummary[["estimates"]], "mean_sdr") <- mean(exp(RoBTT::rho2logsdr$fun(fit[["RoBTT"]][["posteriors"]][["rho"]])))
  averagedSummary <- .robttTableFillCoef(averagedSummary, fitSummary[["estimates"]], options, groupLabels = attr(fit, "groupLabels"), analysis = analysis)
  mainSummary[["averagedSummary"]] <- averagedSummary


  ### create conditional models results tables
  if (options[["inferenceConditionalParameterEstimates"]]) {
    # estimate table
    conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"))
    conditionalSummary$position <- 5
    attr(fitSummary[["estimates_conditional"]], "mean_sdr") <- mean(exp(RoBTT::rho2logsdr$fun(fit[["RoBTT"]][["posteriors_conditional"]][["rho"]])))
    conditionalSummary <- .robttTableFillCoef(conditionalSummary, fitSummary[["estimates_conditional"]], options, groupLabels = attr(fit, "groupLabels"), analysis = analysis)
    mainSummary[["conditionalSummary"]] <- conditionalSummary
  }

  return()
}
.robttModelsOvervievTable      <- function(jaspResults, options, analysis) {

  ### create overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 6
  modelsSummary$dependOn(c(.robttGetDependencies(analysis), "bayesFactorType", "inferenceModelsOverview", "inferenceModelsOverviewBF", "inferenceModelsOverviewOrder", "inferenceShortenPriorName"))
  jaspResults[["mainSummary"]][["modelsSummary"]] <- modelsSummary

  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion")
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettext("Inclusion BF"),
      "BF01"    = gettext("Exclusion BF"),
      "LogBF10" = gettext("log(Inclusion BF)")
    )
  else
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",             title = "#",                          type = "integer")
  modelsSummary$addColumnInfo(name = "distribution",       title = gettext("Distribution"),      type = "string")
  modelsSummary$addColumnInfo(name = "priorEffect",        title = gettext("Effect"),            type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorHeterogeneity", title = gettext("Unequal variances"), type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorOutliers",      title = gettext("Outliers"),          type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",          title = gettext("P(M)"),              type = "number")
  modelsSummary$addColumnInfo(name = "postProb",           title = gettext("P(M|data)"),         type = "number")
  modelsSummary$addColumnInfo(name = "marglik",            title = gettext("log(MargLik)"),      type = "number")
  modelsSummary$addColumnInfo(name = "BF",                 title = titleBF,                      type = "number")

  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    type       = "models",
    short_name = options[["inferenceShortenPriorName"]]
  )

  # do ordering
  if (options[["inferenceModelsOverviewOrder"]] == "marginalLikelihood")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["marglik"]], decreasing = TRUE),]
  else if (options[["inferenceModelsOverviewOrder"]] == "posteriorProbability")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["post_prob"]], decreasing = TRUE),]

  # compute the BF requested
  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion") {
    bf <- fitSummary[["summary"]][, "inclusion_BF"]
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "best") {
    bf <- exp(fitSummary[["summary"]][["marglik"]] - max(fitSummary[["summary"]][["marglik"]]))
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "previous") {
    tempThisMargLik <- fitSummary[["summary"]][["marglik"]][-length(fitSummary[["summary"]][["marglik"]])]
    tempPrevMargLik <- fitSummary[["summary"]][["marglik"]][-1]
    bf <- c(1, exp(tempPrevMargLik - tempThisMargLik))
  }

  # fill the rows
  for (i in 1:nrow(fitSummary[["summary"]])) {
    modelsSummary$addRows(list(
      number             = fitSummary[["summary"]][i, "Model"],
      distribution       = fitSummary[["summary"]][i, "Distribution"],
      priorEffect        = fitSummary[["summary"]][i, "delta"],
      priorHeterogeneity = fitSummary[["summary"]][i, "rho"],
      priorOutliers      = fitSummary[["summary"]][i, "nu"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"],
      postProb           = fitSummary[["summary"]][i, "post_prob"],
      marglik            = fitSummary[["summary"]][i, "marglik"],
      BF                 = .robttFormatBf(bf[i], bayesFactorType = options[["bayesFactorType"]])
    ))
  }

  return()
}
.robttModelsSummaryTable       <- function(jaspResults, options, analysis) {

  if (!is.null(jaspResults[["individualModels"]])) {
    return()
  } else {
    individualModels <- createJaspContainer(title = gettext("Individual Models Summary"))
    individualModels$position <- 5
    individualModels$dependOn(c(.robttGetDependencies(analysis), "bayesFactorType", "inferenceIndividualModels", "inferenceIndividualModelsSingleModel", "inferenceIndividualModelsSingleModelNumber", "inferenceShortenPriorName", "inferenceOutputScale"))
    jaspResults[["individualModels"]] <- individualModels
  }

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  if (is.null(jaspResults[["model"]])) {

    tempModel <- createJaspContainer(title = gettext("Model #"))
    individualModels[["modelI"]] <- tempModel

    tempPriors <- createJaspTable(title = gettext("Priors"))
    tempPriors$addColumnInfo(name = "priorDelta",   title = gettext("Effect"),            type = "string")
    tempPriors$addColumnInfo(name = "priorRho",     title = gettext("Unequal variances"), type = "string")
    if (analysis == "robust") {
      tempPriors$addColumnInfo(name = "priorNu",      title = gettext("Outliers"),          type = "string")
    }
    tempModel[["tempPriors"]] <- tempPriors

    tempInfo <- createJaspTable(title = gettext("Information"))
    tempInfo$addColumnInfo(name = "distribution", title = gettext("Distribution"),  type = "string")
    tempInfo$addColumnInfo(name = "priorProb",    title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",     title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",      title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",           title = titleBF,                  type = "number")
    tempModel[["tempInfo"]] <- tempInfo

    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robttTableFillCoef(tempCoef, NULL, options, individual = TRUE, analysis = analysis)
    tempModel[["tempCoef"]] <- tempCoef

    return()
  }

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    type          = "individual",
    short_name    = options[["inferenceShortenPriorName"]]
  )

  ### create tables for individual models

  # select models to iterate over
  if (options[["inferenceIndividualModelsSingleModel"]]) {
    modelsI <- options[["inferenceIndividualModelsSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      tempError  <- createJaspTable(title = "")
      tempError$setError(gettextf("Model %1$i does not exist. Select one of the models between 1 and %2$i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]]                     <- tempError
      individualModels[[paste0("model", modelsI)]] <- tempModel
      return()
    }
  } else {
    modelsI <- 1:length(fit[["models"]])
  }


  # do the iteration
  for (i in modelsI) {

    tempModel <- createJaspContainer(title = gettextf("Model %i", i))
    individualModels[[paste0("model", i)]] <- tempModel

    ### model priors
    tempPriors <- createJaspTable(title = gettext("Priors"))
    tempPriors$addColumnInfo(name = "priorDelta",  title = gettext("Effect"),            type = "string")
    tempPriors$addColumnInfo(name = "priorRho",    title = gettext("Unequal variances"), type = "string")
    if (analysis == "robust") {
      tempPriors$addColumnInfo(name = "priorNu", title = gettext("Outliers"), type = "string")
    }

    tempRow <- list(
      priorDelta  = print(fit[["models"]][[i]][["priors"]][["delta"]], silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
      priorRho    = print(fit[["models"]][[i]][["priors"]][["rho"]],   silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
    )

    if (analysis == "robust") {
      tempRow$priorNu = if (is.null(fit[["models"]][[i]][["priors"]][["nu"]]))
        ""
      else
        print(fit[["models"]][[i]][["priors"]][["nu"]], silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
    }

    tempPriors$addRows(tempRow)

    tempModel[["tempPriors"]] <- tempPriors


    ### model information
    tempInfo <- createJaspTable(title = gettext("Information"))

    tempInfo$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    tempInfo$addColumnInfo(name = "priorProb",   title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",    title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",     title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",          title = titleBF,                  type = "number")

    tempInfo$addRows(list(
      distribution = fit[["models"]][[i]][["likelihood"]],
      priorProb    = fit[["models"]][[i]][["inference"]][["prior_prob"]],
      postProb     = fit[["models"]][[i]][["inference"]][["post_prob"]],
      marglik      = fit[["models"]][[i]][["inference"]][["marglik"]],
      BF           = .robttFormatBf(fit[["models"]][[i]][["inference"]][["inclusion_BF"]], bayesFactorType = options[["bayesFactorType"]])
    ))

    tempModel[["tempInfo"]] <- tempInfo


    ### model coefficients
    # estimate table
    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robttTableFillCoef(tempCoef, fitSummary$models[[i]][["estimates"]], options, individual = TRUE, analysis = analysis)
    tempModel[["tempCoef"]] <- tempCoef

  }

  return()
}
.robttEstimatesPlot            <- function(jaspResults, options, parameter, analysis) {

  # create / access the container
  if (is.null(jaspResults[["estimatesPlots"]])) {
    estimatesPlots <- createJaspContainer(title = gettext("Posterior Distribution Plots"))
    estimatesPlots$position <- 7
    estimatesPlots$dependOn(c(.robttGetDependencies(analysis), "plotsPooledEstimatesType", "plotsPooledEstimatesPriorDistribution"))
    jaspResults[["estimatesPlots"]] <- estimatesPlots
  } else {
    estimatesPlots <- jaspResults[["estimatesPlots"]]
  }

  # don't redo an already created plot
  if (!is.null(estimatesPlots[[parameter]]))
    return()

  # prepare the plot object
  title  <- sprintf(
    "%1$s %2$s",
    switch(
      options[["plotsPooledEstimatesType"]],
      "conditional" = gettext("Conditional"),
      "averaged"    = gettext("Model Averaged")
    ),
    switch(
      parameter,
      "delta" = gettext("Effect Size Estimate"),
      "rho"   = gettext("Heterogeneity Estimate (Precision Allocation)"),
      "nu"    = gettext("Outliers Estimate (Degrees of Freedom)")
    ))
  height <- 350
  width  <- 600

  tempPlot <- createJaspPlot(title = title, width = width, height = height)
  tempPlot$position <- switch(
    parameter,
    "delta" = 1,
    "rho"   = 2,
    "nu"    = 3
  )
  tempPlot$dependOn(switch(
    parameter,
    "delta" = "plotsPooledEstimatesEffect",
    "rho"   = c("plotsPooledEstimatesUnequalVariances", "inferencePrecisionAllocationAsStandardDeviationRatio"),
    "nu"    = "plotsPooledEstimatesOutliers"
  ))
  estimatesPlots[[parameter]] <- tempPlot

  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # plot
  p <- try(plot(
    fit,
    parameter     = parameter,
    prior         = options[["plotsPooledEstimatesPriorDistribution"]],
    conditional   = options[["plotsPooledEstimatesType"]] == "conditional",
    transform_rho = options[["inferencePrecisionAllocationAsStandardDeviationRatio"]],
    plot_type     = "ggplot"
  ))

  if (jaspBase::isTryError(p)) {
    tempPlot$setError(p)
    return()
  }

  if (attr(p, "sec_axis"))
    p <- p + jaspGraphs::geom_rangeframe(sides = "blr") + jaspGraphs::themeJaspRaw() + ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(vjust = 3.25),
      plot.margin        = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
  else
    p <- p + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()

  estimatesPlots[[parameter]]$plotObject <- p

  return()
}
.robttDiagnosticsOverviewTable <- function(jaspResults, options, analysis) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 9
    diagnostics$dependOn(.robttGetDependencies(analysis))
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }

  if (!is.null(diagnostics[["diagosticsTable"]])) {
    return()
  }


  ### create overview table
  diagosticsTable <-  createJaspTable(title = gettext("Models Diagnostics Overview"))
  diagosticsTable$position <- 1
  diagosticsTable$dependOn(c(.robttGetDependencies(analysis), "mcmcDiagnosticsOverviewTable", "inferenceShortenPriorName"))
  diagnostics[["diagosticsTable"]] <- diagosticsTable

  overtitlePrior <- gettext("Prior Distribution")

  diagosticsTable$addColumnInfo(name = "number",             title = "#",                          type = "integer")
  diagosticsTable$addColumnInfo(name = "distribution",       title = gettext("Distribution"),      type = "string")
  diagosticsTable$addColumnInfo(name = "priorEffect",        title = gettext("Effect"),            type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "priorHeterogeneity", title = gettext("Unequal variances"), type = "string",  overtitle = overtitlePrior)
  if (analysis == "robust") {
    diagosticsTable$addColumnInfo(name = "priorOutliers",      title = gettext("Outliers"),          type = "string",  overtitle = overtitlePrior)
  }
  diagosticsTable$addColumnInfo(name = "ess",                title = gettext("min(ESS)"),          type = "integer")
  diagosticsTable$addColumnInfo(name = "rHat",               title = gettext("max(R-hat)"),        type = "number")


  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # get the diagnostics summary
  fitSummary <- summary(
    fit,
    type       = "diagnostics",
    short_name = options[["inferenceShortenPriorName"]]
  )

  for (i in 1:nrow(fitSummary[["diagnostics"]])) {

    tempRows <- list(
      number             = fitSummary[["diagnostics"]][i, "Model"],
      distribution       = fitSummary[["diagnostics"]][i, "Distribution"],
      priorEffect        = fitSummary[["diagnostics"]][i, "delta"],
      priorHeterogeneity = fitSummary[["diagnostics"]][i, "rho"],
      ess                = round(fitSummary[["diagnostics"]][i, "min_ESS"]),
      rHat               = fitSummary[["diagnostics"]][i, "max_R_hat"]
    )
    if (analysis == "robust") {
      tempRows$priorOutliers <- fitSummary[["diagnostics"]][i, "nu"]
    }

    diagosticsTable$addRows(tempRows)
  }

  return()
}
.robttDiagnosticsPlots         <- function(jaspResults, options, analysis) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 9
    diagnostics$dependOn(.robttGetDependencies(analysis))
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }


  if (is.null(jaspResults[["model"]]))
    wait <- TRUE
  else if (!.robttCheckDiagnostics(options, any = FALSE, analysis))
    wait <- TRUE
  else
    wait <- FALSE

  if (wait) {
    tempWait  <- createJaspHtml(text = gettext("MCMC Diagnostics plots are created once both the plotted parameter ('Plot') and the plot type ('Type') options are selected."))
    tempWait$dependOn(.robttGetDiagnosticsDependencies(analysis))
    diagnostics[["tempWait"]] <- tempWait
    return()
  }


  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # select models to iterate over
  if (options[["mcmcDiagnosticsPlotSingleModel"]]) {
    modelsI <- options[["mcmcDiagnosticsPlotSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      diagnostics[[paste0("model", modelsI)]] <- tempModel
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn("mcmcDiagnosticsPlotSingleModelNumber", "mcmcDiagnosticsPlotSingleModel")
      tempError$setError(gettextf("Model %1$i does not exist. Select one of the models between 1 and %2$i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]] <- tempError
      return()
    }
  } else {
    modelsI <- 1:length(fit[["models"]])
  }

  # collect the parameters
  parameters <- NULL
  if (options[["mcmcDiagnosticsPlotEffect"]])
    parameters <- c(parameters, "delta")
  if (options[["mcmcDiagnosticsPlotUnequalVariances"]])
    parameters <- c(parameters, "rho")
  if (analysis == "robust" && options[["mcmcDiagnosticsPlotOutliers"]])
    parameters <- c(parameters, "nu")


  # do the iterations
  for (i in modelsI) {
    # create / access container for individual models
    if (is.null(diagnostics[[paste0("model_", i)]])) {
      tempModel <- createJaspContainer(title = gettextf("Model %i", i))
      tempModel$position <- 1 + i
      tempModel$dependOn(c("mcmcDiagnosticsPlotSingleModelNumber", "mcmcDiagnosticsPlotSingleModel"))
      diagnostics[[paste0("model", i)]] <- tempModel
    } else {
      tempModel <- diagnostics[[paste0("model", i)]]
    }

    noPars <- TRUE # tracker for checking whether any parameter was plotted

    for (par in parameters) {
      # create / access container for individual parameters
      if (is.null(tempModel[[par]])) {
        tempPar <- createJaspContainer(title = switch(
          par,
          "delta" = gettext("Effect"),
          "rho"   = gettext("Heterogeneity (Precision Allocation)"),
          "nu"    = gettext("Outliers (Degrees of Freedom)")
        ))
        tempPar$position <- switch(
          par,
          "delta" = 1,
          "rho"   = 2,
          "nu"    = 3
        )
        tempPar$dependOn(switch(
          par,
          "delta" = "mcmcDiagnosticsPlotEffect",
          "rho"   = "mcmcDiagnosticsPlotUnequalVariances",
          "nu"    = "mcmcDiagnosticsPlotOutliers"
        ))
        tempModel[[par]] <- tempPar
      } else {
        tempPar <- tempModel[[par]]
      }


      # add trace plots
      if (options[["mcmcDiagnosticsPlotTypeTrace"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["trace"]])) {

          newPlot  <- RoBTT::diagnostics(
            fit,
            parameter     = par,
            type          = "trace",
            show_models   = i,
            title         = FALSE
          )

          if (!is.null(newPlot)) {
            tempPlot <- createJaspPlot(gettext("Trace plots"), width = 400, aspectRatio = .7)
            tempPlot$position <- 1
            tempPlot$dependOn("mcmcDiagnosticsPlotTypeTrace")

            tempPlot$plotObject  <- newPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
            tempPar[["trace"]]  <- tempPlot

            noPars <- FALSE
          }

        } else {
          noPars <- FALSE
        }
      }


      # add autocorrelation plots
      if (options[["mcmcDiagnosticsPlotTypeAutocorrelation"]]) {
        # create / access container for autocorrelations plots
        if (is.null(tempPar[["autocorrelations"]])) {

          newPlot <- RoBTT::diagnostics(
            fit,
            parameter     = par,
            type          = "autocorrelations",
            show_models   = i,
            title         = FALSE
          )

          if (!is.null(newPlot)) {

            tempPlot <- createJaspPlot(gettext("Average autocorrelations"), width = 400, aspectRatio = .7)
            tempPlot$position <- 2
            tempPlot$dependOn("mcmcDiagnosticsPlotTypeAutocorrelation")
            tempPlot$plotObject  <- newPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
            tempPar[["autocorrelations"]] <- tempPlot

            noPars <- FALSE
          }

        } else {
          noPars <- FALSE
        }
      }


      # add sample densities plots
      if (options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) {
        # create / access container for samples plots
        if (is.null(tempPar[["samples"]])) {

          newPlot <- RoBTT::diagnostics(
            fit,
            parameter     = par,
            type          = "densities",
            show_models   = i,
            title         = FALSE
          )

          if (!is.null(newPlot)) {

            tempPlot <- createJaspPlot(gettext("Posterior samples densities"), width = 400, aspectRatio = .7)
            tempPlot$position <- 3
            tempPlot$dependOn("mcmcDiagnosticsPlotTypePosteriorSamplesDensity")
            tempPlot$plotObject  <- newPlot + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()
            tempPar[["samples"]] <- tempPlot

            noPars <- FALSE
          }

        } else {
          noPars <- FALSE
        }


      }

    }

    # show error if only one model is selected but doesn't contain any of the diagnostics
    if (noPars && options[["mcmcDiagnosticsPlotSingleModelNumber"]]) {
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn(.robttGetDiagnosticsDependencies(analysis))
      tempError$setError(gettextf("Model %i does not contain any of the selected parameters.", i))
      tempModel[["tempError"]] <- tempError
    }
  }

  return()
}
.robttFormatBf                 <- function(BF, bayesFactorType){
  return(switch(
    bayesFactorType,
    "BF10"    = BF,
    "BF01"    = 1/BF,
    "LogBF10" = log(BF)
  ))
}
.robttCheckDiagnostics           <- function(options, any, analysis) {

  if (analysis == "robust") {
    parametersAny <-
      options[["mcmcDiagnosticsPlotEffect"]]           ||
      options[["mcmcDiagnosticsPlotUnequalVariances"]] ||
      options[["mcmcDiagnosticsPlotOutliers"]]
    typeAny       <-
      options[["mcmcDiagnosticsPlotTypeTrace"]]            ||
      options[["mcmcDiagnosticsPlotTypeAutocorrelation"]]  ||
      options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]
  } else {
    parametersAny <-
      options[["mcmcDiagnosticsPlotEffect"]]           ||
      options[["mcmcDiagnosticsPlotUnequalVariances"]]
    typeAny       <-
      options[["mcmcDiagnosticsPlotTypeTrace"]]            ||
      options[["mcmcDiagnosticsPlotTypeAutocorrelation"]]
  }

  if (any)
    return(parametersAny || typeAny)
  else
    return(parametersAny && typeAny)
}
.robttGetDiagnosticsDependencies <- function(analysis) {
  if (analysis == "robust") {
    return(c(
      "mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotUnequalVariances", "mcmcDiagnosticsPlotOutliers",
      "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"
    ))
  } else {
    return(c(
      "mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotUnequalVariances",
      "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation"
    ))
  }
}
