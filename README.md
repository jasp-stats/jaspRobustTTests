<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspRobustTTests/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspRobustTTests/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspRobustTTests/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspRobustTTests)
<br>
<b>Maintainer:</b> <a href="https://github.com/FBartos/">František Bartoš</a>

</div>

# The Robust T-Test Module

## Overview

<img src='inst/icons/analysis-bayesian-ttest.svg' width='149' height='173' align='right'/>

**JASP Robust T-Test module** is an add-on module for JASP that provides tools for fitting and interpreting robust Bayesian t-tests. The Robust T-Test module offers an ability to fit a Bayesian model-averaged t-test (model-averaging across t-tests with equal of unequal variances), robust Bayesian model-averaged t-tests (incorporating models with t-likelihoods to account for outliers), and truncated Bayesian model-averaged t-tests (excluding outliers and adjusting for their removal).


## R Packages

<img src='https://www.r-project.org/logo/Rlogo.svg' width='100' height='78' align='right'/>

The functionality is served by several R packages

- **RoBTT** — Package for fitting robust Bayesian T-Tests ([RoBTT on CRAN](https://cran.r-project.org/package=RoBTT))
