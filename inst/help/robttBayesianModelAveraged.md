Bayesian Model-Averaged T-Test
===
The Bayesian model-averaged t-test allows the user to estimate the effect size and test the null hypothesis that the population means of two independent groups are equal while accounting for, estimating, and testing for unequal variances and outliers.

### Assumptions
---

- Continuous dependent variable.
- The observations in both groups are a random sample from the population.
- The dependent variable is normally or t-distributed in both populations.

### Hypotheses
-  Directional hypotheses can be specified using the prior distribution specification under the Models section.
---

### Input
---

#### Assignment Box
- Variables: In this box the dependent variable is selected.  
- Grouping Variable: In this box the variable defining the groups is selected.

#### Conditional parameter estimates
Displays estimates assuming that the alternative models are true.

#### Models overview
Displays overview of the specified models.
- BF: Show different types of Bayes factors
  - Inclusion: Change from prior to posterior odds for each individual model.
  - vs. best: Bayes factor comparing to the best fitting model.
  - vs. previous: Bayes factor comparing to a better fitting model.
- Order: Order the overview of displayed models.
  - Model number: Based on the order of each model.
  - Bayes factor: Based on the inclusion Bayes factor of each model.
  - Posterior prob.: Based on the posterior probability of each model.

#### Individual models
Displays a detailed overview of each specified model.
- Single model: Display the overview for only one of the specified models.

#### Bayes Factor
- BF<sub>10</sub>: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- BF<sub>01</sub>: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- Log(BF<sub>01</sub>) : Natural logarithm of BF<sub>01</sub>.

#### CI width
Width of the credible intervals.

#### Shorten prior names
Abbriviates names of the prior distributions.

#### Precision allocation as standard deviation ration
Whether the precision allocation parameter (rho) should be transformed to (log, for plotting) standard deviation ratio.


### Plots
---

#### Pooled estimates
- Effect: Displays a plot with the estimated effect size (delta).
- Unequal variances (precision allocation): Displays a plot with the estimated precision allocation (rho) unless `Precision allocation as standard deviation ration` is selected, then the log of standard deviation ratio is displayed.
- Outliers (degrees of freedom): Displays a plot with the estimated degrees of freedom (nu).

#### Type
  - Model-averaged: Shows the model-averaged effect/unequal variances/outliers estimate based on all specified models.
  - Conditional: Shows the conditional effect/unequal variances/outliers estimate based on models assuming presence of the effect/unequal variances/outliers.

#### Show priors
Displays prior distribution density on top of the estimates figures.


### MCMC Diagnostics
---
#### Overview table
Displays overview of the individual model diagnostics. The table summarizes the minimal effective sample size, and maximum R-hat per model. More detailed, per-parameter, fit diagnostics can be accessed by displaying individual model summary in the `Inference` tab.

#### Plot
Displays chains summaries according to the selected type for each of the models for the selected parameters:
- Effect: The effect size estimate.
- Unequal variances: The precision allocation estimate.
- Outliers: The degrees of freedom estimate.

#### Type
Type of the chains summaries to be displayed.
- Trace: Displays the overlaying traces of each chain for the selected parameters. Different chains are visualized with different colors.
- Autocorrelation: Displays the average autocorrelations of the chains for the selected parameters.
- Posterior sample densities: Displays the overlaying densities of samples from each chain for the selected parameters. Different chains are visualized with different colors.

#### Single model
Display MCMC chain summaries for only a specific model.


### Models
---
The individual models that form up the Bayesian model-averaged t-test are defined by creating combinations of all specified priors for the effect/unequal variances/outliers components. The individual models' prior odds are obtained by multiplying the prior odds of prior distributions for each of the parameters that form the model.

#### Set null priors
Allows specifying prior distributions for the null models.

### Advanced
---

#### Estimation settings (MCMC)
- Adaptation: Sets the number of iterations to be used for adapting the MCMC chains.
- Samples: Sets the number of iterations to be used for sampling from the MCMC chains.
- Chains: Sets the number of chains for the MCMC estimation.
- Thin: Sets the thinning of the MCMC chains.

#### Repeatability
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.
(note that the analysis uses Stan which does not guarantee reproducibility across operating systems)


### References
---
- Maier, M., Bartoš, F., Quintana, D. S., Dablander, F., van den Bergh, D., Marsman, M., … Wagenmakers, E. (2022). Model-Averaged Bayesian t-Tests. Preprint at https://doi.org/10.31234/osf.io/d5zwc


### R Packages
---
- RoBTT
