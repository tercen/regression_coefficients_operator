# Regression Coefficients

##### Description

The `Regression Coefficients operator` computes regression coefficients for a set of covariates.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, outcome 
`column`        | numeric / character, covariates (multiple columns supported)

Output relations|.
---|---
`covariate`        | covariate name, per row
`coefficient`        | regression coefficient, per row
`std_error`        | standard error on the coefficient, per row
`statistic`        | test statistic, per row
`p_value`        | p-value, per row
`log10_p_value`        | -log10(p-value), per row
`perc_variance`        | percentage of explained variance, per row

##### Details

The regression coefficients are estimated using the `lm()` R function. Each column layer
is treated as an independent covariate in the model. The order of the covariates in the model
is the same as the one specified in the Tercen projection.

##### See Also

[lm_operator](https://github.com/tercen/lm_operator)

