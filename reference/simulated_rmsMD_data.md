# Simulated Data for the Vignette

Generates a synthetic dataset for testing and demonstration purposes in
the `rmsMD` package.

## Usage

``` r
simulated_rmsMD_data(type = c("complete_case", "missing_for_MI"))
```

## Arguments

- type:

  Character string; either `"complete_case"` (no missing data) or
  `"missing_for_MI"` (introduces 10% missing data in each predictor).

## Value

A data frame with simulated variables: `age`, `bmi`, `sex`, `smoking`,
`majorcomplication`, `lengthstay`, `time`, and `event`.
