# pairedmcnemar

Pairwise **McNemar tests vs a control** for paired binary outcomes, plus **Cohen's g** with confidence intervals.

## Features

- Compares **Control** to **2+ treatments** using **contingencytables** McNemar tests:
  - Asymptotic (default)
  - Asymptotic with continuity correction
  - Mid-P (reports `Z = NA`)
- Adjusts p-values across comparisons (Holm default; BH / Hochberg optional)
- Computes Cohen's g (effectsize) + CI
- Adds helpful metadata columns:
  - `method_label`, `p_adjust_label`, `ci_label`
  - `settings` combined label (short/full)
  - `nd` discordant pairs
- S3 `summary()` + `print()` methods with flexible display.

## Install (development)

```r
# install.packages("remotes")
remotes::install_github("PHUoL/pairedmcnemar")
```

## Quick start

```r
library(pairedmcnemar)

set.seed(1)
n <- 40
id <- 1:n
control <- rbinom(n, 1, 0.4)
A <- control
B <- ifelse(runif(n) < 0.25, 1 - control, control)
C <- rbinom(n, 1, 0.6)

example_dat <- data.frame(
  id = rep(id, 4),
  condition = rep(c("Control", "A", "B", "C"), each = n),
  outcome = c(control, A, B, C)
)

fit <- mcnemar_vs_control(
  example_dat, id, condition, outcome,
  mcnemar_method = "midp",
  p_adjust = "BH",
  ci = 0.90,
  settings_style = "short",
  settings = "yes"
)

# print uses the stored default display
fit

# override display
summary(fit, settings = "none")
summary(fit, settings = "yes")
```

## Data

A CSV example is included at `inst/extdata/example_data.csv`.

## License

MIT.
