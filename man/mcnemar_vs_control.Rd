\name{mcnemar_vs_control}
\alias{mcnemar_vs_control}
\alias{summary.mcnemar_vs_control}
\alias{print.mcnemar_vs_control}
\title{Pairwise McNemar tests vs a control (paired binary outcomes)}
\description{Compare a control condition to two or more treatments for paired binary outcomes using McNemar tests (contingencytables) and Cohen's g (effectsize).}
\usage{
mcnemar_vs_control(data, id, condition, outcome, control = "Control",
  treatments = NULL, mcnemar_method = c("asymptotic", "cc", "midp"),
  p_adjust = c("holm", "BH", "hochberg"), ci = 0.95,
  alternative_g = c("two.sided", "greater", "less"),
  midp_nd0 = c("one", "as_is"), settings_style = c("full", "short"),
  settings = c("none", "yes"))

\method{summary}{mcnemar_vs_control}(object, digits = 4, settings = NULL, ...)
\method{print}{mcnemar_vs_control}(x, digits = 4, settings = NULL, ...)
}
\arguments{
\item{data}{A data frame in long format.}
\item{id}{Column name for subject/pair identifier.}
\item{condition}{Column name for condition labels.}
\item{outcome}{Column name for binary outcome (0/1 or logical).}
\item{control}{Control condition label.}
\item{treatments}{Treatments to compare to control.}
\item{mcnemar_method}{McNemar method.}
\item{p_adjust}{Multiple testing adjustment method.}
\item{ci}{Confidence interval level for Cohen's g.}
\item{alternative_g}{Alternative hypothesis for Cohen's g CI.}
\item{midp_nd0}{Handling for nd==0 in mid-P mode.}
\item{settings_style}{short or full combined settings label.}
\item{settings}{Default display mode stored in object.}
\item{object}{An object returned by mcnemar_vs_control.}
\item{digits}{Digits for rounding numeric output.}
\item{x}{An object returned by mcnemar_vs_control.}
\item{...}{Unused/forwarded.}
}
\value{An object of class \code{mcnemar_vs_control}.}
\examples{
set.seed(1)
n <- 20
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

fit <- mcnemar_vs_control(example_dat, id, condition, outcome,
  mcnemar_method = "midp", p_adjust = "BH", ci = 0.90,
  settings_style = "short", settings = "yes")
print(fit)
summary(fit, settings = "none")
}
