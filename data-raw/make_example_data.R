# Run this script to (re)generate inst/extdata/example_data.csv
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
write.csv(example_dat, file = "inst/extdata/example_data.csv", row.names = FALSE)
