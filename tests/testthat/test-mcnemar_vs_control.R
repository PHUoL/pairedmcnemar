test_that("midp returns P and Z=NA; nd==0 handled", {
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

  fit <- mcnemar_vs_control(example_dat, id, condition, outcome,
                            mcnemar_method = "midp",
                            p_adjust = "holm",
                            settings = "none")

  # Z should be NA for midp
  expect_true(all(is.na(fit$results$Z)))

  # Treatment A has nd==0 and should yield P=1
  rowA <- fit$results[fit$results$treatment == "A", ]
  expect_equal(rowA$nd, 0)
  expect_equal(rowA$P, 1)
  expect_equal(rowA$P_adj, 1)
})

test_that("print method returns object invisibly", {
  set.seed(1)
  n <- 10
  id <- 1:n
  control <- rbinom(n, 1, 0.4)
  A <- control
  B <- ifelse(runif(n) < 0.25, 1 - control, control)

  dat <- data.frame(
    id = rep(id, 3),
    condition = rep(c("Control", "A", "B"), each = n),
    outcome = c(control, A, B)
  )

  fit <- mcnemar_vs_control(dat, id, condition, outcome, settings = "yes")
  out <- capture.output(print(fit))
  expect_true(length(out) > 0)
})
