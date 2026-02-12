#' Pairwise McNemar tests vs a control (paired binary outcomes)
#'
#' Compare a **control** condition to **two or more** treatment conditions for paired
#' binary outcomes. For each treatment, the function:
#'
#' - Builds a paired 2x2 table of control vs treatment outcomes.
#' - Runs a McNemar test from **contingencytables**:
#'   - `asymptotic` (default),
#'   - `cc` (asymptotic with continuity correction),
#'   - `midp` (McNemar mid-P test; `Z` is reported as `NA`).
#' - Computes Cohen's *g* and confidence intervals via **effectsize**.
#' - Adjusts p-values across treatment comparisons (Holm by default).
#'
#' The returned object includes a `results` data frame and supports `summary()` and
#' `print()` methods. The summary/print display can either show a combined `settings`
#' column or separate label columns.
#'
#' ## Edge cases
#' If the number of discordant pairs is zero (`nd = n01 + n10 = 0`), there is no
#' disagreement between control and treatment. In this case the function returns
#' `P = 1`, `Z = 0` for asymptotic methods (or `NA` for mid-P), and Cohen's g = 0
#' with CI [0, 0].
#'
#' ## Why `midP` is used as the p-value for `mcnemar_method = "midp"`
#' `contingencytables::McNemar_midP_test_paired_2x2()` returns the mid-P p-value in
#' an element named `midP` (not `Pvalue`).
#'
#' @param data A data frame in long format.
#' @param id Column name for subject/pair identifier.
#' @param condition Column name for condition labels.
#' @param outcome Column name for the paired binary outcome (0/1 or logical).
#' @param control Label of the control condition (character).
#' @param treatments Optional character vector of treatment labels to compare against control.
#'   If `NULL`, uses all non-control conditions.
#' @param mcnemar_method McNemar test method: `"asymptotic"` (default), `"cc"`, or `"midp"`.
#' @param p_adjust Multiple testing correction method for p-values: `"holm"` (default),
#'   `"BH"`, or `"hochberg"`.
#' @param ci Confidence interval level for Cohen's g (default 0.95).
#' @param alternative_g Alternative hypothesis passed to `effectsize::cohens_g()`.
#' @param midp_nd0 Behavior for `nd == 0` when using mid-P: `"one"` forces `P = 1`
#'   (recommended), `"as_is"` keeps the mid-P definition.
#' @param settings_style When creating the combined `settings` label, choose:
#'   `"short"` (method | adjustment | CI) or `"full"` (includes additional options).
#' @param settings Default display mode stored in the object: `"none"` shows separate
#'   columns (`method_label`, `p_adjust_label`, `ci_label`); `"yes"` shows a combined
#'   `settings` column instead.
#'
#' @return An object of class `mcnemar_vs_control` with fields:
#' - `results`: data frame of comparisons.
#' - metadata fields for the chosen methods and defaults.
#'
#' @examples
#' # Example data (also available as inst/extdata/example_data.csv)
#' set.seed(1)
#' n <- 40
#' id <- 1:n
#' control <- rbinom(n, 1, 0.4)
#' A <- control                        # nd == 0 edge case
#' B <- ifelse(runif(n) < 0.25, 1 - control, control)
#' C <- rbinom(n, 1, 0.6)
#'
#' example_dat <- data.frame(
#'   id = rep(id, 4),
#'   condition = rep(c("Control", "A", "B", "C"), each = n),
#'   outcome = c(control, A, B, C)
#' )
#'
#' fit <- mcnemar_vs_control(
#'   example_dat, id, condition, outcome,
#'   mcnemar_method = "midp",
#'   p_adjust = "BH",
#'   ci = 0.90,
#'   settings_style = "short",
#'   settings = "yes"
#' )
#' print(fit)
#' summary(fit, settings = "none")
#'
#' @export
mcnemar_vs_control <- function(data,
                               id,
                               condition,
                               outcome,
                               control = "Control",
                               treatments = NULL,
                               mcnemar_method = c("asymptotic", "cc", "midp"),
                               p_adjust = c("holm", "BH", "hochberg"),
                               ci = 0.95,
                               alternative_g = c("two.sided", "greater", "less"),
                               midp_nd0 = c("one", "as_is"),
                               settings_style = c("full", "short"),
                               settings = c("none", "yes")) {

  if (!requireNamespace("contingencytables", quietly = TRUE)) {
    stop("Package 'contingencytables' is required.")
  }
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    stop("Package 'effectsize' is required.")
  }

  mcnemar_method <- match.arg(mcnemar_method)
  p_adjust <- match.arg(p_adjust)
  alternative_g <- match.arg(alternative_g)
  midp_nd0 <- match.arg(midp_nd0)
  settings_style <- match.arg(settings_style)
  settings <- match.arg(settings)

  method_label <- mcnemar_method
  p_adjust_label <- p_adjust
  ci_label <- ci

  settings_label <- if (settings_style == "short") {
    sprintf("%s | %s | CI=%.2f", method_label, p_adjust_label, ci_label)
  } else {
    sprintf("%s | %s | CI=%.2f | g=%s | midp_nd0=%s",
            method_label, p_adjust_label, ci_label, alternative_g, midp_nd0)
  }

  # Capture column names WITHOUT evaluation
  .colname <- function(arg) {
    s <- deparse(substitute(arg))
    gsub('^"|"$', "", s)
  }
  id <- .colname(id)
  condition <- .colname(condition)
  outcome <- .colname(outcome)

  needed <- c(id, condition, outcome)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing column(s): ", paste(missing_cols, collapse = ", "))
  }

  df <- data[, needed]
  names(df) <- c("..id", "..condition", "..outcome")

  # Coerce outcome to 0/1
  if (is.logical(df$..outcome)) df$..outcome <- as.integer(df$..outcome)
  if (is.factor(df$..outcome)) df$..outcome <- as.character(df$..outcome)
  if (is.character(df$..outcome)) df$..outcome <- suppressWarnings(as.integer(df$..outcome))
  if (!all(df$..outcome %in% c(0L, 1L))) {
    stop("Outcome must be binary coded as 0/1 (or logical).")
  }

  all_conds <- unique(df$..condition)
  if (!control %in% all_conds) stop("Control condition '", control, "' not found in data.")

  if (is.null(treatments)) {
    treatments <- setdiff(all_conds, control)
  } else {
    treatments <- as.character(treatments)
    absent <- setdiff(treatments, all_conds)
    if (length(absent) > 0) stop("Treatment(s) not found: ", paste(absent, collapse = ", "))
  }
  if (length(treatments) < 2) stop("Provide at least 2 treatments.")

  wide <- stats::reshape(df, idvar = "..id", timevar = "..condition", direction = "wide")

  # Helper: works for lists AND named atomic vectors
  .get_first <- function(x, keys) {
    for (k in keys) {
      val <- tryCatch(x[[k]], error = function(e) NULL)
      if (!is.null(val)) return(val)
    }
    NA_real_
  }

  rows <- lapply(treatments, function(trt) {

    ctrl_col <- paste0("..outcome.", control)
    trt_col  <- paste0("..outcome.", trt)

    if (!ctrl_col %in% names(wide)) stop("Missing control column after reshape: ", ctrl_col)
    if (!trt_col %in% names(wide)) stop("Missing treatment column after reshape: ", trt_col)

    x <- wide[[ctrl_col]]
    y <- wide[[trt_col]]
    ok <- stats::complete.cases(x, y)
    x <- x[ok]; y <- y[ok]

    tab <- table(factor(x, levels = c(0, 1)), factor(y, levels = c(0, 1)))
    dimnames(tab) <- list(Control = c("0", "1"), Treatment = c("0", "1"))

    n01 <- tab[1, 2]
    n10 <- tab[2, 1]
    nd  <- n01 + n10

    # Edge case: no discordant pairs
    if (nd == 0) {
      P <- 1
      Z <- if (mcnemar_method == "midp") NA_real_ else 0

      return(data.frame(
        treatment = trt,
        method_label = method_label,
        p_adjust_label = p_adjust_label,
        ci_label = ci_label,
        settings = settings_label,
        Z = Z,
        P = P,
        Cohens_g = 0,
        CI_low = 0,
        CI_high = 0,
        nd = nd,
        stringsAsFactors = FALSE
      ))
    }

    test_obj <- switch(
      mcnemar_method,
      asymptotic = contingencytables::McNemar_asymptotic_test_paired_2x2(tab),
      cc         = contingencytables::McNemar_asymptotic_test_CC_paired_2x2(tab),
      midp       = contingencytables::McNemar_midP_test_paired_2x2(tab)
    )

    P <- if (mcnemar_method == "midp") {
      p_mid <- .get_first(test_obj, c("midP"))
      if (midp_nd0 == "one" && nd == 0) 1 else p_mid
    } else {
      .get_first(test_obj, c("Pvalue", "P"))
    }

    Z <- if (mcnemar_method == "midp") NA_real_ else .get_first(test_obj, c("Z"))

    gtab <- effectsize::cohens_g(tab, ci = ci, alternative = alternative_g)

    data.frame(
      treatment = trt,
      method_label = method_label,
      p_adjust_label = p_adjust_label,
      ci_label = ci_label,
      settings = settings_label,
      Z = Z,
      P = P,
      Cohens_g = gtab$Cohens_g[1],
      CI_low   = gtab$CI_low[1],
      CI_high  = gtab$CI_high[1],
      nd = nd,
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, rows)
  res$P_adj <- stats::p.adjust(res$P, method = p_adjust)

  out <- list(
    results = res,
    control = control,
    treatments = treatments,
    mcnemar_method = mcnemar_method,
    p_adjust = p_adjust,
    ci = ci,
    alternative_g = alternative_g,
    midp_nd0 = midp_nd0,
    settings_style = settings_style,
    settings_default = settings
  )
  class(out) <- "mcnemar_vs_control"
  out
}

#' @rdname mcnemar_vs_control
#' @param object An object returned by [mcnemar_vs_control()].
#' @param digits Number of digits to round numeric columns.
#' @param settings If `NULL`, uses the stored default in the object (or "none");
#'   if "none", shows separate label columns; if "yes", shows combined `settings`.
#' @export
summary.mcnemar_vs_control <- function(object, digits = 4, settings = NULL, ...) {
  x <- object$results

  if (is.null(settings)) {
    settings <- if (!is.null(object$settings_default)) object$settings_default else "none"
  }

  settings <- tolower(settings)
  if (!settings %in% c("none", "yes")) {
    stop("settings must be 'none' or 'yes' (case-insensitive).")
  }

  fmt <- function(v) ifelse(is.na(v), NA, round(v, digits))

  if (settings == "none") {
    out <- x[, c("treatment", "method_label", "p_adjust_label", "ci_label",
                 "Z", "P", "P_adj", "Cohens_g", "CI_low", "CI_high", "nd")]
  } else {
    out <- x[, c("treatment", "settings",
                 "Z", "P", "P_adj", "Cohens_g", "CI_low", "CI_high", "nd")]
  }

  out$Z        <- fmt(out$Z)
  out$P        <- fmt(out$P)
  out$P_adj    <- fmt(out$P_adj)
  out$Cohens_g <- fmt(out$Cohens_g)
  out$CI_low   <- fmt(out$CI_low)
  out$CI_high  <- fmt(out$CI_high)

  rownames(out) <- NULL
  out
}

#' Print method for `mcnemar_vs_control` objects
#'
#' Prints `summary(x)` using the stored default display mode in the object.
#'
#' @param x An object of class `mcnemar_vs_control`.
#' @param digits Number of digits to round numeric columns.
#' @param settings Optional override for display: "none" or "yes".
#' @param ... Passed to [summary.mcnemar_vs_control()].
#' @export
print.mcnemar_vs_control <- function(x, digits = 4, settings = NULL, ...) {
  tbl <- summary(x, digits = digits, settings = settings, ...)
  print(tbl, row.names = FALSE)
  invisible(x)
}
