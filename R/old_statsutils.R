# Similar to `knitr_stats.R`, but these functions need to be applied
# explicitly. Material for expansion of `knitr_stats.R`.


# nolint start: object_usage_linter, object_name_linter.

# `anova` for comparison of models,
# but such that χ² tests are all against the first model
anova_to_first <- function(..., model_names = NULL) {
  if (is.null(model_names)) {
    model_names <- sapply(substitute(c(...)), deparse)[-1]
  }
  models <- list(...)
  mc <- anova(models[[1]], models[[2]], model.names = model_names[1:2])
  for (i in 3:length(models)) {
    mca <- anova(models[[1]], models[[i]], model.names = model_names[c(1, i)])
    mc <- rbind(mc, mca[2, ])
  }
  return(mc)
}

# formatting common to `anova_to_table` and `aov_to_table`
anova_aov_common <- function(tbl) {
  tbl |>
    dplyr::transmute(
      term = termcross(term),
      `*F*-value` = pdl(sprintf("%.2f", `F value`)),
      `df~num~` = pdl(sprintf("%.2f", NumDF)),
      `df~den~` = pdl(sprintf("%.2f", DenDF)),
      `*p*-value` = pval(pdl(sprintf("%.3f", `Pr(>F)`)))
    )
}

# format the output of `anova` (for a single model)
# as a data frame to be used with `df-print: kable`
anova_to_table <- function(anova) {
  anova |>
    tibble::as_tibble(rownames = "term") |>
    anova_aov_common()
}

# format the output of `aov`
# as a data frame to be used with `df-print: kable`
aov_to_table <- function(aov) {
  aov <- broom::tidy(aov)
  tibble::tibble(
    term = aov$term[seq(2, nrow(aov), 2)],
    `F value` = aov$statistic[seq(2, nrow(aov), 2)],
    `NumDF` = aov$df[seq(2, nrow(aov), 2)],
    `DenDF` = aov$df[seq(3, nrow(aov), 2)],
    `Pr(>F)` = aov$p.value[seq(2, nrow(aov), 2)]
  ) |>
    anova_aov_common()
}


# format the output of `anova` (for model comparison)
# as a data frame to be used with `df-print: kable`
anova_comparison_to_table <- function(anova_comparison, minus_two = FALSE) {
  tbl <- anova_comparison |>
    tibble::as_tibble(rownames = "model") |>
    dplyr::transmute(
      model = model,
      `log(*L*)` = pdl(sprintf("%.2f", logLik)),
      `−2 log(*L*)` = pdl(sprintf("%.2f", -2 * logLik)),
      `*n*~par~` = pdl(sprintf("%d", npar)),
      AIC = pdl(sprintf("%.2f", AIC)),
      BIC = pdl(sprintf("%.2f", BIC)),
      `χ²` = pdl(sprintf("%.2f", Chisq)),
      df = pdl(sprintf("%d", Df)),
      `*p*-value` = pval(pdl(sprintf("%.3f", `Pr(>Chisq)`)))
    )
  if (minus_two) {
    tbl <- tbl |>
      dplyr::select(-`log(*L*)`)
  } else {
    tbl <- tbl |>
      dplyr::select(-`−2 log(*L*)`)
  }
  tbl[1, 6:8] <- ""
  return(tbl)
}

# format the output of `summary` for a linear model
# as a data frame to be used with `df-print: kable`
# showing coefficients
summary_to_table <- function(model_summary) {
  model_summary$coefficients |>
    tibble::as_tibble(rownames = "term") |>
    dplyr::transmute(
      term = termcross(term),
      coefficient = pdl(sprintf("%.4g", Estimate)),
      SE = pdl(sprintf("%.4g", `Std. Error`)),
      `*t*-value` = pdl(sprintf("%.3f", `t value`)),
      df = pdl(sprintf("%.2f", df)),
      `*p*-value` = pval(pdl(sprintf("%.3f", `Pr(>|t|)`)))
    )
}

# format the output of `summary` for a linear model
# as a data frame to be used with `df-print: kable`
# showing standard deviations and correlations of a mixed effects model
summary_to_table_sdcor <- function(model_summary, group = NULL) {
  tbl <- tibble::as_tibble(model_summary$varcor)
  if (is.null(group)) {
    return(unique(stringr::str_replace_all(tbl$grp, "\\.\\d+$", "")))
  }
  tbl <- tbl |>
    dplyr::filter(startsWith(grp, group)) |>
    dplyr::select(-vcov, -grp) |>
    dplyr::mutate(
      temp_var2 = dplyr::if_else(is.na(var2), var1, var2),
      var1 = dplyr::if_else(is.na(var2), NA, var1),
      var2 = temp_var2
    ) |>
    dplyr::select(-temp_var2) |>
    tidyr::pivot_wider(names_from = var1, values_from = sdcor)
  if (length(names(tbl)) < 3) {
    tbl <- tbl |>
      dplyr::transmute(
        term = termcross(var2),
        SD = pdl(sprintf("%.4g", `NA`))
      )
  } else {
    term_cols <- names(tbl)[3:length(names(tbl))]
    tbl <- tbl |>
      dplyr::transmute(
        term = termcross(var2),
        SD = pdl(sprintf("%.4g", `NA`)),
        dplyr::across(
          dplyr::all_of(term_cols),
          ~ pdl(naempty(sprintf("%.3f", .)))
        )
      )
    names(tbl)[3:length(names(tbl))] <- termcross(term_cols)
  }
  return(tbl)
}

# format the output of `t.test` (one sample)
# as a data frame to be used with `df-print: kable`
t.test_to_table <- function(tt) {
  if (tt$method == "One Sample t-test") {
    tbl <- tibble::tibble(
      variable = tt$data.name,
      mean = pdl(sprintf("%.4g", tt$estimate)),
      SE = pdl(sprintf("%.4g", tt$stderr)),
      `CI~l~` = pdl(sprintf("%.4g", tt$conf.int[1])),
      `CI~u~` = pdl(sprintf("%.4g", tt$conf.int[2])),
      `*t*-value` = pdl(sprintf("%.3f", tt$statistic)),
      df = pdl(sprintf("%d", tt$parameter)),
      `*p*-value` = pval(pdl(sprintf("%.3f", tt$p.value)))
    )
    return(tbl)
  }
}

# format the output of `confint` for a linear model
# as a data frame to be used with `df-print: kable`
confint_to_table <- function(ci) {
  tbl <- tibble::as_tibble(ci, rownames = "regressor")
  level <- diff(as.numeric(gsub(" %", "", colnames(tbl)[2:3]))) / 100
  colnames(tbl) <- c("regressor", "lower", "upper")
  tbl |>
    transmute(
      regressor = termcross(regressor),
      CI = stringr::str_replace_all(
        sprintf("[%#.3g, %#.3g]", lower, upper),
        "-",
        "−"
      )
    ) |>
    rename_with(
      ~ sprintf("%g-CI", level),
      CI
    )
}

pretty_levels <- function(df) {
  # Apply modifications only to factor columns
  df |>
    mutate(
      across(
        where(is.factor), ~ {
          f <- .
          levels(f) <- paste0("=", levels(f))
          f
        }
      )
    )
}

# nolint end
