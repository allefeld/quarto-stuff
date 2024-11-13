#' Method overrides to display statistical results nicely.
#'
#' @version 0.4.0
#' @date 2024-11-13
#' @author Carsten Allefeld


# nolint start: object_usage_linter, object_name_linter, commented_code_linter.

# TODO: significance stars? use * ⁑ ⁂ or maybe ^***^

# modifies factor level names for prettier regressor names
pretty_levels <- function(data) {
  data |>
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

# pad a vector of strings containing decimal numbers on the left
# such that the decimal points are aligned
pdl <- function(str_vec) {
  # proper minus signs
  str_vec <- stringr::str_replace_all(str_vec, "-", "−")
  # determine necessary padding
  pos <- stringr::str_locate(str_vec, "[−+]?[0-9]+")[, 2]
  pad <- max(pos, na.rm = TRUE) - pos
  pad[is.na(pad)] <- 0
  # pad with figure space
  paste0(strrep("\u2007", pad), str_vec)
}

# format small p-value APA style
pval <- function(str_vec) {
  stringr::str_replace_all(str_vec, "0.000", "< 0.001")
}

# format interactions with ×
termcross <- function(terms) {
  stringr::str_replace_all(terms, ":", " × ")
}

# replace "NA" by ""
naempty <- function(str_vec) {
  stringr::str_replace_all(
    sprintf("%s", str_vec),
    "NA",
    ""
  )
}


# nlme::anova
knit_print.anova.lme <- function(al, options) {
  # extract denominator degrees of freedom from attribute "label"
  denDF <- as.numeric(
    stringr::str_extract(
      attr(al, "label"),
      "\\s*[\\d\\.]+\\s*$"
    )
  )
  al |>
    tibble::as_tibble(rownames = "term") |>
    dplyr::transmute(
      term = termcross(term),
      `*F*-value` = pdl(sprintf("%.2f", `F-value`)),
      `df~num~` = pdl(sprintf("%d", numDF)),
      `df~den~` = pdl(sprintf("%d", denDF)),
      `*p*-value` = pval(pdl(sprintf("%.3f", `p-value`)))
    ) |>
    knitr::kable() |>
    knitr::knit_print()
}

# anova
knit_print.anova <- function(al, options) {
  out <- "Define me in stats_overrides/knit_print.anova!"
  if (all(c("NumDF", "DenDF", "F value", "Pr(>F)") %in% names(al))) {
    # ANOVA table
    out <- al |>
      tibble::as_tibble(rownames = "term") |>
      dplyr::transmute(
        term = termcross(term),
        `*F*-value` = pdl(sprintf("%.2f", `F value`)),
        `df~num~` = pdl(sprintf("%.1f", NumDF)),
        `df~den~` = pdl(sprintf("%.1f", DenDF)),
        `*p*-value` = pval(pdl(sprintf("%.3f", `Pr(>F)`)))
      ) |>
      knitr::kable(caption = attr(al, "heading"))
  }
  if (all(c("npar", "AIC", "BIC", "logLik", "deviance", "Chisq", "Df",
            "Pr(>Chisq)") %in% names(al))) {
    # Model comparison
    out <- al |>
      tibble::as_tibble(rownames = "model") |>
      dplyr::transmute(
        model = model,
        `log(*L*)` = pdl(sprintf("%.2f", logLik)),
        `*n*~par~` = pdl(sprintf("%d", npar)),
        AIC = pdl(sprintf("%.2f", AIC)),
        BIC = pdl(sprintf("%.2f", BIC)),
        `χ²` = naempty(pdl(sprintf("%.2f", Chisq))),
        df = naempty(pdl(sprintf("%d", Df))),
        `*p*-value` = naempty(pval(pdl(sprintf("%.3f", `Pr(>Chisq)`))))
      ) |>
      knitr::kable()
    # use attr(al, "heading")? contains model formulas.
  }
  knitr::knit_print(out)
}

knit_print.intervals.lme <- function(al, options) {
  tbl <- tibble::as_tibble(al$fixed, rownames = "row")
  try({
    tbl <- dplyr::bind_rows(
      tbl,
      tibble::as_tibble(al$corStruct, rownames = "row") |>
        dplyr::mutate(row = paste0("(", row, ")"))
    )
  }, silent = TRUE)
  try({
    tbl <- dplyr::bind_rows(
      tbl,
      tibble::as_tibble_row(al$sigma) |>
        dplyr::mutate(row = "(residual sd)")
    )
  }, silent = TRUE)
  try(suppressWarnings({
    tbl <- dplyr::bind_rows(
      tbl,
      dplyr::bind_rows(al$reStruct, .id = "grouping") |>
        tibble::as_tibble(rownames = "row")
    )
    tbl$grouping <- naempty(tbl$grouping)
  }), silent = TRUE)
  tbl |>
    dplyr::mutate(
      ` ` = row,
      coefficient = pdl(sprintf("%#.3g", `est.`)),
      CI = stringr::str_replace_all(
        sprintf("[%#.3g, %#.3g]", lower, upper),
        "-",
        "−"
      ),
    ) |>
    dplyr::select(-row, -lower, -`est.`, -`upper`) |>
    dplyr::rename_with(
      ~ sprintf("%g-CI", attr(al, "level")),
      CI
    ) |>
    knitr::kable() |>
    knitr::knit_print()
}

knit_print.intervals.gls <- function(al, options) {
  dplyr::bind_rows(
    tibble::as_tibble(al$coef, rownames = "row"),
    tibble::as_tibble(al$corStruct, rownames = "row") |>
      dplyr::mutate(row = paste0("(", row, ")")),
    tibble::as_tibble_row(al$sigma) |>
      dplyr::mutate(row = "(residual sd)")
  ) |>
    dplyr::transmute(
      ` ` = row,
      coefficient = pdl(sprintf("%#.3g", `est.`)),
      CI = stringr::str_replace_all(
        sprintf("[%#.3g, %#.3g]", lower, upper),
        "-",
        "−"
      )
    ) |>
    dplyr::rename_with(
      ~ sprintf("%g-CI", attr(al, "level")),
      CI
    ) |>
    knitr::kable() |>
    knitr::knit_print()
}

# replacement for summary(data.frame)
summary.data.frame <- function(data) {
  names <- base::names(data)
  types <- base::sapply(data, vctrs::vec_ptype_abbr)
  scales <- dplyr::recode(
    types,
    "fct"  = "nom.",
    "ord"  = "ord.",
    "dbl"  = "scale",
    "int"  = "scale",
    "drtn" = "scale",
    "chr"  = "**text**"
  )   # supplement by data type?
  tibble::tibble(
    name = names,
    scale = scales,
    n = base::format(base::colSums(!is.na(data))),
    min = base::sapply(names, function(name) {
      if (! types[name] %in% c("fct")) {
        base::format(base::min(data[[name]], na.rm = TRUE))
      } else {
        "–"
      }
    }),
    median = base::sapply(names, function(name) {
      if (! types[name] %in% c("fct")) {
        base::format(stats::quantile(data[[name]], 0.5, type = 1,
                                     na.rm = TRUE))
      } else {
        "–"
      }
    }),
    max = base::sapply(names, function(name) {
      if (! types[name] %in% c("fct")) {
        base::format(base::max(data[[name]], na.rm = TRUE))
      } else {
        "–"
      }
    }),
    mean = base::sapply(names, function(name) {
      if (! types[name] %in% c("fct", "ord", "chr")) {
        base::format(base::mean(data[[name]], na.rm = TRUE))
      } else {
        "–"
      }
    }),
    sd = base::sapply(names, function(name) {
      if (! types[name] %in% c("fct", "ord", "chr")) {
        base::format(stats::sd(data[[name]], na.rm = TRUE))
      } else {
        "–"
      }
    })
  )
}

knit_print.table <- function(al, options) {
  al |>
    base::as.data.frame.matrix() |>
    knitr::kable(row.names = TRUE, align = "r") |>
    knitr::knit_print()
}

knit_print.summary.merMod <- function(al, options) {
  if (inherits(al, "summary.lme")) {
    stop("The object 'al' is of class 'summary.lme'.")
  }
  # table of coefficients
  tbl_coef <- al$coefficients |>
    tibble::as_tibble(rownames = "regressor") |>
    dplyr::transmute(
      regressor = termcross(regressor),
      coefficient = pdl(sprintf("%.4g", Estimate)),
      SE = pdl(sprintf("%.4g", `Std. Error`)),
      `*t*-value` = pdl(sprintf("%.3f", `t value`)),
      df = pdl(sprintf("%.1f", df)),
      `*p*-value` = pval(pdl(sprintf("%.3f", `Pr(>|t|)`)))
    )
  # table of standard deviations and correlations
  tbl_sdcor <- al$varcor |>
    tibble::as_tibble() |>
    # dplyr::filter(startsWith(grp, group)) |>
    dplyr::select(-vcov, -grp) |>
    dplyr::mutate(
      temp_var2 = dplyr::if_else(is.na(var2), var1, var2),
      var1 = dplyr::if_else(is.na(var2), NA, var1),
      var2 = temp_var2
    ) |>
    dplyr::select(-temp_var2) |>
    tidyr::pivot_wider(names_from = var1, values_from = sdcor)
  if (length(names(tbl_sdcor)) < 3) {
    tbl_sdcor <- tbl_sdcor |>
      dplyr::transmute(
        term = termcross(var2),
        sd = pdl(sprintf("%.4g", `NA`))
      )
  } else {
    term_cols <- names(tbl_sdcor)[3:length(names(tbl_sdcor))]
    tbl_sdcor <- tbl_sdcor |>
      dplyr::transmute(
        term = termcross(var2),
        sd = pdl(sprintf("%.4g", `NA`)),
        dplyr::across(
          dplyr::all_of(term_cols),
          ~ pdl(naempty(sprintf("%.3f", .)))
        )
      )
    names(tbl_sdcor)[3:length(names(tbl_sdcor))] <- termcross(term_cols)
  }
  # print
  knitr::knit_print(knitr::kable(tbl_coef))
  knitr::knit_print(knitr::kable(tbl_sdcor))
}

knit_print.lmerModLmerTest <- function(al, options) {
  fs <- deparse(al@call$formula)
  knitr::asis_output(paste0("Model: `", fs, "`"))
}

knit_print.lme <- function(al, options) {
  if (inherits(al, "summary.lme")) {
    knitr::normal_print(al)
  } else {
    fc <- as.character(formula(al))
    rc <- as.character(al$call$random)
    ind <- nchar(fc[2])
    knitr::asis_output(paste(c(
      "```",
      strwrap(
        paste(fc[2], fc[1], fc[3]),
        width = 70,
        exdent = ind + 3
      ),
      strwrap(
        paste("+ (", rc[2], ")", sep = ""),
        width = 70,
        indent = ind + 1,
        exdent = ind + 3
      ),
      "```"
    ), collapse = "\n"))
  }
}

# nolint end
