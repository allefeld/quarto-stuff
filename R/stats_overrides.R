#' Method overrides to display statistical results nicely.
#'
#' @version 0.2.0
#' @date 2024-10-30
#' @author Carsten Allefeld


# nolint start: object_usage_linter, object_name_linter.

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
  stringr::str_replace_all(str_vec, "NA", "")
}

knit_print.intervals.lme <- function(al, options) {
  tbl <- al$fixed |>
    tibble::as_tibble(rownames = "regressor") |>
    transmute(
      regressor = termcross(regressor),
      coefficient = pdl(sprintf("%#.3g", `est.`)),
      CI = stringr::str_replace_all(
        sprintf("[%#.3g, %#.3g]", lower, upper),
        "-",
        "−"
      )
    ) |>
    rename_with(
      ~ sprintf("%g-CI", attr(al, "level")),
      CI
    ) |>
    knitr::kable() |>
    knitr::knit_print()
}

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

knit_print.intervals.lme <- function(al, options) {
  al$fixed |>
    tibble::as_tibble(rownames = "regressor") |>
    transmute(
      regressor = regressor,
      coefficient = pdl(sprintf("%#.3g", `est.`)),
      CI = stringr::str_replace_all(
        sprintf("[%#.3g, %#.3g]", lower, upper),
        "-",
        "−"
      )
    ) |>
    rename_with(
      ~ sprintf("%g-CI", attr(al, "level")),
      CI
    ) |>
    knitr::kable() |>
    knitr::knit_print()
}

knit_print.intervals.gls <- function(al, options) {
  al$coef |>
    tibble::as_tibble(rownames = "regressor") |>
    transmute(
      regressor = regressor,
      coefficient = pdl(sprintf("%#.3g", `est.`)),
      CI = stringr::str_replace_all(
        sprintf("[%#.3g, %#.3g]", lower, upper),
        "-",
        "−"
      )
    ) |>
    rename_with(
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
    "fct" = "nom.",
    "ord" = "ord.",
    "dbl" = "scale",
    "chr" = "**text**"
  )
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
    # gt::gt(rownames_to_stub = TRUE) |>
    knitr::kable(row.names = TRUE, align = "r") |>
    knitr::knit_print()
}

# nolint end
