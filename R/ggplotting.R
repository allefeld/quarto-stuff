#' Functions building on `ggplot2`.
#'
#' @version 0.1.0
#' @date 2024-10-29
#' @author Carsten Allefeld


# scatter plot matrix with ggplot2
splom <- function(
    data,
    vars = names(data),
    breaks = ggplot2::waiver(),
    minor_breaks = ggplot2::waiver(),
    limits = NULL,
    color = "black",
    size = 1) {
  nvars <- length(vars)
  plots <- list()
  for (row in 1 : nvars) {
    for (col in 1 : nvars) {
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(
          x = ggplot2::.data[[vars[col]]],
          y = ggplot2::.data[[vars[row]]]
        ) +
        ggplot2::scale_x_continuous(
          breaks = breaks,
          minor_breaks = minor_breaks,
          limits = limits
        ) +
        ggplot2::scale_y_continuous(
          breaks = breaks,
          minor_breaks = minor_breaks,
          limits = limits
        ) +
        ggplot2::geom_point(color = color, size = size) +
        ggplot2::theme(aspect.ratio = 1)
      if (row < nvars) {
        p <- p + ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank()
        )
      }
      if (col > 1) {
        p <- p + ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank()
        )
      }
      plots <- c(plots, list(p))
    }
  }
  patchwork::wrap_plots(plots, ncol = nvars)
}
