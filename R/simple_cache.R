#' Simple caching of the result of a code block.
#'
#' @version 0.2.0
#' @date 2024-11-13
#' @author Carsten Allefeld


# cache the result of a code block to a file
simple_cache <- function(filename, code_block) {
  if (file.exists(filename)) {
    result <- readRDS(filename)
  } else {
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    result <- code_block      # lazy evaluation!
    saveRDS(result, file = filename)
  }
  return(result)
}
