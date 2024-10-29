#' Simple caching of the result of a code block.
#'
#' @version 0.1.0
#' @date 2024-10-29
#' @author Carsten Allefeld


# cache the result of a code block to a file
simple_cache <- function(filename, code_block) {
  if (file.exists(filename)) {
    #: message("Loading the result from the cache")
    result <- readRDS(filename)
  } else {
    #: message("Evaluating the code block and saving the result")
    result <- code_block
    saveRDS(result, file = filename)
  }
  return(result)
}
