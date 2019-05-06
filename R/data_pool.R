#' @export
add_data_pool <- function(.data, filename = NULL) {
  qdata <- rlang::enquo(.data)
  
  if (is.null(filename)) {
    filename <- paste0(rlang::quo_text(qdata), ".feather")
  }
  feather::write_feather(.data, path = filename)
  
  curr_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  if (!filename %in% curr_metadata) {
    rmarkdown::output_metadata$set(rsc_output_files = c(curr_metadata, filename))
  }
}
