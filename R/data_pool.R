#' @export
add_data_pool <- function(.data, name = NULL, filename = NULL) {
  qdata <- rlang::enquo(.data)
  
  # TODO: limit the characters allowed in these names [A-Za-z\_\-]
  if (is.null(name)) name <- rlang::quo_text(qdata)
  if (is.null(filename)) filename <- paste0(name, ".feather")
  
  feather::write_feather(.data, path = filename)
  
  curr_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  if (!filename %in% curr_metadata) {
    rmarkdown::output_metadata$set(rsc_output_files = c(curr_metadata, filename))
  }
  
  finfo <- as.list(fs::file_info(filename))
  output_finfo <- list(
    filename = filename,
    size = finfo$size,
    format = "feather",
    nrow = nrow(.data),
    ncol = ncol(.data),
    colname = colnames(.data),
    coltype = as.character(lapply(.data, typeof)),
    colclass = as.character(lapply(.data, class))
  )
  
  output_json_path <- "connectapi_data_pool.json"
  
  if (file.exists(output_json_path)) {
    existing_data_pool <- jsonlite::fromJSON(output_json_path)
  } else {
    existing_data_pool <- list()
  }
  
  new_data_pool <- rlang::set_names(list(output_finfo), name)
  final_data_pool <- purrr::update_list(existing_data_pool, !!!new_data_pool)
  
  jsonlite::write_json(final_data_pool, output_json_path, auto_unbox = TRUE)
  
  invisible(jsonlite::fromJSON(output_json_path))
}
