#' @export
add_data_pool <- function(.data, name = NULL, filename = NULL) {
  qdata <- rlang::enquo(.data)
  
  # TODO: limit the characters allowed in these names [A-Za-z\_\-]
  if (is.null(name)) name <- rlang::quo_text(qdata)
  if (is.null(filename)) filename <- paste0(name, ".feather")
  
  feather::write_feather(.data, path = filename)
  
  # sanity check the filename
  stopifnot(!filename %in% c("connectapi_data_pool.json"))
  
  finfo <- as.list(fs::file_info(filename))
  output_finfo <- list(
    filename = filename,
    size = finfo[["size"]],
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
  
  curr_metadata <- rmarkdown::output_metadata$get("rsc_output_files")
  if (!filename %in% curr_metadata) {
    rmarkdown::output_metadata$set(rsc_output_files = c(curr_metadata, filename))
  }
  if (! output_json_path %in% curr_metadata) {
    rmarkdown::output_metadata$set(rsc_output_files = c(curr_metadata, filename, output_json_path))
  }
  
  invisible(jsonlite::fromJSON(output_json_path))
}

#' @export
list_data_pools <- function(connect, ...) {
  # ?search=pool&filter=content_type:document
  # ?search=pool&filter=content_type:document&filter=tag:121
  # take upwards of 6 seconds if we do not go by tag / search
  dp_tags <- get_data_pool_tag(connect = connect)
  
  pool_name <- connect$get_apps(filter = list(content_type = "document"), search = "pool")
  pool_tags <- lapply(
    as.list(dp_tags),
    function(x){
      connect$get_apps(filter = list(content_type = "document", tag = x))
    }
  )
  
  # uniqueness could become a problem here
  # e.g. same app returns twice
  pool_tags_all <- unlist(pool_tags, recursive = FALSE)
  
  # uniqueness problems again
  pool_all <- c(pool_name, pool_tags_all)
  
  # pretty output
  pretty_prep <- purrr::map_df(
    pool_all, 
    function(x){
        new_x <- x[names(x) %in% c("guid", "name", "title", "bundle_id")]
        new_x[["dummy"]] <- TRUE
        
        dp_json <- get_data_pool_json(connect = connect, guid = new_x[["guid"]])
        
        # parse the data_pools
        dp_prep <- parse_data_pool_json(dp_json)
        if (ncol(dp_prep) == 0) {
          dp_prep <- tibble::tibble(dummy = TRUE)[0,]
        } else {
          dp_prep[["dummy"]] <- TRUE
        }
        
        # join them
        final <- merge(tibble::as_tibble(new_x), dp_prep, by = "dummy")
        final[["dummy"]] <- NULL
        
        tibble::as_tibble(final)
      }
    )
  
  pretty_prep
}

#' @export
data_pool <- function(connect, guid, filename) {
  # some way to use vanity paths...?
  # validate that this data_pool exists before firing!
  download_loc <- fs::file_temp(filename)
  connect$GET(paste0(guid, "/", filename), httr::write_disk(download_loc), "raw", prefix = "content/")
  feather::read_feather(download_loc)
}

# can list_data_pools return a list of S6 objects...?
# is it possible for the print method to look like a tbl?
# then I can just grab which one I want...

#' @export
reactive_data_pool <- function(connect, guid, filename, interval_millis = 1000, session = shiny::getDefaultReactiveDomain()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install the shiny package to make use of connectapi::reactive_data_pool()")
  }
  shiny::reactivePoll(
    intervalMillis = interval_millis,
    session = session,
    checkFunc = function() {
      headers <- connect$HEAD(paste0(guid, "/", filename), prefix = "content/")
      headers[["last-modified"]]
    },
    valueFunc = function() {
      data_pool(connect = connect, guid = guid, filename = filename)
    }
  )
}

parse_data_pool_json <- function(json) {
  purrr::map_df(
          json,
          function(x){
            tibble::as_tibble(clean_up_ragged_character(x))
          }
        )
}

get_data_pool_json <- function(connect, guid) {
  tryCatch({
    suppressMessages(
      connect$GET(
        path = paste0(guid, "/connectapi_data_pool.json"), 
        prefix = "content/"
      )
    )
  },
  error = function(e){list()}
  )
}

clean_up_ragged_character <- function(x){
  purrr::map_if(x, function(x){length(x) > 1}, function(y){list(as.character(y))})
}

get_data_pool_tag <- function(connect, tag_name = "data_pool") {
  tryCatch(connect$get_tag_id(tag_name), error = function(e){return(NULL)})
}

