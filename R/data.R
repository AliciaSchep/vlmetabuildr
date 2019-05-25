#' create_data
#'
#' @param schema imported json schema
#'
#' @return
#' @export
#' @name create_data
#'
#' @examples
create_data_generic <- function(schema){

  data_props <- props(schema, list("$ref" = "#/definitions/Data"))
  data_args <- paste(names(data_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(data_args)), collapse = ", ")

  param_docs <- get_param_docs(data_props)

  template <- system.file("templates/template_data.R", package = 'vlmetabuildr')
  glargs <- list(arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)

}
