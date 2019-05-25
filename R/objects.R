#' @export
#' @name create_object
create_object <- function(obj, schema) {
  
  # Get all props...
  obj_props <- props(schema, list("$ref" = glue("#/definitions/{obj}")))
  obj_args <- paste(names(obj_props), "NULL", sep = " = ")
  arg_list <- paste(unique(obj_args), collapse = ", ")
  
  param_docs <- get_param_docs(obj_props)
  
  template <- system.file("templates/template_object.R", package = 'vlmetabuildr')
  glargs <- list(obj = obj, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}