#' @export
create_binding <- function(type, schema) {
  
  bind_props <- props(schema, list("$ref" = glue("#/definitions/{type}")))
  bind_props <- bind_props[which(names(bind_props) != "type")]
  bind_args <- paste(names(bind_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'selection_name', 'projection_name = NULL', unique(bind_args)), collapse = ", ")
  
  param_docs <- get_param_docs(bind_props)
  
  template <- system.file("templates/template_binding.R", package = 'vlmetabuildr')
  glargs <- list(arg_list = arg_list,
                 param_docs = param_docs,
                 type = type)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}