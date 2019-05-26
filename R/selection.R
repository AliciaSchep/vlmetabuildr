
#' @export
create_selection_type <- function(type, schema) {
  
  sel_props <- props(schema, list("$ref" = glue("#/definitions/{type}")))
  sel_props <- sel_props[which(names(sel_props) != "type")]
  sel_args <- paste(names(sel_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'name', unique(sel_args)), collapse = ", ")
  
  param_docs <- get_param_docs(sel_props)
  
  template <- system.file("templates/template_selection_type.R", package = 'vlmetabuildr')
  glargs <- list(arg_list = arg_list,
                 param_docs = param_docs,
                 type = type)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}




