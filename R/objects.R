
create_object <- function(obj, schema) {
  
  # Get all props...
  obj_props <- props(schema, list("$ref" = glue("#/definitions/{obj}")))
  obj_names <- stringr::str_replace(names(obj_props), "^repeat$","`repeat`")
  obj_args <- paste(obj_names, "NULL", sep = " = ")
  arg_list <- paste(unique(obj_args), collapse = ", ")
  
  param_docs <- get_param_docs(schema, glue("#/definitions/{obj}"))
  
  template <- system.file("templates/template_object.R", package = 'vlmetabuildr')
  glargs <- list(obj = obj, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}


create_additional_objects <- function(schema) {
  
  objs <- c("BinParams",
            "Axis",
            "Scale",
            "Legend",
            "BindCheckbox",
            "BindRange",
            "BindRadioSelect")
  
  purrr::map_chr(objs, create_object, schema = schema)
  
}


