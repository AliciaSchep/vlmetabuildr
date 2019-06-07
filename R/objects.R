
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


create_object_functions <- function(schema) {
  
  refs <- names(schema$definitions)
  is_object <- purrr::map_lgl(schema$definitions,  ~hasName(.,"type") && .[["type"]] == "object")
  objs <- refs[is_object]
  
}

create_object2 <- function(obj, schema) {
  
  obj_props <- props(schema, list("$ref" = glue("#/definitions/{obj}")))
  obj_args <- paste(names(obj_props), "NULL", sep = " = ")
  arg_list <- paste(unique(obj_args), collapse = ", ")

  
  template <- system.file("templates/template_object2.R", package = 'vlmetabuildr')
  glargs <- list(obj = obj, arg_list = arg_list)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

find_objects <- function(){
  
}


