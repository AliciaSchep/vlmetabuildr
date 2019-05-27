create_transform_functions <- function(schema) {
  transform_options <- purrr::map_chr(schema$definitions$Transform$anyOf, get_name_from_ref)
  c(
    purrr::map_chr(transform_options, create_transform, schema = schema),
    purrr::map_chr(transform_options, create_object, schema = schema)
  )
}



create_transform <- function(trans, schema) {
  
  # Get all props...
  transform_props <- props(schema, list("$ref" = glue("#/definitions/{trans}")))
  transform_args <- paste(names(transform_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(transform_args)), collapse = ", ")
  
  param_docs <- get_param_docs(transform_props)
  
  short_trans <- tolower(stringr::str_remove(trans,"Transform"))
  create_pass_function(
    function_suffix = short_trans, 
    recipient_function = ".add_transform",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.trans = '{short_trans}'))"),
    doc_description = glue("#' Add {trans} to a vega-lite spec."),
    param_docs = param_docs
  )
  
}



