is_config_option <- function(prop, schema) {
  
  ref_name = get_name_from_ref(prop)
  if (is.null(ref_name)) {
    return(FALSE)
  }
  
  stringr::str_detect(ref_name, "Config$")
  
}

create_config_functions <- function(schema) {
  top_config_props <-  props(schema, list("$ref" = glue("#/definitions/Config")))
  is_config <- purrr::map_lgl(top_config_props, is_config_option)
  config_options <- names(top_config_props)[is_config]
  config_objs <- stringr::str_subset(names(schema$definitions),"Config$")
  c(
    create_config(schema),
    purrr::map_chr(config_options, create_sub_config, schema = schema),
    purrr::map_chr(config_objs, create_object, schema = schema)
  )
}

create_sub_config <- function(prop, schema) {
  
  top_config_props <-  props(schema, list("$ref" = "#/definitions/Config"))
  config <- get_name_from_ref(top_config_props[[prop]])
  
  config_props <- props(schema, list("$ref" = glue("#/definitions/{config}")))
  config_args <- paste(names(config_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(config_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, glue("#/definitions/{config}"))
  
  create_pass_function(
    function_suffix = glue("add_{prop}_config"), 
    recipient_function = ".add_sub_config",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.config = '{prop}'))"),
    doc_description = glue("#' Add {prop} config ({config}) to a vega-lite spec."),
    param_docs = param_docs
  )
  
}

create_config <- function(schema) {
  
  # Get all props...
  config_props <- props(schema, list("$ref" = "#/definitions/Config"))
  config_args <- paste(names(config_props), "NULL", sep = " = ")
  config_args[config_args == "repeat = NULL"] <- "`repeat` = NULL"
  arg_list <- paste(c('spec', unique(config_args)), collapse = ", ")
  
  param_docs <- get_param_docs(schema, "#/definitions/Config")
  
  create_pass_function(
    function_suffix = "add_config", 
    recipient_function = ".add_config",
    arg_list = arg_list,
    doc_description = glue("#' Add top-level config to a vega-lite spec."),
    param_docs = param_docs
  )
  
}