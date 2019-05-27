create_selection_functions <- function(schema) {
  purrr::map_chr(list("SingleSelection","MultiSelection","IntervalSelection"), 
                 create_selection_type, 
                 schema = schema)
}


create_selection_type <- function(type, schema) {
  
  sel_props <- props(schema, list("$ref" = glue("#/definitions/{type}")))
  sel_props <- sel_props[which(names(sel_props) != "type")]
  sel_args <- paste(names(sel_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'selection_name', unique(sel_args)), collapse = ", ")
  
  param_docs <- paste0(
    "#' @param selection_name Name of selection\n",
    get_param_docs(sel_props)
  )
  
  short_type <- tolower(stringr::str_remove(type,"Selection"))
  create_pass_function(
    function_suffix = glue("add_{short_type}_selection"), 
    recipient_function = ".add_selection",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(type = '{short_type}'))"),
    doc_description = glue("#' Add {type} to a vega-lite spec."),
    param_docs = param_docs)   
  
}




