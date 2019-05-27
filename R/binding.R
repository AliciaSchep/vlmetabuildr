
create_binding <- function(schema, name, ref) {
  
  bind_props <- props(schema, list("$ref" = glue("#/definitions/{ref}")))
  bind_props <- bind_props[which(names(bind_props) != "input")]
  bind_args <- paste(names(bind_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'selection_name', 'projection_name = NULL', unique(bind_args)), collapse = ", ")
  
  param_docs <- paste0(
    "#' @param selection_name Name of selection to add binding to\n",
    "#' @param projection_name Name of projection (field or encoding) within selection\n",
    get_param_docs(bind_props)
  )
  
  create_pass_function(
    function_suffix = glue("bind_{name}_input"), 
    recipient_function = ".add_binding",
    arg_list = arg_list,
    modify_args = glue("args_out$input <- '{name}'"),
    doc_description = glue("#' Add binding to a {name} input to a vega-lite spec."),
    param_docs = param_docs)  
  
}


create_radio_binding <- function(schema) {
  
  create_binding(schema, "radio", "BindRadioSelect")
  
}

create_select_binding <- function(schema) {
  
  create_binding(schema, "select", "BindRadioSelect")
  
}

create_checkbox_binding <- function(schema) {
  
  create_binding(schema, "checkbox", "BindCheckbox")
  
}

create_range_binding <- function(schema) {
  
  create_binding(schema, "range", "BindRange")
  
}

#' @export
create_binding_functions <- function(schema){
  c(
    create_radio_binding(schema),
    create_select_binding(schema),
    create_checkbox_binding(schema),
    create_range_binding(schema)
  )
}