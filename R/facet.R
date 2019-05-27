create_row_facet <- function(schema) {
  
  # Get all props...
  facet_props <- props(schema, list("$ref" = glue("#/definitions/FacetFieldDef")))
  facet_args <- paste(names(facet_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(facet_args)), collapse = ", ")
  
  param_docs <- get_param_docs(facet_props)
  
  create_pass_function(
    function_suffix = "facet_row", 
    recipient_function = ".add_facet_row",
    arg_list = arg_list,
    doc_description = glue("#' Add faceting by row to a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_facet_col()], [vl_facet_wrap()]"),
    param_docs = param_docs)   
  
}

create_col_facet <- function(schema) {
  
  # Get all props...
  facet_props <- props(schema, list("$ref" = glue("#/definitions/FacetFieldDef")))
  facet_args <- paste(names(facet_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(facet_args)), collapse = ", ")
  
  param_docs <- get_param_docs(facet_props)
  
  create_pass_function(
    function_suffix = "facet_col", 
    recipient_function = ".add_facet_col",
    arg_list = arg_list,
    doc_description = glue("#' Add faceting by column to a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_facet_row()], [vl_facet_wrap()]"),
    param_docs = param_docs)   
  
}

create_wrap_facet <- function(schema) {
  
  # Get all props...
  facet_props <- props(schema, list("$ref" = glue("#/definitions/FacetFieldDef")))
  facet_args <- paste(names(facet_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'columns = 2', unique(facet_args)), collapse = ", ")
  
  param_docs <- paste0(
    get_param_docs(facet_props),
    "\n#' @param columns number of columns to add"
    )
  
  create_pass_function(
    function_suffix = "facet_wrap", 
    recipient_function = ".add_facet_wrap",
    arg_list = arg_list,
    doc_description = glue("#' Add faceting by column to a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_facet_row()], [vl_facet_wrap()]"),
    param_docs = param_docs)   
  
}

#' @export
create_facet_functions <- function(schema){
  c(
    create_row_facet(schema),
    create_col_facet(schema),
    create_wrap_facet(schema)
  )
}