create_data_generic <- function(schema) {

  data_props <- props(schema, list("$ref" = "#/definitions/Data"))
  data_args <- paste(names(data_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(data_args)), collapse = ", ")

  param_docs <- get_param_docs(schema, "#/definitions/Data")

  create_pass_function(
    function_suffix = "add_data", 
    recipient_function = ".add_data",
    arg_list = arg_list,
    doc_description = "#' Add data to a vega-lite spec.",
    param_docs = param_docs)   

}
