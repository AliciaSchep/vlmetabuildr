create_repeat <- function(type, schema) {
  
  
  arg_list <- "spec, ..., columns = 2"
  
  param_docs <- "#' @param ... fields to use for repeat (strings)"
    
  
  if (type == "wrap") {
    param_docs <- paste(
      param_docs,
      "#' @param columns number of columns to use",
      sep = "\n"
    )
    arg_mod <- glue("args_out <- c(list(spec = spec, .type = '{type}'),list(...), columns = columns)")
  } else {
    arg_mod <- glue("args_out <- c(list(spec = spec, .type = '{type}'),list(...))")
  }
  
  opts <- c("col","row","wrap")
  see_also <- paste(glue("[vl_repeat_{opts[opts != type]}()]"), collapse = ", ")
  
  create_custom_pass_function(
    function_suffix = glue("repeat_{type}"), 
    recipient_function = ".add_repeat",
    arg_list = arg_list,
    modify_args = arg_mod,
    doc_description = glue("#' Add repeating by {type} to a vega-lite spec."),
    extra_docs = glue("#' @seealso {see_also}"),
    param_docs = param_docs)   
  
}



#' @export
create_repeat_functions <- function(schema){
  purrr::map_chr(c("row","col","wrap"), create_repeat, schema = schema)
}