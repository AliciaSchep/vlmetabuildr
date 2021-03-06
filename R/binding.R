
create_binding <- function(schema, name, ref) {
  
  reference <- glue("#/definitions/{ref}")
  suffix <- glue::glue("bind_{name}_input")
  
  spec_doc <- glue("#' @param spec An input vega-lite spec")
  extra_doc <- paste(
    "#' @param selection_name Name of selection to add binding to",
    "#' @param projection_name Name of projection (field or encoding) within selection",
    sep = "\n")
  param_docs <- get_param_docs(schema, reference, exclude = "input")
  
  docs <- make_docs_helper(
    glue("vl_{suffix}"),
    glue::glue("Add {name} binding to a vega-lite spec."),
    paste(spec_doc,extra_doc, param_docs, sep = "\n")
  )
  
  ## Make the inner function
  param_names <- get_params(schema, reference)
  modifier <- glue("  args <- .modify_args(list(input = '{name}'), {deparse_c(param_names)})")
  
  adder <- glue(".add_binding(args$spec, args$object, '{reference}', selection_name = args$extra$selection_name,
                projection_name = args$extra$projection_name)")
  
  inner_fn <- paste(
    modifier,
    adder, 
    sep = "\n  "
  )
  
  ## Get args
  args <- paste(c('projection_name', get_params(schema, reference, exclude = "input")), "NULL", sep = " = ")
  arg_list <- paste(c('spec', 'selection_name', args), collapse = ", ")
  
  make_function_helper(suffix, docs, inner_fn, arg_list)
  
}


 

  


#' @export
create_binding_functions <- function(schema){
  c(
    create_binding(schema, "radio", "BindRadioSelect"),
    create_binding(schema, "select", "BindRadioSelect"),
    create_binding(schema, "checkbox", "BindCheckbox"),
    create_binding(schema, "range", "BindRange")
  )
}