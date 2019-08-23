make_function <- function(
  reference,
  schema,
  suffix,
  adder_function,
  description = "",
  override_args = NULL,
  priority_args = NULL,
  pass_to_adder = NULL,
  doc_group = NULL
) {
  
  # Make the documentation
  if (is.null(doc_group)) {
    docs <- make_docs(reference, schema, suffix,  exclude_args = names(override_args), 
                      description = description)
  } else {
    docs <- make_docs_for_group(doc_group)
  }
  
  ## Make the inner function
  inner_fn <- make_function_innards(reference, schema, override_args, adder_function, pass_to_adder)
  
  ## Get args
  args <- make_arg_list(reference, schema, override_args, priority_args)
  
  ## Make the outer function
  fn <- glue::glue("vl_{suffix} <- function({args}){{\n{inner_fn}\n}}")
  
  # Combine docs and function
  glue::glue_collapse(c(docs, fn), sep = "\n", last = "\n")
}

make_function_innards <- function(reference, schema, override_args, adder_function, pass_to_adder) {
  
  param_names <- get_params(schema, reference)
  modifier <- glue::glue("  args <- .modify_args({deparse_c(override_args)}, {deparse_c(param_names)})")
  
  extras <- paste(c("args$extra", paste(names(pass_to_adder), purrr::map_chr(pass_to_adder, deparse_c), sep = " = ")), 
                       collapse = ", ")
  
  adder <- glue::glue("{adder_function}(args$spec, args$object, '{reference}', {extras})")
  
  paste(
    modifier,
    adder, 
    sep = "\n  "
  )
  
}

make_arg_list <- function(reference, schema, exclude_args, priority_args){
  
  param_names <- get_params(schema, reference, exclude_args)
  
  param_names <- unique(c(intersect(priority_args,param_names), param_names))
  args <- paste(param_names, "NULL", sep = " = ")
  arg_list <- paste(c('spec', args), collapse = ", ")
  
  arg_list
}

make_docs <- function(reference, schema, suffix,  exclude_args,  description = "") {
  
  title <- glue::glue("\n#' vl_{suffix}\n#'")
  
  desc <- roxy_wrap(description)
  
  spec_doc <- glue::glue("#' @param spec An input vega-lite spec")
  
  param_docs <- get_param_docs(schema, reference, exclude = exclude_args)

  returns <- "#' @return A modified Vega-Lite Spec"
  export <- "#' @export"
  
  paste(
    title,
    desc,
    spec_doc,
    param_docs,
    returns,
    export,
    sep = "\n"
  )
  
}

make_docs_for_group <- function(doc_group){
  glue::glue("\n#' @name {doc_group}\n#'@export\n", )
}

make_group_doc <- function(){
  
  
}




