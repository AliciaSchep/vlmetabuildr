#' vl_{function_suffix}
#' 
{doc_description}
#' @param spec A vega-lite spec.
{param_docs}
#' @return A modified spec
#' @export
{extra_docs}
vl_{function_suffix} <- function({arg_list}) {{
  {modify_args}
  rlang::exec({recipient_function}, !!!args_out)
}}
