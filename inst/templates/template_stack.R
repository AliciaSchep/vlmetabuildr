#' vl_stack_{enc}
#' 
#' Add stack transform to {enc} encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
{param_docs}
#' @return A modified spec
#' @export
vl_stack_{enc} <- function({arg_list}) {{
  args_out <- list(spec = spec, .enc = '{enc}', stack = match.arg(stack))
  rlang::exec(.add_stack_to_encoding, !!!args_out)
}}