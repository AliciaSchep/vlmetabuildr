#' vl_aggregate_{enc}
#' 
#' Add aggregate transform to {enc} encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
{param_docs}
#' @return A modified spec
#' @export
vl_aggregate_{enc} <- function({arg_list}) {{
  args_out <- list(spec = spec, .enc = '{enc}', op = match.arg(op))
  rlang::exec(.add_aggregate_to_encoding, !!!args_out)
}}