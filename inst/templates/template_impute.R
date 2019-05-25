#' vl_impute_{enc}
#' 
#' Add impute transform to {enc} encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
{param_docs}
#' @return A modified spec
#' @export
vl_impute_{enc} <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = '{enc}'))
  rlang::exec(.add_impute_to_encoding, !!!args_out)
}}