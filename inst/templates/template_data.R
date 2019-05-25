#' vl_add_data
#'
#' Add data to a vega-lite spec.
#' @param spec A vega-lite spec
{param_docs}
#' @return A modified spec
#' @export
#' @name add_data
vl_add_data <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  rlang::exec(.add_data, !!!args_out)
}}