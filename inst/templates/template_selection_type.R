#' vl_add_{tolower(stringr::str_remove(type,"Selection"))}_selection
#'
#' Add selection to a vega-lite spec.
#' @param spec A vega-lite spec
#' @param name Name of selection
{param_docs}
#' @return A modified spec
#' @seealso [vl_add_single_selection()], [vl_add_multi_selection()],
#' [vl_add_interval_selection()], [vl_{type}()]
#' @export
vl_add_{tolower(stringr::str_remove(type,"Selection"))}_selection <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(type = '{tolower(stringr::str_remove(type,"Selection"))}'))
  rlang::exec(.add_selection, !!!args_out)
}}