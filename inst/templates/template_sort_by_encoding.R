#' vl_sort_{enc}_by_encoding
#' 
#' Sort {enc} encoding by another encoding in a vega-lite spec.
#' @param spec A vega-lite spec.
{param_docs}
#' @return A modified spec
#' @seealso [vl_sort_{enc}_by_field()] to sort by another field, 
#' [vl_sort_{enc}] to sort by this encoding or a custom sort
#' @export
vl_sort_{enc}_by_encoding <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.enc = '{enc}'))
  rlang::exec(.add_sort_obj_to_encoding, !!!args_out)
}}