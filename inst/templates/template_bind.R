#' vl_bind_{tolower(stringr::str_remove(type,"Bind"))}
#'
#' Add {type} to a slection within a vega-lite spec.
#' @param spec A vega-lite spec
#' @param selection_name Name of selection to add binding to 
#' @param projection_name Name of projection (field or encoding) within selection
#' to add binding to
{param_docs}
#' @return A modified spec
#' @seealso [vl_add_single_selection()], [vl_add_multi_selection()],
#' [vl_add_interval_selection()]
#' @export
vl_bind_{tolower(stringr::str_remove(type,"Bind"))} <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  rlang::exec(.add_binding, !!!args_out)
}}