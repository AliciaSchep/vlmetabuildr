#' vl_transform_{tolower(stringr::str_remove(trans,"Transform"))}
#' 
#' Add {trans} to a vega-lite spec.
#' @param spec A vega-lite spec
{param_docs}
#' @return A modified spec
#' @export
vl_{tolower(stringr::str_remove(trans,"Transform"))} <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out <- c(args_out, list(.trans = '{tolower(stringr::str_remove(trans,"Transform"))}'))
  rlang::exec(.add_transform, !!!args_out)
}}