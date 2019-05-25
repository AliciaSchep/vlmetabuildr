#' vl_{obj}
#' 
#' Create spec for {obj}.
{param_docs}
#' @return A modified spec
#' @export
vl_{obj} <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  args_out
}}