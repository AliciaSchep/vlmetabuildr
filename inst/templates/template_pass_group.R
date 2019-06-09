#' @name {doc_name}
#' @export
{extra_docs}
vl_{function_suffix} <- function({arg_list}) {{
  args_in <- rlang::fn_fmls_syms()
  args_eval <- lapply(args_in,eval, env = rlang::current_env())
  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]
  {modify_args}
  rlang::exec({recipient_function}, !!!args_out)
}}
