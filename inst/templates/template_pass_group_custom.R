#' @name {doc_name}
#' @export
vl_{function_suffix} <- function({arg_list}) {{
  {modify_args}
  rlang::exec({recipient_function}, !!!args_out)
}}
