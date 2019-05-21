#' create_transform
#'
#' @param transform name of transform, e.g 'BinTransform'
#' @param schema imported json schema
#'
#' @return
#' @export
#'
#' @name create_transform
create_transform <- function(trans, schema) {
  
  # Get all props...
  trans_short <- tolower(stringr::str_remove(trans,"Transform"))
  transform_props <- props(schema, list("$ref" = glue("#/definitions/{trans}")))
  transform_args <- paste(names(transform_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(transform_args)), collapse = ", ")
  
  param_docs <- get_param_docs(transform_props)
  
  glue("\n#' vl_transform_{trans_short}\n#' \n",
       "#' Add {trans} to a vega-lite spec.\n",
       "#' @param spec A vega-lite spec\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "vl_transform_{trans_short} <- function({arg_list}) {{\n",
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  args_out <- c(args_out, list(.trans = '{trans_short}'))\n",
       "  rlang::exec(.add_transform, !!!args_out)\n",
       "}}\n", .trim = FALSE)
  
}