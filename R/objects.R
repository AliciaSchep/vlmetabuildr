#' @export
#' @name create_object
create_object <- function(obj, schema) {
  
  # Get all props...
  obj_props <- props(schema, list("$ref" = glue("#/definitions/{obj}")))
  obj_args <- paste(names(obj_props), "NULL", sep = " = ")
  arg_list <- paste(unique(obj_args), collapse = ", ")
  
  param_docs <- get_param_docs(obj_props)
  
  glue("\n#' vl_{obj}\n#' \n",
       "#' Create spec for {obj}.\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "#' @md\n",
       "vl_{obj} <- function({arg_list}) {{\n",
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  args_out\n",
       "}}\n", .trim = FALSE)
  
}