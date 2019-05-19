
get_mark_props <- function(schema){
 props(schema, list("$ref" = "#/definitions/MarkDef"))
}

#' create_mark
#'
#' @param schema
#'
#' @return
#' @export
#' @name create_mark
#'
#' @examples
create_mark_generic <- function(schema){

  mark_props <- get_mark_props(schema)
  mark_args <- paste(names(mark_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(mark_args)), collapse = ", ")

  param_docs <- get_param_docs(mark_props)

  glue("\n#' vl_mark\n#' \n",
       "#' Add a mark to a vega-lite spec.\n",
       "#' @param spec A vega-lite spec\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "#' @name vl_mark\n",
       "vl_mark <- function({arg_list}) {{\n",
       "  pass_call(quote(.add_mark))\n",
       "}}\n", .trim = FALSE)

}


#' @param mark
#' @name create_mark
#' @export
create_mark <- function(mark, schema) {

  mark_props <- get_mark_props(schema)
  mark_args <- paste(names(mark_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(mark_args)), collapse = ", ")

  glue("\n#' @name vl_mark\n",
       "#' @export\n",
       "vl_mark_{mark} <- function({arg_list}) {{\n",
       "  pass_call(quote(.add_mark), add = list(.mark = '{mark}'))\n",
       "}}\n", .trim = FALSE)
}
