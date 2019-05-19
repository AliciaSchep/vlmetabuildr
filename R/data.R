#' create_data
#'
#' @param schema
#'
#' @return
#' @export
#' @name create_data
#'
#' @examples
create_data_generic <- function(schema){

  data_props <- props(VL_SCHEMA, list("$ref" = "#/definitions/Data"))
  data_args <- paste(names(data_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(data_args)), collapse = ", ")

  param_docs <- get_param_docs(data_props)

  glue("\n#' vl_add_data\n#' \n",
       "#' Add data to a vega-lite spec.\n",
       "#' @param spec A vega-lite spec\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "#' @name add_data\n",
       "vl_add_data <- function({arg_list}) {{\n",
       "  pass_call(quote(.add_data))\n",
       "}}\n", .trim = FALSE)

}
