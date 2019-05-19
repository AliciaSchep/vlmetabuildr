

#' create_encoder
#'
#' @param enc
#' @param schema
#'
#' @return
#' @export
#'
#' @examples
create_encoder <- function(enc, schema) {

  # Get all props...
  encode_props <- props(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  encode_args <- paste(names(encode_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(encode_args)), collapse = ", ")

  param_docs <- get_param_docs(encode_props)

  glue("\n#' vl_encode_{enc}\n#' \n",
       "#' Add encoding for {enc} to a vega-lite spec.\n",
       "#' @param spec A vega-lite spec\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "vl_encode_{enc} <- function({arg_list}) {{\n",
       "  pass_call(quote(.add_encoding), add = list(.enc = '{enc}'))\n",
       "}}\n", .trim = FALSE)

}

