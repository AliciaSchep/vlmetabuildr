

#' create_encoder
#'
#' @param enc name of encoding, e.g 'x'
#' @param schema imported json schema
#'
#' @return
#' @export
#'
#' @name create_encoder
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
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  args_out <- c(args_out, list(.enc = '{enc}'))\n",
       "  rlang::exec(.add_encoding, !!!args_out)\n",
       "}}\n", .trim = FALSE)

}

#' @export
#' @name create_encoder
create_encode_object <- function(enc, schema) {

  # Get all props...
  encode_props <- props(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  encode_args <- paste(names(encode_props), "NULL", sep = " = ")
  arg_list <- paste(unique(encode_args), collapse = ", ")

  param_docs <- get_param_docs(encode_props)
  Enc <- capitalize(enc)
  glue("\n#' vl_{Enc}\n#' \n",
       "#' Create spec for {enc} encoding.\n",
       "{param_docs}\n",
       "#' @return A modified spec\n",
       "#' @export\n",
       "#' @md\n",
       "#' @seealso [vl_encode_{enc}()]\n",
       "vl_{Enc} <- function({arg_list}) {{\n",
       "  args_in <- rlang::fn_fmls_syms()\n",
       "  args_eval <- lapply(args_in,eval, env = rlang::current_env())\n",
       "  args_out <- args_eval[!vapply(args_eval,is.null,FALSE)]\n",
       "  args_out\n",
       "}}\n", .trim = FALSE)

}
