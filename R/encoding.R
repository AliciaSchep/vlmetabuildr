create_encoding_functions <- function(schema) {
  
  encoding_options <- props(schema, list("$ref" = "#/definitions/Encoding"))
  
  c(
    purrr::map_chr(names(encoding_options), create_encoder, schema = schema),
    purrr::map_chr(names(encoding_options), create_encode_object, schema = schema)
  )
  
}




create_encoder <- function(enc, schema) {

  # Get all props...
  encode_props <- props(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  encode_names <- unique(c("field", "type", names(encode_props)))
  encode_args <- paste(encode_names, "NULL", sep = " = ")
  arg_list <- paste(c('spec', encode_args), collapse = ", ")

  param_docs <- get_param_docs2(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))

  create_pass_function(
    function_suffix = glue("encode_{enc}"), 
    recipient_function = ".add_encoding",
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    doc_description = glue("#' Add encoding for {enc} to a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_encode()], [vl_{capitalize(enc)}()]"),
    param_docs = param_docs)   
  
}


create_encode_object <- function(enc, schema) {

  # Get all props...
  encode_props <- props(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  encode_args <- paste(names(encode_props), "NULL", sep = " = ")
  arg_list <- paste(unique(encode_args), collapse = ", ")

  param_docs <- get_param_docs2(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  Enc <- capitalize(enc)
  
  template <- system.file("templates/template_object.R", package = 'vlmetabuildr')
  glargs <- list(obj = Enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)

}

