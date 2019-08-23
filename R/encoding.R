create_encoding_functions <- function(schema) {
  
  encoding_options <- props(schema, list("$ref" = "#/definitions/Encoding"))
  
  c(
    purrr::map_chr(names(encoding_options), create_encoder, schema = schema),
    purrr::map_chr(names(encoding_options), create_encode_object, schema = schema)
  )
  
}




create_encoder <- function(enc, schema) {

  
  make_function( glue("#/definitions/Encoding/properties/{enc}"), 
                 schema, 
                 glue::glue("encode_{enc}"), 
                 ".add_encoding", 
                 priority_args = c("field","type"),
                 description = glue::glue("Add encoding for {enc} to a vega-lite spec."),
                 pass_to_adder = list(encoding = enc))
}


create_encode_object <- function(enc, schema) {

  # Get all props...
  encode_props <- props(schema, list("$ref" = glue("#/definitions/Encoding/properties/{enc}")))
  encode_args <- paste(names(encode_props), "NULL", sep = " = ")
  arg_list <- paste(unique(encode_args), collapse = ", ")

  param_docs <- get_param_docs(schema, glue("#/definitions/Encoding/properties/{enc}"))
  Enc <- capitalize(enc)
  
  template <- system.file("templates/template_object.R", package = 'vlmetabuildr')
  glargs <- list(obj = Enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)

}

