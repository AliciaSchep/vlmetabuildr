

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

  template <- system.file("templates/template_encode.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)

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
  
  template <- system.file("templates/template_object.R", package = 'vlmetabuildr')
  glargs <- list(obj = Enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)

}

#' @export
create_sort_for_encoding <- function(enc, schema){
  
  arg_list <- "spec, value"
  
  template <- system.file("templates/template_sort.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}


#' @export
create_sort_by_field_for_encoding <- function(enc, schema){
  
  sort_props <- props(schema, list("$ref" = "#/definitions/EncodingSortField"))
  sort_args <- paste(names(sort_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(sort_args)), collapse = ", ")
  
  param_docs <- get_param_docs(sort_props)
  
  template <- system.file("templates/template_sort_by_field.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_sort_by_encoding_for_encoding <- function(enc, schema){
  
  sort_props <- props(schema, list("$ref" = "#/definitions/SortByEncoding"))
  sort_args <- paste(names(sort_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(sort_args)), collapse = ", ")
  
  param_docs <- get_param_docs(sort_props)
  
  template <- system.file("templates/template_sort_by_encoding.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_axis_for_encoding <- function(enc, schema){
  
  axis_props <- props(schema, list("$ref" = "#/definitions/Axis"))
  axis_args <- paste(names(axis_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(axis_args), "remove = FALSE"), collapse = ", ")
  
  param_docs <- get_param_docs(axis_props)
  
  template <- system.file("templates/template_axis.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_scale_for_encoding <- function(enc, schema) {
  
  scale_props <- props(schema, list("$ref" = "#/definitions/Scale"))
  scale_args <- paste(names(scale_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(scale_args)), collapse = ", ")
  
  param_docs <- get_param_docs(scale_props)
  
  template <- system.file("templates/template_encode_param.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs, param = 'scale')
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_legend_for_encoding <- function(enc, schema) {
  
  legend_props <- props(schema, list("$ref" = "#/definitions/Legend"))
  legend_args <- paste(names(legend_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(legend_args)), collapse = ", ")
  
  param_docs <- get_param_docs(legend_props)
  
  template <- system.file("templates/template_encode_param.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs, param = 'legend')
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_condition_for_encoding <- function(enc, schema) {
  
  condition_props <- props(schema, list("$ref" = "#/definitions/ConditionalValueDef"))
  condition_args <- paste(names(condition_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(condition_args)), collapse = ", ")
  
  param_docs <- get_param_docs(condition_props)
  
  template <- system.file("templates/template_encode_param.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs, param = 'condition')
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}
