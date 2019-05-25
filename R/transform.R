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
  transform_props <- props(schema, list("$ref" = glue("#/definitions/{trans}")))
  transform_args <- paste(names(transform_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(transform_args)), collapse = ", ")
  
  param_docs <- get_param_docs(transform_props)
  
  template <- system.file("templates/template_transform.R", package = 'vlmetabuildr')
  glargs <- list(trans = trans, arg_list = arg_list,
                    param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)

  
}

#' @export
get_enc_with_prop <- function(schema, prop) {
  encs <- names(props(schema, list("$ref" = "#/definitions/Encoding")))
  with_prop <-
    purrr::map_lgl(
      encs, 
      function(x) {
        prop_names <- names(
          props(schema, 
                list("$ref" = glue("#/definitions/Encoding/properties/{x}"))
          ))
        prop %in% prop_names
      }
    )
  encs[with_prop]
}

#' @export
create_bin_for_encoding <- function(enc, schema){
  
  bin_props <- props(schema, list("$ref" = "#/definitions/BinParams"))
  bin_args <- paste(names(bin_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(bin_args)), collapse = ", ")
  
  param_docs <- get_param_docs(bin_props)
  
  template <- system.file("templates/template_bin.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_impute_for_encoding <- function(enc, schema){
  
  impute_props <- props(schema, list("$ref" = "#/definitions/ImputeParams"))
  impute_args <- paste(names(impute_props), "NULL", sep = " = ")
  arg_list <- paste(c('spec', unique(impute_args)), collapse = ", ")
  
  param_docs <- get_param_docs(impute_props)
  
  template <- system.file("templates/template_impute.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_stack_for_encoding <- function(enc, schema){
  
  stack_options <- unlist(enums(schema, list("$ref" = "#/definitions/StackOffset")))
  stack_opt_list <- paste(c(paste("'",stack_options,"'", sep = ""),NA), 
                          collapse = ", ")
  stack_args <- glue::glue_data(
    list(opts = stack_opt_list),
    "stack = c({opts})"
  )
  arg_list <- paste(c('spec', unique(stack_args)), collapse = ", ")
  
  param_docs <- glue::glue_data(
    list(opts = stack_opt_list),
    "#' @param stack one of {stack_opt_list}"
  )
  
  template <- system.file("templates/template_stack.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

#' @export
create_aggregate_for_encoding <- function(enc, schema){
  
  agg_options <- unlist(enums(schema, list("$ref" = "#/definitions/Aggregate")))
  agg_opt_list <- paste(c(paste("'",agg_options,"'", sep = ""),NA), 
                          collapse = ", ")
  agg_args <- glue::glue_data(
    list(opts = agg_opt_list),
    "op = c({opts})"
  )
  arg_list <- paste(c('spec', unique(agg_args)), collapse = ", ")
  
  param_docs <- glue::glue_data(
    list(opts = agg_opt_list),
    "#' @param op Aggregation op, one of {agg_opt_list}"
  )
  
  template <- system.file("templates/template_aggregate.R", package = 'vlmetabuildr')
  glargs <- list(enc = enc, arg_list = arg_list,
                 param_docs = param_docs)
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}
