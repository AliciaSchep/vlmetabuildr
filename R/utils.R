capitalize <- function(x){
  first <- substring(x, first = 1L, last = 1L)
  rest <- substring(x, first = 2L, last = nchar(x))
  paste0(toupper(first),rest)
}

type_or_ref <- function(x){
  if (hasName(x,"$ref")){
    sp <- strsplit(x[["$ref"]],"/")[[1]]
    sp[length(sp)]
  } else if (hasName(x,"type")){
    paste(x[["type"]],collapse = " OR ")
  } else {
    "Varies"
  }
}

get_description <- function(x) {
  if (hasName(x, "description")) {
    purrr::pluck(x,"description")
  } else if (hasName(x, "enum")) {
    paste(purrr::pluck(x,"enum"), collapse = ", ")
  } else {
    ""
  }
}

get_param_docs <- function(properties) {

  d <- purrr::map_chr(properties, get_description)
  d <- stringr::str_replace_all(d, "\n","\n#' ")
  t <- purrr::map_chr(properties, type_or_ref)

  docs <- paste(d, glue("(type = {t})"))

  param_names <- unique(names(properties))

  param_desc <- purrr::map_chr(
    param_names,
    ~ paste(unique(docs[which(names(properties) == .)]), collapse = " OR ")
  )
  
  param_desc <- stringr::str_replace_all(
    param_desc,
    "_\\[([^[\\]_]]*)\\]_",
    "\\\\\\[\\1\\\\\\]")
  
  param_desc <- stringr::str_replace_all(
    param_desc,
    "%E2%80%93",
    "-"
  )

  paste("#' @param", param_names, param_desc, sep = " ", collapse = "\n")

}

create_pass_function <- function(function_suffix, 
                                 recipient_function,
                                 arg_list,
                                 modify_args = "",
                                 doc_description = "",
                                 extra_docs = "",
                                 param_docs = "" ) {

  template <- system.file(file.path("templates","template_pass.R"), 
                          package = 'vlmetabuildr')
  glargs <- list(
    function_suffix = function_suffix,
    recipient_function = recipient_function,
    arg_list = arg_list,
    modify_args = modify_args,
    doc_description = doc_description,
    extra_docs = extra_docs,
    param_docs = param_docs
  )
  
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

create_custom_pass_function <- function(function_suffix, 
                                 recipient_function,
                                 arg_list,
                                 modify_args,
                                 doc_description = "",
                                 extra_docs = "",
                                 param_docs = "" ) {
  
  template <- system.file(file.path("templates","template_pass_custom.R"), 
                          package = 'vlmetabuildr')
  glargs <- list(
    function_suffix = function_suffix,
    recipient_function = recipient_function,
    arg_list = arg_list,
    modify_args = modify_args,
    doc_description = doc_description,
    extra_docs = extra_docs,
    param_docs = param_docs
  )
  
  glue::glue_data(glargs, readr::read_file(template), .trim = FALSE)
  
}

create_function_for_encode_param <- function(
  enc,
  param,
  arg_list,
  param_docs ) {

  create_pass_function(
    function_suffix = glue("{param}_{enc}"), 
    recipient_function = glue(".add_{param}_to_encoding"),
    arg_list = arg_list,
    modify_args = glue("args_out <- c(args_out, list(.enc = '{enc}'))"),
    doc_description = glue("#' Add {param} to encoding for {enc} in a vega-lite spec."),
    extra_docs = glue("#' @seealso [vl_{enc}()]"),
    param_docs = param_docs
  )

}


